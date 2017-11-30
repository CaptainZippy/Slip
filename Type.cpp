#include "pch/Pch.h"
#include "Ast.h"
#include "Type.h"

namespace Sema {
    typedef std::function< void() > Callback;

    struct TypeInfo {
        Ast::Node* node{ nullptr };
        Ast::Type* declared{ nullptr };
        TypeInfo* equal{ nullptr };
        unsigned num_prec{ 0 };
        std::vector<TypeInfo*> deps;
        std::vector<TypeInfo*> compatible;
        Callback pre_resolve_cb;
    };
    struct TypeChecker {

        void operator()(Ast::Node* n) {
            assert(n->m_type);
        }
        void operator()(Ast::Module* n) {
            assign(n, &Ast::s_typeVoid);
        }
        void operator()(Ast::Number* n) {
            //TODO
        }
        void operator()(Ast::String* n) {
            assign(n, &Ast::s_typeString);
        }
        void operator()(Ast::FunctionCall* n) {
            std::vector<Ast::Node*> deps;
            deps.push_back(n->m_func);
            deps.insert(deps.end(), n->m_args.begin(), n->m_args.end());
            depends(n, deps, [deps, n, this]() {
                assign(n, deps[0]->m_type->m_extra);
            });
        }
        void operator()(Ast::Argument* n) {
            assert(n->m_name->m_decltype);
            assert(n->m_type);
        }
        void operator()(Ast::Sequence* n) {
            if (n->m_items.size()) {
                equal(n, n->m_items.back());
            }
            else {
                assign(n, &Ast::s_typeVoid);
            }
        }
        void operator()(Ast::FunctionDecl* n) {
            std::vector<Ast::Node*> deps;
            deps.push_back(n->m_body);
            /*if (n->m_returnType) {
                deps.push_back(n->m_returnType);
            }*/
            deps.insert(deps.begin(), n->m_args.begin(), n->m_args.end());
            depends(n, deps, [deps,n,this]() {
                n->m_type = this->_create_function_type(deps);
            });
        }
        void operator()(Ast::Reference* n) {
            equal(n, n->m_target);
        }
        void operator()(Ast::Scope* n) {
            if (n->m_child) {
                equal(n, n->m_child);
            }
            else {
                assign(n, &Ast::s_typeVoid);
            }
        }
        void operator()(Ast::Definition* n) {
            equal(n, n->m_value);
        }

        Result resolve(array_view<Ast::Node*> nodes) {
            std::vector<TypeInfo*> todo;
            for (auto node : nodes) {
                auto inf = new TypeInfo{};
                todo.push_back(inf);
                node->m_data = inf;
                inf->node = node;
            }
            for (auto node : nodes) {
                Ast::dispatch(node, *this);
            }
            auto res = _resolve(todo);
            for (auto node : nodes) {
                delete static_cast<TypeInfo*>(node->m_data);
                node->m_data = nullptr;
            }
            return res;
        }

    protected:
        
        Result _resolve1(TypeInfo* inf) {
            assert(inf->node);
            if (inf->pre_resolve_cb) {
                inf->pre_resolve_cb();
            }
            if (inf->node->m_type) {
            }
            else if (inf->declared) {
                inf->node->m_type = inf->declared;
            }
            else {
                assert(inf->equal);
                assert(inf->equal->node);
                assert(inf->equal->node->m_type);
                inf->node->m_type = inf->equal->node->m_type;
            }
            for (auto dep : inf->deps) {
                dep->num_prec -= 1;
            }

            return Result::OK;
        }

        Result _resolve(std::vector<TypeInfo*>& todo) {
            while (todo.size()) {
                std::vector<TypeInfo*> next;
                for (auto cur : todo) {
                    if (cur->num_prec == 0) {
                        _resolve1(cur);
                    }
                    else {
                        next.push_back(cur);
                    }
                }
                if (todo.size() == next.size()) {
                    return Result::ERR;
                }
                todo.swap(next);
            }

            return Result::OK;
        }

        TypeInfo* info(Ast::Node* n) {
            return static_cast<TypeInfo*>(n->m_data);
        }

        void assign(Ast::Node* node, Ast::Type* t) {
            info(node)->declared = t;
        }

        void equal(Ast::Node* tgt, Ast::Node* src) {
            auto itgt = info(tgt);
            auto isrc = info(src);
            itgt->equal = isrc;
            itgt->num_prec += 1;
            isrc->deps.push_back(itgt);
        }

        void depends(Ast::Node* tgt, array_view<Ast::Node*> deps, Callback&& cb ) {
            auto itgt = info(tgt);
            itgt->pre_resolve_cb = cb;
            for (auto d : deps) {
                auto id = info(d);
                itgt->num_prec += 1;
                id->deps.push_back(itgt);
            }
        }

        Ast::Type* _create_function_type(array_view<Ast::Node*> sig) {
            std::string name;
            name.append(sig[0]->m_type->text());
            name.append(" (");
            const char* sep = "";
            for (auto a : sig.ltrim(1)) {
                name.append(sep); sep = ", ";
                name.append(a->m_type->text());
            }
            name.append(")");
            auto r = new Ast::Type(name);
            r->m_extra = sig[0]->m_type;
            return r;
        }
    };

    void collect_nodes(std::vector<Ast::Node*>& out, Reflect::Var var) {
        switch (var.type->kind) {
            case Reflect::Kind::Pointer: {
                if (var.type->sub->extends<Ast::Node>()) {
                    auto p = *static_cast<Ast::Node**>(var.addr);
                    if (p && std::find(out.begin(), out.end(), p)==out.end()) {
                        out.push_back(p);
                        collect_nodes(out, p);
                    }
                }
                break;
            }
            case Reflect::Kind::Array: {
                auto a = static_cast<std::vector<char>*>(var.addr);
                size_t esize = var.type->sub->size;
                size_t bsize = a->size();
                assert(bsize % esize == 0);
                size_t count = bsize / esize;
                for (size_t i = 0; i < count; ++i) {
                    collect_nodes(out, Reflect::Var(&a->at(0) + i * esize, var.type->sub));
                }
                break;
            }
            case Reflect::Kind::Record: {
                for (const Reflect::Type* c = var.type; c; c = c->parent) {
                    for (auto& f : c->fields) {
                        collect_nodes(out, var[f]);
                    }
                }
                break;
            }
            case Reflect::Kind::String:
            case Reflect::Kind::Void:
                break;
            default:
                assert(0);
        }
    }
}

Result Sema::type_check(Ast::Node* top_node) {
    TypeChecker checker;
    std::vector<Ast::Node*> nodes{ top_node };
    collect_nodes(nodes, top_node);
    return checker.resolve(nodes);
}


