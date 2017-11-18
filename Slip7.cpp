
// 1. lex + tree
// 2. tree shape check
// 3. Lex basics
// 4. Lex 

#include "Pch.h"
#include "SourceManager.h"
#include "Lex.h"
#include "Reflect.h"
#include "Ast.h"
#include "Parse.h"


namespace Sema {
    struct TypeChecker {

        void operator()(Ast::Node* n) {
            assert(0);
        }
        void operator()(Ast::Module* n) {
            assign(n, &Ast::s_typeVoid);
        }
        void operator()(Ast::Number* n) {
            assign(n, &Ast::s_typeDouble);
        }
        void operator()(Ast::FunctionCall* n) {
            assign(n, &Ast::s_typeDouble);
            //if (n->m_type) { m_type = &s_typeDouble; }
        }
        void operator()(Ast::Argument* n) {
            assert(n->m_sym->m_decltype);
            assign(n, &Ast::s_typeDouble);
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
            //if (any_of(m_args, [](Argument*s) { return s->m_type == nullptr; })) { return; }
            assign(n, &Ast::s_typeF_double_double);
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
            assign(n, &Ast::s_typeVoid);
        }

        Result resolve(array_view<Ast::Node*> nodes) {
            std::vector<Info*> todo;
            for (auto node : nodes) {
                auto inf = new Info{};
                todo.push_back(inf);
                node->m_data = inf;
                inf->node = node;
            }
            for (auto node : nodes) {
                Ast::dispatch(node, *this);
            }
            auto res = _resolve(todo);
            for (auto node : nodes) {
                delete static_cast<TypeChecker::Info*>(node->m_data);
                node->m_data = nullptr;
            }
            return res;
        }

    protected:
        
        struct Info {
            Ast::Node* node{ nullptr };
            Ast::Type* declared{ nullptr };
            Info* equal{ nullptr };
            unsigned num_prec{ 0 };
            std::vector<Info*> deps;
        };

        Result _resolve1(Info* inf) {
            assert(inf->node);
            assert(inf->node->m_type == nullptr);
            if (inf->declared) {
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

        Result _resolve(std::vector<Info*>& todo) {
            while (todo.size()) {
                std::vector<Info*> next;
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

        Info* info(Ast::Node* n) {
            return static_cast<Info*>(n->m_data);
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

        void compatible(Ast::Node* tgt, Ast::Node* src) {
            /*auto id = info(lhs);
            auto is = info(ts);
            id->compatible = ts;
            is->deps.push_back(id);*/
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
                for (int i = 0; i < count; ++i) {
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

    Result type_check(Ast::Node* top_node) {
        TypeChecker checker;
        std::vector<Ast::Node*> nodes{ top_node };
        collect_nodes(nodes, top_node);
        return checker.resolve(nodes);
    }
}

namespace Code {
    struct Generator {
        int pushScope() {
            m_stack.push_back(m_counter++);
            return m_stack.back();
        }
        void popScope() {
            m_stack.pop_back();
        }

        Io::TextOutput out;
        inline void dispatch(Ast::Node* n) {
            return Ast::dispatch(n, *this);
        }
        
        void operator()(Ast::Node* n) {
            assert(0);
        }
        void operator()(Ast::Argument* n) {
            out.write(n->m_sym->text());
        }
        void operator()(Ast::Number* n) {
            out.write(n->m_num->text());
        }
        void operator()(Ast::Definition* n) {
            out.nl();
            out.write(string_format("%s = ",
                n->m_sym->text().c_str()));
            dispatch(n->m_value);
            out.write(";");
        }
        void operator()(Ast::Sequence* n) {
            for (auto a : array_view_t::make(n->m_items).rtrim(1)) {
                dispatch(a);
                out.nl();
            }
            out.write(string_format("_%i = ", m_stack.back()));
            dispatch(n->m_items.back());
            out.write(";");
            out.nl();
        }
        void operator()(Ast::Scope* n) {
            out.begin("{");
            dispatch(n->m_child);
            out.end("}");
            out.nl();
        }
        void operator()(Ast::FunctionDecl* n) {
            out.nl();
            out.begin(string_format("int %s(", n->m_name->text().c_str()));
            out.nl();
            const char* sep = "";
            for (auto a : n->m_args) {
                assert(a->m_type);
                assert(a->m_sym);
                out.write(string_format("%s %s%s", a->m_type->m_name.c_str(), a->m_sym->text().c_str(), sep));
                sep = ", ";
            }
            out.write(") {");
            out.nl();
            int scope = pushScope();
            out.write(string_format("int _%i;", scope));
            dispatch(n->m_body);
            out.write(string_format("return _%i;", scope));
            popScope();
            out.nl();
            out.end("}");
            out.nl();
        }
        void operator()(Ast::FunctionCall* n) {
            if (auto d = dynamic_cast<Ast::FunctionDecl*>(n->m_func)) {
                out.write(d->m_name->text());
            }
            else {
                assert(0);
            }
            out.write("(");
            auto sep = "";
            for (auto a : n->m_args) {
                out.write(sep);
                sep = ", ";
                dispatch(a);
            }
            out.write(")");
        }

        void operator()(Ast::Module* n) {
            out.begin("namespace XX {");
            for (auto n : n->m_items) {
                dispatch(n);
            }
            out.end("}");
        }
        std::vector<int> m_stack;
        int m_counter = 1;
    };
    void generate(Ast::Module* module) {
        Generator g;
        Ast::dispatch(module, g);
    }
    
}


int main( int argc, const char* argv[] ) {
    if( argc < 2 ) {
        Error.fmt( "Need a script to run" );
        return 1;
    }
    try {
        Lex::SourceManager smanager;
        Lex::List* Lex = Lex::parse_file( smanager, argv[1] );
        verify( Lex );
        Ast::Module* ast = Parse::module( Lex );
        verify(ast);
        Ast::print(ast);
        printf("\n\n");
        Sema::type_check(ast);
        printf("\n\n");
        Reflect::printVar(ast);
        printf("\n\n");
        Code::generate(ast);
        printf("\n\n");
    }
    catch( float ) {
        return 1;
    }
    return 0;
}
