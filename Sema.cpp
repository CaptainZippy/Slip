#include "pch/Pch.h"
#include "Ast.h"
#include "Sema.h"

namespace Sema {
    typedef std::function< void() > Callback;

    struct TypeDep {
        unsigned num_deps{ 0 };
        virtual ~TypeDep() {}
        virtual void trigger() = 0;
    };

    Ast::Type* _create_function_type(Ast::Type* ret, array_view<Ast::Type*> sig) {
        std::string name; //TODO: intern these types
        name.append("(");
        const char* sep = "";
        for (auto a : sig) {
            name.append(sep); sep = ", ";
            name.append(a->m_name);
        }
        name.append(") -> ");
        name.append(ret->m_name);
        auto r = new Ast::Type(name);
        r->m_extra = ret;
        r->m_args = sig.std_vec();
        return r;
    }

    struct FuncInfo {
        TypeInfo* ret{ nullptr };
        std::vector<TypeInfo*> args;
    };


    struct TypeInfo {
        bool dispatched{ false };
        Ast::Node* node{ nullptr };
        Ast::Type* type{ nullptr };
        FuncInfo* func{ nullptr };
        std::vector<TypeInfo*> isa;
        std::vector<TypeDep*> deps;

        void add_dep(TypeDep* dep) {
            assert(dep);
            dep->num_deps += 1;
            deps.push_back(dep);
        }

        void resolve(Ast::Type* t) {
            assert(t);
            assert(type == nullptr || type == t);
            type = t;
            for (auto dep : deps) {
                assert(dep->num_deps > 0);
                dep->num_deps -= 1;
                if (dep->num_deps == 0) {
                    dep->trigger();
                }
            }
        }

        void print(Io::TextOutput& out) {
            out.begin(string_format("TypeNode %p\n", this));
            out.begin(string_format("node %p %s\n", node, node->dynamicType()->name));
            out.begin(string_format("isa %u\n", unsigned(isa.size())));
            for (auto i : isa) {
                out.write(string_format("isa %p\n", i) );
            }
            out.end("\n");
            out.end("\n");
        }
        void print() {
            Io::TextOutput out;
            print(out);
        }
    };

    struct FuncDeclSynth : TypeDep {
        TypeInfo* target{ nullptr }; // will assign to target->type
        TypeInfo* ret{ nullptr }; // func ret
        std::vector<TypeInfo*> args; // func args

        virtual void trigger() {
            assert(target);
            assert(target->type == nullptr);
            assert(ret);
            assert(ret->type);
            auto rtype = dynamic_cast<Ast::Type*>(ret->type);
            assert(rtype);
            std::vector<Ast::Type*> at;
            for (auto a : args) {
                assert(a->type);
                at.push_back(a->type);
            }
            target->resolve( _create_function_type(rtype, at) );
        }
    };

    struct FuncApplCheck : TypeDep {
        TypeInfo* target{ nullptr }; // will check this target
        TypeInfo* func{ nullptr };
        std::vector<TypeInfo*> args;

        virtual void trigger() {
            FuncInfo* fi = func->func;
            assert(fi);
            target->resolve(fi->ret->type);
        }
    };

    struct ForwardSynth : TypeDep {
        TypeInfo* target{ nullptr }; // will check this target
        TypeInfo* source{ nullptr };

        virtual void trigger() {
            target->resolve(source->type);
        }
    };


    struct ConstraintBuilder {

        void operator()(Ast::Node* n) {
            assert(n->m_type);
        }

        void operator()(Ast::Module* n) {
            n->m_type = &Ast::s_typeVoid;
            for (auto i : n->m_items) {
                dispatch(i);
            }
        }

        void operator()(Ast::Number* n) {
            //TODO
        }

        void operator()(Ast::String* n) {
            n->m_type = &Ast::s_typeString;
            //isa(n, &Ast::s_typeString);
        }

        void operator()(Ast::FunctionCall* n) {
            dispatch(n->m_func);
            for (auto a : n->m_args) {
                dispatch(a);
            }
            applicable(n, n->m_func, n->m_args);
        }

        void operator()(Ast::Argument* n) {
            assert(n->m_type);
        }

        void operator()(Ast::Sequence* n) {
            if (n->m_items.size()) {
                isa(n, n->m_items.back());
                for (auto i : n->m_items) {
                    dispatch(i);
                }
            }
            else {
                n->m_type = &Ast::s_typeVoid;
            }
        }

        void operator()(Ast::FunctionDecl* n) {
            auto in = info(n);
            in->func = new FuncInfo;
            if (n->m_returnType) {
                synth(n, n->m_returnType, n->m_args);
                isa(n->m_body, n->m_returnType);
                in->func->ret = info(n->m_returnType);
            }
            else {
                synth(n, n->m_body, n->m_args);
                in->func->ret = info(n->m_body);
            }
            for (auto a : n->m_args) {
                dispatch(a);
                in->func->args.push_back(info(a));
            }
            dispatch(n->m_body);
        }

        void operator()(Ast::Reference* n) {
            isa(n, n->m_target);
            dispatch(n->m_target);
            if (auto f = n->m_target->m_data->func) {
                info(n)->func = f;
            }
        }

        void operator()(Ast::Scope* n) {
            if (n->m_child) {
                isa(n->m_child, n);
                dispatch(n->m_child);
            }
            else {
                isa(n, &Ast::s_typeVoid);
            }
        }

        void operator()(Ast::Definition* n) {
            //isa(n, &Ast::s_typeVoid); //fixme
            if (n->m_type) {
                isa(n->m_value, n);
            }
            else {
                isa(n, n->m_value);
            }
            dispatch(n->m_value);
        }

        void operator()(Ast::If* n) {
            isa(n->m_true, n);
            isa(n->m_false, n);
            dispatch(n->m_true);
            dispatch(n->m_false);
            //isa(n->m_cond, &Ast::s_typeBool);
            dispatch(n->m_cond);
        }

        void operator()(Ast::Cond* n) {
            for (auto c : n->m_cases) {
                //isa(c.first, &Ast::s_typeBool);
                dispatch(c.first);
                dispatch(c.second);
                isa(c.second, n);
            }
        }

    public:

        std::vector<Ast::Node*> m_visited;
        std::vector<TypeInfo*> m_targets;
        std::vector<TypeDep*> m_typeDeps;
        Ast::Node* m_topNode{ nullptr };

        Result build(Ast::Node* top) {
            m_topNode = top;
            return dispatch(top);
        }


        Result dispatch(Ast::Node* top) {
            if (top->m_data == nullptr || top->m_data->dispatched ==false) {
                info(top)->dispatched = true;
                Ast::dispatch(top, *this);
            }
            return Result::OK;
        }

        ~ConstraintBuilder() {
            for (auto tgt : m_targets) {
                delete tgt;
            }
        }

    protected:

        TypeInfo* info(Ast::Node* node) {
            if (!node->m_data) {
                auto inf = new TypeInfo{};
                inf->node = node;
                m_targets.push_back(inf);
                node->m_data = inf;
            }
            return node->m_data;
        }

        void applicable(Ast::Node* tgt, Ast::Node* func, array_view<Ast::Node*> args) {
            auto tc = new FuncApplCheck();
            tc->target = info(tgt);
            tc->func = info(func);
            FuncInfo* fi = tc->func->func;
            assert(fi);
            tc->func->add_dep(tc);
            assert(fi->args.size() == args.size());
            for (unsigned i = 0; i < args.size(); ++i) {
                auto ai = info(args[i]);
                ai->isa.push_back(fi->args[i]);
                tc->args.push_back(ai);
            }
            tc->target->isa.push_back(fi->ret);
            m_typeDeps.push_back(tc);
        }

        void isa(TypeInfo* tgt, TypeInfo* src) {
            if (find(tgt->isa, src) == tgt->isa.end()) {
                tgt->isa.push_back(src);
            }
        }

        void isa(Ast::Node* tgt, Ast::Node* src) {
            auto itgt = info(tgt);
            auto isrc = info(src);
            isa(itgt, isrc);
        }



        void synth(Ast::Node* tgt, Ast::Node* ret, array_view<Ast::Argument*> args) {
            auto ts = new FuncDeclSynth();
            ts->target = info(tgt);
            ts->ret = info(ret);
            ts->ret->add_dep(ts);
            for (auto a : args) {
                auto ia = info(a);
                ia->add_dep(ts);
                ts->args.push_back(ia);
            }
            m_typeDeps.push_back(ts);
        }
    };


    struct ConstraintSolver {
        Result dispatch(ConstraintBuilder& builder) {

            // apply explicit declarations in source
            for (auto target : builder.m_targets) {
                assert(target->type == nullptr);
                assert(target->node);
                if (auto t = target->node->m_type) {
                    target->resolve(t);
                }
                target = nullptr;
            }

            // 
            std::vector<TypeInfo*> todo = builder.m_targets;
            do {
                std::vector<TypeInfo*> again;
                for (auto target : todo) {
                    //HACK: if we didn't resolve and there is exactly one type constraint
                    // then lets take the constraint as the type.
                    bool resolved = false;
                    if (target->type == nullptr) {
                        if (target->isa.size()==1) {
                            if (auto t = target->isa[0]->type) {
                                target->resolve( t );
                                resolved = true;
                            }
                        }
                        else { //TODO: multiple intersecting constraints
                            std::set<Ast::Type*> types;
                            std::for_each(target->isa.begin(), target->isa.end(),
                                [&types](const TypeInfo* t) { types.insert(t->type); });
                            if (types.size() == 1) {
                                if (auto t = *types.begin()) {
                                    target->resolve(t);
                                    resolved = true;
                                }
                            }
                        }

                        if (resolved == false) {
                            again.push_back(target);
                        }
                    }
                }
                if (again.size() == todo.size()) {
                    Ast::print(builder.m_topNode);
                    for (auto a : again) {
                        a->print();
                    }
                    RETURN_RES_IF_REACHED(Result::ERR, "Failed to resolve");
                }
                again.swap(todo);
            } while (todo.size());
            print(builder);

            // type checks!
            for (auto target : builder.m_targets) {
                for (auto is : target->isa) {
                    assert(target->type == is->type);
                }
            }

            // Write results back to the ast
            for (auto target : builder.m_targets) {
                target->node->m_type = target->type;
            }

            return Result::OK;
        }
    protected:

        void print(ConstraintBuilder& builder) {
            Io::TextOutput io("deps.dgml");
            io.begin("<?xml version=\"1.0\" encoding=\"utf-8\"?>\n");
            io.begin("<DirectedGraph xmlns=\"http://schemas.microsoft.com/vs/2009/dgml\">\n");
            io.begin("<Nodes>\n");
            for (auto src : builder.m_targets) {
                io.write(string_format("<Node Id='0x%p' Category='Node' Label='", src));
                io.write(src->node->dynamicType()->name);
                Ast::print(src->type, io);
                io.write("' />\n");
            }
            io.end("</Nodes>\n");
            io.begin("<Links>\n");
            for (auto src : builder.m_targets) {
                for (auto d : src->isa) {
                    io.write(string_format("<Link Source='0x%p' Target='0x%p' Category='Compatible'/>\n", src, d));
                }
            }
            /*for (auto syn : builder.m_synths) {
                io.write(string_format("<Link Source='0x%p' Target='0x%p' Category='SynthRet'/>\n", syn->target, syn->ret));
                for (auto d : syn->args) {
                    io.write(string_format("<Link Source='0x%p' Target='0x%p' Category='SynthArg'/>\n", syn->target, d));
                }
            }
            for (auto& syn : builder.m_checks) {
                io.write(string_format("<Link Source='0x%p' Target='0x%p' Category='CheckFunc'/>\n", syn->target, syn->func));
                for (auto a : syn->args) {
                    io.write(string_format("<Link Source='0x%p' Target='0x%p' Category='CheckArg'/>\n", syn->func, a));
                }
            }*/
            io.end("</Links>\n");
            io.begin("<Categories>\n");
            io.write("<Category Id=\"Compatible\"  StrokeDashArray=\"1\"/>\n");
            io.write("<Category Id=\"SynthArg\" StrokeThickness=\"3\" Stroke=\"#ff00ff00\" />\n");
            io.write("<Category Id=\"SynthRet\" StrokeThickness=\"3\" Stroke=\"#ffffff00\"/>\n");
            io.write("<Category Id=\"CheckArg\" StrokeThickness=\"1\" Stroke=\"#ff00ffff\"/>\n");
            io.end("</Categories>\n");
            io.end("</DirectedGraph>\n");
        }
    };
}

Result Sema::type_check(Ast::Node* top_node) {

    ConstraintBuilder builder;
    RETURN_IF_FAILED( builder.build(top_node) );

    ConstraintSolver solver;
    RETURN_IF_FAILED(solver.dispatch(builder));

    return Result::OK;
}


