
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
    void collect_nodes(std::vector<Ast::Node*>& out, Reflect::Var var) {
        switch (var.type->kind) {
            case Reflect::Kind::Pointer: {
                if (var.type->sub->extends<Ast::Node>()) {
                    auto p = *static_cast<Ast::Node**>(var.addr);
                    if (p) {
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
        
    bool type_check(Ast::Node* top_node) {
        std::vector<Ast::Node*> todo{ top_node };
        collect_nodes(todo, top_node);
        // very dumb, just iterate
        while (todo.size()) {
            for (auto n : todo) {
                n->type_check();
            }
            auto pre = todo.size();
            erase_if(todo, [](Ast::Node* n) { return n->m_type != nullptr;  });
            if (todo.size() == pre) {
                int x; x = 0;
            }
        }
        return true;
    }
}

namespace Code {
    struct Generator {
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
            out.write(string_format("%s %s = ",
                n->m_sym->m_type->m_name.c_str(), n->m_sym->text().c_str()));
            dispatch(n->m_value);
            out.write(";");
        }
        void operator()(Ast::Sequence* n) {
            for (auto a : n->m_items) {
                dispatch(a);
            }
        }
        void operator()(Ast::Scope* n) {
            out.begin("{");
            dispatch(n->m_child);
            out.end("}");
        }
        void operator()(Ast::FunctionDecl* n) {
            out.nl();
            out.begin(string_format("int %s(", n->m_name->text().c_str()));
            out.nl();
            const char* sep = "";
            for (auto a : n->m_arg_syms) {
                out.write(string_format("%s %s%s", a->m_type->m_name.c_str(), a->text().c_str(), sep));
                sep = ", ";
            }
            out.end(") {");
            dispatch(n->m_body);
            out.write("}");
            out.nl();
        }
        void operator()(Ast::FunctionCall* n) {
            out.write("(");
            if (auto d = dynamic_cast<Ast::FunctionDecl*>(n->m_func)) {
                out.write(d->m_name->text());
            }
            else {
                assert(0);
            }
            out.write(")");
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
        Reflect::printVar(ast);
        Sema::type_check(ast);
        Reflect::printVar(ast);
        Code::generate(ast);
    }
    catch( float ) {
        return 1;
    }
    return 0;
}
