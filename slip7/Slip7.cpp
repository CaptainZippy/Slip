
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
        void operator()(Ast::Node* n) {
            assert(0);
        }
        void operator()(Ast::Definition* n) {

        }
        void operator()(Ast::Module* n) {
            out.write("namespace XX {");
            for (auto n : n->m_items) {
                Ast::dispatch(n, *this);
            }
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
