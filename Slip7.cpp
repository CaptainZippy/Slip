
// 1. lex + tree
// 2. tree shape check
// 3. Lex basics
// 4. Lex 

#include "pch/Pch.h"
#include "SourceManager.h"
#include "Lex.h"
#include "Reflect.h"
#include "Ast.h"
#include "Parse.h"
#include "Type.h"

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
        void operator()(Ast::Reference* n) {
            if (auto d = dynamic_cast<Ast::Named*>(n->m_target)) {
                out.write( d->m_name->text() );
            }
            else {
                assert(0);
            }
        }
        void operator()(Ast::Argument* n) {
            out.write(n->m_name->text());
        }
        void operator()(Ast::Number* n) {
            out.write(n->m_num->text());
        }
        void operator()(Ast::Definition* n) {
            out.nl();
            out.write(string_concat(n->m_type->text(), " ", 
                n->m_name->text(), " = "));
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
            out.begin(string_concat(n->m_body->m_type->text(), " ", n->m_name->text(), "("));
            out.nl();
            const char* sep = "";
            for (auto a : n->m_args) {
                assert(a->m_type);
                assert(a->m_name);
                out.write(string_concat(a->m_type->text(), " ", a->m_name->text(), sep));
                sep = ", ";
            }
            out.write(") {");
            out.nl();
            int scope = pushScope();
            out.write(string_format("%s _%i;", n->m_body->m_type->text(), scope));
            dispatch(n->m_body);
            out.write(string_format("return _%i;", scope));
            popScope();
            out.nl();
            out.end("}");
            out.nl();
        }
        void operator()(Ast::FunctionCall* n) {
            dispatch(n->m_func);
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
        Ast::print(ast);
        printf("\n\n");
        Code::generate(ast);
        printf("\n\n");
    }
    catch( float ) {
        return 1;
    }
    return 0;
}
