
// 1. lex + tree
// 2. tree shape check
// 3. Lex basics
// 4. Lex 

#include "pch/Pch.h"
#include "Source.h"
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
            out.write( n->m_target->m_name->text() );
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
    Result generate(Ast::Module* module) {
        Generator g;
        Ast::dispatch(module, g);
        return Result::OK;
    }
}

Result compile(const char* fname) {
    Lex::SourceManager smanager;
    Lex::List* lex;
    RETURN_IF_FAILED(Lex::parse_file(smanager, fname, &lex));
    Ast::Module* ast;
    RETURN_IF_FAILED(Parse::module(lex, &ast));
    verify(ast);
    Ast::print(ast);
    printf("\n\n");
    RETURN_IF_FAILED(Sema::type_check(ast));
    Ast::print(ast);
    printf("\n\n");
    RETURN_IF_FAILED(Code::generate(ast));
    return Result::OK;
}


int main( int argc, const char* argv[] ) {
    if( argc < 2 ) {
        Error.fmt( "Need a script to run" );
        return 1;
    }
    return compile(argv[1]).isOk() ? 0 : 1;
}
