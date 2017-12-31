
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
        std::string newVarId() {
            return string_format("_%i", m_counter++);
        }

        Io::TextOutput out;
        inline std::string dispatch(Ast::Node* n) {
            return Ast::dispatch(n, *this);
        }
        
        std::string operator()(Ast::Node* n) {
            assert(0);
            return "";
        }
        std::string operator()(Ast::Reference* n) {
            return n->m_target->m_name.std_str();
        }
        std::string operator()(Ast::Argument* n) {
            return n->m_name.std_str();
        }
        std::string operator()(Ast::Number* n) {
            return n->m_num;
        }
        std::string operator()(Ast::Definition* n) {
            auto val = dispatch(n->m_value);
            out.write(string_format("%s %s = %s", n->m_type->m_name.c_str(), n->m_name, val.c_str()));
            return n->m_name.std_str();
        }
        std::string operator()(Ast::Sequence* n) {
            for (auto a : array_view_t::make(n->m_items).rtrim(1)) {
                dispatch(a);
                out.nl();
            }
            return dispatch(n->m_items.back());
        }
#if 0
        std::string operator()(Ast::Scope* n) {
            auto id = newVarId();
            out.write(string_format("%s %s", n->m_type->m_name.c_str(), n->m_name));
            out.begin("{");
            auto r = dispatch(n->m_child);
            out.end("}");
            out.nl();
            out.write(string_format("%s = %s", id.c_str(), n->m_name));
            return id;
        }
#endif
        std::string operator()(Ast::If* n) {
            auto ret = newVarId();
            out.write(string_format("%s %s;", n->m_type->m_name.std_str(), ret.c_str()));
            out.begin("{");
            out.nl();
            auto cond = dispatch(n->m_cond);
            out.begin(string_format("if(%s) {", cond.c_str()));
            out.nl();
            std::string t = dispatch(n->m_true);
            out.write(string_concat(ret, " = ", t, ";"));
            out.nl();
            out.end("}");
            out.nl();
            out.begin("else {");
            out.nl();
            std::string f = dispatch(n->m_false);
            out.write(string_concat(ret, " = ", f, ";"));
            out.nl();
            out.end("}}");
            out.nl();
            return ret;
        }

        std::string operator()(Ast::Cond* n) {
            auto ret = newVarId();
            out.write(string_format("%s %s;", n->m_type->m_name.std_str(), ret.c_str()));
            out.begin("{");
            out.nl();
            for (auto c : n->m_cases) {
                auto cond = dispatch(c.first);
                out.begin(string_format("if(%s) {", cond.c_str()));
                out.nl();
                std::string t = dispatch(c.second);
                out.write(string_concat(ret, " = ", t, ";"));
                out.nl();
                out.end("}");
                out.nl();
                out.begin("else {");
                out.nl();
            }
            for (auto c : n->m_cases) {
                out.end("}");
            }
            out.end("}");
            out.nl();
            return ret;
        }

        std::string operator()(Ast::FunctionDecl* n) {
            out.nl();
            out.begin(string_concat(n->m_body->m_type->m_name, " ", n->m_name, "("));
            out.nl();
            const char* sep = "";
            for (auto a : n->m_args) {
                assert(a->m_type);
                assert(a->m_name);
                out.write(string_concat(sep, a->m_type->m_name, " ", a->m_name));
                sep = ", ";
            }
            out.write(") {");
            out.nl();
            std::string ret = dispatch(n->m_body);
            out.nl();
            out.write(string_concat("return ", ret, ";"));
            out.end("}");
            out.nl();
            return n->m_name.std_str();
        }

        std::string operator()(Ast::FunctionCall* n) {
            auto func = dispatch(n->m_func);
            std::vector<std::string> args;
            for (auto a : n->m_args) {
                args.push_back( dispatch(a) );
            }
            auto retId = newVarId();
            out.write(string_concat("auto ", retId, " = ", func, "("));
            auto sep = "";
            for (auto a : args) {
                out.write(string_concat(sep, a));
                sep = ", ";
            }
            out.write(");");
            out.nl();
            return retId;
        }

        std::string operator()(Ast::Module* n) {
            out.begin("namespace XX {");
            for (auto n : n->m_items) {
                dispatch(n);
            }
            out.end("}");
            return "";
        }
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
