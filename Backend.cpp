#include "pch/Pch.h"
#include "Backend.h"
#include "Ast.h"
#include "Io.h"

namespace {
    using namespace std;
    using namespace Slip;
    struct Generator {
        string newVarId() {
            return string_format("_%i", m_counter++);
        }

        Io::TextOutput& out;
        inline string dispatch(Ast::Node* n) {
            return Ast::dispatch<string>(n, *this);
        }

        string operator()(Ast::Node* n) {
            assert(0);
            return "";
        }
        string operator()(Ast::Reference* n) {
            if( auto t = dynamic_cast<Ast::Named*>( n->m_target ) ) {
                return sanitize(t->name());
            }
            return "??";
        }
        string operator()(Ast::Argument* n) {
            return n->m_name.std_str();
        }
        string operator()(Ast::Number* n) {
            return n->m_num;
        }
        string operator()(Ast::String* n) {
            return string_concat("\"", n->m_str, "\"_str");
        }
        string operator()(Ast::Definition* n) {
            auto val = dispatch(n->m_value);
            out.write(string_format("%s %s = %s", n->m_type->name().c_str(), n->m_name, val.c_str()));
            return n->m_name.std_str();
        }
        string operator()(Ast::Sequence* n) {
            for (auto a : n->items().rtrim(1)) {
                dispatch(a);
                out.nl();
            }
            return dispatch(n->m_items.back());
        }
#if 0
        string operator()(Ast::Scope* n) {
            auto id = newVarId();
            out.write(string_format("%s %s", n->m_type->m_name.c_str(), n->m_name));
            out.begin(" {\n");
            auto r = dispatch(n->m_child);
            out.end("}\n");
            out.write(string_format("%s = %s", id.c_str(), n->m_name));
            return id;
        }
#endif
        string operator()(Ast::If* n) {
            auto ret = newVarId();
            out.write(string_format("%s %s;", n->m_type->name().c_str(), ret.c_str()));
            out.begin(" {\n");
            auto cond = dispatch(n->m_cond);
            out.begin(string_format("if(%s) {\n", cond.c_str()));
            string t = dispatch(n->m_true);
            out.write(string_concat(ret, " = ", t, ";\n"));
            out.end("}\n");
            out.begin("else {\n");
            string f = dispatch(n->m_false);
            out.write(string_concat(ret, " = ", f, ";\n"));
            out.end("}");
            out.end("}\n");
            return ret;
        }

        string operator()(Ast::Cond* n) {
            auto ret = newVarId();
            out.write(string_format("%s %s;", n->m_type->name().c_str(), ret.c_str()));
            out.begin(" {\n");
            for (auto c : n->m_cases) {
                auto cond = dispatch(c.first);
                out.begin(string_format("if(%s) {\n", cond.c_str()));
                string t = dispatch(c.second);
                out.write(string_concat(ret, " = ", t, ";\n"));
                out.end("}\n");
                out.begin("else {\n");
            }
            for (auto c : n->m_cases) {
                (void)c;
                out.end("}");
            }
            out.end("\n}\n");
            return ret;
        }

        static string sanitize( string_view inp ) {
            string s;
            size_t cur = 0;
            while(true) {
                auto p = inp.find_first_of( "?!"sv, cur );
                if( p == string::npos ) {
                    s.append( inp.begin() + cur, inp.end() );
                    return s;
                }
                s.append( inp.begin() + cur, inp.begin() + cur + p );
                s.append( "_"sv );
                cur = p + 1;
            }
        }

        string operator()(Ast::FunctionDecl* n) {
            out.begin(string_concat("\n", n->m_type->m_callable[0]->name(), " ", n->m_name, "("));
            const char* sep = "";
            for (auto a : n->m_args) {
                assert(a->m_type);
                assert(a->m_name);
                out.write(string_concat(sep, a->m_type->name(), " ", a->m_name));
                sep = ", ";
            }
            out.write(") {\n");
            string ret = dispatch(n->m_body);
            out.write(string_concat("return ", ret, ";\n"));
            out.end("}\n");
            return n->m_name.std_str();
        }

        string operator()(Ast::FunctionCall* n) {
            auto func = dispatch(n->m_func);
            vector<string> args;
            for (auto a : n->m_args) {
                args.push_back(dispatch(a));
            }
            auto retId = newVarId();
            out.write(string_concat("auto ", retId, " = ", func, "("));
            auto sep = "";
            for (auto a : args) {
                out.write(string_concat(sep, a));
                sep = ", ";
            }
            out.write(");\n");
            return retId;
        }

        string operator()(Ast::Module* n) {
            out.begin( "#include<stdio.h>\n" );
            out.begin("namespace XX {");
            out.write( "inline bool eq_(int a, int b) { return a==b; }\n" );
            out.write( "inline int add(int a, int b) { return a+b; }\n" );
            out.write( "inline int sub(int a, int b) { return a-b; }\n" );
            out.write( "inline int puti(int a) { return printf(\"%i\\n\", a); }\n" );
            for (auto n : n->m_items) {
                dispatch(n);
            }
            out.end("}");
            return "";
        }
        int m_counter = 1;
    };
}

void Slip::Backend::generate(Ast::Module& module, Io::TextOutput& out) {
    Generator g{ out };
    Ast::dispatch<string>(&module, g);
}

