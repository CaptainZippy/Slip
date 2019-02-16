#include "pch/Pch.h"
#include "Backend.h"
#include "Ast.h"
#include "Io.h"

#if 0
namespace {
    using namespace std;
    using namespace Slip;
    struct Generator {
        string newVarId() {
            return string_format("_%i", m_counter++);
        }

        Io::TextOutput out;
        inline string dispatch(Ast::Node* n) {
            return Ast::dispatch(n, *this);
        }

        string operator()(Ast::Node* n) {
            assert(0);
            return "";
        }
        string operator()(Ast::Reference* n) {
            return n->m_target->m_name.std_str();
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
            out.write(string_format("%s %s = %s", n->m_type.name().c_str(), n->m_name, val.c_str()));
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
            out.write(string_format("%s %s;", n->m_type.name().c_str(), ret.c_str()));
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
            out.write(string_format("%s %s;", n->m_type.name().c_str(), ret.c_str()));
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

        string operator()(Ast::FunctionDecl* n) {
            out.begin(string_concat("\n", n->m_body->m_type.name(), " ", n->m_name, "("));
            const char* sep = "";
            for (auto a : n->m_args) {
                assert(a->m_type);
                assert(a->m_name);
                out.write(string_concat(sep, a->m_type.name(), " ", a->m_name));
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
            out.begin("namespace XX {");
            for (auto n : n->m_items) {
                dispatch(n);
            }
            out.end("}");
            return "";
        }
        int m_counter = 1;
    };
}
#endif

void Slip::Backend::generate(Ast::Module& module) {
    #if 0
    Generator g;
    Ast::dispatch(&module, g);
    #endif
}

