#include "pch/Pch.h"
#include "Backend.h"
#include "Ast.h"

namespace {
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
        std::string operator()(Ast::String* n) {
            return string_concat("\"", n->m_str, "\"_str");
        }
        std::string operator()(Ast::Definition* n) {
            auto val = dispatch(n->m_value);
            out.write(string_format("%s %s = %s", n->m_type->m_name.c_str(), n->m_name, val.c_str()));
            return n->m_name.std_str();
        }
        std::string operator()(Ast::Sequence* n) {
            for (auto a : n->items().rtrim(1)) {
                dispatch(a);
                out.nl();
            }
            return dispatch(n->m_items.back());
        }
#if 0
        std::string operator()(Ast::Scope* n) {
            auto id = newVarId();
            out.write(string_format("%s %s", n->m_type->m_name.c_str(), n->m_name));
            out.begin(" {\n");
            auto r = dispatch(n->m_child);
            out.end("}\n");
            out.write(string_format("%s = %s", id.c_str(), n->m_name));
            return id;
        }
#endif
        std::string operator()(Ast::If* n) {
            auto ret = newVarId();
            out.write(string_format("%s %s;", n->m_type->m_name.std_str(), ret.c_str()));
            out.begin(" {\n");
            auto cond = dispatch(n->m_cond);
            out.begin(string_format("if(%s) {\n", cond.c_str()));
            std::string t = dispatch(n->m_true);
            out.write(string_concat(ret, " = ", t, ";\n"));
            out.end("}\n");
            out.begin("else {\n");
            std::string f = dispatch(n->m_false);
            out.write(string_concat(ret, " = ", f, ";\n"));
            out.end("}");
            out.end("}\n");
            return ret;
        }

        std::string operator()(Ast::Cond* n) {
            auto ret = newVarId();
            out.write(string_format("%s %s;", n->m_type->m_name.std_str(), ret.c_str()));
            out.begin(" {\n");
            for (auto c : n->m_cases) {
                auto cond = dispatch(c.first);
                out.begin(string_format("if(%s) {\n", cond.c_str()));
                std::string t = dispatch(c.second);
                out.write(string_concat(ret, " = ", t, ";\n"));
                out.end("}\n");
                out.begin("else {\n");
            }
            for (auto c : n->m_cases) {
                out.end("}");
            }
            out.end("\n}\n");
            return ret;
        }

        std::string operator()(Ast::FunctionDecl* n) {
            out.begin(string_concat("\n", n->m_body->m_type->m_name, " ", n->m_name, "("));
            const char* sep = "";
            for (auto a : n->m_args) {
                assert(a->m_type);
                assert(a->m_name);
                out.write(string_concat(sep, a->m_type->m_name, " ", a->m_name));
                sep = ", ";
            }
            out.write(") {\n");
            std::string ret = dispatch(n->m_body);
            out.write(string_concat("return ", ret, ";\n"));
            out.end("}\n");
            return n->m_name.std_str();
        }

        std::string operator()(Ast::FunctionCall* n) {
            auto func = dispatch(n->m_func);
            std::vector<std::string> args;
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
}

Result Backend::generate(Ast::Module* module) {
    Generator g;
    Ast::dispatch(module, g);
    return Result::OK;
}
