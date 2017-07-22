#include "Pch.h"
#include "Reflect.h"

namespace Reflect {
    namespace Detail {
        struct Output {
            void begin(const char* s) {
                printf("%s", s);
                m_indent.push_back(' ');
            }
            void write(const char* s) {
                if (s) printf("%s", s);
            }
            void field(const char* s) {
                printf("%s = ", s);
            }
            void write(const void* s) {
                printf("%p", s);
            }
            void end(const char* s = nullptr) {
                m_indent.erase(m_indent.size() - 1);
                write(s);
            }
            void nl() {
                printf("\n%s", m_indent.c_str());
            }
            std::string m_indent;
        };
        void printVar(Output& out, Var top) {
            if (top.addr == nullptr) {
                out.write("null");
                return;
            }
            switch (top.type->kind) {
                case Kind::Pointer: {
                    auto obj = *(void**)top.addr;
                    if (obj) {
                        auto sub = top.type->sub;
                        Var e{ obj, sub->dynamicType(obj) };
                        printVar(out, e);
                    }
                    else {
                        out.write("null");
                    }
                    break;
                }
                case Kind::Record: {
                    out.begin(top.type->name);
                    out.write("{");
                    std::vector<const Reflect::Type*> chain;
                    for (auto c = top.type; c; c = c->parent) {
                        chain.push_back(c);
                    }
                    for (auto c : reversed(chain)) {
                        if (c->toString) {
                            out.write(c->toString(top.addr).c_str());
                        }
                        else {
                            for (auto f : c->fields) {
                                //if (f.name == "m_type") continue;
                                out.nl();
                                //out.field(f.name.c_str());
                                Var v = top[f];
                                printVar(out, v);
                            }
                        }
                    }
                    out.end("}");
                    break;
                }
                case Kind::Array: {
                    auto et = top.type->sub;
                    char* s = ((char**)top.addr)[0];
                    char* e = ((char**)top.addr)[1];
                    unsigned count = unsigned((e - s) / et->size);
                    out.begin("[");
                    for (unsigned i = 0; i < count; ++i) {
                        if (i != 0) out.nl();
                        Var e{ s + i * et->size, et };
                        printVar(out, e);
                    }
                    out.end("]");
                    break;
                }
                case Kind::String: {
                    std::string s = top.type->toString(top.addr);
                    out.write(string_format("\"%s\"", s.c_str()).c_str());
                    break;
                }
                default:
                    out.write("???");
                    break;
            }
        }
    }
}

void Reflect::printVar(Var top) {
    Detail::Output output;
    Detail::printVar(output, top);
    output.nl();
}