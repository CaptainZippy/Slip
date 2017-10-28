#include "Pch.h"
#include "Ast.h"

namespace Ast {
    #define AST_NODE(X) int X::tag() const { return Detail::TagOf<X>::Tag; }
    #include "Ast.inc"
    #undef AST_NODE

    REFLECT_BEGIN(Node)
    //REFLECT_FIELD2(m_type, Flags::Abbrev)
    REFLECT_END()

    REFLECT_BEGIN(Type)
    REFLECT_PARENT(Node)
    REFLECT_FIELD(m_name)
    REFLECT_END()

    REFLECT_BEGIN(Decl)
    REFLECT_PARENT(Node)
    REFLECT_END()

    REFLECT_BEGIN(Number)
    REFLECT_PARENT(Node)
    REFLECT_FIELD(m_num)
    REFLECT_END()

    REFLECT_BEGIN(Module)
    REFLECT_PARENT(Node)
    REFLECT_FIELD2(m_items, Flags::Child)
    REFLECT_END()

    REFLECT_BEGIN(FunctionCall)
    REFLECT_PARENT(Node)
    //REFLECT_FIELD(m_func)
    REFLECT_FIELD2(m_args, Flags::Child)
    REFLECT_END()

    REFLECT_BEGIN(FunctionDecl)
    REFLECT_PARENT(Node)
    REFLECT_FIELD(m_name)
    REFLECT_FIELD2(m_args, Flags::Child)
    REFLECT_FIELD2(m_body, Flags::Child)
    REFLECT_END()

    REFLECT_BEGIN(Sequence)
    REFLECT_PARENT(Node)
    REFLECT_FIELD2(m_items, Flags::Child)
    REFLECT_END()

    REFLECT_BEGIN(Argument)
    REFLECT_PARENT(Node)
    REFLECT_FIELD(m_sym)
    REFLECT_END()

    REFLECT_BEGIN(Reference)
    REFLECT_PARENT(Node)
    REFLECT_FIELD(m_target)
    REFLECT_END()

    REFLECT_BEGIN(Scope)
    REFLECT_PARENT(Node)
    REFLECT_FIELD(m_child)
    REFLECT_END()

    REFLECT_BEGIN(Definition)
    REFLECT_PARENT(Node)
    REFLECT_FIELD(m_sym)
    REFLECT_FIELD2(m_value, Flags::Child)
    REFLECT_END()
}


namespace Ast {

    Ast::Type s_typeType("Type");
    Ast::Type s_typeInt("int");
    Ast::Type s_typeDouble("double");
    Ast::Type s_typeVoid("void");
    Ast::Type s_typeF_double_double("double(*)(double)");

    Type::Type(const std::string& s) : m_name(s) {
        m_type = &s_typeType;
    }

    
    void print(Reflect::Var top, Io::TextOutput& out, bool abbrev) {
        if(auto f = top.type->toString ) {
            std::string s = (*f)(top.addr);
            out.write(s);
            return;
        }
        switch (top.type->kind) {
            case Reflect::Kind::Array: {
                auto et = top.type->sub;
                char* s = ((char**)top.addr)[0];
                char* e = ((char**)top.addr)[1];
                unsigned count = unsigned((e - s) / et->size);
                for (unsigned i = 0; i < count; ++i) {
                    Reflect::Var e{ s + i * et->size, et };
                    print(e, out, false);
                }
                break;
            }
            case Reflect::Kind::Record: {
                std::vector<const Reflect::Type*> chain;
                for (auto c = top.type; c; c = c->parent) {
                    chain.push_back(c);
                }
                if (abbrev) {
                    for (auto c : reversed(chain)) {
                        for (auto f : c->fields) {
                            if ((f.flags & (Flags::Abbrev|Flags::Child)) == Flags::Abbrev) {
                                out.write(string_format(" %s={", f.name.c_str()));
                                print(top[f], out, true);
                                out.write("}");
                            }
                        }
                    }
                }
                else {
                    out.nl();
                    out.write(string_format("%s "/*0x%p*/, top.type->name, top.addr));
                    for (auto c : reversed(chain)) {
                        for (auto f : c->fields) {
                            if ((f.flags & Flags::Child) == 0) {
                                out.write(string_format(" %s={", f.name.c_str()));
                                print(top[f], out, true);
                                out.write("}");
                            }
                        }
                    }

                    out.begin(nullptr);
                    for (auto c : reversed(chain)) {
                        for (auto f : c->fields) {
                            if (f.flags & Flags::Child) {
                                print(top[f], out, false);
                            }
                        }
                    }
                    out.end();
                }
                break;
            }
            case Reflect::Kind::Pointer: {
                if (void* obj = *(void**)top.addr) {
                    auto sub = top.type->sub;
                    Reflect::Var e{ obj, sub->dynamicType(obj) };
                    print(e, out, abbrev);
                }
                else {
                    out.write("null");
                }
                break;
            }
            case Reflect::Kind::String: {
                std::string s = top.type->toString(top.addr);
                out.write(string_format("\"%s\"", s.c_str()));
                break;
            }
            default: {
                verify(false);
            }
        }
    }

    void print(Node* node) {
        Io::TextOutput out;
        Reflect::Var top{ node };
        print(top, out, false);
        out.nl();
    }
}
