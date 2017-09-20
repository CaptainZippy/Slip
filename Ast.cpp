#include "Pch.h"
#include "Ast.h"

namespace Ast {
    #define AST_NODE(X) int X::tag() const { return Detail::TagOf<X>::Tag; }
    #include "Ast.inc"
    #undef AST_NODE

    REFLECT_BEGIN(Node)
    REFLECT_FIELD(m_type)
    REFLECT_END()

    REFLECT_BEGIN(Type)
    //REFLECT_PARENT(Node)
    REFLECT_FIELD(m_name)
    REFLECT_END()

    REFLECT_BEGIN(Decl)
    REFLECT_PARENT(Node)
    REFLECT_END()

    REFLECT_BEGIN(Number)
    REFLECT_PARENT(Node)
    REFLECT_TO_STRING(Number::toString)
    REFLECT_END()

    REFLECT_BEGIN(Module)
    REFLECT_PARENT(Node)
    REFLECT_FIELD(m_items)
    REFLECT_END()

    REFLECT_BEGIN(FunctionCall)
    REFLECT_PARENT(Node)
    //REFLECT_FIELD(m_func)
    REFLECT_FIELD(m_args)
    REFLECT_END()

    REFLECT_BEGIN(Symbol)
    REFLECT_PARENT(Node)
    //REFLECT_TO_STRING(Symbol::toString)
    REFLECT_FIELD(m_name)
    //REFLECT_FIELD(m_sym)
    REFLECT_END()

    REFLECT_BEGIN(FunctionDecl)
    REFLECT_PARENT(Node)
    REFLECT_FIELD(m_arg_syms)
    REFLECT_FIELD(m_body)
    REFLECT_END()

    REFLECT_BEGIN(Sequence)
    REFLECT_PARENT(Node)
    REFLECT_FIELD(m_items)
    REFLECT_END()

    REFLECT_BEGIN(Argument)
    REFLECT_PARENT(Node)
    REFLECT_FIELD(m_sym)
    REFLECT_END()

    REFLECT_BEGIN(Reference)
    REFLECT_PARENT(Node)
    //REFLECT_FIELD(m_sym)
    REFLECT_END()

    REFLECT_BEGIN(Scope)
    REFLECT_PARENT(Node)
    REFLECT_FIELD(m_child)
    REFLECT_END()

    REFLECT_BEGIN(Definition)
    REFLECT_PARENT(Node)
    REFLECT_FIELD(m_sym)
    REFLECT_FIELD(m_value)
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

}
