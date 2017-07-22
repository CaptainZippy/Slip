#pragma once

namespace Ast {
    struct Node;
    struct Type;
    struct FunctionCall;
    struct FunctionDecl;
    struct Number;

    struct Node : Reflect::AbstractReflected {
        REFLECT_DECL();
        virtual void type_check() {
            assert(0);
        }

        Type* m_type{ nullptr };
    };
    REFLECT_BEGIN(Node)
        REFLECT_FIELD(m_type)
    REFLECT_END()


    struct Type : Node {
        REFLECT_DECL();
        Type(const std::string& s);
        std::string m_name;
    };
    REFLECT_BEGIN(Type)
        //REFLECT_PARENT(Node)
        REFLECT_FIELD(m_name)
    REFLECT_END()

    static Ast::Type s_typeType("Type");
    static Ast::Type s_typeInt("int");
    static Ast::Type s_typeDouble("double");
    static Ast::Type s_typeVoid("void");
    static Ast::Type s_typeF_double_double("double(*)(double)");

    Type::Type(const std::string& s) : m_name(s) {
        m_type = &s_typeType;
    }

    struct Decl : Node {
        REFLECT_DECL();
    };

    REFLECT_BEGIN(Decl)
        REFLECT_PARENT(Node)
    REFLECT_END()

    struct Number : Node {
        REFLECT_DECL();
        Number( Lexer::Number* n) : m_num(n) {}
        Lexer::Number* m_num;

        void type_check() {
            m_type = &s_typeDouble;
        }
        static std::string toString(const void* p) {
            auto n = static_cast<const Number*>(p);
            return n->m_num->text();
        }
    };
    REFLECT_BEGIN(Number)
        REFLECT_PARENT(Node)
        REFLECT_TO_STRING(Number::toString)
    REFLECT_END()

    struct Module : Node {
        REFLECT_DECL();
        void type_check() override {
            m_type = &s_typeVoid;
        }
        std::vector<Node*> m_items;
    };

    REFLECT_BEGIN(Module)
        REFLECT_PARENT(Node)
        REFLECT_FIELD(m_items)
    REFLECT_END()

    struct FunctionCall : public Node {
        REFLECT_DECL();
        const Node* m_func{ nullptr };
        std::vector< Node* > m_args;
        FunctionCall( const Node* func, std::vector< Node* >&& args )
            : m_func(func), m_args(args) {
        }

        void type_check() {
            if (m_func->m_type) {
                m_type = &s_typeDouble;//TODO
            }
        }
    };

    REFLECT_BEGIN(FunctionCall)
        REFLECT_PARENT(Node)
        //REFLECT_FIELD(m_func)
        REFLECT_FIELD(m_args)
    REFLECT_END()

    struct Symbol : public Node {
        REFLECT_DECL();

        Symbol(std::string&& n, Lexer::Symbol* s) : m_name(n), m_sym(s) {
        }

        void type_check() {
        }

        std::string m_name;
        Lexer::Symbol* m_sym;

        std::string text() const {
            return m_sym->text();
        }

        static std::string toString(const void* obj) {
            auto sym = static_cast<const Symbol*>(obj);
            return string_format("%s\"}", sym->dynamicType()->name, sym->m_name.c_str());
        }
    };

    REFLECT_BEGIN(Symbol)
        REFLECT_PARENT(Node)
        //REFLECT_TO_STRING(Symbol::toString)
        REFLECT_FIELD(m_name)
        //REFLECT_FIELD(m_sym)
    REFLECT_END()

    struct FunctionDecl : public Node {
        REFLECT_DECL();

        std::vector< Symbol* > m_arg_syms;
        Node* m_body = nullptr;

        FunctionDecl() {}
        FunctionDecl( std::vector<Symbol*>& args, Node* body ) {
            m_arg_syms.swap( args );
            m_body = body;
        }
        void type_check() {
            if (m_body->m_type == nullptr) return;
            if (any_of(m_arg_syms, [](Symbol*s) { return s->m_type == nullptr; })) { return; }
            m_type = &s_typeF_double_double;//TODO
        }
    };

    REFLECT_BEGIN(FunctionDecl)
        REFLECT_PARENT(Node)
        REFLECT_FIELD(m_arg_syms)
        REFLECT_FIELD(m_body)
    REFLECT_END()


    struct Sequence : public Node {
        REFLECT_DECL();
        std::vector<Node*> m_items;
        void type_check() {
            if (m_items.size() == 0 ) {
                m_type = &s_typeVoid;
            }
            else if (auto t = m_items.back()->m_type) {
                m_type = t;
            }
        }
    };

    REFLECT_BEGIN(Sequence)
        REFLECT_PARENT(Node)
        REFLECT_FIELD(m_items)
    REFLECT_END()

    struct Argument : public Node {
        REFLECT_DECL();
        Argument(Symbol* s) : m_sym(s) {
        }
        Symbol* m_sym;

        void type_check() {
            if (m_sym->m_type) {
                m_type = m_sym->m_type;
            }
        }
    };

    REFLECT_BEGIN(Argument)
        REFLECT_PARENT(Node)
        REFLECT_FIELD(m_sym)
    REFLECT_END()

    struct Reference : public Node {
        REFLECT_DECL();
        Reference(Node* s) : m_sym(s) {
        }
        Node* m_sym;
    };

    REFLECT_BEGIN(Reference)
        REFLECT_PARENT(Node)
        //REFLECT_FIELD(m_sym)
    REFLECT_END()

    struct Scope : public Node {
        REFLECT_DECL();
        Scope(Node* c) : m_child(c) {}
        Node* m_child{ nullptr };

        void type_check() {
            if (m_child->m_type) {
                m_type = m_child->m_type;
            }
        }
    };

    REFLECT_BEGIN(Scope)
        REFLECT_PARENT(Node)
        REFLECT_FIELD(m_child)
    REFLECT_END()


    struct Definition : public Node {
        REFLECT_DECL();

        Symbol* m_sym = nullptr;
        Node* m_value = nullptr;

        Definition() {}
        Definition( Symbol* sym, Node* value ) : m_sym( sym ), m_value( value ) {}

        virtual void type_check() {
            if (auto t = m_value->m_type) {
                m_type = t;
                if (m_sym->m_type == nullptr) {
                    m_sym->m_type = t;
                }
                m_type = &s_typeVoid;
            }
        }
    };

    REFLECT_BEGIN(Definition)
        REFLECT_PARENT(Node)
        REFLECT_FIELD(m_sym)
		REFLECT_FIELD(m_value)
    REFLECT_END()
}