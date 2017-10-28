#pragma once
#include "Reflect.h"
#include "Lex.h"

namespace Ast {
    #define AST_NODE(X) struct X;
    #include "Ast.inc"
    #undef AST_NODE

#define AST_DECL() \
    REFLECT_DECL(); \
    int tag() const override

    namespace Flags {
        const unsigned Child = 1; // Default is "ref"
        const unsigned Abbrev = 1; // Default is "ref"
    };

    struct Node : Reflect::AbstractReflected {
        REFLECT_DECL();
        virtual int tag() const;

        virtual void type_check() {
            assert(0);
        }

        Type* m_type{ nullptr };
    };
    
    struct Type : Node {
        AST_DECL();
        Type(const std::string& s);
        void type_check() override {
        }
        std::string m_name;
    };

    extern Ast::Type s_typeType;
    extern Ast::Type s_typeInt;
    extern Ast::Type s_typeDouble;
    extern Ast::Type s_typeVoid;
    extern Ast::Type s_typeF_double_double;


    struct Decl : Node {
        AST_DECL();
    };


    struct Number : Node {
        AST_DECL();
        Number( Lex::Number* n) : m_num(n) {}
        Lex::Number* m_num;

        void type_check() {
            m_type = &s_typeDouble;
        }
        static std::string toString(const void* p) {
            auto n = static_cast<const Number*>(p);
            return n->m_num->text();
        }
    };
    

    struct Module : Node {
        AST_DECL();
        void type_check() override {
            m_type = &s_typeVoid;
        }

        std::vector<Node*> m_items;
    };


    struct FunctionCall : public Node {
        AST_DECL();
        Node* m_func{ nullptr };
        std::vector< Node* > m_args;
        FunctionCall( Node* func, std::vector< Node* >&& args )
            : m_func(func), m_args(args) {
        }

        void type_check() {
            if (m_func->m_type) {
                m_type = &s_typeDouble;//TODO
            }
        }
    };

    struct Argument : public Node {
        AST_DECL();
        Argument(Lex::Symbol* s) : m_sym(s) {
        }
        Lex::Symbol* m_sym;

        void type_check() {
            assert(m_sym->m_decltype);
            m_type = &s_typeDouble;
        }
    };

    struct FunctionDecl : public Node {
        AST_DECL();

        Lex::Symbol* m_name = nullptr;
        std::vector< Argument* > m_args;
        Node* m_body = nullptr;

        FunctionDecl() {}
        FunctionDecl( Lex::Symbol* name, std::vector<Argument*>& args, Node* body )
            : m_name(name), m_body(body) {
            m_args.swap( args );
        }
        void type_check() {
            if (m_body->m_type == nullptr) return;
            if (any_of(m_args, [](Argument*s) { return s->m_type == nullptr; })) { return; }
            m_type = &s_typeF_double_double;//TODO
        }
    };


    struct Sequence : public Node {
        AST_DECL();
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

    
    


    struct Reference : public Node {
        AST_DECL();
        Reference(Node* s) : m_target(s) {
        }
        virtual void type_check() {
            if (auto t = m_target->m_type) {
                m_type = t;
            }
        }
        Node* m_target;
    };


    struct Scope : public Node {
        AST_DECL();
        Scope(Node* c) : m_child(c) {}
        Node* m_child{ nullptr };

        void type_check() {
            if (m_child->m_type) {
                m_type = m_child->m_type;
            }
        }
    };


    struct Definition : public Node {
        AST_DECL();

        Lex::Symbol* m_sym = nullptr;
        Node* m_value = nullptr;

        Definition() {}
        Definition( Lex::Symbol* sym, Node* value ) : m_sym( sym ), m_value( value ) {}

        virtual void type_check() {
            if (auto t = m_value->m_type) {
                m_type = t;
                m_type = &s_typeVoid;
            }
        }
    };

    void print(Node* node);
    void print(Node* node, Io::TextOutput& out);

    namespace Detail {
        template<typename T> struct TagOf;
        #define AST_NODE(X) template<> struct TagOf<X> { enum { Tag = __LINE__}; };
        #include "Ast.inc"
        #undef AST_NODE
    }

    template <typename Handler>
    auto dispatch(Node* n, Handler&& handler) {
        switch (n->tag()) {
            #define AST_NODE(X) case Detail::TagOf<X>::Tag: return handler(static_cast<X*>(n));
            #include "Ast.inc"
            #undef AST_NODE
        }
    }
}