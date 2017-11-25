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
        const unsigned Abbrev = 2; // Default is "ref"
    };

    struct Node : Reflect::AbstractReflected {
        REFLECT_DECL();
        virtual int tag() const;

        Type* m_type{ nullptr };
        void* m_data{ nullptr };
    };

     struct Named : public Node {
        AST_DECL();
        Lex::Symbol* m_name = nullptr;

        Named(Lex::Symbol* sym) : m_name(sym) {}
    };

    struct Type : Named {
        AST_DECL();
        Type(std::string n);

        Type(Lex::Symbol* sym)
            : Named(sym)
            , m_text(sym->text()) {
        }
        const char* text() const {
            return m_text.c_str();
        }
        std::string m_text;
        Ast::Type* m_extra{ nullptr }; //TODO func return type
    };

    extern Ast::Type s_typeType;
    extern Ast::Type s_typeInt;
    extern Ast::Type s_typeDouble;
    extern Ast::Type s_typeVoid;


    struct Decl : Node {
        AST_DECL();
    };


    struct Number : Node {
        AST_DECL();
        Number( Lex::Number* n) : m_num(n) {}
        Lex::Number* m_num;

        static std::string toString(const void* p) {
            auto n = static_cast<const Number*>(p);
            return n->m_num->text();
        }
    };
    

    struct Module : Node {
        AST_DECL();

        std::vector<Node*> m_items;
    };


    struct FunctionCall : public Node {
        AST_DECL();
        Node* m_func{ nullptr };
        std::vector< Node* > m_args;
        FunctionCall( Node* func, std::vector< Node* >&& args )
            : m_func(func), m_args(args) {
        }
    };

    struct Argument : public Named {
        AST_DECL();
        Argument(Lex::Symbol* s) : Named(s) {
        }
    };

    struct FunctionDecl : public Named {
        AST_DECL();

        std::vector< Argument* > m_args;
        Ast::Node* m_returnType{ nullptr };
        Node* m_body{ nullptr };

        FunctionDecl(Lex::Symbol* name)
            : Named(name) {
        }
        FunctionDecl( Lex::Symbol* name, std::vector<Argument*>& args, Node* body )
            : Named(name), m_body(body) {
            m_args.swap( args );
        }
    };


    struct Sequence : public Node {
        AST_DECL();
        std::vector<Node*> m_items;
    };


    struct Reference : public Node {
        AST_DECL();
        Reference(Named* s) : m_target(s) {
        }
        Node* m_target;
    };


    struct Scope : public Node {
        AST_DECL();
        Scope(Node* c) : m_child(c) {}
        Node* m_child{ nullptr };
    };


    struct Definition : public Named {
        AST_DECL();

        Node* m_value = nullptr;

        Definition( Lex::Symbol* sym, Node* value ) : Named( sym ), m_value( value ) {}
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