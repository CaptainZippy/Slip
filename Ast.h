#pragma once
#include "Reflect.h"
#include "Source.h"

namespace Sema {
    struct TypeInfo;
}

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
        Sema::TypeInfo* m_data{ nullptr };
        Lex::SourceLocation m_loc;
    };

    struct Named : Node {
        AST_DECL();
        istring m_name{};

        Named(string_view sym) : m_name(istring::make(sym)) {}
    };

    struct If : Node {
        AST_DECL();
        Node* m_cond;
        Node* m_true;
        Node* m_false;

        If(Node* c, Node* t, Node* f) : m_cond(c), m_true(t), m_false(f) {}
    };

    extern Ast::Type s_typeType;
    extern Ast::Type s_typeInt;
    extern Ast::Type s_typeBool;
    extern Ast::Type s_typeDouble;
    extern Ast::Type s_typeVoid;
    extern Ast::Type s_typeString;

    struct Type : Named {
        AST_DECL();
        Type(string_view sym)
            : Named(istring::make(sym)) {
            m_type = &s_typeType;
        }
        //Ast::Type* m_extra{ nullptr }; //TODO func return type
        //std::vector<Ast::Type*> m_args{ nullptr }; //TODO func arg types
    };

    

    struct Decl : Node {
        AST_DECL();
    };


    struct Number : Node {
        AST_DECL();
        Number( string_view n) : m_num(n) {}
        std::string m_num;

        static std::string toString(const void* p) {
            auto n = static_cast<const Number*>(p);
            return n->m_num;
        }
    };
    
    struct String : Node {
        AST_DECL();
        String(string_view n) : m_str(n) {}
        std::string m_str;

        static std::string toString(const void* p) {
            auto n = static_cast<const String*>(p);
            return n->m_str;
        }
    };

    struct Module : Node {
        AST_DECL();

        std::vector<Node*> m_items;
    };


    struct FunctionCall : Node {
        AST_DECL();
        Node* m_func{ nullptr };
        std::vector< Node* > m_args;
        FunctionCall( Node* func, std::vector< Node* >&& args )
            : m_func(func), m_args(args) {
        }
    };

    struct Argument : Named {
        AST_DECL();
        Argument(string_view s, Type* t=nullptr) : Named(s) {
            m_type = t;
        }
    };

    struct FunctionDecl : Named {
        AST_DECL();

        std::vector< Argument* > m_args;
        Ast::Node* m_returnType{ nullptr };
        Node* m_body{ nullptr };

        FunctionDecl(string_view name)
            : Named(name) {
        }
        FunctionDecl(string_view name, std::vector<Argument*>& args, Node* body )
            : Named(name), m_body(body) {
            m_args.swap( args );
        }
    };

    struct BinaryOperation : FunctionDecl {
        AST_DECL();
        BinaryOperation(string_view name, Argument* a, Argument* b, Type* ret )
            : FunctionDecl(name) {
            m_returnType = ret;
            m_body = new Node();
            m_body->m_type = ret;
            m_args.push_back(a);
            m_args.push_back(b);
        }
    };

    struct Sequence : Node {
        AST_DECL();
        std::vector<Node*> m_items;
    };


    struct Reference : public Node {
        AST_DECL();
        Reference(Named* s) : m_target(s) {
        }
        Named* m_target;
    };


    struct Scope : public Node {
        AST_DECL();
        Scope(Node* c) : m_child(c) {}
        Node* m_child{ nullptr };
    };


    struct Definition : public Named {
        AST_DECL();

        Node* m_value = nullptr;

        Definition(string_view sym, Node* value ) : Named( sym ), m_value( value ) {}
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
