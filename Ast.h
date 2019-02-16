#pragma once
#include "Slip.h"
#include "Io.h"
#include "Reflect.h"

namespace Slip::Ast {
    using namespace std;
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

        Node() = default;

        template<typename With>
        Node(With&& w) {
            w(*this);
        }

        Ast::Node* m_declTypeExpr{nullptr};
        Io::SourceLocation m_loc;
    };

    struct Named : Node {
        AST_DECL();
        istring m_name{};

        Named(istring s) : m_name(move(s)) {}
        Named(string_view sym) : m_name(istring::make(sym)) {}
    };

    struct If : Node {
        AST_DECL();
        Node* m_cond;
        Node* m_true;
        Node* m_false;

        If(Node* c, Node* t, Node* f) : m_cond(c), m_true(t), m_false(f) {}
    };

    struct Cond : Node {
        AST_DECL();
        vector< pair<Node*, Node*> > m_cases;
    };

    extern Ast::Type s_typeType;
    extern Ast::Type s_typeInt;
    extern Ast::Type s_typeBool;
    extern Ast::Type s_typeDouble;
    extern Ast::Type s_typeVoid;
    extern Ast::Type s_typeString;

    struct Type : Named {
        AST_DECL();
        Type(string_view sym);
        Type(istring sym);
        //TypeRef m_elemType; //TODO ptr/array type
        //TypeRef m_extra; //TODO func return type
        //vector<TypeRef> m_args; //TODO func arg types
    };


    struct Decl : Node {
        AST_DECL();
    };


    struct Number : Node {
        AST_DECL();
        Number( string_view n) : m_num(n) {
        }
        template<typename With>
        Number(string_view n, With&& with) : m_num(n) {
            with(*this);
        }
        string m_num;

        static string toString(const void* p) {
            auto n = static_cast<const Number*>(p);
            return n->m_num;
        }
    };
    
    struct String : Node {
        AST_DECL();
        String(string_view n) : m_str(n) {
        }
        template<typename With>
        String(string_view n, With&& with)
            : m_str(n) {
            with(*this);
        }
        string m_str;

        static string toString(const void* p) {
            auto n = static_cast<const String*>(p);
            return n->m_str;
        }
    };

    struct Module : Node {
        AST_DECL();

        vector<Node*> m_items;
    };


    struct FunctionCall : Node {
        AST_DECL();
        Node* m_func{ nullptr };
        vector< Node* > m_args;
        template<typename With>
        FunctionCall( Node* func, vector< Node* >&& args, With&& with )
            : m_func(func), m_args(args) {
            with( *this );
        }
    };

    struct Argument : Named {
        AST_DECL();
        template<typename With>
        Argument(string_view s, With&& with) : Named(s) {
            with(*this);
        }
    };

    struct FunctionDecl : Named {
        AST_DECL();
        typedef Result (*Intrinsic)(Parse::Evaluator* eval, array_view<Node*> args, Ast::Node** out);

        vector< Argument* > m_args;
        Ast::Node* m_declReturnTypeExpr{ nullptr };
        Node* m_body{ nullptr };
        Intrinsic m_intrinsic{ nullptr };

        FunctionDecl(string_view name)
            : Named(name) {
        }

        template<typename With>
        FunctionDecl(string_view name, With&& with)
            : Named(name) {
            with(*this);
        }

        Result invoke(Parse::Evaluator* eval, array_view<Node*> args, Ast::Node** out) {
            RETURN_RES_IF(Result::ERR, args.size() != m_args.size());
            //todo check args
            if (m_intrinsic) {
                return (*m_intrinsic)(eval, args, out);
            }
            return Result::ERR;
        }

            /// Create a named function of two arguments.
        static FunctionDecl* makeBinaryOp(string_view name, Argument* a, Argument* b, Node* ret);

            /// Create a named intrinsic function.
        static FunctionDecl* makeIntrinsic(string_view name, Intrinsic intrin, Node* ret, initializer_list<Argument*> args);
    };

    struct Sequence : Node {
        AST_DECL();
        array_view<Node*> items() {
            return m_items;
        }
        vector<Node*> m_items;
    };


    struct Reference : public Node {
        AST_DECL();
        Reference(Node* s) : m_target(s) {
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

        Definition(string_view sym, Node* value) : Named(sym), m_value(value) {}
        template<typename With>
        Definition(string_view sym, Node* value, With&& with )
            : Named( sym )
            , m_value( value ) {
            with(*this);
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

    template <typename Handler, typename...Args>
    auto dispatch(Node* n, Handler&& handler, Args...args) {
        switch (n->tag()) {
            #define AST_NODE(X) case Detail::TagOf<X>::Tag: return handler(static_cast<X*>(n), args...);
            default:
            #include "Ast.inc"
            #undef AST_NODE
        }
    }
}
