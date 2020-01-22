#pragma once
#include "Io.h"
#include "Reflect.h"
#include "Slip.h"

namespace Slip::Ast {
    using namespace std;
#define AST_NODE( X ) struct X;
#include "Ast.inc"
#undef AST_NODE

#define AST_DECL()  \
    REFLECT_DECL(); \
    int tag() const override

    namespace Flags {
        const unsigned Child = 1;   // Default is "ref"
        const unsigned Abbrev = 2;  // Default is "ref"
    };                              // namespace Flags

    struct Node : Reflect::AbstractReflected {
        REFLECT_DECL();

        virtual int tag() const;

        Node() = default;

        template <typename With>
        Node( With&& w ) {
            w( *this );
        }

        virtual Node* resolve();

        size_t m_userData;
        Ast::Type* m_type{nullptr};
        Ast::Node* m_declTypeExpr{nullptr};
        Io::SourceLocation m_loc;
    };

    struct Named : Node {
        AST_DECL();
        istring m_name{};

        Named( istring s ) : m_name( move( s ) ) {}
        Named( string_view sym ) : m_name( istring::make( sym ) ) {}

        istring name() { return m_name; }
    };

    struct If : Node {
        AST_DECL();
        Node* m_cond;
        Node* m_true;
        Node* m_false;

        template <typename With>
        If( Node* c, Node* t, Node* f, With&& with ) : m_cond( c ), m_true( t ), m_false( f ) {
            with( *this );
        }
    };

    struct While : Node {
        AST_DECL();
        Node* m_cond;
        Node* m_body;

        template <typename With>
        While( Node* c, Node* b, With&& with ) : m_cond( c ), m_body( b ) {
            with( *this );
        }
    };

    struct Cond : Node {
        AST_DECL();
        vector<pair<Node*, Node*> > m_cases;

        template <typename With>
        Cond( With&& with ) {
            with( *this );
        }
    };

    extern Ast::Type s_typeType;
    extern Ast::Type s_typeInt;
    extern Ast::Type s_typeBool;
    extern Ast::Type s_typeDouble;
    extern Ast::Type s_typeFloat;
    extern Ast::Type s_typeVoid;
    extern Ast::Type s_typeString;

    struct Type : Named {
        AST_DECL();
        Type( string_view sym );
        Type( istring sym );
        // non-empty for callable types.
        std::vector<Ast::Type*> m_callable;  //[0]=return [1:]=args
    };

    struct Decl : Node {
        AST_DECL();
    };

    struct Number : Node {
        AST_DECL();
        template <typename With>
        Number( string_view n, With&& with ) : m_num( n ) {
            with( *this );
        }
        string m_num;

        static string toString( const void* p ) {
            auto n = static_cast<const Number*>( p );
            return n->m_num;
        }
    };

    struct String : Node {
        AST_DECL();
        template <typename With>
        String( string_view n, With&& with ) : m_str( n ) {
            with( *this );
        }
        string m_str;

        static string toString( const void* p ) {
            auto n = static_cast<const String*>( p );
            return n->m_str;
        }
    };

    struct Module : Node {
        AST_DECL();

        vector<Node*> m_items;
    };

    struct FunctionCall : Node {
        AST_DECL();
        Node* m_func{nullptr};
        vector<Node*> m_args;

        template <typename With>
        FunctionCall( Node* func, vector<Node*>&& args, With&& with ) : m_func( func ), m_args( args ) {
            with( *this );
        }
    };

    struct Argument : Named {
        AST_DECL();
        template <typename With>
        Argument( string_view s, With&& with ) : Named( s ) {
            with( *this );
        }
    };

    struct FunctionDecl : Named {
        AST_DECL();
        // using Intrinsic = Result (*)(Parse::Evaluator* eval, array_view<Node*> args, Ast::Node** out);
        using Intrinsic = Result ( * )( array_view<Node*> args, Ast::Node** out );

        vector<Argument*> m_args;
        Ast::Node* m_declReturnTypeExpr{nullptr};
        Node* m_body{nullptr};
        Intrinsic m_intrinsic{nullptr};

        FunctionDecl( string_view name ) : Named( name ) {}

        template <typename With>
        FunctionDecl( string_view name, With&& with ) : Named( name ) {
            with( *this );
        }

        /// Create a named function of two arguments.
        static FunctionDecl* makeBinaryOp( string_view name, Argument* a, Argument* b, Node* ret );

        /// Create a named intrinsic function.
        static FunctionDecl* makeIntrinsic( string_view name, Intrinsic intrin, Node* ret, initializer_list<Argument*> args );
    };

    // Overloaded function call.
    // We may not be able to tell which overload is chosen until after type inference.
    struct UnresolvedCall : Named {
        AST_DECL();

        UnresolvedCall( string_view name ) : Named( name ) {}

        template <typename With>
        UnresolvedCall( string_view name, const std::vector<Node*>& candidates, std::vector<Node*>&& args, With&& with )
            : Named( name ), m_candidates( candidates ), m_args( std::move( args ) ) {
            with( *this );
        }

        std::vector<Node*> m_candidates;
        std::vector<Node*> m_args;
        Node* m_resolved{nullptr};  // null until one of the candidates is chosen.
    };

    struct VariableDecl : Named {
        AST_DECL();
        template <typename With>
        VariableDecl( istring name, With&& with ) : Named( name ) {
            with( *this );
        }
        Node* m_initializer{nullptr};
    };

    struct Assignment : Node {
        AST_DECL();
        Node* m_lhs{nullptr};
        Node* m_rhs{nullptr};

        template <typename With>
        Assignment( Node* lhs, Node* rhs, With&& with ) : m_lhs( lhs ), m_rhs( rhs ) {
            with( *this );
        }
    };

    struct Sequence : Node {
        AST_DECL();
        array_view<Node*> items() { return m_items; }
        vector<Node*> m_items;
    };

    struct Reference : public Node {
        AST_DECL();
        Reference( Node* s ) : m_target( s ) {}
        Node* resolve() override;

        Node* m_target;
    };

    struct Scope : public Node {
        AST_DECL();
        Scope( Node* c ) : m_child( c ) {}
        Node* m_child{nullptr};
    };

    struct Definition : public Named {
        AST_DECL();

        Node* m_value = nullptr;

        template <typename With>
        Definition( string_view sym, Node* value, With&& with ) : Named( sym ), m_value( value ) {
            with( *this );
        }
    };

    void print( Node* node );
    void print( Node* node, Io::TextOutput& out );

    namespace Detail {
        template <typename T>
        struct TagOf;
#define AST_NODE( X )            \
    template <>                  \
    struct TagOf<X> {            \
        enum { Tag = __LINE__ }; \
    };
#include "Ast.inc"
#undef AST_NODE
    }  // namespace Detail

    template <typename RetType, typename Handler, typename... Args>
    auto dispatch( Node* n, Handler&& handler, Args&&... args ) -> RetType {
        switch( n->tag() ) {
#define AST_NODE( X )           \
    case Detail::TagOf<X>::Tag: \
        return handler( static_cast<X*>( n ), std::forward<Args...>( args... ) );
            default:
#include "Ast.inc"
#undef AST_NODE
        }
    }

    template <typename RetType, typename Handler>
    auto dispatch( Node* n, Handler&& handler ) -> RetType {
        switch( n->tag() ) {
#define AST_NODE( X )           \
    case Detail::TagOf<X>::Tag: \
        return handler( static_cast<X*>( n ) );
            default:
#include "Ast.inc"
#undef AST_NODE
        }
    }
}  // namespace Slip::Ast
