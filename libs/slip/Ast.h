#pragma once
#include "slip/Io.h"
#include "slip/Reflect.h"
#include "slip/Slip.h"

namespace Slip::Ast {
    using namespace std;
#define AST_NODE( X ) struct X;
#include "slip/Ast.inc"
#undef AST_NODE

#define AST_DECL()  \
    REFLECT_DECL(); \
    int tag() const override

    namespace Flags {
        const unsigned Hidden = 0;  // Default is "ref"
        const unsigned Child = 1;   // Default is "ref"
        const unsigned Abbrev = 2;  // Default is "ref"
    };                              // namespace Flags

    using SourceLocation = Io::SourceLocation;

    struct Expr : Reflect::AbstractReflected {
        REFLECT_DECL();

       private:
        static size_t s_serial;

       public:
        virtual int tag() const;

        Expr() = default;
        Expr( const SourceLocation& loc ) : m_loc( loc ) {}

        template <typename With>
        Expr( With&& w ) {
            w( *this );
        }

        virtual Expr* resolve();

        size_t m_userData;
        Ast::Type* m_type{nullptr};
        Ast::Expr* m_declTypeExpr{nullptr};
        SourceLocation m_loc;
        size_t m_serial{s_serial++};
    };

    struct LexNode : public Expr {
       public:
        AST_DECL();
        LexNode( const SourceLocation& loc ) : Expr( loc ) {}

        LexNode* m_decltype = nullptr;
        vector<LexNode*> m_attrs;

        LexNode() = default;
    };

    struct LexValue : LexNode {
        AST_DECL();
        LexValue() = default;
        LexValue( const SourceLocation& loc ) : LexNode( loc ) {}
        string_view text() const {
            auto s = m_loc.m_file->m_contents.c_str();
            return {s + m_loc.m_start, m_loc.m_end - m_loc.m_start};
        }
    };

    struct LexDot : public LexNode {
        AST_DECL();
        LexDot( LexNode* lhs, LexNode* rhs ) : m_lhs( lhs ), m_rhs( rhs ) {}

        LexNode* m_lhs;
        LexNode* m_rhs;
    };

    struct LexString : LexValue {
        AST_DECL();
        LexString( const SourceLocation& loc ) : LexValue( loc ) {}
    };

    struct LexIdent : LexValue {
        AST_DECL();
        LexIdent( const SourceLocation& loc ) : LexValue( loc ) {}
    };

    struct LexNowExpr : LexNode {
        AST_DECL();
        LexNowExpr( const SourceLocation& loc, LexNode* expr ) : LexNode( loc ), m_expr( expr ) {}
        LexNode* m_expr;
    };

    struct LexNumber : LexValue {
        AST_DECL();
        LexNumber( const SourceLocation& loc ) : LexValue( loc ) {}
    };

    struct LexList : public LexNode {
        AST_DECL();
        LexList( const SourceLocation& loc ) : LexNode( loc ) {}
        size_t size() const { return m_items.size(); }
        LexNode* at( int i ) const { return m_items[i]; }
        LexNode* at( size_t i ) const { return m_items[i]; }
        void append( LexNode* a ) { m_items.push_back( a ); }
        void insertAt( int idx, LexNode* a ) { m_items.insert( m_items.begin() + idx, a ); }
        array_view<LexNode*> items() { return m_items; }
        vector<LexNode*> m_items;
    };

    struct Named : Expr {
        AST_DECL();
        istring m_name{};

        Named( istring s ) : m_name( move( s ) ) {}
        Named( string_view sym ) : m_name( istring::make( sym ) ) {}

        istring name() { return m_name; }
    };

    struct Assignment : Expr {
        AST_DECL();
        Expr* m_lhs{nullptr};
        Expr* m_rhs{nullptr};

        template <typename With>
        Assignment( Expr* lhs, Expr* rhs, With&& with ) : m_lhs( lhs ), m_rhs( rhs ) {
            with( *this );
        }
    };

    struct Block : Named {
        AST_DECL();
        Block( istring name, Ast::Expr* contents ) : Named( name ), m_contents( contents ) {}
        Ast::Expr* m_contents;
    };

    struct Break : Expr {
        AST_DECL();
        Break( Ast::Block* target, Ast::Expr* value ) : m_target( target ), m_value( value ) {}
        Ast::Block* m_target;
        Ast::Expr* m_value;
    };

    struct Builtin : Named {
        AST_DECL();
        using ParseProto = Result( Ast::Environment* env, LexList* list, Ast::Expr** out );
        using ParseFunc = Func<ParseProto>;

        Builtin( string_view name, ParseFunc&& func ) : Named( name ), m_func( std::move( func ) ) {}

        Result parse( Ast::Environment* env, LexList* list, Ast::Expr** out ) { return m_func( env, list, out ); }

        ParseFunc m_func;
    };

    struct CatchExpr : Expr {
        AST_DECL();

        template <typename With>
        CatchExpr( Ast::Expr* expr, Ast::Expr* fail, With&& with ) : m_expr( expr ), m_fail( fail ) {
            with( *this );
        }

        Ast::Expr* m_expr;
        Ast::Expr* m_fail;
    };

    struct Cond : Expr {
        AST_DECL();
        vector<pair<Expr*, Expr*> > m_cases;

        template <typename With>
        Cond( With&& with ) {
            with( *this );
        }
    };

    struct CoroutineDecl : Named {
        AST_DECL();

        vector<Parameter*> m_params;
        Ast::Expr* m_declReturnTypeExpr{nullptr};
        Expr* m_body{nullptr};

        CoroutineDecl( string_view name ) : Named( name ) {}

        template <typename With>
        CoroutineDecl( string_view name, With&& with ) : Named( name ) {
            with( *this );
        }
    };

    struct CoroutineYield : Expr {
        AST_DECL();
        Expr* m_expr{nullptr};
        CoroutineDecl* m_coro{nullptr};
    };

    struct Decl : Expr {
        AST_DECL();
    };

    struct Definition : public Named {
        AST_DECL();

        Expr* m_value = nullptr;

        template <typename With>
        Definition( string_view sym, Expr* value, With&& with ) : Named( sym ), m_value( value ) {
            with( *this );
        }
    };

    struct Environment : Expr {
        AST_DECL();

        Environment( Environment* parent ) : parent_( parent ) {}
        struct LookupIter {
            const Environment* env{nullptr};
            std::multimap<istring, Expr*>::const_iterator it;
        };
        bool lookup_iter( istring sym, Expr** out, LookupIter& iter ) const {
            *out = nullptr;
            // iter is null first time, or the env of the previous find.
            if( iter.env == nullptr ) {
                iter.env = this;
                iter.it = iter.env->syms_.lower_bound( sym );
            }
            while( iter.it == iter.env->syms_.end() || iter.it->first != sym ) {
                if( auto p = iter.env->parent_ ) {
                    iter.env = p;
                    iter.it = iter.env->syms_.lower_bound( sym );
                } else {
                    return false;
                }
            }
            *out = iter.it->second;
            ++iter.it;
            return true;
        }

        Result lookup( string_view sym, Expr** out ) const {
            *out = nullptr;
            auto s = istring::make( sym );
            LookupIter iter;
            if( lookup_iter( s, out, iter ) == false ) {
                return Result::ERR;
            }
            if( iter.it != iter.env->syms_.end() && iter.it->first == s ) {
                // This api expects only 1 match
                // If overloads are OK, the caller should use lookup_iter
                return Result::ERR;
            }
            return Result::OK;
        }

        Result bind( istring sym, Expr* value );
        auto bind( string_view sym, Expr* value ) { return bind( istring::make( sym ), value ); }

        Environment* parent_;
        std::multimap<istring, Expr*> syms_;
    };

    struct FunctionCall : Expr {
        AST_DECL();
        Expr* m_func{nullptr};
        vector<Expr*> m_args;

        template <typename With>
        FunctionCall( Expr* func, vector<Expr*>&& args, With&& with ) : m_func( func ), m_args( args ) {
            with( *this );
        }
    };

    struct FunctionDecl : Named {
        AST_DECL();
        using IntrinsicProto = Result( array_view<Expr*> args, Ast::Expr** out );
        static Result NotImplemented( array_view<Expr*> args, Ast::Expr** out );

        vector<Parameter*> m_params;
        Ast::Expr* m_declReturnTypeExpr{nullptr};
        Expr* m_body{nullptr};
        Func<IntrinsicProto> m_intrinsic;

        FunctionDecl( string_view name ) : Named( name ) {}

        template <typename With>
        FunctionDecl( string_view name, With&& with ) : Named( name ) {
            with( *this );
        }

        /// Create a named function of two parameters.
        static FunctionDecl* makeBinaryOp( string_view name, Parameter* a, Parameter* b, Expr* ret );

        /// Create a named intrinsic function.
        static FunctionDecl* makeIntrinsic( string_view name, Func<IntrinsicProto>&& intrin, Expr* ret,
                                            initializer_list<Parameter*> params );
    };

    struct If : Expr {
        AST_DECL();
        Expr* m_cond;
        Expr* m_true;
        Expr* m_false;

        template <typename With>
        If( Expr* c, Expr* t, Expr* f, With&& with ) : m_cond( c ), m_true( t ), m_false( f ) {
            with( *this );
        }
    };

    struct MacroDecl : Named {
        AST_DECL();

        vector<Parameter*> m_params;
        istring m_dynEnvSym;
        Environment* m_staticEnv;
        std::vector<LexNode*> m_body;

        template <typename With>
        MacroDecl( string_view name, string_view dynEnvSym, Environment* staticEnv, With&& with )
            : Named( name ), m_dynEnvSym( istring::make( dynEnvSym ) ), m_staticEnv( staticEnv ) {
            with( *this );
        }
    };

    struct MacroExpansion : Expr {
        AST_DECL();
        Expr* m_expansion{nullptr};
        MacroDecl* m_macro{nullptr};
        vector<Expr*> m_args;
        MacroExpansion( MacroDecl* macro, vector<Expr*>&& args ) : m_macro( macro ), m_args( args ) {}
        template <typename With>
        MacroExpansion( MacroDecl* macro, vector<Expr*>&& args, With&& with ) : m_macro( macro ), m_args( args ) {
            with( *this );
        }
    };

    struct Module : Expr {
        AST_DECL();

        istring m_name;
        vector<Expr*> m_items;
    };

    // We may not be able to tell which overload is chosen until after type inference.
    struct NamedFunctionCall : Named {
        AST_DECL();

        template <typename With>
        NamedFunctionCall( istring name, std::vector<Expr*>&& candidates, std::vector<Expr*>&& args, With&& with )
            : Named( name ), m_candidates( candidates ), m_args( args ) {
            with( *this );
        }

        Expr* m_resolved{nullptr};  // null until one of the candidates or a generic is chosen.
        std::vector<Expr*> m_candidates;
        std::vector<Expr*> m_args;
    };

    struct Nop : Expr {
        AST_DECL();
        Nop() {}
    };

    struct Number : Expr {
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

    struct Parameter : Named {
        AST_DECL();
        bool m_ref{false};
        template <typename With>
        Parameter( string_view s, With&& with ) : Named( s ) {
            with( *this );
        }
    };

    struct PipelineExpr : Expr {
        AST_DECL();
        template <typename With>
        PipelineExpr( With&& with ) {
            with( *this );
        }
        struct Stage {
            REFLECT_DECL();
            Stage( Ast::Expr* e ) : expr( e ) {}
            Ast::Expr* expr;
            bool canFail{false};
        };
        void addStage( Ast::Expr* e ) { m_stages.emplace_back( e ); }
        std::vector<Stage> m_stages;
    };

    struct Reference : public Expr {
        AST_DECL();
        Reference( Expr* s ) : m_target( s ) {}
        Expr* resolve() override;

        Expr* m_target;
    };

    struct Scope : public Expr {
        AST_DECL();
        Scope( Expr* c ) : m_child( c ) {}
        Expr* m_child{nullptr};
    };

    struct Selector : public Expr {
        AST_DECL();
        template <typename With>
        Selector( Expr* l, LexIdent* r, With&& with ) : m_lhs( l ), m_rhs( r ) {
            with( *this );
        }
        Expr* m_lhs{nullptr};
        LexIdent* m_rhs{nullptr};
    };

    struct Sequence : Expr {
        AST_DECL();
        Sequence() = default;
        Sequence( std::vector<Expr*>&& items ) : m_items( items ) {}
        array_view<Expr*> items() { return m_items; }
        vector<Expr*> m_items;
    };

    struct String : Expr {
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

    struct StructDecl : Named {
        AST_DECL();
        template <typename With>
        StructDecl( string_view n, With&& with ) : Named( n ) {
            with( *this );
        }
        std::vector<StructField*> m_fields;
    };

    struct StructField : Named {
        AST_DECL();
        template <typename With>
        StructField( string_view n, With&& with ) : Named( n ) {
            with( *this );
        }
    };

    struct UnwrapResult : Expr {
        AST_DECL();
        Expr* m_src;
        UnwrapResult( Expr* src ) : m_src( src ) {}
    };

    extern Ast::Type s_typeType;
    extern Ast::Type s_typeInt;
    extern Ast::Type s_typeBool;
    extern Ast::Type s_typeError;
    extern Ast::Type s_typeDouble;
    extern Ast::Type s_typeFloat;
    extern Ast::Type s_typeVoid;
    extern Ast::Type s_typeString;

    struct Type : Named {
        AST_DECL();
        Type( string_view sym );
        Type( istring sym );

        // callable can fail
        bool m_callCanFail{false};
        // non-empty for callable types.
        std::vector<Ast::Type*> m_callable;  //[0]=return [1:]=args
        // TODO remove?
        std::vector<Ast::FunctionDecl*> m_methods;
        // non-empty for record type
        Ast::StructDecl* m_struct{nullptr};
        // non-empty for array type
        Ast::Type* m_array{nullptr};
        // non-empty for sum type
        std::vector<Ast::Type*> m_sum;
    };

    struct TryExpr : Expr {
        AST_DECL();

        template <typename With>
        TryExpr( Ast::Expr* expr, With&& with ) : m_expr( expr ) {
            with( *this );
        }

        Ast::Expr* m_expr;
    };

    struct VariableDecl : Named {
        AST_DECL();
        template <typename With>
        VariableDecl( istring name, With&& with ) : Named( name ) {
            with( *this );
        }
        std::vector<Expr*> m_initializer;
        enum class Kind {
            Immutable,  // can't change beyond initialization
            Mutable,    // can change
            Constant,   // immutable, known at compile time
        };
        Kind m_kind{Kind::Immutable};
    };

    struct While : Expr {
        AST_DECL();
        Expr* m_cond;
        Expr* m_body;

        template <typename With>
        While( Expr* c, Expr* b, With&& with ) : m_cond( c ), m_body( b ) {
            with( *this );
        }
    };

    void print( Expr* node );
    void print( Expr* node, Io::TextOutput& out );

    namespace Detail {
        template <typename T>
        struct TagOf;
#define AST_NODE( X )            \
    template <>                  \
    struct TagOf<X> {            \
        enum { Tag = __LINE__ }; \
    };
#include "slip/Ast.inc"
#undef AST_NODE
    }  // namespace Detail

    template <typename RetType, typename Handler, typename... Args>
    auto dispatch( Expr* n, Handler&& handler, Args&&... args ) -> RetType {
        switch( n->tag() ) {
#define AST_NODE( X )           \
    case Detail::TagOf<X>::Tag: \
        return handler( static_cast<X*>( n ), std::forward<Args>( args )... );
            default:
#include "slip/Ast.inc"
#undef AST_NODE
        }
    }

    template <typename RetType, typename Handler>
    auto dispatch( Expr* n, Handler&& handler ) -> RetType {
        switch( n->tag() ) {
#define AST_NODE( X )           \
    case Detail::TagOf<X>::Tag: \
        return handler( static_cast<X*>( n ) );
            default:
#include "slip/Ast.inc"
#undef AST_NODE
        }
    }
}  // namespace Slip::Ast
