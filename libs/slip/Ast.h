#pragma once
#include "slip/Io.h"
#include "slip/Reflect.h"
#include "slip/Slip.h"

namespace Slip::Ast {
#define AST_NODE( X ) struct X;
#include "slip/Ast.inc"
#undef AST_NODE

#define AST_DECL()     \
    REFLECT_DECL_VT(); \
    int tag() const override

    static constexpr Expr* fixMeDeclContext{ nullptr };

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
        Ast::Type* m_type{ nullptr };
        Ast::Expr* m_declTypeExpr{ nullptr };
        SourceLocation m_loc;
        size_t m_serial{ s_serial++ };
    };

    struct LexNode : Expr {
       public:
        AST_DECL();
        LexNode( const SourceLocation& loc ) : Expr( loc ) {}

        LexNode* m_declTypeLex = nullptr;
        std::vector<LexNode*> m_attrs;

        LexNode() = default;
    };

    struct LexValue : LexNode {
        AST_DECL();
        LexValue() = default;
        LexValue( const SourceLocation& loc ) : LexNode( loc ) {}
        string_view text() const {
            auto s = m_loc.m_file->m_contents.c_str();
            return { s + m_loc.m_start, m_loc.m_end - m_loc.m_start };
        }
        istring istr() const {
            auto s = m_loc.m_file->m_contents.c_str();
            return istring::make( s + m_loc.m_start, m_loc.m_end - m_loc.m_start );
        }
    };

    struct LexDot : LexNode {
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

    struct LexList : LexNode {
        AST_DECL();
        LexList( const SourceLocation& loc, int openingChar ) : LexNode( loc ), m_openingChar( openingChar ) {
            assert( openingChar == '(' || openingChar == '[' );
        }
        size_t size() const { return m_items.size(); }
        LexNode* at( int i ) const { return m_items[i]; }
        LexNode* at( size_t i ) const { return m_items[i]; }
        void append( LexNode* a ) { m_items.push_back( a ); }
        void insertAt( int idx, LexNode* a ) { m_items.insert( m_items.begin() + idx, a ); }
        array_view<LexNode*> items() { return m_items; }
        std::vector<LexNode*> m_items;
        int m_openingChar;  // '{', '[', '(' etc
    };

    struct Decl : Expr {
        AST_DECL();
        Decl( Expr* declContext ) : declContext_( declContext ) {}
        Expr* declContext_{ nullptr };
    };

    struct NamedDecl : Decl {
        AST_DECL();
        istring m_name{};

        NamedDecl( istring name, Expr* declContext ) : Decl( declContext ), m_name( name ) {}

        istring name() { return m_name; }
    };

    struct Assignment : Expr {
        AST_DECL();
        Expr* m_lhs{ nullptr };
        Expr* m_rhs{ nullptr };

        template <typename With>
        Assignment( Expr* lhs, Expr* rhs, With&& with ) : m_lhs( lhs ), m_rhs( rhs ) {
            with( *this );
        }
    };

    struct Block : NamedDecl {
        AST_DECL();
        Block( istring name, Expr* declContext, Ast::Expr* contents ) : NamedDecl( name, declContext ), m_contents( contents ) {}
        Ast::Expr* m_contents;
    };

    struct Break : Expr {
        AST_DECL();
        Break( Ast::Block* target, Ast::Expr* value ) : m_target( target ), m_value( value ) {}
        Ast::Block* m_target;
        Ast::Expr* m_value;
    };

    struct Builtin : NamedDecl {
        AST_DECL();
        using ParseProto = Result( Ast::Environment* env, LexList* list, Ast::Expr** out );
        using ParseFunc = Func<ParseProto>;

        Builtin( istring name, Expr* declContext, ParseFunc&& func ) : NamedDecl( name, declContext ), m_func( std::move( func ) ) {}

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
        std::vector<std::pair<Expr*, Expr*> > m_cases;

        template <typename With>
        Cond( With&& with ) {
            with( *this );
        }
    };

    struct CoroutineDecl : NamedDecl {
        AST_DECL();

        std::vector<Parameter*> m_params;
        Ast::Expr* m_declReturnTypeExpr{ nullptr };
        Expr* m_body{ nullptr };

        template <typename With>
        CoroutineDecl( istring name, Expr* declContext, With&& with ) : NamedDecl( name, declContext ) {
            with( *this );
        }
    };

    struct CoroutineYield : Expr {
        AST_DECL();
        Expr* m_expr{ nullptr };
        CoroutineDecl* m_coro{ nullptr };
    };

    struct Definition : NamedDecl {
        AST_DECL();

        Expr* m_value = nullptr;

        template <typename With>
        Definition( istring sym, Expr* declContext, Expr* value, With&& with ) : NamedDecl( sym, declContext ), m_value( value ) {
            with( *this );
        }
    };

    struct DataList : Expr {
        AST_DECL();

        template <typename With>
        DataList( With&& with ) {
            with( *this );
        }
        array_view<Expr*> items() { return m_items; }
        std::vector<Expr*> m_items;
    };

    struct Dictlike {
        virtual ~Dictlike() = default;
        virtual void lookupAll( istring sym, std::vector<Expr*>& out ) const = 0;
        virtual Result lookup( string_view sym, Expr** out ) const = 0;
    };

    struct Environment : Expr, Dictlike {
        AST_DECL();

        Environment( Module* module ) : module_( module ) {}
        Environment( Environment* parent ) : parent_( parent ), module_( parent->module_ ) {}

        void lookupAll( istring sym, std::vector<Expr*>& out ) const override;
        Result lookup( string_view sym, Expr** out ) const override;
        Result bind( istring sym, Expr* value );
        auto bind( string_view sym, Expr* value ) { return bind( istring::make( sym ), value ); }
        auto parent() const { return parent_; }
        Result importAll( Environment* other );
        // Each env knows the module in which it is contained
        Module* module() { return module_; }
        auto syms() { return rng::make( syms_ ); }
        auto addUsing( Module* mod, istring alias ) { usings_.emplace_back( mod, alias ); }
        void nameSuffixPush( std::string s ) { suffixes_.push_back( s ); }
        void nameSuffixPop() { suffixes_.pop_back(); }
        const std::string& nameSuffix() const { return suffixes_.back(); }

       private:
        Environment* parent_{};
        Module* module_{};
        std::multimap<istring, Expr*> syms_{};
        std::vector<std::pair<Module*, istring> > usings_{};
        std::vector<std::string> suffixes_{ "" };
    };

    struct FunctionCall : Expr {
        AST_DECL();
        Expr* m_func{ nullptr };
        std::vector<Expr*> m_args;

        template <typename With>
        FunctionCall( Expr* func, std::vector<Expr*>&& args, With&& with ) : m_func( func ), m_args( args ) {
            with( *this );
        }
    };

    struct FunctionDecl : NamedDecl {
        AST_DECL();
        using IntrinsicProto = Result( array_view<Expr*> args, Ast::Expr** out );
        static Result NotImplemented( array_view<Expr*> args, Ast::Expr** out );

        std::vector<Parameter*> m_params{};
        Ast::Expr* m_declReturnTypeExpr{};
        Expr* m_body{};
        Func<IntrinsicProto> m_intrinsic{};
        Environment* environment_{};

        template <typename With>
        FunctionDecl( istring name, Expr* declContext, With&& with ) : NamedDecl( name, declContext ) {
            with( *this );
        }
    };

    struct GenericDecl : NamedDecl {
        AST_DECL();

        std::vector<Ast::Parameter*> params_{};
        LexNode* body_{};
        Environment* environment_{};

        template <typename With>
        GenericDecl( istring name, Expr* declContext, With&& with ) : NamedDecl( name, declContext ) {
            with( *this );
        }
    };

    struct GenericInstantiation : Expr {
        AST_DECL();
        std::vector<Ast::Expr*> args_{};
        GenericDecl* decl_{};

        template <typename With>
        GenericInstantiation( With&& with ) {
            with( *this );
        }
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

    struct MacroDecl : NamedDecl {
        AST_DECL();

        std::vector<Parameter*> m_params;
        istring m_dynEnvSym;
        Environment* m_staticEnv;
        std::vector<LexNode*> m_body;

        template <typename With>
        MacroDecl( istring name, Expr* declContext, string_view dynEnvSym, Environment* staticEnv, With&& with )
            : NamedDecl( name, declContext ), m_dynEnvSym( istring::make( dynEnvSym ) ), m_staticEnv( staticEnv ) {
            with( *this );
        }
    };

    struct MacroExpansion : Expr {
        AST_DECL();
        Expr* m_expansion{ nullptr };
        MacroDecl* m_macro{ nullptr };
        std::vector<Expr*> m_args;
        MacroExpansion( MacroDecl* macro, std::vector<Expr*>&& args ) : m_macro( macro ), m_args( args ) {}
        template <typename With>
        MacroExpansion( MacroDecl* macro, std::vector<Expr*>&& args, With&& with ) : m_macro( macro ), m_args( args ) {
            with( *this );
        }
    };

    struct Module : NamedDecl {
        AST_DECL();
        Module( istring n, Expr* declContext ) : NamedDecl( n, declContext ), environment_( new Environment( this ) ) {}

        struct Export {
            REFLECT_DECL();
            Export( istring n, Ast::Expr* e ) : name( n ), expr( e ) {}
            istring name;
            Ast::Expr* expr;
        };
        array_view<Export> exports() { return exports_; }
        Ast::Environment* env() { return environment_; }

        void addExport( istring n, Expr* e ) { exports_.emplace_back( n, e ); }
        // Instantiate a template and return the type.
        // Instantiations are memorized - 'create' is only called if the name isn't known
        Result instantiate( istring name, const Func<Result( Ast::Type** )>& create, Ast::Type** out );

        auto instantiations() { return rng::make( instantiations_ ); }

       protected:
        Ast::Environment* environment_{};
        std::unordered_map<istring, Ast::Type*> instantiations_;
        std::vector<Export> exports_;
    };

    // We may not be able to tell which overload is chosen until after type inference.
    struct NamedFunctionCall : NamedDecl {
        AST_DECL();

        template <typename With>
        NamedFunctionCall( istring name, Expr* declContext, std::vector<Expr*> candidates, std::vector<Expr*>&& args, With&& with )
            : NamedDecl( name, declContext ), m_candidates( candidates ), m_args( args ) {
            with( *this );
        }

        Expr* m_resolved{ nullptr };  // null until one of the candidates or a generic is chosen.
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
        Number( istring n, With&& with ) : m_num( n ) {
            with( *this );
        }
        std::string m_num;

        static std::string toString( const void* p ) {
            auto n = static_cast<const Number*>( p );
            return n->m_num;
        }
    };

    struct Parameter : NamedDecl {
        AST_DECL();
        bool m_ref{ false };
        template <typename With>
        Parameter( istring name, Expr* declContext, With&& with ) : NamedDecl( name, declContext ) {
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
            bool canFail{ false };
        };
        void addStage( Ast::Expr* e ) { m_stages.emplace_back( e ); }
        std::vector<Stage> m_stages;
    };

    struct Reference : Expr {
        AST_DECL();
        Reference( Expr* s ) : m_target( s ) {}
        Expr* resolve() override;

        Expr* m_target;
    };

    struct Scope : Expr {
        AST_DECL();
        Scope( Expr* c ) : m_child( c ) {}
        Expr* m_child{ nullptr };
    };

    struct Selector : Expr {
        AST_DECL();
        template <typename With>
        Selector( Expr* l, LexIdent* r, With&& with ) : m_lhs( l ), m_rhs( r ) {
            with( *this );
        }
        Expr* m_lhs{ nullptr };
        LexIdent* m_rhs{ nullptr };
    };

    struct Sequence : Expr {
        AST_DECL();
        Sequence() = default;
        Sequence( std::vector<Expr*>&& items ) : m_items( items ) {}
        array_view<Expr*> items() { return m_items; }
        std::vector<Expr*> m_items;
    };

    struct String : Expr {
        AST_DECL();
        template <typename With>
        String( string_view n, With&& with ) : m_str( n ) {
            with( *this );
        }
        std::string m_str;

        static std::string toString( const void* p ) {
            auto n = static_cast<const String*>( p );
            return n->m_str;
        }
    };

    struct StructDecl : NamedDecl {
        AST_DECL();
        template <typename With>
        StructDecl( istring name, Expr* declContext, With&& with ) : NamedDecl( name, declContext ) {
            with( *this );
        }
        std::vector<StructField*> m_fields;
    };

    struct StructField : NamedDecl {
        AST_DECL();
        template <typename With>
        StructField( istring name, Expr* declContext, With&& with ) : NamedDecl( name, declContext ) {
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

    struct Type : NamedDecl {
        AST_DECL();
        Type( istring sym, Expr* declContext );

        // callable can fail
        bool m_callCanFail{ false };
        // non-empty for callable types.
        std::vector<Ast::Type*> m_callable;  //[0]=return [1:]=args
        // TODO remove?
        std::vector<Ast::FunctionDecl*> m_methods;
        // non-empty for record type
        Ast::StructDecl* m_struct{ nullptr };
        // non-empty for array type
        Ast::Type* m_array{ nullptr };
        // non-empty for sum type
        std::vector<Ast::Type*> m_sum;
        // Set if this came from a generic
        Ast::GenericInstantiation* generic_{ nullptr };
    };

    struct TryExpr : Expr {
        AST_DECL();

        template <typename With>
        TryExpr( Ast::Expr* expr, With&& with ) : m_expr( expr ) {
            with( *this );
        }

        Ast::Expr* m_expr;
    };

    struct VariableDecl : NamedDecl {
        AST_DECL();
        template <typename With>
        VariableDecl( istring name, Expr* declContext, With&& with ) : NamedDecl( name, declContext ) {
            with( *this );
        }
        std::vector<Expr*> m_initializer;
        enum class Kind {
            Immutable,  // can't change beyond initialization
            Mutable,    // can change
            Constant,   // immutable, known at compile time
        };
        Kind m_kind{ Kind::Immutable };
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
