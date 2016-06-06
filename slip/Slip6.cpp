// TODO : iterator type
// TODO : ellipsis
// TODO : typing
// TODO : GC

#include "Pch.h"

void error( const char* msg ) {
    __debugbreak();
}
template<typename T>
void assert2( T t, const char* msg ) {
    if( !t ) Slip::Detail::_Error( msg );
}
#define assert(A) assert2(A, #A)
#define cast(T,a) a.unbox<T*>()

struct Result {
    enum Code { OK = 0, ERR = 1 };
    Result( Code c ) : code( c ) {}
    bool isOk() const { return code == OK; }
    Code code;
};
#define R_OK Result::OK

#define Error( fmt, ... ) Slip::Detail::_Error("%s(%i,%i): error " fmt, __FILE__, __LINE__, 0, __VA_ARGS__)
#define Error_At( loc, fmt, ... ) Slip::Detail::_Error("%s(%i,%i): error " fmt, loc.filename(), loc.line(), loc.col(), __VA_ARGS__)

namespace Slip {
    namespace Detail {
        void _Error( const char* fmt, ... ) {
            va_list va;
            va_start( va, fmt );
            char buf[2048];
            vsnprintf( buf, sizeof( buf ), fmt, va );
            va_end( va );
            printf( "%s", buf );
            OutputDebugStringA( buf );
            throw 0;
        }
    }
}

template<typename T>
struct view_ptr {
    view_ptr() : m_ptr( nullptr ) {}
    view_ptr( T* p ) : m_ptr( p ) {}
    operator T*( ) const { return m_ptr; }
    T* operator->() const { return m_ptr; }
    T* m_ptr;
};

template<typename T>
struct array_view {
    typedef T* iterator;
    array_view() : m_begin( nullptr ), m_end( nullptr ) {}
    array_view( const T* s, const T* e ) : m_begin( s ), m_end( e ) {}
    array_view( const std::vector<T>& a ) : m_begin( a.data() ), m_end( m_begin + a.size() ) {}

    size_t size() const { return m_end - m_begin; }
    const T& operator[]( unsigned i ) const { assert( i < size() ); return m_begin[i]; }
    const T* begin() const { return m_begin; }
    const T* end() const { return m_end; }
    array_view<T> ltrim( unsigned n ) const { assert( n <= size() ); return array_view<T>( m_begin + n, m_end ); }
    const T* m_begin;
    const T* m_end;
};

namespace array_view_t {
    template<typename T>
    array_view<T> from_single( const T& t ) { return array_view<T>( &t, &t + 1 ); }
    template<typename T, int N>
    array_view<T> make( T( &arr )[N] ) { return array_view<T>( t, t + N ); }
}

template<typename S>
struct safe_cast_t {
    safe_cast_t( const S& s ) : m_s( s ) {}
    template<typename D>
    operator D() { assert( S( D( m_s ) ) == m_s ); return D( m_s ); }
    const S& m_s;
};

template<typename T>
safe_cast_t<T> safe_cast( const T& t ) {
    return safe_cast_t<T>( t );
};

struct State;
struct GcBase;
struct Atom;
struct Symbol;
struct Callable;
struct Env;
struct List;
struct Type;
struct Box;

Box State_getSymbol( State* s, Symbol sym );
template<typename T, typename...P> T* State_create( State* s, P...p );
Box State_eval( State* s, Box b, Env* e );
Env* State_getEnv( State* s );
Box State_defineSymbol( State* s, Symbol sym, Box b );
void State_updateSymbol( State* s, Symbol sym, Box b );

struct SourceManager {
    struct FileInfo {
        std::string m_name;
        std::string m_contents;
    };

    struct Location {
        view_ptr<const FileInfo> m_file;
        long m_off;
        const char* filename() const {
            return m_file ? m_file->m_name.data() : "<unnamed>";
        }
        int line() const {
            if( m_file ) {
                auto& txt = m_file->m_contents;
                return safe_cast( std::count( txt.begin(), txt.begin() + m_off, '\n' ) + 1 );
            }
            return 0;
        }
        int col() const {
            if( m_file ) {
                auto& txt = m_file->m_contents;
                auto nl = txt.rfind( '\n', m_off );
                return safe_cast( m_off - ( ( nl == std::string::npos ) ? 0 : nl ) );
            }
            return 0;
        }
    };

    struct Input {
        Input( const char* s, const char* e, FileInfo* n ) : start( s ), cur( s ), end( e ), info( n ) {}
        explicit operator bool() const {
            return cur != end;
        }
        void eatwhite() {
            while( cur != end && isspace( *cur ) ) {
                ++cur;
            }
        }
        int peek() const {
            return *cur;
        }
        int next() {
            if( cur == end ) return -1;
            return *cur++;
        }
        const char* peekbuf() const {
            return cur;
        }
        Location location() const {
            return Location{ info, safe_cast( cur - start ) };
        }
        const char* cur;
        const char* start;
        const char* end;
        const FileInfo* info;
    };

    Input load( const char* fname ) {
        while( 1 ) {
            auto it = m_files.find( fname );
            if( it != m_files.end() ) {
                auto& txt = it->second->m_contents;
                return Input( &txt[0], &txt[0] + txt.size(), it->second );
            }
            else if( FILE* fin = fopen( fname, "r" ) ) {
                std::string txt;
                while( 1 ) {
                    char buf[4096];
                    size_t n = fread( buf, 1, sizeof( buf ), fin );
                    if( n == -1 ) error( "Read error" );
                    else if( n == 0 ) break;
                    else txt.append( buf, buf + n );
                }
                m_files[fname] = new FileInfo{ fname, txt };
            }
            else {
                return Input( nullptr, nullptr, nullptr );
            }
        }
    }

    std::map< std::string, FileInfo* > m_files;
};

namespace Detail {
    void _Error_At( SourceManager::Location loc, char* fmt, ... ) {
        va_list va;
        va_start( va, fmt );
        vprintf( fmt, va );
        va_end( va );
    }
}

struct Symbol {
    const char* m_sym;
};

bool operator==( Symbol a, Symbol b ) { return a.m_sym == b.m_sym; }
bool operator!=( Symbol a, Symbol b ) { return a.m_sym != b.m_sym; }

namespace std {
    template<>
    struct hash<Symbol>
        : public _Bitwise_hash<Symbol> {
    };
}

#if 0
struct Box {
    // not NaN, QNaN, SNaN, inf - float
    // sign bit set - atom*
    // 
    static const uint64_t VAL_HEAD = 0x7FFCull << 48;
    static const uint64_t PTR_HEAD = 0xFFFCull << 48;
    static const uint64_t MASK_HEAD = 0xFFFFull << 48;
    static const uint64_t MASK_SINGLE = 0xFFFFFull << 44;

    enum Kind {
        Kind32 = 0,
        KindSymbol = 1,
    };

    enum Single {
        SingleNil = 0,
        SingleTrue = 1,
        SingleFalse = 2,
        SingleTrue = 3,
    };

    static inline uint64_t VAL_ENC( uint64_t b2, uint64_t lo ) {
        return VAL_HEAD | ( b2 << 48 ) | lo;
    }
    static inline uint64_t SINGLE_ENC( uint64_t b2, uint64_t lo ) {
        return VAL_HEAD | ( 3 << 48 ) | ( b2 << 44 ) | lo;
    }
    static inline uint64_t PTR_ENC( const void* ptr ) {
        return PTR_HEAD | uint64_t( ptr );
    }
    #define SIGN_BIT ((uint64_t)1 << 63)

    static const Box s_true;
    static const Box s_false;
    Box() : m_val( VAL_ENC( KindSingle, SingleNil ) ) {}
    Box( Symbol s ) : m_val( VAL_ENC( KindSymbol, uint64_t( s.m_sym ) ) ) {}
    Box( double d ) { memcpy( &m_val, &d, sizeof( d ) ); }
    Box( int i ) : m_val( VAL_ENC( KindInt, uint64_t( i ) ) ) {}
    Box( Atom* a ) : m_val( PTR_ENC( a ) ) {}
    //explicit operator bool() { return m_val!=VAL_ENC(0,0); }

    bool isFloat() const { return ( m_val & VAL_HEAD ) != VAL_HEAD; }
    double toFloat() const {
        double d; memcpy( &d, &m_val, sizeof( d ) ); return d;
    }

    bool unbox<int>() const { return ( m_val & MASK_HEAD ) == VAL_ENC( KindInt, 0 ); }
    int unbox<int>() const { assert( unbox<int>() ); return static_cast<int>( m_val ); }

    bool isSymbol() const { return ( m_val & MASK_HEAD ) == VAL_ENC( KindSymbol, 0 ); }
    Symbol toSymbol() const { assert( isSymbol() ); return Symbol( reinterpret_cast<const char*>( m_val & ~PTR_HEAD ) ); }

    bool isBool() const { return ( m_val & MASK_HEAD ) == VAL_ENC( KindSingle ); }
    bool unbox<bool>() const { assert( isBool() ); return false; }

    bool isAtom() const { return ( m_val & PTR_HEAD ) == PTR_HEAD; }
    Atom* toAtom() const { assert( isAtom() ); return reinterpret_cast<Atom*>( m_val & ~PTR_HEAD ); }

    void print() const;// { aptr->print(); }
    Box eval( State* state ) const;// { aptr->print(); }
    uint64_t m_val;
    //Kind kind;
    //Atom* aptr;
};
#else

struct Box {
    enum Kind {
        KIND_NIL,
        KIND_BOOL,
        KIND_INT,
        KIND_FLOAT,
        KIND_SYMBOL,
        KIND_ATOM
    };

    Box() : m_kind( KIND_NIL ) { m_val.a = nullptr; }
    Box( Symbol s ) : m_kind( KIND_SYMBOL ) { m_val.s = s; }
    Box( double f ) : m_kind( KIND_FLOAT ) { m_val.f = f; }
    Box( bool b ) : m_kind( KIND_BOOL ) { m_val.i = b; }
    Box( int i ) : m_kind( KIND_INT ) { m_val.i = i; }
    Box( Atom* a ) : m_kind( KIND_ATOM ) { m_val.a = a; }
    explicit operator bool() { return m_kind != KIND_NIL; }

    template<typename T> bool has() const;
    template<typename T> T unbox() const;

    Atom* asAtom() const { return m_kind == KIND_ATOM ? m_val.a : nullptr; }

    void print() const;
    Box eval( State* state ) const;

    static const Box s_true;
    static const Box s_false;

    Kind m_kind;
    union Union {
        Atom* a;
        int i;
        double f;
        Symbol s;
    };
    Union m_val;
};

const Box Box::s_true( true );
const Box Box::s_false( false );

namespace Detail {
    template<typename T> struct KindFromType;
    template<> struct KindFromType<int> { enum { Value = Box::KIND_INT }; };
    template<> struct KindFromType<bool> { enum { Value = Box::KIND_BOOL }; };
    template<> struct KindFromType<double> { enum { Value = Box::KIND_FLOAT }; };
    template<> struct KindFromType<Symbol> { enum { Value = Box::KIND_SYMBOL }; };
    //template<typename T> struct KindFromType<T*> { enum { Value = Box::KIND_ATOM }; };

    template<typename T> struct Trait {
        static bool has( Box b ) {
            return b.m_kind == KindFromType<T>::Value;
        }
        static T unbox( Box b ) {
            assert( has( b ) );
            T ret; memcpy( &ret, &b.m_val, sizeof( T ) );
            return ret;
        }
    };

    template<typename T> struct Trait<T*> {
        static bool has( Box b ) {
            return b.m_kind == Box::KIND_ATOM && dynamic_cast<T*>( b.m_val.a );
        }
        static T* unbox( Box b ) {
            assert( b.has<T*>() );
            return static_cast<T*>( b.m_val.a );
        }
    };

    template<> struct Trait<Box> {
        static bool has( Box b ) {
            return true;
        }
        static
            Box unbox( Box b ) {
            return b;
        }
    };
}

template<typename T>
bool Box::has() const {
    return Detail::Trait<T>::has( *this );
}
template<typename T>
T Box::unbox() const {
    return Detail::Trait<T>::unbox( *this );
}
#endif
struct GcBase;

// Interface for things which may hold references to gc objects
struct GcWalkable {
    virtual ~GcWalkable() {}

    typedef void( *GcWalker )( GcBase* );
    virtual void gcWalk( GcWalker w ) = 0;
};

// Base class of things which are garbage collected
struct GcBase : public GcWalkable {
    enum { WHITE = 0, BLACK = 0x2, SEEN=0xf0 };
    GcBase() : m_gcprev( nullptr ), m_gcnext( s_gchead  ), m_gcflags( WHITE ) {
        if( s_gchead ) {
            s_gchead->m_gcprev = this;
        }
        s_gchead = this;
    }
    virtual ~GcBase() {}
    void unhook() {
        if( m_gcprev ) {
            m_gcprev->m_gcnext = m_gcnext;
        }
        else {
            assert( s_gchead == this );
            s_gchead = m_gcnext;
        }

        if( m_gcnext ) {
            m_gcnext->m_gcprev = m_gcprev;
        }
    }

    GcBase* m_gcprev;
    GcBase* m_gcnext;
    unsigned short m_gcsize;
    unsigned short m_gcflags;
    static GcBase* s_gchead;
};
GcBase* GcBase::s_gchead;

// Utility which holds on to a single gc object for its lifetime
struct GcAnchor : private GcWalkable {
    State* m_s;
    GcBase* m_obj;
    GcAnchor( State* s, GcBase* b );
    ~GcAnchor();
    void gcWalk( GcWalker w ) {
        ( *w )( m_obj );
    }
};


struct Atom : public GcBase {
    void* operator new( size_t ) = delete;
    void* operator new( size_t, void* p ){ return p; }
    virtual ~Atom() {}
    Box eval( State* state ) {
        return _eval( state );
    }
    void print() const { _print(); }
    SourceManager::Location m_loc;
    Atom* m_type;
protected:
    virtual Box _eval( State* state ) = 0;
    virtual void _print() const = 0;
};

struct Value : Atom {
    Box _eval( State* state ) override {
        return Box( this );
    }
};

struct Type : public Value {
    void _print() const override {
        printf( "type" );
    }
    #if 0
    static Type s_bool;
    static Type s_string;
    static Type s_int;
    static Type s_float;
    static Type s_list;
    Type Type::s_int;
    Type Type::s_float;
    Type Type::s_string;

    #endif
};


struct String : Value {
    String( const char* s = nullptr ) : m_str( s ) { /*m_type = &Type::s_string;*/ }
    void _print() const override {
        printf( "%s", m_str );
    }
    void gcWalk( GcWalker w ) override {
    }

    const char* m_str;
};

struct Env : Value {
    Env( Env *p ) : m_parent( p ) {}
    Box get( Symbol sym ) {
        for( Env* e = this; e; e = e->m_parent ) {
            auto it = e->m_tab.find( sym );
            if( it != e->m_tab.end() ) {
                return it->second;
            }
        }
        Error( "Symbol '%s' not found", sym.m_sym );
        return Box();
    }
    void _print() const override {
        printf( "<env" );
        for( auto& e : m_tab ) {
            printf( " %s=", e.first.m_sym );
            e.second.print();
        }
        if( m_parent && m_parent->m_parent ) m_parent->print();
        printf( ">" );
    }
    void put( Symbol sym, Box val ) {
        m_tab[sym] = val;
    }
    void update( Symbol sym, Box val ) {
        for( Env* e = this; e; e = e->m_parent ) {
            auto it = e->m_tab.find( sym );
            if( it != e->m_tab.end() ) {
                it->second = val;
                return;
            }
        }
        assert( 0 );
    }
    void gcWalk( GcWalker w ) override {
        for( auto& item : m_tab ) {
            if( Atom* a = item.second.asAtom() ) {
                ( *w )( a );
            }
        }
    }
    Env* m_parent;
    typedef std::unordered_map<Symbol, Box> Table;
    Table m_tab;
};

struct Callable : Value {
    typedef array_view<Box> ArgList;
    Box call( State* state, Box arg0, ArgList args ) {
        return _call( state, arg0, args );
    }
    void _print() const override {
        printf( "<callable>" );
    }
protected:
    virtual Box _call( State* state, Box arg0, ArgList args ) = 0;
};

void Box::print() const {
    switch( m_kind ) {
        case KIND_NIL:
            printf( "nil" );
            break;
        case KIND_BOOL:
            printf( m_val.i ? "true" : "false" );
            break;
        case KIND_INT:
            printf( "%i", m_val.i );
            break;
        case KIND_FLOAT:
            printf( "%lf", m_val.f );
            break;
        case KIND_SYMBOL:
            printf( "%s", m_val.s.m_sym );
            break;
        case KIND_ATOM:
            return m_val.a->print();
        default:
            Error( "fixme" );
    }
}

Box Box::eval( State* state ) const {
    switch( m_kind ) {
        case KIND_SYMBOL:
            return State_getSymbol( state, m_val.s );
        case KIND_ATOM:
            return m_val.a->eval( state );
        default:
            return *this;
    }
}

template<typename T>
struct Optional {
    Optional() : m_ptr( nullptr ) {}
    explicit Optional( const T& t ) {
        m_ptr = new( m_buf ) T( t );
    }
    ~Optional() {
        reset();
    }
    void reset() {
        if( m_ptr ) m_ptr->~T();
    }
    explicit operator bool() const {
        return m_ptr != nullptr;
    }
    T operator*() {
        assert( m_ptr );
        return *m_ptr;
    }
    T& operator->() {
        assert( m_ptr );
        return *m_ptr;
    }
    void set( const T& t ) {
        reset();
        m_ptr = new( m_buf ) T( t );
    }
    T get() {
        return *m_ptr;
    }
private:
    Optional( const Optional& o );
    void operator=( const Optional& o );
    void* m_buf[( sizeof( T ) + sizeof( void* ) - 1 ) / sizeof( void* )];
    T* m_ptr;
};

namespace Args {
    template<typename T>
    using Star = std::vector<T>;

    template<typename T>
    struct ListOf {
        ListOf() {}
        void append( T t ) {
            m_list.push_back( t );
        }
        std::vector<T> m_list;
    };

    template<typename F, typename S>
    struct PairOf {
        PairOf() {}
        void append( F f, S s ) {
            first = f;
            second = s;
        }
        F first;
        S second;
    };

    namespace Detail {
        struct ArgIter {
            bool more() const {
                return m_cur < m_end;
            }
            bool next() {
                ++m_cur;
                return more();
            }
            Box cur() const {
                return *m_cur;
            }
            Box const* m_cur;
            Box const* m_end;
        };
        template<typename T>
        void bind( unsigned eval, T& out, State* state, ArgIter& iter ) {
            assert( iter.more() );
            Box c = iter.cur();
            Box b = eval ? State_eval(state, c, nullptr) : c;
            assert2( b.has<T>(), "Wrong argument type" );
            out = b.unbox<T>();
            iter.next();
        }
        template<typename T>
        void bind( unsigned eval, Star<T>& out, State* state, ArgIter& iter ) {
            for( ; iter.more(); ) {
                T t;
                bind( eval, t, state, iter );
                out.push_back( t );
            }
        }
        template<typename T>
        void bind( unsigned eval, ListOf<T>& out, State* state, ArgIter& iter ) {
            List* lst = cast( List, iter.cur() );
            assert( lst );
            iter.next();
            ArgIter iter2{ lst->view().begin(), lst->view().end() };
            for( ; iter2.more(); ) {
                T t;
                bind( eval, t, state, iter2 );
                out.append( t );
            }
        }
        template<typename F, typename S>
        void bind( unsigned eval, PairOf<F, S>& out, State* state, ArgIter& iter ) {
            List* lst = cast( List, iter.cur() );
            if( !lst || lst->size() != 2 )
                Error( "Expected a pair of items" );
            //               Error_At( iter.cur()->m_loc, "Expected a pair of items" );
            iter.next();
            ArgIter iter2{ lst->view().begin(), lst->view().end() };
            F f; S s;
            bind( eval, f, state, iter2 );
            bind( eval, s, state, iter2 );
            out.append( f, s );
        }
        template<typename T>
        void bind( unsigned eval, Optional<T>& out, State* state, ArgIter& args ) {
            if( args.more() ) {
                T t;
                bind( eval, t, state, args );
                out.set( t );
            }
        }
    }

    struct Binder {
        template<typename T>
        void operator() ( unsigned eval, T& out, State* state, Detail::ArgIter& iter ) {
            Detail::bind( eval, out, state, iter );
        }
    };
}

namespace Gc {
    namespace Detail{
        void walk( GcBase* gc, GcWalkable::GcWalker w ) {
            ( *w )( gc );
        }
        void walk( bool b, GcWalkable::GcWalker w ) {
        }
        void walk( int b, GcWalkable::GcWalker w ) {
        }
        void walk( double b, GcWalkable::GcWalker w ) {
        }
        void walk( Symbol s, GcWalkable::GcWalker w ) {
        }
        void walk( Box b, GcWalkable::GcWalker w ) {
            if( Atom* a = b.asAtom() ) {
                ( *w )( a );
            }
        }
        template<typename T>
        void walk( Optional<T>& o, GcWalkable::GcWalker w ) {
            if( o ) {
                walk( o.get(), w );
            }
        }
        template<typename T>
        void walk( std::vector<T>& o, GcWalkable::GcWalker w ) {
            for( auto& i : o ) {
                walk( i, w );
            }
        }
        template<typename T>
        void walk( Args::ListOf<T>& o, GcWalkable::GcWalker w ) {
            for( auto& i : o.m_list ) {
                walk( i, w );
            }
        }
        template<typename F, typename S>
        void walk( Args::PairOf<F,S>& o, GcWalkable::GcWalker w ) {
            walk( o.first, w );
            walk( o.second, w );
        }
    }
}

template<typename CALL>
struct BuiltinCallable : Callable {
    // Prevents GC freeing its arguments
    struct ArgsAnchor : GcWalkable {
        ArgsAnchor(State* s, CALL* call) : m_s(s), m_call(call) {
            m_s->m_anchors.push_back( this );
        }
        ~ArgsAnchor() {
            assert( m_s->m_anchors.back() == this );
            m_s->m_anchors.pop_back();
        }
        void gcWalk( GcWalker w ) override {
            m_call->visit<ArgsAnchor&, GcWalker>( *this, w );
        }
        template<typename T>
        void operator() ( unsigned eval, T& out, GcWalker w ) {
            Gc::Detail::walk( out, w );
        }
        State* m_s;
        CALL* m_call;
    };
    virtual Box _call( State* state, Box arg0, ArgList args ) override {
        CALL call;
        ArgsAnchor anchor( state, &call );
        Args::Detail::ArgIter iter{ args.begin(), args.end() };
        call.visit<Args::Binder, State*, Args::Detail::ArgIter&>( Args::Binder(), state, iter );
        assert2( iter.more() == false, "Too many arguments" );
        return call.call( state );
    }
    void gcWalk( GcWalker w ) override {
    }
};


struct List : Atom {
    typedef std::vector<Box> vector;
    Box _eval( State* state ) override {
        if( lst.empty() ) {
            Error_At( m_loc, "Empty list is illegal" );
        }
        Box atom = lst[0].eval( state );
        if( auto call = cast( Callable, atom ) ) {
            return call->call( state, lst[0], Callable::ArgList( lst ).ltrim( 1 ) );
        }
        //Error_At( lst[0]->m_loc, "Expected a callable as the first argument" );
        return Box();
    }

    vector::const_iterator begin() const {
        return lst.begin();
    }
    vector::const_iterator end() const {
        return lst.end();
    }
    size_t size() const {
        return lst.size();
    }
    void append( Box a ) {
        lst.push_back( a );
    }
    Box at( int i ) const {
        return lst[i];
    }
    Box set( int i, Box a ) {
        lst[i] = a;
        return a;
    }
    void _print() const override {
        printf( "(" );
        const char* sep = "";
        for( auto a : lst ) {
            printf( sep );
            a.print();
            sep = " ";
        }
        printf( ")" );
    }
    void resize( int n ) {
        lst.resize( n );
    }
    array_view<Box> view() const { return lst; }

    void gcWalk( GcWalker w ) override {
        for( auto& item : lst ) {
            if( Atom* a = item.asAtom() ) {
                ( *w )( a );
            }
        }
    }

protected:
    std::vector<Box> lst;
};

struct Lambda : Callable {
    std::vector<Symbol> m_arg_names;
    Env* m_lex_env = nullptr;
    Box m_body;

    Lambda( Env* lex_env, const std::vector<Symbol>& arg_names, Box body )
        : m_arg_names( arg_names ), m_lex_env( lex_env ), m_body( body ) {
    }

    Box _call( State* state, Box arg0, ArgList args ) override {
        //Args::Detail::ArgIter iter{ args_in.begin(), args_in.end() };
        assert( args.size() == m_arg_names.size() );
        Env* e = State_create<Env>(state, m_lex_env );
        GcAnchor anchor( state, e );
        unsigned ni = 0;
        for( auto arg : args ) {
            Box a = arg.eval( state );
            //auto s = cast( Symbol, m_arg_names->at( ni ) );
            //if( s->m_type != nullptr ) {
                /*auto nt = s->m_type.eval( env );
                auto at = a->m_type.eval( env );
                if( at != nt ) {
                    Error_At( arg->m_loc, "Mismatched type for argument '%i'", ni );
                }*/
                //}
            e->put( m_arg_names[ni], a );
            ni += 1;
        }
        return State_eval(state, m_body, e );
    }
    void _print() const override {
        printf( "(lambda (" );
        for( auto a : m_arg_names ) {
            printf( "%s ", a.m_sym );
        }
        printf( ") " );
        m_body.print();
    }
    void gcWalk( GcWalker w ) override {
        ( *w )( m_lex_env );
        if( Atom* a = m_body.asAtom() ) {
            ( *w )( a );
        }
    }
};
//Bool Lambda::s_trampoline(false);

struct Vau : public Callable {
    std::vector<Symbol> m_arg_names;
    Env* m_lex_env = nullptr;
    Symbol m_env_sym;
    Box m_body;
    Vau( Env* lex_env, const std::vector<Symbol>& arg_names, Symbol symbol, Box body )
        : m_arg_names( arg_names ), m_lex_env( lex_env ), m_env_sym( symbol ), m_body( body ) {
    }
    Box _call( State* state, Box arg0, ArgList args ) override {
        Env* e = State_create<Env>( state, m_lex_env );
        GcAnchor anchor( state, e );
        e->put( m_env_sym, State_getEnv( state ) );
        assert( args.size() == m_arg_names.size() );
        for( unsigned i = 0; i < args.size(); ++i ) {
            e->put( m_arg_names[i], args[i] );
        }
        return State_eval( state, m_body, e );
    }
    void gcWalk( GcWalker w ) override {
        ( *w )( m_lex_env );
        if( Atom* a = m_body.asAtom() ) {
            ( *w )( a );
        }
    }
};

struct BuiltinVau {
    Args::ListOf< Symbol > m_arg_names;
    Symbol m_env_sym;
    Box m_body;

    template<typename FUNC, typename...VISITARGS>
    void visit( FUNC func, VISITARGS...visitargs ) {
        func( 0, m_arg_names, visitargs... );
        func( 0, m_env_sym, visitargs... );
        func( 0, m_body, visitargs... );
    }

    Box call( State* state ) {
        return State_create<Vau>( state, State_getEnv( state ), m_arg_names.m_list, m_env_sym, m_body );
    }
};

struct BuiltinPrint {
    Args::Star< Box > m_args;
    template<typename FUNC, typename...VISITARGS>
    void visit( FUNC func, VISITARGS...visitargs ) {
        func( 1, m_args, visitargs... );
    }
    Box call( State* state ) {
        const char* sep = "";
        for( auto arg : m_args ) {
            printf( "%s", sep );
            arg.print();
            sep = " ";
        }
        printf( "\n" );
        return Box();
    }
};

//struct LambdaTail : Callable {
//    LambdaTail() {}
//    Box _call( State* state, Box arg0, ArgList args ) override {
//        assert( args.size() >= 1 );
//        Callable* c = cast( Callable, args[0].eval(env) );
//        assert( c->m_arg_names );
//        ArgList fwd = args.ltrim( 1 );
//        assert( fwd.size() == c->m_arg_names->size() );
//        std::vector<Box> newa;
//        for( unsigned i = 0; i < fwd.size(); ++i ) {
//            Box a = fwd[i].eval( env );
//            newa.push_back( a );
//        }
//        for( unsigned i = 0; i < fwd.size(); ++i ) {
//            env->update( cast( Symbol, c->m_arg_names->at( i ) )->m_sym, newa[i] );
//        }
//        return &Lambda::s_trampoline;
//    }
//    Box _normalize() override {
//        return this;
//    }
//};

struct State {
private:
    template<typename T, typename... P>
    T* gcnew( P... p ) {
        if( sizeof( T ) + m_heapStats.allocated >= m_heapStats.limit ) {
            garbageCollect();
        }
        m_heapStats.allocated += sizeof( T );
        void* addr = malloc( sizeof( T ) );
        //void* addr = VirtualAlloc( nullptr, sizeof( T ), MEM_COMMIT | MEM_RESERVE, PAGE_READWRITE );
        T* ret = new ( addr ) T( p... );
        ret->m_gcsize = sizeof( T );
        return ret;
    }
    static void gcwalk( GcBase* gc ) {
        if( gc == nullptr ) {
            return;
        }
        if( gc->m_gcflags & GcBase::SEEN ) {
            return;
        }
        gc->m_gcflags |= GcBase::SEEN | GcBase::BLACK;
        gc->gcWalk( &gcwalk );
    }
    void garbageCollect() {
        //printf( ">\n" );
        // clear
        for( GcBase* gc = GcBase::s_gchead; gc; gc = gc->m_gcnext ) {
            gc->m_gcflags = GcBase::WHITE;
        }

        // mark
        gcwalk( m_env );
        for( auto& env : m_envStack ) {
            gcwalk( env );
        }
        for( auto& item : m_stack.m_stack ) {
            if( item.m_kind == Box::KIND_ATOM ) {
                gcwalk( item.unbox<Atom*>() );
            }
        }

        for( auto& item : m_anchors ) {
            item->gcWalk(gcwalk);
        }

        // sweep
        for( GcBase* gc = GcBase::s_gchead; gc; ) {
            GcBase* next = gc->m_gcnext;
            if( gc->m_gcflags & GcBase::BLACK ) {
                //printf( "KEEP %p %ull\n", gc, gc->m_gcflags );
            }
            else {
                gc->unhook();
                int size = gc->m_gcsize;
                m_heapStats.allocated -= size;
                //printf( "COLL %p %ull\n", gc, gc->m_gcflags );
                gc->~GcBase();
                memset( gc, -1, size );
                //DWORD old; VirtualProtect( gc, size, PAGE_NOACCESS, &old);
                free( gc );
            }
            gc = next;
        }
        m_heapStats.limit = m_heapStats.allocated * 2;
        //printf( "< alloc=%i limit=%i\n\n", m_heapStats.allocated, m_heapStats.limit );
    }

public:
    
    template<typename T, typename...P>
    T* create(P...p) {
        return gcnew<T>(p...);
    }

    State() {
        m_heapStats.limit = 1024;
        m_env = create<Env>( nullptr );
        m_stack.push_back( m_env );
    }
    
    Result let( const char* name, Box value ) {
       return let( Symbol{ intern( name ) }, value );
    }
    Result let( Symbol name, Box value ) {
        m_env->put( name, value );
        return R_OK;
    }
    void newList() {
        m_stack.push_back( gcnew<List>( ) );
    }
    void newSymbol( const char* sym ) {
        m_stack.push_back( Symbol{ intern( sym ) } );
    }
    void newInteger( int value ) {
        m_stack.push_back( value );
    }
    void newFloat( double value ) {
        m_stack.push_back( value );
    }
    void newString( const char* s ) {
        m_stack.push_back( gcnew<String>( s ) );
    }
    Box pop() {
        Box r = m_stack[-1];
        m_stack.pop();
        return r;
    }
    void listAppend( int idx ) {
        if( List* l = cast( List, m_stack[idx] ) ) {
            l->append( m_stack[-1] );
            m_stack.pop( 1 );
        }
        else {
            throw 0;
        }
    }
    Box eval( Box b, Env* e = nullptr ) {
        if( e ) {
            m_envStack.push_back( m_env );
            m_env = e;
            Box r = b.eval( this );
            assert( e == nullptr || e == m_env );
            m_env = m_envStack.back();
            m_envStack.pop_back();
            return r;
        }
        else {
            return b.eval( this );
        }
    }
    Box getSymbol( Symbol sym ) {
        Box a = m_env->get( sym );
        assert(a);
        return a;
    }
    void updateSymbol( Symbol sym, Box b ) {
        m_env->update( sym, b );
    }
    Result getGlobal( const char* sym ) {
        return getGlobal( Symbol{ intern( sym ) } );
    }
    Result getGlobal( Symbol sym ) {
        Box a = m_env->get( sym );
        m_stack.push_back( a );
        return a ? R_OK : Result::ERR;
    }
    Result call( int narg, int nret ) {
        if( Callable* c = cast( Callable, m_stack[-narg - 1] ) ) {
            Box* begin = m_stack.m_stack.data();
            int size = m_stack.size();
            Box r = c->call( this, c, Callable::ArgList( begin + size - narg, begin + size ) );
            m_stack.pop( narg + 1 );
            m_stack.push_back( r );
            assert( nret == 1 );
            return R_OK;
        }
        return Result::ERR;
    }

    Env* m_env;
    std::vector<Env*> m_envStack;
    std::vector<GcWalkable*> m_anchors;

    struct Stack {
        Box operator[]( int i ) const {
            return i >= 0 ? m_stack[i] : m_stack[m_stack.size() + i];
        }
        void push_back( Box a ) {
            m_stack.push_back( a );
        }
        void pop( int n = 1 ) {
            m_stack.erase( m_stack.end() - 1, m_stack.end() );
        }
        int size() const {
            return (int) m_stack.size();
        }
        std::vector<Box> m_stack;
    };
    Stack m_stack;
protected:
    struct HeapStats {
        HeapStats() {
            memset( this, 0, sizeof( *this ) );
        }
        long allocated;
        long limit;
    };
    HeapStats m_heapStats;
    std::unordered_set<std::string> m_interns;
    const char* intern( const char* s ) {
        auto it = m_interns.insert( s );
        return it.first->c_str();
    }
};

GcAnchor::GcAnchor( State* s, GcBase* b ) {
    m_s = s;
    m_obj = b;
    s->m_anchors.push_back( this );
}
GcAnchor::~GcAnchor() {
    assert( m_s->m_anchors.back() == this );
    m_s->m_anchors.pop_back();
}

Box State_getSymbol( State* s, Symbol sym ) {
    return s->getSymbol( sym );
}
template<typename T, typename...P> T* State_create( State* s, P...p ) {
    return s->create<T>( p... );
}
Box State_eval( State* s, Box b, Env* e ) {
    return s->eval( b, e );
}
Box State_defineSymbol( State* s, Symbol sym, Box b ) {
    Result r = s->let( sym, b );
    assert( r.isOk() );
    return b;
}
Env* State_getEnv( State* s ) {
    return s->m_env;
}
void State_updateSymbol( State* s, Symbol sym, Box b ) {
    s->updateSymbol( sym, b );
}

struct BuiltinEval {
    Box m_expr;
    Optional< Env* > m_env;

    template<typename FUNC, typename...VISITARGS>
    void visit( FUNC func, VISITARGS...visitargs ) {
        func( 0, m_expr, visitargs... );
        func( 1, m_env, visitargs... );
    }
    Box call( State* state ) {
        return State_eval( state, m_expr, m_env.get() );
    }
};

struct BuiltinEvSym {
    Box m_expr;
    Optional<Env*> m_env;

    template<typename FUNC, typename...VISITARGS>
    void visit( FUNC func, VISITARGS...visitargs ) {
        func( 1, m_expr, visitargs... );
        func( 1, m_env, visitargs... );
    }
    Box call( State* state ) {
        return State_eval( state, m_expr, m_env.get() );
    }
};

Box v_type( State* state, Callable::ArgList args ) {
    #if 0
    assert( args.size() == 2 );
    auto t = cast( Type, args[1].eval( env ) );
    auto s = cast( Symbol, args[0] );
    assert( s );
    assert( t );
    //s->m_type = t;
    #endif
    return Box();// s;
}

struct BuiltinBegin {
    Args::Star< Box > m_exprs;
    template<typename FUNC, typename...VISITARGS>
    void visit( FUNC func, VISITARGS...visitargs ) {
        func( 0, m_exprs, visitargs... );
    }
    Box call( State* state ) {
        Box r;
        for( auto a : m_exprs ) {
            r = a.eval( state );
        }
        return r;
    }
};

struct BuiltinModule {
    List* m_exprs = nullptr;
    template<typename FUNC, typename...VISITARGS>
    void visit( FUNC func, VISITARGS...visitargs ) {
        func( 0, m_exprs, visitargs... );
    }
    Box call( State* state ) {
        Box r;
        for( auto a : *m_exprs ) {
            r = a.eval( state );
        }
        return r;
    }
};

struct BuiltinQuote {
    Args::Star< Box > m_args;
    template<typename FUNC, typename...VISITARGS>
    void visit( FUNC func, VISITARGS...visitargs ) {
        func( 0, m_args, visitargs... );
    }
    Box call( State* state ) {
        List* l = state->create<List>( );
        for( auto a : m_args ) {
            l->append( a );
        }
        return l;
    }
};

struct BuiltinInc {
    Symbol m_sym;
    template<typename FUNC, typename...VISITARGS>
    void visit( FUNC func, VISITARGS...visitargs ) {
        func( 0, m_sym, visitargs... );
    }
    Box call( State* state ) {
        Box a = state->getSymbol( m_sym );
        assert( a.has<int>() );
        int n = a.unbox<int>() + 1;
        state->updateSymbol( m_sym, n );
        return n;
    }
};

struct BuiltinDefine {
    Symbol m_sym;
    Box m_value;
    template<typename FUNC, typename...VISITARGS>
    void visit( FUNC func, VISITARGS...visitargs ) {
        func( 0, m_sym, visitargs... );
        func( 1, m_value, visitargs... );
    }
    Box call( State* state ) {
        State_defineSymbol(state, m_sym, m_value );
        return m_value;
    }
};

struct BuiltinLambda {
    Args::ListOf< Symbol > m_arg_names;
    Box m_body;
    template<typename FUNC, typename...VISITARGS>
    void visit( FUNC func, VISITARGS...visitargs ) {
        func( 0, m_arg_names, visitargs... );
        func( 0, m_body, visitargs... );
    }
    Box call( State* state ) {
        auto r = state->create<Lambda>( state->m_env, m_arg_names.m_list, m_body );
        //r->m_loc = m_body->m_loc;
        return r;
    }
};

struct BuiltinLet {
    Args::ListOf< Args::PairOf<Symbol, Box> > m_lets;
    Box m_body;
    template<typename FUNC, typename...VISITARGS>
    void visit( FUNC func, VISITARGS...visitargs ) {
        func( 0, m_lets, visitargs... );
        func( 0, m_body, visitargs... );
    }
    Box call( State* state ) {
        Env* e = state->create<Env>( state->m_env );
        GcAnchor anchor( state, e );
        for( auto item : m_lets.m_list ) {
            Box a = State_eval(state, item.second, e );
            e->put( item.first, a );
        }
        return State_eval( state, m_body, e );
    }
};

struct BuiltinCond {
    Args::Star< Args::PairOf< Box, Box > > m_cases;
    //Args::Optional<Box> m_else;
    template<typename FUNC, typename...VISITARGS>
    void visit( FUNC func, VISITARGS...visitargs ) {
        func( 0, m_cases, visitargs... );
        //visit( m_else, visitargs... );
    }
    Box call( State* state ) {
        for( auto cur : m_cases ) {
            Box c = State_eval(state, cur.first, nullptr );
            bool yes = false;
            switch(c.m_kind) {
                case Box::KIND_NIL:
                    break;
                case Box::KIND_BOOL:
                    yes = c.unbox<bool>();
                    break;
                case Box::KIND_INT:
                    yes = c.unbox<int>()!=0;
                    break;
                default:
                    Error( "Can't convert to bool" );
            }
            if( yes ) {
                return State_eval(state, cur.second, nullptr);
            }
        }
        return Box();
    }
};


struct BuiltinApply {
    Callable* m_callable = nullptr;
    List* m_args = nullptr;
    Env* m_env = nullptr;
    template<typename FUNC, typename...VISITARGS>
    void visit( FUNC func, VISITARGS...visitargs ) {
        func( 1, m_callable, visitargs... );
        func( 1, m_args, visitargs... );
        func( 1, m_env, visitargs... );
    }

    Box call( State* state ) {
        assert( 0 );
        //return m_callable->call( m_env, m_callable, m_args->view() );
        return Box();
    }
};

template<typename OPER, typename ATOM>
struct BuiltinBinOp {
    ATOM first;
    ATOM second;
    template<typename FUNC, typename...VISITARGS>
    void visit( FUNC func, VISITARGS...visitargs ) {
        func( 1, first, visitargs... );
        func( 1, second, visitargs... );
    }
    Box call( State* state ) {
        OPER oper;
        return oper( first, second ) ? Box::s_true : Box::s_false;
    }
};

struct BuiltinVecNew {
    int count;
    template<typename FUNC, typename...VISITARGS>
    void visit( FUNC func, VISITARGS...visitargs ) {
        func( 1, count, visitargs... );
    }
    Box call( State* state ) {
        int n = count;
        if( n < 0 ) {
            //Error_At( count->m_loc, "Expected n > 0" );
        }
        List* l = state->create<List>( );
        //l->m_loc = count->m_loc;
        l->resize( n );
        return l;
    }
};

struct BuiltinVecSize {
    List* vec = nullptr;
    template<typename FUNC, typename...VISITARGS>
    void visit( FUNC func, VISITARGS...visitargs ) {
        func( 1, vec, visitargs... );
    }
    Box call( State* state ) {
        return int( vec->size() );
    }
};

struct BuiltinVecIdx {
    List* vec = nullptr;
    int idx = -1;
    template<typename FUNC, typename...VISITARGS>
    void visit( FUNC func, VISITARGS...visitargs ) {
        func( 1, vec, visitargs... );
        func( 1, idx, visitargs... );
    }
    Box call( State* state ) {
        auto n = unsigned( idx );
        if( n < vec->size() ) {
            return vec->at( n );
        }
        else {
            Error( "Out of bounds %i (%i)", n, vec->size() );
            return Box();
        }
    }
};

struct BuiltinVecSet {
    List* vec = nullptr;
    int idx = -1;
    Box val;
    template<typename FUNC, typename...VISITARGS>
    void visit( FUNC func, VISITARGS...visitargs ) {
        func( 1, vec, visitargs... );
        func( 1, idx, visitargs... );
        func( 1, val, visitargs... );
    }
    Box call( State* state ) {
        auto i = unsigned( idx );
        if( i >= vec->size() ) {
            Error( "bad index" );
        }
        vec->set( i, val );
        return val;
    }
};

struct BuiltinSet {
    Symbol sym;
    Box val;
    template<typename FUNC, typename...VISITARGS>
    void visit( FUNC func, VISITARGS...visitargs ) {
        func( 0, sym, visitargs... );
        func( 1, val, visitargs... );
    }
    Box call( State* state ) {
        State_updateSymbol(state, sym, val);
        return val;
    }
};

struct BuiltinFloat {
    Box val;
    template<typename FUNC, typename...VISITARGS>
    void visit( FUNC func, VISITARGS...visitargs ) {
        func( 1, val, visitargs... );
    }
    Box call( State* state ) {
        if( val.has<double>() ) {
            return val;
        }
        else if( val.has<int>() ) {
            return double( val.unbox<int>() );
        }
        Error( "Expected a number" );
        return Box();
    }
};

struct BuiltinMap {
    Callable* callable = nullptr;
    List* list = nullptr;
    template<typename FUNC, typename...VISITARGS>
    void visit( FUNC func, VISITARGS...visitargs ) {
        func( 1, callable, visitargs... );
        func( 1, list, visitargs... );
    }

    Box call( State* state ) {
        List* r = state->create<List>( );
        for( auto i : *list ) {
            r->append( callable->call( state, callable, array_view_t::from_single( i ) ) );
        }
        return r;
    }
};

struct BuiltinList {
    std::vector<Box> args;
    template<typename FUNC, typename...VISITARGS>
    void visit( FUNC func, VISITARGS...visitargs ) {
        func( 1, args, visitargs... );
    }

    Box call( State* state ) {
        List* l = state->create<List>( );
        for( auto& a : args ) {
            l->append( a );
        }
        return l;
    }
};

struct BuiltinRange {
    int first;
    Optional<int> second;
    Optional<int> step;
    template<typename FUNC, typename...VISITARGS>
    void visit( FUNC func, VISITARGS...visitargs ) {
        func( 1, first, visitargs... );
        func( 1, second, visitargs... );
        func( 1, step, visitargs... );
    }

    Box call( State* state ) {
        int lo = 0, hi = first, st = 1;
        if( second ) {
            lo = hi;
            hi = *second;
        }
        if( step ) {
            st = *step;
        }
        List* l = state->create<List>( );
        if( hi < lo ) return l;
        if( st < 1 ) Error( "" );
        int n = ( hi - lo + st - 1 ) / st;
        l->resize( n );
        for( int i = 0; i < n; i += 1 ) {
            l->set( i, lo + i * st );
        }
        return l;
    }
};

struct BuiltinFor {
    Symbol sym;
    List* iter = nullptr;
    Args::Star< Box > body;
    template<typename FUNC, typename...VISITARGS>
    void visit( FUNC func, VISITARGS...visitargs ) {
        func( 0, sym, visitargs... );
        func( 1, iter, visitargs... );
        func( 0, body, visitargs... );
    }

    Box call( State* state ) {
        Env* e = state->create<Env>( State_getEnv(state) );
        GcAnchor anchor( state, e );
        Box r;
        for( auto a : *iter ) {
            e->put( sym, a );
            for( auto& b : body ) {
                r = State_eval(state, b, e );
            }
        }
        return r;
    }
};

template<typename REDUCE, typename ATOM>
struct ReduceCallable {
    ATOM first;
    Args::Star<ATOM> rest;
    template<typename FUNC, typename...VISITARGS>
    void visit( FUNC func, VISITARGS...visitargs ) {
        func( 1, first, visitargs... );
        func( 1, rest, visitargs... );
    }
    Box call( State* state ) {
        ATOM acc = first;
        REDUCE reduce;
        for( auto a : rest ) {
            acc = reduce( acc, a );
        }
        return acc;
    }
};

struct BinOps {
    struct Add {
        template<typename T>
        T operator()( T a, T b ) {
            return a + b; }
    };
    struct Sub {
        template<typename T>
        T operator()( T a, T b ) {
            return a - b; }
    };
    struct Mul {
        template<typename T>
        T operator()( T a, T b ) {
            return a * b; }
    };
    struct Div {
        template<typename T>
        T operator()( T a, T b ) {
            return a / b; }
    };
    struct Lt {
        template<typename T>
        bool operator()( T a, T b ) {
            return a < b; }
    };
    struct Le {
        template<typename T>
        bool operator()( T a, T b ) {
            return a <= b; }
    };
    struct Eq {
        template<typename T>
        bool operator()( T a, T b ) {
            return a == b; }
    };
    struct Lsh {
        template<typename T>
        T operator()( T a, T b ) {
            return a << b; }
    };
};

const char* strndup( const char* s, const char* e ) {
    size_t n = e - s;
    char* p = new char[n + 1];
    memcpy( p, s, n );
    p[n] = 0;
    return p;
}

Result parse_one( State* state, SourceManager::Input& in ) {
    while( 1 ) {
        switch( in.peek() ) {
            case '\0':
                return Result::ERR;
            case ' ':
            case '\r':
            case '\n':
            case '\t':
                in.eatwhite();
                break;
            case ';':
                while( int c = in.next() ) {
                    if( c == '\n' ) { break; }
                }
                break;
            case '(': {
                state->newList();
                //lst->m_loc = in.location();
                in.next();
                while( parse_one( state, in ).isOk() ) {
                    state->listAppend( -2 );
                }
                if( in.next() != ')' ) {
                    //Error_At( lst->m_loc, "Missing ')' for list begun here" );
                    throw 0;
                }
                return R_OK;
            }
            case ')':
                return Result::ERR;
            case '0': case '1': case '2': case '3': case '4':
            case '5': case '6': case '7': case '8': case '9': {
                auto loc = in.location();
                const char* s = in.peekbuf();
                bool isFloat = false;
                while( int c = in.peek() ) {
                    //if(isdigit(c) || isalpha(c) || c == '_'){
                    if( isdigit( c ) ) {
                        in.next();
                    }
                    else if( c == '.' && isFloat == false ) {
                        isFloat = true;
                        in.next();
                    }
                    else break;
                }
                int len = int( in.peekbuf() - s );
                char buf[128]; strncpy( buf, s, len );
                buf[len] = 0;
                if( isFloat ) {
                    double d = atof( buf );
                    state->newFloat( d );
                }
                else {
                    int i = atoi( buf );
                    state->newInteger( i );
                }
                //ret->m_loc = loc;
                return R_OK;
            }
            case '"': {
                auto loc = in.location();
                in.next();
                const char* s = in.peekbuf();
                while( 1 ) {
                    switch( int c = in.next() ) {
                        case -1:
                        case 0:
                            Error_At( loc, "End of input while parsing quoted string" );
                            return Result::ERR;
                        case '"':
                            state->newString( strndup( s, in.peekbuf() - 1 ) );
                            //ret->m_loc = loc;
                            return R_OK;
                        default:
                            break;
                    }
                }
                break;
            }
            default: {
                if( isalpha( in.peek() ) || in.peek() == '_' || in.peek() == '@' ) {
                    auto loc = in.location();
                    const char* s = in.peekbuf();
                    in.next();
                    while( int c = in.peek() ) {
                        if( isdigit( c ) || isalpha( c ) || c == '@' || c == '_' || c == '?' || c == '!' ) {
                            in.next();
                        }
                        else break;
                    }
                    state->newSymbol( strndup( s, in.peekbuf() ) );
                    return R_OK;
                    //ret->m_loc = loc;
                }
                else throw 0;
            }
        }
    }
}

Result parse_string( State* state, SourceManager::Input& in ) {
    if( parse_one( state, in ).isOk() ) {
        in.eatwhite();
        if( 0 ) if( in.peek() == ':' ) {
            in.next();
            //Box rhs = parse_one( state, in );
            //rhs->m_type = ret;
            //ret = rhs;
        }
        return R_OK;
    }
    return Result::ERR;
}

Result parse_file( State* state, SourceManager& sm, const char* fname ) {
    if( SourceManager::Input input = sm.load( fname ) ) {
        state->newList();
        while( parse_string( state, input ).isOk() ) {
            state->listAppend( -2 );
        }
        return Result::OK;
    }
    else {
        Error( "Unable to open '%s'", fname );
        return Result::ERR;
    }
}

Result initBuiltins( State* state ) {
    state->let( "module", state->create<BuiltinCallable<BuiltinModule>>( ) );
    state->let( "eval", state->create<BuiltinCallable<BuiltinEval>>( ) );
    state->let( "evsym", state->create<BuiltinCallable<BuiltinEvSym>>( ) );
    state->let( "begin", state->create<BuiltinCallable<BuiltinBegin>>( ) );
    state->let( "quote", state->create<BuiltinCallable<BuiltinQuote>>( ) );
    state->let( "inc!", state->create<BuiltinCallable<BuiltinInc>>( ) );
    state->let( "define", state->create<BuiltinCallable<BuiltinDefine>>( ) );
    state->let( "lambda", state->create<BuiltinCallable<BuiltinLambda>>( ) );
    state->let( "vau", state->create<BuiltinCallable<BuiltinVau>>( ) );
    state->let( "let", state->create<BuiltinCallable<BuiltinLet>>( ) );
    state->let( "cond", state->create<BuiltinCallable<BuiltinCond>>( ) );

    //state->let( "tail", state->create<LambdaTail>( ) );
//    state->let( "wrap", state->create<BuiltinWrap>() );

    state->let( "map", state->create<BuiltinCallable<BuiltinMap>>( ) );
    state->let( "print", state->create<BuiltinCallable<BuiltinPrint>>( ) );
    state->let( "list", state->create<BuiltinCallable<BuiltinList>>( ) );
    state->let( "add_i", state->create<BuiltinCallable<ReduceCallable<BinOps::Add, int>>>( ) );
    state->let( "add_f", state->create<BuiltinCallable<ReduceCallable<BinOps::Add, double>>>( ) );
    state->let( "sub_i", state->create<BuiltinCallable<ReduceCallable<BinOps::Sub, int>>>( ) );
    state->let( "sub_f", state->create<BuiltinCallable<ReduceCallable<BinOps::Sub, double>>>( ) );
    state->let( "mul_i", state->create<BuiltinCallable<ReduceCallable<BinOps::Mul, int>>>( ) );
    state->let( "mul_f", state->create<BuiltinCallable<ReduceCallable<BinOps::Mul, double>>>( ) );
    state->let( "div_i", state->create<BuiltinCallable<ReduceCallable<BinOps::Div, int>>>( ) );
    state->let( "div_f", state->create<BuiltinCallable<ReduceCallable<BinOps::Div, double>>>( ) );
    state->let( "lsh", state->create<BuiltinCallable<ReduceCallable<BinOps::Lsh, int>>>( ) );
    state->let( "apply", state->create<BuiltinCallable<BuiltinApply>>( ) );
    state->let( "lt_i?", state->create<BuiltinCallable<BuiltinBinOp<BinOps::Lt, int>>>( ) );
    state->let( "lt_f?", state->create<BuiltinCallable<BuiltinBinOp<BinOps::Lt, double>>>( ) );
    state->let( "le_i?", state->create<BuiltinCallable<BuiltinBinOp<BinOps::Le, int>>>( ) );
    state->let( "le_f?", state->create<BuiltinCallable<BuiltinBinOp<BinOps::Le, double>>>( ) );
    state->let( "eq_i?", state->create<BuiltinCallable<BuiltinBinOp<BinOps::Eq, int>>>( ) );
    state->let( "eq_f?", state->create<BuiltinCallable<BuiltinBinOp<BinOps::Eq, double>>>( ) );
    state->let( "range", state->create<BuiltinCallable<BuiltinRange>>( ) );
    state->let( "for", state->create<BuiltinCallable<BuiltinFor>>( ) );
    state->let( "vec_new", state->create<BuiltinCallable<BuiltinVecNew>>( ) );
    state->let( "vec_idx", state->create<BuiltinCallable<BuiltinVecIdx>>( ) );
    state->let( "vec_set!", state->create<BuiltinCallable<BuiltinVecSet>>( ) );
    state->let( "vec_size", state->create<BuiltinCallable<BuiltinVecSize>>( ) );
    state->let( "float", state->create<BuiltinCallable<BuiltinFloat>>( ) );
    state->let( "set!", state->create<BuiltinCallable<BuiltinSet>>( ) );

    state->let( "true", Box::s_true );
    state->let( "false", Box::s_false );
    #if 0
    state->let( "Int", &Type::s_int );
    state->let( "Float", &Type::s_float );
    
    #endif
    return R_OK;
}

int main( int argc, const char* argv[] ) {
    if( argc < 2 ) {
        Error( "Need a script to run" );
        return 1;
    }
    try {
        State* state = new State();
        initBuiltins( state );

        SourceManager sm;
        state->getGlobal( "module" );
        if( parse_file( state, sm, argv[1] ).isOk() ) {
            state->call( 1, 1 );
            state->getGlobal( "main" );
            state->newList();
            state->newSymbol( "quote" );
            state->listAppend( -2 );
            static SourceManager::FileInfo cmdline{ "cmdline" };
            for( int i = 2; i < argc; ++i ) {
                SourceManager::Input in( argv[i], argv[i] + strlen( argv[i] ), &cmdline );
                if( parse_string( state, in ).isOk() ) {
                    state->listAppend( -2 );
                }
            }
            state->call( 1, 1 );
            int ret = 0;
            Box r = state->pop();
            switch( r.m_kind ) {
                case Box::KIND_INT:
                    ret = r.unbox<int>();
                    break;
                case Box::KIND_NIL:
                case Box::KIND_ATOM:
                    ret = 0;
                    break;
                default:
                    Error( "main() must return a value" );
            }
            return ret;
        }
    }
    catch( float ) {
        return 1;
    }
}
