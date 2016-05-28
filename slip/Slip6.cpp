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

template<typename T, typename... P>
T* gcnew( P... p ) {
    void* addr = malloc( sizeof( T ) );
    return new ( addr ) T( p... );
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

struct Atom;
struct Symbol;
struct Callable;
struct Env;
struct List;
struct Type;
struct Box;

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
    Box eval( Env* env ) const;// { aptr->print(); }
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

    Box() : m_kind( KIND_NIL ) {}
    Box( Symbol s ) : m_kind( KIND_SYMBOL ) { m_val.s = s; }
    Box( double f ) : m_kind( KIND_FLOAT ) { m_val.f = f; }
    Box( bool b ) : m_kind( KIND_BOOL ) { m_val.i = b; }
    Box( int i ) : m_kind( KIND_INT ) { m_val.i = i; }
    Box( Atom* a ) : m_kind( KIND_ATOM ) { m_val.a = a; }
    explicit operator bool() { return m_kind != KIND_NIL; }

    template<typename T> bool has() const;
    template<typename T> T unbox() const;

    void print() const;
    Box eval( Env* env ) const;

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
const Box Box::s_false( true );

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

struct Atom {
    void* operator new( size_t ) = delete;
    void* operator new( size_t, void* p ){ return p; }
        virtual ~Atom() {}
    Box eval( Env* env ) {
        return _eval( env );
    }
    void print() const { _print(); }
    SourceManager::Location m_loc;
    Atom* m_type;
protected:
    virtual Box _eval( Env* env ) = 0;
    virtual void _print() const = 0;
};

struct Value : Atom {
    Box _eval( Env* env ) override {
        return Box( this );
    }
};

struct Type : public Value {
    void _print() const override {
        printf( "type" );
    }
    static Type s_bool;
    static Type s_string;
    static Type s_int;
    static Type s_float;
    static Type s_list;
};

Type Type::s_int;
Type Type::s_float;
Type Type::s_string;

struct String : Value {
    String( const char* s = nullptr ) : m_str( s ) { m_type = &Type::s_string; }
    void _print() const override {
        printf( "%s", m_str );
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
    Env* m_parent;
    typedef std::unordered_map<Symbol, Box> Table;
    Table m_tab;
};

struct Callable : Value {
    typedef array_view<Box> ArgList;
    Box call( Env* env, Box arg0, ArgList args ) {
        return _call( env, arg0, args );
    }
    void _print() const override {
        printf( "<callable>" );
    }
protected:
    virtual Box _call( Env* env, Box arg0, ArgList args ) = 0;
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

Box Box::eval( Env* env ) const {
    switch( m_kind ) {
        case KIND_SYMBOL:
            return env->get( m_val.s );
        case KIND_ATOM:
            return m_val.a->eval( env );
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
private:
    Optional( const Optional& o );
    void operator=( const Optional& o );
    void* m_buf[( sizeof( T ) + sizeof( void* ) - 1 ) / sizeof( void* )];
    T* m_ptr;
};

namespace Args {
    template<typename T>
    struct Unevaluated {
        void operator=( const T& t ) {
            m_value = t;
        }
        T& get() { return m_value; }
        operator T&( ) { return m_value; }
        operator Box() { return m_value; }
        T& operator->() { return m_value; }
        T m_value;
    };
    template<typename T>
    struct Evaluated {
        void operator=( const T& t ) {
            m_value = t;
        }
        T& get() { return m_value; }
        operator T&( ) { return m_value; }
        operator Box() { return m_value; }
        T& operator->() { return m_value; }
        T m_value;
    };
    template<>
    struct Unevaluated<Box> {
        void operator=( Box t ) {
            m_value = t;
        }
        Box& get() { return m_value; }
        operator Box() { return m_value; }
        Box m_value;
    };

    template<typename T>
    using Star = std::vector<T>;

    template<typename T>
    struct ListOf {
        ListOf() : m_list( gcnew<List>( ) ) {}
        operator List*( ) {
            return m_list;
        }
        void append( T t ) {
            m_list->append( Box( t ) );
        }
        List* m_list;
    };

    template<typename F, typename S>
    struct PairOf {
        PairOf() : m_list( gcnew<List>( ) ) {}
        operator List*( ) {
            return m_list;
        }
        void append( F f, S s ) {
            m_list->append( f );
            m_list->append( s );
        }
        List* m_list;
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
        void bind( Evaluated<T>& out, Env* env, ArgIter& iter ) {
            assert( iter.more() );
            Box b = iter.cur().eval( env );
            assert2( b.has<T>(), "Wrong argument type" );
            out = b.unbox<T>();
            iter.next();
        }
        template<typename T>
        void bind( Star<T>& out, Env* env, ArgIter& iter ) {
            for( ; iter.more(); ) {
                T t;
                bind( t, env, iter );
                out.push_back( t );
            }
        }
        template<typename T>
        void bind( ListOf<T>& out, Env* env, ArgIter& iter ) {
            List* lst = cast( List, iter.cur() );
            assert( lst );
            iter.next();
            ArgIter iter2{ lst->view().begin(), lst->view().end() };
            for( ; iter2.more(); ) {
                T t;
                bind( t, env, iter2 );
                out.append( t );
            }
        }
        template<typename F, typename S>
        void bind( PairOf<F, S>& out, Env* env, ArgIter& iter ) {
            List* lst = cast( List, iter.cur() );
            if( !lst || lst->size() != 2 )
                Error( "Expected a pair of items" );
            //               Error_At( iter.cur()->m_loc, "Expected a pair of items" );
            iter.next();
            ArgIter iter2{ lst->view().begin(), lst->view().end() };
            F f; S s;
            bind( f, env, iter2 );
            bind( s, env, iter2 );
            out.append( f, s );
        }
        template<typename T>
        void bind( Unevaluated<T>& out, Env* env, ArgIter& iter ) {
            assert( iter.more() );
            assert2( iter.cur().has<T>(), "Wrong argument type" );
            out = iter.cur().unbox<T>();
            iter.next();
        }
        template<typename T>
        void bind( Optional<T>& out, Env* env, ArgIter& args ) {
            if( args.more() ) {
                T t;
                bind( t, env, args );
                out.set( t );
            }
        }
    }

    struct Binder {
        template<typename T>
        void operator() ( T& out, Env* env, Detail::ArgIter& iter ) {
            Detail::bind( out, env, iter );
        }
    };
}

template<typename CALL>
struct BuiltinCallable : Callable {
    virtual Box _call( Env* env, Box arg0, ArgList args ) override {
        CALL call;
        Args::Detail::ArgIter iter{ args.begin(), args.end() };
        call.visit<Args::Binder, Env*, Args::Detail::ArgIter&>( Args::Binder(), env, iter );
        assert2( iter.more() == false, "Too many arguments" );
        return call.call( env );
    }
};


struct List : Atom {
    typedef std::vector<Box> vector;
    Box _eval( Env* env ) override {
        if( lst.empty() ) {
            Error_At( m_loc, "Empty list is illegal" );
        }
        Box atom = lst[0].eval( env );
        if( auto call = cast( Callable, atom ) ) {
            return call->call( env, lst[0], Callable::ArgList( lst ).ltrim( 1 ) );
        }
        //Error_At( lst[0]->m_loc, "Expected a callable as the first argument" );
        return Box();
    }
    #if 0
    Box _normalize() override {
        std::vector<List*> lets;
        List* fin = gcnew<List>( );
        for( auto arg : lst ) {
            auto a = normalize2( arg, lets );
            fin->append( a );
        }
        List* ret = gcnew<List>( );
        ret->append( gcnew<Symbol>( "let" ) );
        List* loc = gcnew<List>( );
        for( auto l : lets ) {
            loc->append( l );
        }
        ret->append( loc );
        ret->append( fin );
        return ret;
    }
    static Box normalize2( Box arg, std::vector<List*>& lets ) {
        if( auto l = cast( List, arg ) ) {
            List* simp = gcnew<List>( );
            for( auto a : l->lst ) {
                Box b = normalize2( a, lets );
                simp->append( b );
            }
            Symbol* sym;
            {
                char buf[256];
                sprintf( buf, "t%zi", lets.size() );
                sym = gcnew<Symbol>( strdup( buf ) );
            }
            List* let = gcnew<List>( );
            let->append( sym );
            let->append( simp );
            lets.push_back( let );
            return sym;
        }
        else {
            return arg->normalize();
        }
    }
    #endif

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
protected:
    std::vector<Box> lst;
};

struct Lambda : Callable {
    //    static Bool s_trampoline;
    List* m_arg_names;
    Env* m_lex_env;
    Box m_body;

    Lambda( Env* lex_env, List* arg_names, Box body )
        : m_arg_names( arg_names ), m_lex_env( lex_env ), m_body( body ) {
    }

    Box _call( Env* env, Box arg0, ArgList args ) override {
        //Args::Detail::ArgIter iter{ args_in.begin(), args_in.end() };
        assert( args.size() == m_arg_names->size() );
        Env* e = gcnew<Env>( m_lex_env );
        unsigned ni = 0;
        for( auto arg : args ) {
            Box a = arg.eval( env );
            //auto s = cast( Symbol, m_arg_names->at( ni ) );
            auto s = m_arg_names->at( ni ).unbox<Symbol>();
            //if( s->m_type != nullptr ) {
                /*auto nt = s->m_type.eval( env );
                auto at = a->m_type.eval( env );
                if( at != nt ) {
                    Error_At( arg->m_loc, "Mismatched type for argument '%i'", ni );
                }*/
                //}
            e->put( s, a );
            ni += 1;
        }
        while( 1 ) {
            Box ret = m_body.eval( e );
            //            if( ret != &s_trampoline ) {
            return ret;
            //          }
        }
    }
    #if 0
    Box _normalize() override {
        Box body = m_body->normalize();
        return gcnew<Lambda>( m_lex_env, m_arg_names, body );
    }
    #endif
    void _print() const override {
        printf( "(lambda (" );
        for( auto a : *m_arg_names ) {
            a.print();
            printf( " " );
        }
        printf( ") " );
        m_body.print();
    }
};
//Bool Lambda::s_trampoline(false);

struct Vau : public Callable {
    List* m_arg_names;
    Env* m_lex_env;
    Symbol m_env_sym;
    Box m_body;
    Vau( Env* lex_env, List* arg_names, Symbol symbol, Box body )
        : m_arg_names( arg_names ), m_lex_env( lex_env ), m_env_sym( symbol ), m_body( body ) {
    }
    Box _call( Env* env, Box arg0, ArgList args ) override {
        Env* e = gcnew<Env>( m_lex_env );
        e->put( m_env_sym, env );
        assert( args.size() == m_arg_names->size() );
        for( unsigned i = 0; i < args.size(); ++i ) {
            e->put( m_arg_names->at( i ).unbox<Symbol>(), args[i] );
        }
        return m_body.eval( e );
    }

};

struct BuiltinVau {
    Args::ListOf< Args::Unevaluated<Symbol> > m_arg_names;
    Args::Unevaluated<Symbol> m_env_sym;
    Args::Unevaluated<Box> m_body;

    template<typename VISIT, typename...VISITARGS>
    void visit( VISIT visit, VISITARGS...visitargs ) {
        visit( m_arg_names, visitargs... );
        visit( m_env_sym, visitargs... );
        visit( m_body, visitargs... );
    }

    Box call( Env* env ) {
        return gcnew<Vau>( env, m_arg_names, m_env_sym, m_body );
    }
};

struct BuiltinPrint {
    Args::Star< Args::Evaluated<Box> > m_args;
    template<typename VISIT, typename...VISITARGS>
    void visit( VISIT visit, VISITARGS...visitargs ) {
        visit( m_args, visitargs... );
    }
    Box call( Env* env ) {
        const char* sep = "";
        for( auto arg : m_args ) {
            printf( "%s", sep );
            arg.get().print();
            sep = " ";
        }
        printf( "\n" );
        return nullptr;
    }
};

//struct LambdaTail : Callable {
//    LambdaTail() {}
//    Box _call( Env* env, Box arg0, ArgList args ) override {
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
    State() {
        m_env = gcnew<Env>( nullptr );
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
    int popInteger( int idx ) {
        int r = 0;
        if( m_stack[idx].unbox<int>() ) {
            r = m_stack[idx].unbox<int>();
            m_stack.pop( 1 );
        }
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
            Box r = c->call( m_env, c, Callable::ArgList( begin + size - narg, begin + size ) );
            m_stack.pop( narg + 1 );
            m_stack.push_back( r );
            assert( nret == 1 );
            return R_OK;
        }
        return Result::ERR;
    }

    Env* m_env;
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
    std::unordered_set<std::string> m_interns;
    const char* intern( const char* s ) {
        auto it = m_interns.insert( s );
        return it.first->c_str();
    }
};


struct BuiltinEval {
    Args::Unevaluated<Box> m_expr;
    Optional< Args::Evaluated<Env*> > m_env;

    template<typename VISIT, typename...VISITARGS>
    void visit( VISIT visit, VISITARGS...visitargs ) {
        visit( m_expr, visitargs... );
        visit( m_env, visitargs... );
    }
    Box call( Env* env ) {
        if( m_env ) {
            return m_expr.get().eval( *m_env );
        }
        else {
            return m_expr.get().eval( env );
        }
    }
};

struct BuiltinEvSym {
    Args::Evaluated<Box> m_expr;
    Optional<Args::Evaluated<Env*>> m_env;

    template<typename VISIT, typename...VISITARGS>
    void visit( VISIT visit, VISITARGS...visitargs ) {
        visit( m_expr, visitargs... );
        visit( m_env, visitargs... );
    }
    Box call( Env* env ) {
        if( m_env ) {
            return m_expr.get().eval( *m_env );
        }
        else {
            return m_expr.get().eval( env );
        }
    }
};

Box v_type( Env* env, Callable::ArgList args ) {
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
    Args::Star< Args::Unevaluated<Box > > m_exprs;
    template<typename VISIT, typename...VISITARGS>
    void visit( VISIT visit, VISITARGS...visitargs ) {
        visit( m_exprs, visitargs... );
    }
    Box call( Env* env ) {
        Box r = nullptr;
        for( auto a : m_exprs ) {
            r = a.get().eval( env );
        }
        return r;
    }
};

struct BuiltinModule {
    Args::Unevaluated< List* > m_exprs;
    template<typename VISIT, typename...VISITARGS>
    void visit( VISIT visit, VISITARGS...visitargs ) {
        visit( m_exprs, visitargs... );
    }
    Box call( Env* env ) {
        Box r = nullptr;
        for( auto a : *m_exprs ) {
            r = a.eval( env );
        }
        return r;
    }
};

struct BuiltinQuote {
    Args::Star< Args::Unevaluated<Box> > m_args;
    template<typename VISIT, typename...VISITARGS>
    void visit( VISIT visit, VISITARGS...visitargs ) {
        visit( m_args, visitargs... );
    }
    Box call( Env* env ) {
        List* l = gcnew<List>( );
        for( auto a : m_args ) {
            l->append( a );
        }
        return l;
    }
};

struct BuiltinInc {
    Args::Unevaluated< Symbol > m_sym;
    template<typename VISIT, typename...VISITARGS>
    void visit( VISIT visit, VISITARGS...visitargs ) {
        visit( m_sym, visitargs... );
    }
    Box call( Env* env ) {
        Box a = env->get( m_sym );
        assert( a.has<int>() );
        int n = a.unbox<int>() + 1;
        env->put( m_sym.get(), n );
        return n;
    }
};

struct BuiltinDefine {
    Args::Unevaluated<Symbol> m_sym;
    Args::Evaluated<Box> m_value;
    template<typename VISIT, typename...VISITARGS>
    void visit( VISIT visit, VISITARGS...visitargs ) {
        visit( m_sym, visitargs... );
        visit( m_value, visitargs... );
    }
    Box call( Env* env ) {
        env->put( m_sym, m_value.get() );
        return m_value.get();
    }
};

struct BuiltinLambda {
    Args::ListOf< Args::Unevaluated<Symbol> > m_arg_names;
    Args::Unevaluated<Box> m_body;
    template<typename VISIT, typename...VISITARGS>
    void visit( VISIT visit, VISITARGS...visitargs ) {
        visit( m_arg_names, visitargs... );
        visit( m_body, visitargs... );
    }
    Box call( Env* env ) {
        auto r = gcnew<Lambda>( env, m_arg_names, m_body );
        //r->m_loc = m_body->m_loc;
        return r;
    }
};

struct BuiltinLet {
    Args::ListOf< Args::PairOf< Args::Unevaluated<Symbol>, Args::Unevaluated<Box> > > m_lets;
    Args::Unevaluated< Box > m_body;
    template<typename VISIT, typename...VISITARGS>
    void visit( VISIT visit, VISITARGS...visitargs ) {
        visit( m_lets, visitargs... );
        visit( m_body, visitargs... );
    }
    Box call( Env* env ) {
        Env* e = gcnew<Env>( env );
        for( auto item : *m_lets ) {
            List* l = cast( List, item );
            assert( l );
            Symbol s = l->at( 0 ).unbox<Symbol>();
            Box a = l->at( 1 ).eval( e );
            e->put( s, a );
        }
        return m_body.get().eval( e );
    }

};

struct BuiltinCond {
    Args::Star< Args::PairOf< Args::Unevaluated<Box>, Args::Unevaluated<Box> > > m_cases;
    //Args::Optional<Box> m_else;
    template<typename VISIT, typename...VISITARGS>
    void visit( VISIT visit, VISITARGS...visitargs ) {
        visit( m_cases, visitargs... );
        //visit( m_else, visitargs... );
    }
    Box call( Env* env ) {
        for( auto cur : m_cases ) {
            List* l = cur.m_list;
            assert( l && l->size() == 2 );
            if( l->at( 0 ).eval( env ).unbox<bool>() ) {
                return l->at( 1 ).eval( env );
            }
        }
        return nullptr;
    }
};


struct BuiltinApply {
    Args::Evaluated<Callable*> m_callable;
    Args::Evaluated<List*> m_args;
    Args::Evaluated<Env*> m_env;
    template<typename VISIT, typename...VISITARGS>
    void visit( VISIT visit, VISITARGS...visitargs ) {
        visit( m_callable, visitargs... );
        visit( m_args, visitargs... );
        visit( m_env, visitargs... );
    }

    Box call( Env* env ) {
        return m_callable->call( m_env, m_callable, m_args->view() );
    }
};

template<typename OPER, typename ATOM>
struct BuiltinBinOp {
    Args::Evaluated<ATOM> first;
    Args::Evaluated<ATOM> second;
    template<typename VISIT, typename...VISITARGS>
    void visit( VISIT visit, VISITARGS...visitargs ) {
        visit( first, visitargs... );
        visit( second, visitargs... );
    }
    Box call( Env* env ) {
        OPER oper;
        return oper( first.get(), second.get() ) ? Box::s_true : Box::s_false;
    }
};

struct BuiltinVecNew {
    Args::Evaluated<int> count;
    template<typename VISIT, typename...VISITARGS>
    void visit( VISIT visit, VISITARGS...visitargs ) {
        visit( count, visitargs... );
    }
    Box call( Env* env ) {
        int n = count;
        if( n < 0 ) {
            //Error_At( count->m_loc, "Expected n > 0" );
        }
        List* l = gcnew<List>( );
        //l->m_loc = count->m_loc;
        l->resize( n );
        return l;
    }
};

struct BuiltinVecSize {
    Args::Evaluated<List*> vec;
    template<typename VISIT, typename...VISITARGS>
    void visit( VISIT visit, VISITARGS...visitargs ) {
        visit( vec, visitargs... );
    }
    Box call( Env* env ) {
        return int( vec->size() );
    }
};

struct BuiltinVecIdx {
    Args::Evaluated<List*> vec;
    Args::Evaluated<int> idx;
    template<typename VISIT, typename...VISITARGS>
    void visit( VISIT visit, VISITARGS...visitargs ) {
        visit( vec, visitargs... );
        visit( idx, visitargs... );
    }
    Box call( Env* env ) {
        auto n = unsigned( idx );
        if( n < vec->size() ) {
            return vec->at( n );
        }
        else {
            //Error_At( idx->m_loc, "Out of bounds %i (%i)", n, vec->size() );
            return nullptr;
        }
    }
};

struct BuiltinVecSet {
    Args::Evaluated<List*> vec;
    Args::Evaluated<int> idx;
    Args::Evaluated<Box> val;
    template<typename VISIT, typename...VISITARGS>
    void visit( VISIT visit, VISITARGS...visitargs ) {
        visit( vec, visitargs... );
        visit( idx, visitargs... );
        visit( val, visitargs... );
    }
    Box call( Env* env ) {
        auto i = unsigned( idx );
        if( i >= vec->size() ) {
            Error( "bad index" );
        }
        vec->set( i, val.get() );
        return val.get();
    }
};


Box l_float( Env* env, Callable::ArgList args ) {
    if( args.size() != 1 ) {
        Error( "Expected 1 argument" );
    }
    Box a = args[0];
    if( a.unbox<double>() ) {
        return a;
    }
    else if( a.unbox<int>() ) {
        return double( a.unbox<int>() );
    }
    Error( "Expected a number" );
    return Box();
}

struct BuiltinMap {
    Args::Evaluated<Callable*> func;
    Args::Evaluated<List*> list;
    template<typename VISIT, typename...VISITARGS>
    void visit( VISIT visit, VISITARGS...visitargs ) {
        visit( func, visitargs... );
        visit( list, visitargs... );
    }

    Box call( Env* env ) {
        List* r = gcnew<List>( );
        for( auto i : *list ) {
            r->append( func->call( env, func, array_view_t::from_single( i ) ) );
        }
        return r;
    }
};

struct BuiltinList {
    std::vector<Args::Evaluated<Box>> args;
    template<typename VISIT, typename...VISITARGS>
    void visit( VISIT visit, VISITARGS...visitargs ) {
        visit( args, visitargs... );
    }

    Box call( Env* env ) {
        List* l = gcnew<List>( );
        for( auto& a : args ) {
            l->append( a.get() );
        }
        return l;
    }
};

struct BuiltinRange {
    Args::Evaluated<int> first;
    Optional<Args::Evaluated<int>> second;
    Optional<Args::Evaluated<int>> step;
    template<typename VISIT, typename...VISITARGS>
    void visit( VISIT visit, VISITARGS...visitargs ) {
        visit( first, visitargs... );
        visit( second, visitargs... );
        visit( step, visitargs... );
    }

    Box call( Env* env ) {
        int lo = 0, hi = first, st = 1;
        if( second ) {
            lo = hi;
            hi = ( *second ).get();
        }
        if( step ) {
            st = ( *step ).get();
        }
        List* l = gcnew<List>( );
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
    Args::Unevaluated<Symbol> sym;
    Args::Evaluated<List*> iter;
    Args::Star< Args::Unevaluated< Box > > body;
    template<typename VISIT, typename...VISITARGS>
    void visit( VISIT visit, VISITARGS...visitargs ) {
        visit( sym, visitargs... );
        visit( iter, visitargs... );
        visit( body, visitargs... );
    }

    Box call( Env* env ) {
        Env* e = gcnew<Env>( env );
        Box r = nullptr;
        for( auto a : *iter ) {
            e->put( sym, a );
            for( auto& b : body ) {
                r = b.m_value.eval( e );
            }
        }
        return r;
    }
};

template<typename REDUCE, typename ATOM>
struct ReduceCallable {
    Args::Evaluated<ATOM> first;
    Args::Star<Args::Evaluated<ATOM>> rest;
    template<typename VISIT, typename...VISITARGS>
    void visit( VISIT visit, VISITARGS...visitargs ) {
        visit( first, visitargs... );
        visit( rest, visitargs... );
    }
    Box call( Env* env ) {
        ATOM acc = first.get();
        REDUCE reduce;
        for( auto a : rest ) {
            acc = reduce( acc, a.get() );
        }
        return acc;
    }
};

struct BinOps {
    struct Add {
        template<typename T>
        T operator()( T a, T b ) { return a + b; }
    };
    struct Sub {
        template<typename T>
        T operator()( T a, T b ) { return a - b; }
    };
    struct Mul {
        template<typename T>
        T operator()( T a, T b ) { return a * b; }
    };
    struct Div {
        template<typename T>
        T operator()( T a, T b ) { return a / b; }
    };
    struct Lt {
        template<typename T>
        bool operator()( T a, T b ) { return a < b; }
    };
    struct Le {
        template<typename T>
        bool operator()( T a, T b ) { return a <= b; }
    };
    struct Eq {
        template<typename T>
        bool operator()( T a, T b ) { return a == b; }
    };
    struct Lsh {
        template<typename T>
        T operator()( T a, T b ) { return a << b; }
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
    state->let( "module", gcnew<BuiltinCallable<BuiltinModule>>( ) );
    state->let( "eval", gcnew<BuiltinCallable<BuiltinEval>>( ) );
    state->let( "evsym", gcnew<BuiltinCallable<BuiltinEvSym>>( ) );
    state->let( "begin", gcnew<BuiltinCallable<BuiltinBegin>>( ) );
    state->let( "quote", gcnew<BuiltinCallable<BuiltinQuote>>( ) );
    state->let( "inc!", gcnew<BuiltinCallable<BuiltinInc>>( ) );
    state->let( "define", gcnew<BuiltinCallable<BuiltinDefine>>( ) );
    state->let( "lambda", gcnew<BuiltinCallable<BuiltinLambda>>( ) );
    state->let( "vau", gcnew<BuiltinCallable<BuiltinVau>>( ) );
    state->let( "let", gcnew<BuiltinCallable<BuiltinLet>>( ) );
    state->let( "cond", gcnew<BuiltinCallable<BuiltinCond>>( ) );

    //state->let( "tail", gcnew<LambdaTail>( ) );
//    state->let( "wrap", gcnew<BuiltinWrap>() );

    state->let( "map", gcnew<BuiltinCallable<BuiltinMap>>( ) );
    state->let( "print", gcnew<BuiltinCallable<BuiltinPrint>>( ) );
    state->let( "list", gcnew<BuiltinCallable<BuiltinList>>( ) );
    state->let( "add_i", gcnew<BuiltinCallable<ReduceCallable<BinOps::Add, int>>>( ) );
    state->let( "add_f", gcnew<BuiltinCallable<ReduceCallable<BinOps::Add, double>>>( ) );
    state->let( "sub_i", gcnew<BuiltinCallable<ReduceCallable<BinOps::Sub, int>>>( ) );
    state->let( "sub_f", gcnew<BuiltinCallable<ReduceCallable<BinOps::Sub, double>>>( ) );
    state->let( "mul_i", gcnew<BuiltinCallable<ReduceCallable<BinOps::Mul, int>>>( ) );
    state->let( "mul_f", gcnew<BuiltinCallable<ReduceCallable<BinOps::Mul, double>>>( ) );
    state->let( "div_i", gcnew<BuiltinCallable<ReduceCallable<BinOps::Div, int>>>( ) );
    state->let( "div_f", gcnew<BuiltinCallable<ReduceCallable<BinOps::Div, double>>>( ) );
    state->let( "lsh", gcnew<BuiltinCallable<ReduceCallable<BinOps::Lsh, int>>>( ) );
    state->let( "apply", gcnew<BuiltinCallable<BuiltinApply>>( ) );
    state->let( "lt_i?", gcnew<BuiltinCallable<BuiltinBinOp<BinOps::Lt, int>>>( ) );
    state->let( "lt_f?", gcnew<BuiltinCallable<BuiltinBinOp<BinOps::Lt, double>>>( ) );
    state->let( "le_i?", gcnew<BuiltinCallable<BuiltinBinOp<BinOps::Le, int>>>( ) );
    state->let( "le_f?", gcnew<BuiltinCallable<BuiltinBinOp<BinOps::Le, double>>>( ) );
    state->let( "eq_i?", gcnew<BuiltinCallable<BuiltinBinOp<BinOps::Eq, int>>>( ) );
    state->let( "eq_f?", gcnew<BuiltinCallable<BuiltinBinOp<BinOps::Eq, double>>>( ) );
    state->let( "range", gcnew<BuiltinCallable<BuiltinRange>>( ) );
    state->let( "for", gcnew<BuiltinCallable<BuiltinFor>>( ) );
    state->let( "vec_new", gcnew<BuiltinCallable<BuiltinVecNew>>( ) );
    state->let( "vec_idx", gcnew<BuiltinCallable<BuiltinVecIdx>>( ) );
    state->let( "vec_set!", gcnew<BuiltinCallable<BuiltinVecSet>>( ) );
    state->let( "vec_size", gcnew<BuiltinCallable<BuiltinVecSize>>( ) );

    #if 0
    state->let( "float", gcnew<BuiltinLambda>( &l_float ) );
    #endif
    state->let( "Int", &Type::s_int );
    state->let( "Float", &Type::s_float );
    state->let( "true", Box::s_true );
    state->let( "false", Box::s_false );
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
            int ret = state->popInteger( -1 );
            return ret;
        }
    }
    catch( float ) {
        return 1;
    }
}
