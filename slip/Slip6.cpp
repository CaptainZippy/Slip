
#include "Pch.h"

void error( const char* msg ) {
    __debugbreak();
}
template<typename T>
void assert2( T t, const char* msg ) {
    if( !t ) Detail::_Error( msg );
}
#define assert(A) assert2(A, #A)
#define cast(T,a) dynamic_cast<T*>(a)

struct Result {
    enum Code { OK=0, ERR=1 };
    Result(Code c) : code(c) {}
    bool isOk() const { return code == OK; }
    Code code;
};
#define R_OK Result::OK

#define Error( fmt, ... ) Detail::_Error("%s(%i,%i): error " fmt, __FILE__, __LINE__, 0, __VA_ARGS__)
#define Error_At( loc, fmt, ... ) Detail::_Error("%s(%i,%i): error " fmt, loc.filename(), loc.line(), loc.col(), __VA_ARGS__)

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
    array_view( ) : m_begin( nullptr ), m_end( nullptr ) {}
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

namespace array_view_t
{
    template<typename T>
    array_view<T> from_single( const T& t ) { return array_view<T>( &t, &t + 1 ); }
    template<typename T, int N>
    array_view<T> make( T( &arr )[N] ) { return array_view<T>( t, t + N ); }
}


template<typename T, typename... P>
T* gcnew(P... p) {
    void* addr = malloc(sizeof(T));
    return new (addr) T(p...);
}

template<typename S>
struct safe_cast_t {
    safe_cast_t(const S& s) : m_s(s) {}
    template<typename D>
    operator D() { assert(S(D(m_s)) == m_s); return D(m_s); }
    const S& m_s;
};

template<typename T>
safe_cast_t<T> safe_cast(const T& t) {
    return safe_cast_t<T>(t);
};

struct Atom;
struct Symbol;
struct Env;
struct List;
struct Type;

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
                return safe_cast(std::count( txt.begin(), txt.begin() + m_off, '\n' ) + 1);
            }
            return 0;
        }
        int col() const {
            if( m_file ) {
                auto& txt = m_file->m_contents;
                auto nl = txt.rfind( '\n', m_off );
                return safe_cast(m_off - ( ( nl == std::string::npos ) ? 0 : nl ));
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
            return Location{ info, safe_cast(cur - start) };
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

struct Atom {
    void* operator new(size_t) = delete;
    void* operator new(size_t, void* p){ return p; }
    virtual ~Atom() {}
    Atom* eval(Env* env) {
        //printf( "\nEVAL " );print();
        return _eval( env );
    }
    void print() const { _print(); }
    Atom() : m_type( nullptr ) {}
    SourceManager::Location m_loc;
    Atom* m_type;
protected:
    virtual Atom* _eval( Env* env ) = 0;
    virtual void _print() const = 0;
};

struct Value : Atom {
    Atom* _eval( Env* env ) override {
        return this;
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

struct Bool : Value {
    Bool( bool v ) : m_val( v ) {}
    void _print() const override {
        printf( m_val ? "true" : "false" );
    }
    bool m_val;

    static Bool s_false;
    static Bool s_true;
};

Bool Bool::s_false{ false };
Bool Bool::s_true{ true };


struct Int : Value {
    Int( int n ) : m_num( n ) { m_type = &Type::s_int; }
    bool equals( const Int* f ) const {
        return m_num == f->m_num;
    }
    void _print() const override {
        printf( "%i", m_num );
    }
    int m_num;
};

struct Float : Value {
    Float( double d ) : m_num( d ) { m_type = &Type::s_float; }
    bool equals( const Float* f ) const {
        return m_num == f->m_num;
    }
    void _print() const override {
        printf( "%lf", m_num );
    }
    double m_num;
};

struct String : Value {
    String( const char* s = nullptr ) : m_str( s ) { m_type = &Type::s_string; }
    void _print() const override {
        printf( "%s", m_str );
    }
    const char* m_str;
};

struct Symbol : Atom {
    Symbol( const char* s ) : m_sym( s ) {}
    Atom* _eval( Env* env ) override;
    void _print() const override {
        printf( "'%s", m_sym );
    }
    const char* m_sym;
};

struct Env : Value {
    Env( Env *p ) : m_parent( p ) {}
    Atom* get( const Symbol* sym ) {
        if( Atom* a = get( sym->m_sym ) ) {
            return a;
        }
        Error_At( sym->m_loc, "Symbol '%s' not found", sym->m_sym );
        return nullptr;
    }
    Atom* get( const char* sym ) {
        for( Env* e = this; e; e = e->m_parent ) {
            auto it = e->m_tab.find( sym );
            if( it != e->m_tab.end() ) {
                return it->second;
            }
        }
        return nullptr;
    }
    void _print() const override {
        printf( "<env" );
        for(auto& e : m_tab) {
            printf(" %s=", e.first.data());
            e.second->print();
        }
        if(m_parent && m_parent->m_parent) m_parent->print();
        printf(">");
    }
    void put( const char* sym, Atom* val ) {
        m_tab.emplace( sym, val );
    }
    void update( const char* sym, Atom* val ) {
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
    typedef std::unordered_map<std::string, Atom*> Table;
    Table m_tab;
};

Atom* Symbol::_eval( Env* env ) {
    return env->get( this );
}


struct Callable : Value {
    typedef array_view<Atom*> ArgList;
    //Callable( List* arg_names ) : m_arg_names(arg_names) {}
    Atom* call( Env* env, Atom* arg0, ArgList args ) {
        return _call( env, arg0, args );
    }
    void _print() const override {
        printf( "<callable>" );
    }
    //List* m_arg_names;
protected:
    virtual Atom* _call( Env* env, Atom* arg0, ArgList args ) = 0;
};

template<typename T>
struct Optional {
    Optional() : m_ptr( nullptr) {}
    Optional( const T& t ) {
        m_ptr = new( m_buf ) T( t );
    }
    ~Optional() {
        reset();
    }
    void reset() {
        if( m_ptr ) m_ptr->~T();
    }
    const T* get() const {
        return m_ptr;
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

namespace XXX {
    struct bind {
        template<typename T>
        void operator() ( T** out, Env* env, Callable::ArgList& args ) {
            Atom* a = args[0]->eval( env );
            T* t = cast( T, a );
            assert2( t, "Wrong argument type" );
            *out = t;
            args = args.ltrim( 1 );
        }
        template<typename T>
        void operator()( std::vector<T*>* out, Env* env, Callable::ArgList& args ) {
            for( auto a : args ) {
                Atom* e = a->eval( env );
                T* t = cast( T, e );
                assert( t );
                out->push_back( t );
            }
            args = Callable::ArgList();
        }
        template<typename T>
        void operator()( Optional<T*>* out, Env* env, Callable::ArgList& args ) {
            if( args.size() ) {
                Atom* e = args[0]->eval( env );
                T* t = cast( T, e );
                assert( t );
                out->set(t);
                args = args.ltrim(1);
            }
        }
    };
}

template<typename CALL>
struct BuiltinCallable : Callable {
    
    virtual Atom* _call( Env* env, Atom* arg0, ArgList args ) override {
        CALL call;
        XXX::bind binder;
        call.visit( binder, env, args );
        return call.call( env );
    }
};


struct List : Atom {
    typedef std::vector<Atom*> vector;
    Atom* _eval( Env* env ) override {
        if( lst.empty() ) {
            Error_At( m_loc, "Empty list is illegal" );
        }
        Atom* atom = lst[0]->eval( env );
        if( auto call = cast( Callable, atom ) ) {
            return call->call( env, lst[0], Callable::ArgList( lst ).ltrim( 1 ) );
        }
        Error_At( lst[0]->m_loc, "Expected a callable as the first argument" );
        return nullptr;
    }
    #if 0
    Atom* _normalize() override {
        std::vector<List*> lets;
        List* fin = gcnew<List>();
        for( auto arg : lst ) {
            auto a = normalize2( arg, lets );
            fin->append( a );
        }
        List* ret = gcnew<List>();
        ret->append( gcnew<Symbol>( "let" ) );
        List* loc = gcnew<List>();
        for( auto l : lets ) {
            loc->append( l );
        }
        ret->append( loc );
        ret->append( fin );
        return ret;
    }
    static Atom* normalize2( Atom* arg, std::vector<List*>& lets ) {
        if( auto l = cast( List, arg ) ) {
            List* simp = gcnew<List>();
            for( auto a : l->lst ) {
                Atom* b = normalize2( a, lets );
                simp->append( b );
            }
            Symbol* sym;
            {
                char buf[256];
                sprintf( buf, "t%zi", lets.size() );
                sym = gcnew<Symbol>( strdup( buf ) );
            }
            List* let = gcnew<List>();
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
    void append( Atom* a ) {
        lst.push_back( a );
    }
    Atom* at( int i ) const {
        return lst[i];
    }
    Atom* set(int i, Atom* a) {
        lst[i] = a;
        return a;
    }
    void _print() const override {
        printf( "(" );
        const char* sep = "";
        for( auto a : lst ) {
            printf( sep );
            a->print();
            sep = " ";
        }
        printf( ")" );
    }
    void resize( int n ) {
        lst.resize( n, nullptr );
    }
    array_view<Atom*> view() const { return lst; }
protected:
    std::vector<Atom*> lst;
};

struct Lambda : Callable {
    static Bool s_trampoline;
    Lambda( Env* lex_env, List* arg_names, Atom* body )
        : m_arg_names( arg_names), m_lex_env( lex_env ), m_body( body ) {
        for( auto a : *arg_names ) {
            if( auto bs = cast( Symbol, a ) ) {
             //   m_arg_names->append( bs );
            }
            else {
                assert( 0 );
                auto ts = a->eval( lex_env );
                m_arg_names->append( ts );
            }
        }
    }
    Atom* _call( Env* env, Atom* arg0, ArgList arg_vals ) override {
        Env* e = gcnew<Env>( m_lex_env );
        if( m_arg_names->size() != arg_vals.size() ) {
            Error_At( arg0->m_loc, "Wrong number of arguments. Expected %i, got %i", m_arg_names->size(), arg_vals.size() );
        }
        for( unsigned i = 0; i < m_arg_names->size(); ++i ) {
            Atom* a = arg_vals[i]->eval( env );
            auto s = cast( Symbol, m_arg_names->at( i ) );
            if( s->m_type != nullptr ) {
                auto nt = s->m_type->eval( env );
                auto at = a->m_type->eval( env );
                if( at != nt ) {
                    Error_At( arg_vals[i]->m_loc, "Mismatched type for argument '%i'", i );
                }
            }
            e->put( s->m_sym, a );
        }
        while( 1 ) {
            Atom* ret = m_body->eval( e );
            if( ret != &s_trampoline ) {
                return ret;
            }
        }
    }
    #if 0
    Atom* _normalize() override {
        Atom* body = m_body->normalize();
        return gcnew<Lambda>( m_lex_env, m_arg_names, body );
    }
    #endif
    void _print() const override {
        printf( "(lambda (" );
        for( auto a : *m_arg_names ) {
            a->print();
            printf( " " );
        }
        printf( ") " );
        m_body->print();

    }
    List* m_arg_names;
    Env* m_lex_env;
    Atom* m_body;
};
Bool Lambda::s_trampoline(false);

struct Vau : Callable {
    List* m_arg_names;
    Env* m_lex_env;
    Atom* m_env_sym;
    Atom* m_body;
    Vau( Env* lex_env, List* arg_names, Atom* symbol, Atom* body )
        : m_arg_names(arg_names), m_lex_env( lex_env ), m_env_sym( symbol ), m_body( body ) {
        for( auto a : *arg_names ) {
            if( auto bs = cast( Symbol, a ) ) {
                //m_arg_names->append( bs );
            }
            else {
                assert( 0 );
                auto ts = a->eval( lex_env );
                m_arg_names->append( ts );
            }
        }
    }

    Atom* _call( Env* env, Atom* arg0, ArgList args ) override {
        Env* e = gcnew<Env>( m_lex_env );
        e->put( cast(Symbol,m_env_sym)->m_sym, env);
        assert( args.size() == m_arg_names->size() );
        for( unsigned i = 0; i < args.size(); ++i ) {
            e->put( cast( Symbol, m_arg_names->at( i ) )->m_sym, args[i]);
        }
        return m_body->eval( e );
    }
};

#if 0
template<typename T>
static Callable::ArgList bind_arg( Env* env, Callable::ArgList args, T** out ) {
    Atom* a = args[0]->eval( env );
    T* t = cast( T, a );
    assert2( t, "Wrong argument type" );
    *out = t;
    return args.ltrim(1);
}

template<typename T>
static Callable::ArgList bind_arg( Env* env, Callable::ArgList args, std::vector<T*>* out ) {
    for( auto a : args ) {
        Atom* e = a->eval( env );
        T* t = cast( T, e );
        assert( t );
        out->push_back(t);
    }
    return Callable::ArgList();
}

static Result bind1( Env* env, Callable::ArgList args) {
    assert( args.size() == 0 );
    return Result::OK;
}
template<typename T, typename... REST>
static Result bind1( Env* env, Callable::ArgList args, T* out, REST... rest ) {
    auto args2 = bind_arg( env, args, out );
    return bind1( env, args2, rest... );
}

template<typename... VARS>
static Result bind( Env* env, Callable::ArgList args, T* out, VARS... vars ) {
    if( vars.size() == 0 ) {
        assert( args.size() == 0 );
        return Result::OK;
    }
    assert( args.size() );
    args = bind_arg(env, args, vars)
    return bind( env, args, vars... );
}
#endif

struct BuiltinPrint : BuiltinCallable<BuiltinPrint> {
    template<typename VISIT, typename...VISITARGS>
    void visit( VISIT visit, VISITARGS...visitargs ) {
        visit( &args, visitargs... );
    }
    std::vector<Atom*> args;
    Atom* call(Env* env) {
        const char* sep = "";
        for( auto arg : args ) {
            printf( "%s", sep );
            arg->print();
            sep = " ";
        }
        printf( "\n" );
        return nullptr;
    }
};

//struct LambdaTail : Callable {
//    LambdaTail() {}
//    Atom* _call( Env* env, Atom* arg0, ArgList args ) override {
//        assert( args.size() >= 1 );
//        Callable* c = cast( Callable, args[0]->eval(env) );
//        assert( c->m_arg_names );
//        ArgList fwd = args.ltrim( 1 );
//        assert( fwd.size() == c->m_arg_names->size() );
//        std::vector<Atom*> newa;
//        for( unsigned i = 0; i < fwd.size(); ++i ) {
//            Atom* a = fwd[i]->eval( env );
//            newa.push_back( a );
//        }
//        for( unsigned i = 0; i < fwd.size(); ++i ) {
//            env->update( cast( Symbol, c->m_arg_names->at( i ) )->m_sym, newa[i] );
//        }
//        return &Lambda::s_trampoline;
//    }
//    Atom* _normalize() override {
//        return this;
//    }
//};


struct BuiltinVau : Callable {
    typedef Atom* ( *Func )( Env*, ArgList );
    BuiltinVau( Func f ) : m_func( f ) {}
    Atom* _call( Env* env, Atom* arg0, ArgList arg_vals ) override {
        return ( *m_func )( env, arg_vals );
    }
    Func m_func;
};

struct State {
    State() {
        m_env = gcnew<Env>( nullptr );
        m_stack.push_back( m_env );
    }
    Result let( const char* name, Atom* value ) {
        m_env->put( name, value );
        return R_OK;
    }
    void newList() {
        m_stack.push_back( gcnew<List>( ) );
    }
    void newSymbol(const char* sym) {
        m_stack.push_back( gcnew<Symbol>(sym) );
    }
    void newInteger( int value ) {
        m_stack.push_back( gcnew<Int>( value ) );
    }
    void newFloat( double value ) {
        m_stack.push_back( gcnew<Float>( value ) );
    }
    void newString( const char* s ) {
        m_stack.push_back( gcnew<String>( s ) );
    }
    int popInteger( int idx ) {
        int r = 0;
        if( Int* n = cast( Int, m_stack[idx] ) ) {
            r = n->m_num;
            m_stack.pop(1);
        }
        return r;
    }
    void listAppend( int idx ) {
        if( List* l = cast(List, m_stack[idx]) ) {
            l->append( m_stack[-1] );
            m_stack.pop( 1 );
        }
        else {
            throw 0;
        }
    }
    Result getGlobal( const char* sym ) {
        if( Atom* a = m_env->get( sym ) ) {
            m_stack.push_back( a );
            return R_OK;
        }
        return Result::ERR;
    }
    Result call( int narg, int nret ) {
        if( Callable* c = cast( Callable, m_stack[-narg - 1] ) ) {
            Atom** begin = m_stack.m_stack.data();
            int size = m_stack.size();
            Atom* r = c->call( m_env, c, Callable::ArgList(begin+size-narg, begin+size));
            m_stack.pop( narg + 1 );
            m_stack.push_back( r );
            assert( nret == 1 );
            return R_OK;
        }
        return Result::ERR;
    }

    
    Env* m_env;
    struct Stack {
        Atom* operator[]( int i ) const {
            return i >= 0 ? m_stack[i] : m_stack[m_stack.size() + i];
        }
        void push_back( Atom* a ) {
            m_stack.push_back( a );
        }
        void pop( int n=1 ) {
            m_stack.erase( m_stack.end()-1, m_stack.end() );
        }
        int size() const {
            return (int)m_stack.size();
        }
        std::vector<Atom*> m_stack;
    };
    Stack m_stack;
};


Atom* v_eval(Env* env, Callable::ArgList args) {
    if(args.size() == 1) {
        return args[0]->eval(env);
    }
    else if(args.size() == 2) {
        Env* e = cast(Env, args[0]->eval(env));
        Atom* a = args[1]->eval(env);
        return a->eval(e);
    }
    Error("too many arguments");
    return nullptr;
}

Atom* v_type(Env* env, Callable::ArgList args) {
    assert(args.size() == 2);
    auto t = cast(Type, args[1]->eval(env));
    auto s = cast(Symbol, args[0]);
    assert(s);
    assert(t);
    s->m_type = t;
    return s;
}

struct VauBegin : public Callable {
    Atom* _call( Env* env, Atom* arg0, ArgList args ) override {
        Atom* r = nullptr;
        for( auto a : args ) {
            r = a->eval( env );
        }
        return r;
    }
};

struct VauModule : public Callable {
    Atom* _call( Env* env, Atom* arg0, ArgList args ) override {
        assert( args.size() == 1 );
        Env* r = env;// gcnew<Env>( env );
        List* l = cast( List, args[0]);
        assert( l );
        for( auto a : *l ) {
            a->eval( r );
        }
        return r;
    }
};

struct VauQuote : public Callable {
    Atom* _call( Env* env, Atom* arg0, ArgList args ) override {
        List* l = gcnew<List>( );
        for( auto a : args ) {
            l->append( a );
        }
        return l;
    }
};

struct VauInc : public Callable {
    Atom* _call( Env* env, Atom* arg0, ArgList args ) override {
        assert( args.size() == 1 );
        Symbol* s = cast( Symbol, args[0] );
        assert( s );
        Atom* a = env->get( s );
        assert( a );
        Int* n = cast( Int, a );
        assert( n );
        n->m_num += 1;
        return n;
    }
};

Atom* v_define( Env* env, Callable::ArgList args ) {
    assert( args.size() == 2 );
    if( auto s = cast( Symbol, args[0] ) ) {
        Atom* v = args[1]->eval( env );
        env->put( s->m_sym, v );
        return v;
    }
    error( "define needs a symbol" );
    return nullptr;
}

Atom* v_lambda( Env* env, Callable::ArgList args ) {
    assert( args.size() == 2 );
    if( List* names = cast( List, args[0] ) ) {
        auto r = gcnew<Lambda>( env, names, args[1] );
        r->m_loc = args[0]->m_loc;
        return r;
    }
    return nullptr;
}

Atom* v_vau( Env* env, Callable::ArgList args ) {
    assert( args.size() == 3 );
    assert( cast( List, args[0] ) );
    assert( cast( Symbol, args[1] ) );
    if( List* names = cast( List, args[0] ) ) {
        return gcnew<Vau>( env, names, args[1], args[2] );
    }
    return nullptr;
}

Atom* v_let( Env* env, Callable::ArgList args ) {
    assert( args.size() == 2 );
    List* lets = cast( List, args[0] );
    Env* e = gcnew<Env>( env );
    for( auto item : *lets ) {
        List* l = cast( List, item );
        assert( l->size() == 2 );
        Symbol* s = cast( Symbol, l->at( 0 ) );
        Atom* a = l->at( 1 )->eval( e );
        e->put( s->m_sym, a );
    }
    return args[1]->eval( e );
}

Atom* v_normalize( Env* env, Callable::ArgList args ) {
    assert( args.size() >= 1 );
    for( auto arg : args ) {

    }
    return nullptr;
}

Atom* v_cond( Env* env, Callable::ArgList args ) {
    for( auto arg : args ) {
        if( List* l = cast( List, arg ) ) {
            if( l->size() != 2 ) {
                Error_At( l->m_loc, "Expected list of size 2" );
            }
            if( l->at( 0 )->eval( env ) != &Bool::s_false ) {
                return l->at( 1 )->eval( env );
            }
        }
        else {
            Error_At( arg->m_loc, "Expected list of size 2" );
        }
    }
    return nullptr;
}

struct BuiltinApply : public BuiltinCallable<BuiltinApply > {
    Callable* m_callable;
    List* m_args;
    Env* m_env;
    template<typename VISIT, typename...VISITARGS>
    void visit( VISIT visit, VISITARGS...visitargs ) {
        visit( &m_callable, visitargs... );
        visit( &m_args, visitargs... );
        visit( &m_env, visitargs... );
    }

    Atom* call( Env* env ) {
        return m_callable->call( m_env, m_callable, m_args->view() );
    }
};

#if 0
struct BuiltinApplyWrap : public BuiltinCallable<BuiltinApplyWrap> {
    List* m_args;
    Env* m_env;
    template<typename VISIT, typename...VISITARGS>
    void visit( VISIT visit, VISITARGS...visitargs ) {
        visit( &m_args, visitargs... );
        visit( &m_env, visitargs... );
    }
    Atom* call( Env* env ) {
        List* dlst = gcnew<List>( );
        for( auto& a : *m_args ) {
            dlst->append( a->eval( env ) );
        }
        Atom* a2[2] = { m_env, dlst };
        return v_eval( m_env, array_view_t::make( a2 ) );
    }
};
#endif

#if 0
struct BuiltinWrap : public BuiltinCallable<BuiltinWrap> { 
    assert(args.size() == 1);
    Callable* c = cast(Callable, args[0]);
    //if(cast(Lambda, c) || cast(BuiltinLambda, c)) {
      //  return c;
    //}
    return nullptr;// new Lambda(env, c->->call(e, c, a->view());
}
#endif

Atom* l_vec_set(Env* env, Callable::ArgList args) {
    if(args.size() != 3) {
        Error("3 args");
    }
    if(List* l = cast(List, args[0])) {
        if(Int* n = cast(Int, args[1])) {
            auto i = unsigned(n->m_num);
            if(i >= l->size()) {
                Error("bad index");
            }
            l->set(i, args[2]);
            return args[2];
        }
    }
    Error("Bad args");
    return nullptr;
}

template<typename OPER, typename ATOM>
struct BuiltinBinOp : BuiltinCallable< BuiltinBinOp<OPER, ATOM> > {
    ATOM* first;
    ATOM* second;
    template<typename VISIT, typename...VISITARGS>
    void visit( VISIT visit, VISITARGS...visitargs ) {
        visit( &first, visitargs... );
        visit( &second, visitargs... );
    }
    Atom* call( Env* env ) {
        OPER oper;
        return oper(first->m_num, second->m_num) ? &Bool::s_true : &Bool::s_false;
    }
};

Atom* l_vec_new( Env* env, Callable::ArgList args ) {
    if( args.size() != 1 ) {
        Error( "Expected list of size 1" );
    }
    if( Int* num = cast( Int, args[0] ) ) {
        int n = num->m_num;
        if( n < 0 ) {
            Error_At( num->m_loc, "Expected n > 0" );
        }
        List* l = gcnew<List>();
        l->m_loc = num->m_loc;
        l->resize( n );
        return l;
    }
    Error_At( args[0]->m_loc, "Expected number" );
    return nullptr;
}

Atom* l_vec_size(Env* env, Callable::ArgList args) {
    if(args.size() != 1) {
        Error("Expected 1 argument");
    }
    if(List* l = cast(List, args[0])) {
        return gcnew<Int>( int(l->size()) );
    }
    Error("Expected a list");
    return nullptr;
}

Atom* l_float( Env* env, Callable::ArgList args ) {
    if( args.size() != 1 ) {
        Error( "Expected 1 argument" );
    }
    if( Float* f = cast( Float, args[0] ) ) {
        return f;
    }
    else if( Int* i = cast( Int, args[0] ) ) {
        return gcnew<Float>( double( i->m_num) );
    }
    Error( "Expected a number" );
    return nullptr;
}

Atom* l_vec_idx( Env* env, Callable::ArgList args ) {
    if( args.size() != 2 ) {
        Error( "Expected list of size 2" );
    }
    if( List* lst = cast( List, args[0] ) ) {
        if( Int* num = cast( Int, args[1] ) ) {
            auto n = unsigned(num->m_num);
            if( unsigned(n) < lst->size() ) {
                return lst->at( n );
            }
            else {
                Error_At( num->m_loc, "Out of bounds %i (%i)", n, lst->size() );
            }
        }
        else {
            Error_At( args[1]->m_loc, "Expected num" );
        }
    }
    else {
        Error_At( args[0]->m_loc, "Expected list" );
    }
    return nullptr;
 }


struct BuiltinMap : BuiltinCallable<BuiltinMap> {
    Callable* func;
    List* list;
    template<typename VISIT, typename...VISITARGS>
    void visit( VISIT visit, VISITARGS...visitargs ) {
        visit( &func, visitargs... );
        visit( &list, visitargs... );
    }

    Atom* call( Env* env ) {
        List* r = gcnew<List>( );
        for( auto i : *list ) {
            r->append( func->call( env, func, array_view_t::from_single( i ) ) );
        }
        return r;
    }
};

struct BuiltinList : BuiltinCallable<BuiltinList> {
    std::vector<Atom*> args;
    template<typename VISIT, typename...VISITARGS>
    void visit( VISIT visit, VISITARGS...visitargs ) {
        visit( &args, visitargs... );
    }
     
    Atom* call( Env* env ) {
        List* l = gcnew<List>( );
        for( auto& a : args ) {
            l->append( a );
        }
        return l;
    }
};

struct BuiltinRange : public BuiltinCallable<BuiltinRange> {
    Int* first;
    Optional<Int*> second;
    Optional<Int*> step;
    template<typename VISIT, typename...VISITARGS>
    void visit( VISIT visit, VISITARGS...visitargs ) {
        visit( &first, visitargs... );
        visit( &second, visitargs... );
        visit( &step, visitargs... );
    }

    Atom* call( Env* env ) {
        int lo=0, hi=first->m_num, st=1;
        if( auto s = second.get() ) {
            lo = hi;
            hi = s[0]->m_num;
        }
        if( auto s = step.get() ) {
            st = s[0]->m_num;
        }
        if( hi < lo ) Error( "" );
        if( st < 1 ) Error( "" );
        List* l = gcnew<List>( );
        l->resize( hi - lo );
        for( int i = 0; i < hi - lo; i += st ) {
            l->set( i, gcnew<Int>( lo + i * st ) );
        }
        return l;
    }
};

template<typename REDUCE, typename ATOM>
struct ReduceCallable : BuiltinCallable< ReduceCallable<REDUCE,ATOM> > {
    ATOM* first;
    std::vector<ATOM*> rest;
    template<typename VISIT, typename...VISITARGS>
    void visit( VISIT visit, VISITARGS...visitargs ) {
        visit( &first, visitargs... );
        visit( &rest, visitargs... );
    }
    Atom* call( Env* env ) {
        ATOM* acc = gcnew<ATOM>( first->m_num );
        REDUCE reduce;
        for( auto a : rest ) {
            acc->m_num = reduce(acc->m_num, a->m_num);
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
                while( parse_one(state, in ).isOk() ) {
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
                int len = int(in.peekbuf() - s);
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
    if( parse_one(state, in).isOk() ) {
        in.eatwhite();
        if(0) if( in.peek() == ':' ) {
            in.next();
            //Atom* rhs = parse_one( state, in );
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
    state->let( "eval", gcnew<BuiltinVau>( &v_eval ) );
    state->let( "begin", gcnew<VauBegin>( ) );
    state->let( "module", gcnew<VauModule>( ) );
    state->let( "quote", gcnew<VauQuote>( ) );
    state->let( "inc!", gcnew<VauInc>( ) );
    //state->let( "tail", gcnew<LambdaTail>( ) );
    state->let( "define", gcnew<BuiltinVau>( &v_define ) );
    state->let( "lambda", gcnew<BuiltinVau>( &v_lambda ) );
    state->let( "vau", gcnew<BuiltinVau>( &v_vau ) );
    state->let( "let", gcnew<BuiltinVau>( &v_let ) );
    state->let( "cond", gcnew<BuiltinVau>( &v_cond ) );
//    state->let( "apply_wrap", gcnew<BuiltinVau>( &v_apply_wrap ) );

    state->let( "map", gcnew<BuiltinMap>() );
    state->let( "print", gcnew<BuiltinPrint>() );
    state->let( "list", gcnew<BuiltinList>() );
    state->let( "add_i", gcnew<ReduceCallable<BinOps::Add, Int>>( ) );
    state->let( "add_f", gcnew<ReduceCallable<BinOps::Add, Float>>( ) );
    state->let( "sub_i", gcnew<ReduceCallable<BinOps::Sub, Int>>( ) );
    state->let( "sub_f", gcnew<ReduceCallable<BinOps::Sub, Float>>( ) );
    state->let( "div_i", gcnew<ReduceCallable<BinOps::Div, Int>>( ) );
    state->let( "div_f", gcnew<ReduceCallable<BinOps::Div, Float>>( ) );
    state->let( "apply", gcnew<BuiltinApply>() );
    state->let( "lt_i?", gcnew<BuiltinBinOp<BinOps::Lt, Int>>( ) );
    state->let( "lt_f?", gcnew<BuiltinBinOp<BinOps::Lt, Float>>() );
    state->let( "le_i?", gcnew<BuiltinBinOp<BinOps::Le, Int>>( ) );
    state->let( "le_f?", gcnew<BuiltinBinOp<BinOps::Le, Float>>( ) );
    state->let( "eq_i?", gcnew<BuiltinBinOp<BinOps::Eq, Int>>( ) );
    state->let( "eq_f?", gcnew<BuiltinBinOp<BinOps::Eq, Float>>( ) );
    state->let( "range", gcnew<BuiltinRange>() );
    #if 0
    state->let( "vec_new", gcnew<BuiltinLambda>( &l_vec_new ) );
    state->let( "vec_idx", gcnew<BuiltinLambda>( &l_vec_idx ) );
    state->let( "vec_set!", gcnew<BuiltinLambda>( &l_vec_set ) );
    state->let( "vec_size", gcnew<BuiltinLambda>( &l_vec_size ) );
    state->let( "float", gcnew<BuiltinLambda>( &l_float ) );
    #endif
//    state->let( "wrap", gcnew<BuiltinWrap>() );
    state->let( "Int", &Type::s_int );
    state->let( "Float", &Type::s_float );
    state->let( "true", &Bool::s_true );
    state->let( "false", &Bool::s_false );
    return R_OK;
}

int main( int argc, const char* argv[] ) {
    if( argc < 2 ) {
        Error( "Need a script to run" );
        return 1;
    }
    try {
        State* state = new State();
        initBuiltins(state);

        SourceManager sm;
        state->getGlobal( "module" );
        if( parse_file( state, sm, argv[1]).isOk() ) {
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
            int ret = state->popInteger(-1);
            return ret;
        }
    }
    catch( float ) {
        return 1;
    }
}
