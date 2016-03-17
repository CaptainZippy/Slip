
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
    array_view( const T* s, const T* e ) : m_begin( s ), m_end( e ) {}
    array_view( const std::vector<T>& a ) : m_begin( a.data() ), m_end( m_begin + a.size() ) {}
    static array_view<T> from_single( const T& t ) { return array_view( &t, &t + 1 ); }
    size_t size() const { return m_end - m_begin; }
    const T& operator[]( unsigned i ) const { assert( i < size() ); return m_begin[i]; }
    const T* begin() const { return m_begin; }
    const T* end() const { return m_end; }
    array_view<T> ltrim( unsigned n ) const { assert( n <= size() ); return array_view<T>( m_begin + n, m_end ); }
    const T* m_begin;
    const T* m_end;
};

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
    Atom* normalize() { return _normalize(); }
    void print() const { _print(); }
    Atom() : m_type( nullptr ) {}
    SourceManager::Location m_loc;
    Atom* m_type;
protected:
    virtual Atom* _eval( Env* env ) = 0;
    virtual Atom* _normalize() = 0;
    virtual void _print() const = 0;
};

struct Value : Atom {
    Atom* _eval( Env* env ) override {
        return this;
    }
    Atom* _normalize() override {
        return this;
    }
};

struct Type : public Value {
    void _print() const override {
        printf( "type" );
    }
    static Type s_bool;
    static Type s_string;
    static Type s_num;
    static Type s_list;
};

Type Type::s_num;
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


struct Num : Value {
    Num( int n ) : m_num( n ) { m_type = &Type::s_num; }
    void _print() const override {
        printf( "%i", m_num );
    }
    int m_num;
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
    Atom* _normalize() {
        return this;
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
    Callable( List* arg_names ) : m_arg_names(arg_names) {}
    Atom* call( Env* env, Atom* arg0, ArgList args ) {
        return _call( env, arg0, args );
    }
    void _print() const override {
        printf( "<callable>" );
    }
    List* m_arg_names;
protected:
    virtual Atom* _call( Env* env, Atom* arg0, ArgList args ) = 0;
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
        : Callable( arg_names), m_lex_env( lex_env ), m_body( body ) {
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
    Atom* _normalize() override {
        Atom* body = m_body->normalize();
        return gcnew<Lambda>( m_lex_env, m_arg_names, body );
    }
    void _print() const override {
        printf( "(lambda (" );
        for( auto a : *m_arg_names ) {
            a->print();
            printf( " " );
        }
        printf( ") " );
        m_body->print();

    }
    Env* m_lex_env;
    Atom* m_body;
};
Bool Lambda::s_trampoline(false);

struct Vau : Callable {
    Env* m_lex_env;
    Atom* m_env_sym;
    Atom* m_body;
    Vau( Env* lex_env, List* arg_names, Atom* symbol, Atom* body )
        : Callable(arg_names), m_lex_env( lex_env ), m_env_sym( symbol ), m_body( body ) {
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
    Atom* _normalize() override {
        assert( 0 );
        return nullptr;
    }
};

struct BuiltinLambda : Callable {
    typedef Atom* ( *Func )( Env* env, ArgList args );
    BuiltinLambda( Func f ) : Callable(nullptr), m_func( f ) {}
    Atom* _call( Env* env, Atom* arg0, ArgList arg_vals ) override {
        std::vector<Atom*> args;
        for( auto arg : arg_vals ) {
            Atom* a = arg->eval( env );
            args.push_back( a );
        }
        return m_func( env, args );
    }
    Atom* _normalize() override {
        return this;
    }
    Func m_func;
};

struct LambdaTail : Callable {
    LambdaTail() : Callable( nullptr ) {}
    Atom* _call( Env* env, Atom* arg0, ArgList args ) override {
        assert( args.size() >= 1 );
        Callable* c = cast( Callable, args[0]->eval(env) );
        assert( c->m_arg_names );
        ArgList fwd = args.ltrim( 1 );
        assert( fwd.size() == c->m_arg_names->size() );
        std::vector<Atom*> newa;
        for( unsigned i = 0; i < fwd.size(); ++i ) {
            Atom* a = fwd[i]->eval( env );
            newa.push_back( a );
        }
        for( unsigned i = 0; i < fwd.size(); ++i ) {
            env->update( cast( Symbol, c->m_arg_names->at( i ) )->m_sym, newa[i] );
        }
        return &Lambda::s_trampoline;
    }
    Atom* _normalize() override {
        return this;
    }
};


struct BuiltinVau : Callable {
    typedef Atom* ( *Func )( Env*, ArgList );
    BuiltinVau( Func f ) : Callable(nullptr), m_func( f ) {}
    Atom* _call( Env* env, Atom* arg0, ArgList arg_vals ) override {
        return ( *m_func )( env, arg_vals );
    }
    Atom* _normalize() override {
        return this;
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
        m_stack.push_back( gcnew<Num>( value ) );
    }
    void newString( const char* s ) {
        m_stack.push_back( gcnew<String>( s ) );
    }
    int popInteger( int idx ) {
        int r = 0;
        if( Num* n = cast( Num, m_stack[idx] ) ) {
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
            assert( nret == 1 && r );
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
    VauBegin() : Callable(nullptr) {}
    Atom* _call( Env* env, Atom* arg0, ArgList args ) override {
        Atom* r = nullptr;
        for( auto a : args ) {
            r = a->eval( env );
        }
        return r;
    }
    Atom* _normalize() override {
        return this;
    }
};

struct VauModule : public Callable {
    VauModule() : Callable(nullptr) {}
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
    Atom* _normalize() override {
        return this;
    }
};

struct VauQuote: public Callable {
    VauQuote() : Callable(nullptr) {}
    Atom* _call( Env* env, Atom* arg0, ArgList args ) override {
        List* l = gcnew<List>( );
        for( auto a : args ) {
            l->append( a );
        }
        return l;
    }
    Atom* _normalize() override {
        return this;
    }
};

struct VauInc : public Callable {
    VauInc() : Callable( nullptr ) {}
    Atom* _call( Env* env, Atom* arg0, ArgList args ) override {
        assert( args.size() == 1 );
        Symbol* s = cast( Symbol, args[0] );
        assert( s );
        Atom* a = env->get( s );
        assert( a );
        Num* n = cast( Num, a );
        assert( n );
        n->m_num += 1;
        return n;
    }
    Atom* _normalize() override {
        return this;
    }
};

Atom* v_define(Env* env, Callable::ArgList args) {
    assert(args.size() == 2);
    if(auto s = cast(Symbol, args[0])) {
        Atom* v = args[1]->eval(env);
        env->put(s->m_sym, v);
        return v;
    }
    error("define needs a symbol");
    return nullptr;
}

Atom* v_lambda(Env* env, Callable::ArgList args) {
    assert(args.size() == 2);
    if(List* names = cast(List, args[0])) {
        auto r = gcnew<Lambda>(env, names, args[1]);
        r->m_loc = args[0]->m_loc;
        return r;
    }
    return nullptr;
}

Atom* v_vau(Env* env, Callable::ArgList args) {
    assert(args.size() == 3);
    assert(cast(List, args[0]));
    assert(cast(Symbol, args[1]));
    if(List* names = cast(List, args[0])) {
        return gcnew<Vau>(env, names, args[1], args[2]);
    }
    return nullptr;
}

Atom* v_let(Env* env, Callable::ArgList args) {
    assert(args.size() == 2);
    List* lets = cast(List, args[0]);
    Env* e = gcnew<Env>(env);
    for(auto item : *lets) {
        List* l = cast(List, item);
        assert(l->size() == 2);
        Symbol* s = cast(Symbol, l->at(0));
        Atom* a = l->at(1)->eval(e);
        e->put(s->m_sym, a);
    }
    return args[1]->eval(e);
}

Atom* v_normalize(Env* env, Callable::ArgList args) {
    assert(args.size() >= 1);
    for(auto arg : args) {

    }
    return nullptr;
}

Atom* v_cond(Env* env, Callable::ArgList args) {
    for(auto arg : args) {
        if(List* l = cast(List, arg)) {
            if(l->size() != 2) {
                Error_At(l->m_loc, "Expected list of size 2");
            }
            if(l->at(0)->eval(env) != &Bool::s_false) {
                return l->at(1)->eval(env);
            }
        }
        else {
            Error_At(arg->m_loc, "Expected list of size 2");
        }
    }
    return nullptr;
}

Atom* l_apply(Env* env, Callable::ArgList args) {
    assert(args.size() == 3);
    Callable* c = cast(Callable, args[0]);
    List* a = cast(List, args[1]);
    Env* e = cast(Env, args[2]);
    return c->call(e, c, a->view());
}

Atom* l_apply_wrap(Env* env, Callable::ArgList args) {
    assert(args.size() == 2);
    List* slst = cast(List, args[0]);
    List* dlst = gcnew<List>();
    for(auto& a : *slst) {
        dlst->append(a->eval(env));
    }
    Env* e = cast(Env, args[1]->eval(env));
    std::vector<Atom*> a2; a2.push_back(e); a2.push_back(dlst);
    return v_eval(e, array_view<Atom*>(a2));
}

Atom* l_wrap(Env* env, Callable::ArgList args) {
    assert(args.size() == 1);
    Callable* c = cast(Callable, args[0]);
    if(cast(Lambda, c) || cast(BuiltinLambda, c)) {
        return c;
    }
    return nullptr;// new Lambda(env, c->->call(e, c, a->view());
}

Atom* l_vec_set(Env* env, Callable::ArgList args) {
    if(args.size() != 3) {
        Error("3 args");
    }
    if(List* l = cast(List, args[0])) {
        if(Num* n = cast(Num, args[1])) {
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


Atom* l_eq( Env* env, Callable::ArgList args ) {
    if( args.size() != 2 ) {
        Error( "Expected list of size 2" );
    }
    if( args[0] == args[1] ) { return &Bool::s_true; }
    if( !(args[0] && args[1]) ) { return &Bool::s_false; }

    if( Num* n0 = cast( Num, args[0] ) ) { // hack
        if( Num* n1 = cast( Num, args[1] ) ) { // hack
            return n0->m_num == n1->m_num ? &Bool::s_true : &Bool::s_false;
        }
    }
    Error( "fixme" );
    return &Bool::s_false;
}

Atom* l_lt(Env* env, Callable::ArgList args) {
    if(args.size() != 2) {
        Error("Expected list of size 2");
    }
    if(args[0] < args[1]) { return &Bool::s_true; }
    if(!(args[0] && args[1])) { return &Bool::s_false; }

    if(Num* n0 = cast(Num, args[0])) { // hack
        if(Num* n1 = cast(Num, args[1])) { // hack
            return n0->m_num < n1->m_num ? &Bool::s_true : &Bool::s_false;
        }
    }
    Error("fixme");
    return &Bool::s_false;
}

Atom* l_vec_new( Env* env, Callable::ArgList args ) {
    if( args.size() != 1 ) {
        Error( "Expected list of size 1" );
    }
    if( Num* num = cast( Num, args[0] ) ) {
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
        return gcnew<Num>( int(l->size()) );
    }
    Error("Expected a list");
    return nullptr;
}

Atom* l_vec_idx( Env* env, Callable::ArgList args ) {
    if( args.size() != 2 ) {
        Error( "Expected list of size 2" );
    }
    if( List* lst = cast( List, args[0] ) ) {
        if( Num* num = cast( Num, args[1] ) ) {
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



Atom* l_map( Env* env, Callable::ArgList args ) {
    if( args.size() != 2 ) {
        Error( "Expected list of size 2" );
    }
    if( Callable* c = cast( Callable, args[0] ) ) {
        if( List* l = cast( List, args[1] ) ) {
            List* r = gcnew<List>();
            for( auto i : *l ) {
                r->append( c->call( env, c, Callable::ArgList::from_single( i ) ) );
            }
            return r;
        }
    }
    Error( "fixme" );
    return nullptr;
}

Atom* l_list(Env* env, Callable::ArgList args) {
    if(args.size() == 0) Error("Need args");
    List* l = gcnew<List>();
    for(auto& a : args) {
        l->append(a);
    }
    return l;
}

Atom* l_range(Env* env, Callable::ArgList args) {
    if(args.size() == 0) Error("");
    if(args.size() > 3) Error("");
    int nums[3];
    int nn = 0;
    for(auto& a : args) {
        if(Num* n = cast(Num, a)) {
            nums[nn++] = n->m_num;
        }
    }
    int lo, hi, step;
    switch(nn) {
        case 1: lo = 0, hi = nums[0], step = 1; break;
        case 2: lo = nums[0], hi = nums[1]; step = 1; break;
        case 3: lo = nums[0], hi = nums[1]; step = nums[2]; break;
    }
    if(hi < lo) Error("");
    if(step < 1) Error("");
    List* l = gcnew<List>();
    l->resize(hi - lo);
    for(int i = 0; i < hi-lo; i += 1) {
        l->set(i, gcnew<Num>(lo + i * lo));
    }
    return l;
}

Atom* l_add( Env* env, Callable::ArgList args ) {
    assert( args.size() >= 1 );
    int acc = 0;
    for( auto arg : args ) {
        Num* a = cast( Num, arg );
        acc += a->m_num;
    }
    return gcnew<Num>( acc );
}

Atom* l_sub( Env* env, Callable::ArgList args ) {
    assert( args.size() >= 1 );
    int acc = cast( Num, args[0] )->m_num;
    for( auto arg : args.ltrim( 1 ) ) {
        Num* a = cast( Num, arg );
        acc -= a->m_num;
    }
    return gcnew<Num>( acc );
}

Atom* l_mul( Env* env, Callable::ArgList args ) {
    assert( args.size() >= 1 );
    int acc = 1;
    for( auto arg : args ) {
        Num* a = cast( Num, arg );
        acc *= a->m_num;
    }
    return gcnew<Num>( acc );
}

Atom* l_div( Env* env, Callable::ArgList args ) {
    assert( args.size() >= 1 );
    int acc = cast( Num, args[0] )->m_num;
    for( auto arg : args.ltrim( 1 ) ) {
        Num* a = cast( Num, arg );
        acc /= a->m_num;
    }
    return gcnew<Num>( acc );
}

Atom* l_print( Env* env, Callable::ArgList args ) {
    const char* sep = "";
    for( auto arg : args ) {
        printf( "%s", sep );
        arg->print();
        sep = " ";
    }
    printf( "\n" );
    return nullptr;
}

Atom* l_normalize( Env* env, Callable::ArgList args ) {
    assert( args.size() == 1 );
    return args[0]->normalize();
}

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
                int value = in.next() - '0';
                while( int c = in.peek() ) {
                    //if(isdigit(c) || isalpha(c) || c == '_'){
                    if( isdigit( c ) ) {
                        value = value * 10 + c - '0';
                        in.next();
                    }
                    else break;
                }
                state->newInteger( value );
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
    state->let( "tail", gcnew<LambdaTail>( ) );
    state->let( "define", gcnew<BuiltinVau>( &v_define ) );
    state->let( "lambda", gcnew<BuiltinVau>( &v_lambda ) );
    state->let( "vau", gcnew<BuiltinVau>( &v_vau ) );
    state->let( "let", gcnew<BuiltinVau>( &v_let ) );
    state->let( "cond", gcnew<BuiltinVau>( &v_cond ) );
    state->let( "apply_wrap", gcnew<BuiltinVau>( &l_apply_wrap ) );
    state->let( "apply", gcnew<BuiltinLambda>( &l_apply ) );
    state->let( "wrap", gcnew<BuiltinLambda>( &l_wrap ) );
    state->let( "print", gcnew<BuiltinLambda>( &l_print ) );
    state->let( "add", gcnew<BuiltinLambda>( &l_add ) );
    state->let( "mul", gcnew<BuiltinLambda>( &l_mul ) );
    state->let( "div", gcnew<BuiltinLambda>( &l_div ) );
    state->let( "sub", gcnew<BuiltinLambda>( &l_sub ) );
    state->let( "normalize", gcnew<BuiltinLambda>( &l_normalize ) );
    state->let( "eq?", gcnew<BuiltinLambda>( &l_eq ) );
    state->let( "lt?", gcnew<BuiltinLambda>( &l_lt ) );
    state->let( "map", gcnew<BuiltinLambda>( &l_map ) );
    state->let( "list", gcnew<BuiltinLambda>( &l_list ) );
    state->let( "range", gcnew<BuiltinLambda>( &l_range ) );
    state->let( "vec_new", gcnew<BuiltinLambda>( &l_vec_new ) );
    state->let( "vec_idx", gcnew<BuiltinLambda>( &l_vec_idx ) );
    state->let( "vec_set!", gcnew<BuiltinLambda>( &l_vec_set ) );
    state->let( "vec_size", gcnew<BuiltinLambda>( &l_vec_size ) );
    state->let( "Num", &Type::s_num );
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
