#pragma once
// Atom
// .List
// .Number
// .String
// .Type

static const char* indent( int n ) {
    static char buf[128];
    for( int i = 0; i < n; ++i )
        buf[i] = ' ';
    buf[n] = 0;
    return buf;
}

struct State;
struct Atom;
struct Type;
struct Symbol;
Atom* State_evalSym( State* s, std::string& sym );
Atom* State_eval( State* s, Atom* a );

struct AtomList {
    Atom* pop_front() {
        assert( args.m_begin < args.m_end );
        Atom* a = args[0];
        args.m_begin += 1;
        return a;
    }
    int size() {
        return (int)args.size();
    }
    array_view<Atom*> args;
};


struct Atom {
    static const Reflect::Type s_reflectType;
public:
    typedef SourceManager::Location Location;
    Atom( const Location& loc ) : m_loc( loc ) {}
    Atom( ) {}
    virtual ~Atom() {}
    void print( int i ) const {
        _print( i );
    }
    Atom* parse( State* s, AtomList* args=nullptr );
    Atom* eval( State* s );
    virtual void codegen() {

    }
    static const Reflect::Type* staticType() { return &s_reflectType; }
    virtual const Reflect::Type* dynamicType() const { return &s_reflectType; }

protected:

    void _printr( int i, const void* addr, const Reflect::Type* type ) const {
        printf( "%s%s", indent( i ), type->name );
        for( auto& f : type->fields ) {
            printf( " %s=", f.name );
            if( f.type ) {
            }
        }
        printf( "\n" );
    }

    virtual void _print( int i ) const {
        const Reflect::Type* type = dynamicType();
        _printr( i, this, type );
    }
    virtual Atom* _eval( State* s ) { assert( 0 ); return nullptr; }
    virtual Atom* _parse( State* s, AtomList* args );
public:

    Location m_loc{};
    Atom* m_typeExpr = nullptr;
    view_ptr<Type> m_type;
};

struct Value : Atom {
    Value( ) {}
    Value( const Location& loc ) : Atom( loc ), m_text(text()) {}
    void _print( int i ) const override {
        auto s = m_loc.m_file->m_contents.substr( m_loc.m_start, m_loc.m_end - m_loc.m_start );
        printf( "%s%s\n", indent( i ), s.c_str() );
    }
    std::string text() const {
        return m_loc.m_file->m_contents.substr( m_loc.m_start, m_loc.m_end - m_loc.m_start );
    }
    Atom* _eval( State* state ) override {
        return this;
    }
    std::string m_text;
};

struct Type : public Value {

};

Atom* Atom::parse( State* s, AtomList* args ) {
    return _parse( s, args );
}

Atom* Atom::_parse( State* s, AtomList* args ) {
    if( m_type == nullptr ) {
        if( m_typeExpr ) {
            auto* a = m_typeExpr->eval( s );
            auto* t = cast( Type, a );
            assert( t );
            m_type = t;
        }
    }
    return this;
}

Atom* Atom::eval( State* s ) {
    return _eval( s );
}

struct String : Value {
    String( const Location& loc ) : Value( loc ) {}
    
    void _print( int i ) const override {
        printf( "%s\"%s\"\n", indent( i ), text().c_str() );
    }
};

struct Symbol : Value {
    Symbol( const Location& loc ) : Value( loc ) {}
    Atom* _parse( State* state, AtomList* args ) override;
    Atom* _eval( State* state ) override {
        auto s = this->text();
        return State_evalSym(state, s);
    }
};

struct Number : Value {
    Number( const Location& loc ) : Value( loc ) {}
    void _print( int i ) const override {
        auto s = m_loc.m_file->m_contents.substr( m_loc.m_start, m_loc.m_end - m_loc.m_start );
        printf( "%s#%s\n", indent( i ), s.c_str() );
    }
};

struct Callable : public Value {
    Callable() {}
    std::vector<std::string> m_arg_names;
    Atom* m_body;
    void _print( int i ) const override {
        Error;
    }
    virtual Atom* call( State* state, AtomList* args ) const {
        Error;
        return nullptr;
    }
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
        void set( F f, S s ) {
            first = f;
            second = s;
        }
        F first;
        S second;
    };

    namespace Detail {
        template<typename T>
        void bind1( State* state, AtomList* args, T** out ) {
            Atom* c = args->pop_front();
            *out = dynamic_cast<T*>(c);
            assert2( out, "Wrong argument type" );
        }
        template<typename F, typename S>
        void bind1( State* state, AtomList* args, PairOf<F, S>* out ) {
            List* lst = cast( List, args->pop_front() );
            if( !lst || lst->size() != 2 )
                Error.fmt( "Expected a pair of items" );
            AtomList todo{ lst->items };
            bind1( state, &todo, &out->first );
            bind1( state, &todo, &out->second );
        }
        template<typename T>
        void bind1( State* state, AtomList* args, Star<T>* out ) {
            for( ; iter.more(); ) {
                T t;
                bind1( state, iter, t );
                out->push_back( t );
            }
        }
        template<typename T>
        void bind1( State* state, AtomList* args, ListOf<T>* out ) {
            auto* arg = args->pop_front();
            List* lst = cast( List, arg );
            assert( lst );
            AtomList todo{ lst->items };
            for( ; todo.size(); ) {
                T t;
                bind1( state, &todo, &t );
                out->append( t );
            }
        }
    }
    struct StructureBinder {
        template<typename T>
        void operator() ( T& out, State* state, AtomList* args ) {
            //Detail::bind1( state, args );
        }
    };
    void bind( State* state, AtomList* args ) {
        assert( args->size() == 0 );
    }
    template<typename CAR, typename... CDR>
    void bind( State* state, AtomList* args, CAR car, CDR...cdr ) {
        Detail::bind1( state, args, car );
        bind( state, args, cdr... );
    }
}

template<typename ATOM>
struct BuiltinAtom : Atom {
    ATOM atom{};
    Atom* _parse( State* state, AtomList* args) override {
        args->pop_front();
        atom.visit( Args::StructureBinder(), state, args );
        return atom.parse(state);
    }
    //Atom* call( State* state, AtomList* args ) const override {
    //    args->pop_front( );
    //    assert2( args->size() == 0, "Too many arguments" );
    //    return node.call( state );
    //}
};

struct List : public Atom {
    List() {
    }
    List( const Location& loc ) : Atom( loc ) {
    }
    int size() const {
        return (int)items.size();
    }
    void append( Atom* a ) {
        items.push_back( a );
    }
    void _print(int i) const override {
        printf( "%s(\n", indent(i) );
        for( auto& a : items ) {
            a->print(i+1);
        }
        printf( "%s)\n", indent(i) );
    }
    Atom* _parse( State* state, AtomList* args ) override {
        assert( args == nullptr );
        if( items.empty() ) {
            Error.fmt( "Empty list is illegal" ).at( m_loc );
        }
        Atom* first = items[0]->eval( state );
        verify( first );
        AtomList tmp{ items }; tmp.pop_front();
        return first->parse( state, &tmp);
        //if( auto call = cast( Callable, first ) ) {
            
            //return call->call( state, &args );
        //}
        //Error.fmt( "Expected a callable as the first argument" ).at( items[0]->m_loc );
        //return nullptr;
    }

    Atom* _eval( State* state ) override {
        if( items.empty() ) {
            Error.fmt("Empty list is illegal").at(m_loc);
        }
        Atom* first = items[0]->eval( state );
        if( auto call = cast( Callable, first) ) {
            AtomList args{ items };
            return call->call( state, &args );
        }
        Error.fmt( "Expected a callable as the first argument" ).at( items[0]->m_loc );
        return nullptr;
    }
    std::vector< Atom* > items;
};


struct Env : Value {
    Env( Env *p ) : m_parent( p ) {}
//    Atom* get( Symbol sym ) {
//        for( Env* e = this; e; e = e->m_parent ) {
//            auto it = e->m_tab.find( sym );
//            if( it != e->m_tab.end() ) {
//                return it->second;
//            }
//        }
//        Error( "Symbol '%s' not found", sym.m_sym );
//        return Box();
//    }
//    void _print() const override {
//        printf( "<env" );
//        for( auto& e : m_tab ) {
//            printf( " %s=", e.first.m_sym );
//            e.second.print();
//        }
//        if( m_parent && m_parent->m_parent ) m_parent->print();
//        printf( ">" );
//    }
//    void put( Symbol sym, Box val ) {
//        m_tab[sym] = val;
//    }
//    void update( Symbol sym, Box val ) {
//        for( Env* e = this; e; e = e->m_parent ) {
//            auto it = e->m_tab.find( sym );
//            if( it != e->m_tab.end() ) {
//                it->second = val;
//                return;
//            }
//        }
//        assert( 0 );
//    }
//
     Env* m_parent;
//    typedef std::unordered_map<Symbol, Box> Table;
//    Table m_tab;
};


struct Lambda : Callable {
    std::vector<Symbol*> m_arg_names;
    Env* m_lex_env = nullptr;
    Atom* m_body = nullptr;

    Lambda( Env* lex_env, const std::vector<Symbol*>& arg_names, Atom* body )
        : m_arg_names( arg_names ), m_lex_env( lex_env ), m_body( body ) {
    }

    Atom* call( State* state, AtomList* args ) const override {
        //Args::Detail::ArgIter iter{ args_in.begin(), args_in.end() };
        //assert( args.size() == m_arg_names.size() );
        //Env* e = State_create<Env>( state, m_lex_env );
        //GcAnchor anchor( state, e );
        //unsigned ni = 0;
        //for( auto arg : args ) {
        //    Box a = arg.eval( state );
        //    //auto s = cast( Symbol, m_arg_names->at( ni ) );
        //    //if( s->m_type != nullptr ) {
        //    /*auto nt = s->m_type.eval( env );
        //    auto at = a->m_type.eval( env );
        //    if( at != nt ) {
        //    Error_At( arg->m_loc, "Mismatched type for argument '%i'", ni );
        //    }*/
        //    //}
        //    e->put( m_arg_names[ni], a );
        //    ni += 1;
        //}
        return m_body->eval(state);
    }
    void _print(int i) const override {
        printf( "%s(lambda (", indent(i) );
        for( auto a : m_arg_names ) {
            a->print( i + 1 );// "%s ", a.c_str());
        }
        printf( ") " );
        m_body->print(i+1);
    }
};


