
// 1. lex + tree
// 2. tree shape check
// 3. syntax basics
// 4. syntax 
#include "Pch.h"
#include "SourceManager.h"
#include "Syntax.h"

#if 0
const Reflect::Type Atom::s_reflectType = { nullptr, "Atom" };


struct State {
    void define( const char* sym, Atom* value ) {
        syms[ intern( sym ) ] = value;
    }

    std::map<const char*, view_ptr<Atom> > syms;
    Atom* evalSym( Symbol& sym ) {
        auto it = syms.find(intern(sym.text().c_str()));
        assert( it != syms.end() );
        return it->second;
    }

    std::unordered_set<std::string> m_interns;
    const char* intern( const char* s ) {
        auto it = m_interns.insert( s );
        return it.first->c_str();
    }

    Type* newCallableType( array_view<Type*> inTypes, Type* retType ) {
        return nullptr;
    }

};
Atom* State_evalSym( State* s, std::string& sym ) {
    auto it = s->syms.find(s->intern( sym.c_str() ));
    if( it == s->syms.end() ) {
        Error.fmt( "Undefined symbol %s", sym.c_str() );
    }
    return it->second;
}

struct FunctionCall : public Atom {
    std::vector< Atom* > m_args;
    FunctionCall( std::vector< Atom* >& args ) {
        m_args.swap( args );
    }
};

struct FunctionDefn : public Atom {
    static const Reflect::Type s_reflectType;
    static const Reflect::Type& staticType() { return s_reflectType; }
    virtual const Reflect::Type& reflect() const { return s_reflectType; }

    std::vector< Symbol* > m_arg_syms;
    Atom* m_body = nullptr;

    FunctionDefn( std::vector<Symbol*>& args, Atom* body ) {
        m_arg_syms.swap( args );
        m_body = body;
    }

    Atom* _parse( State* state, AtomList* args ) {
        std::vector<Atom*> vals;
        while( args->size() ) {
            Atom* arg = args->pop_front();
            vals.push_back( arg->parse( state ) );
        }
        return new FunctionCall( vals );
    }
};

const Reflect::Type FunctionDefn::s_reflectType = {
    &Atom::s_reflectType, "FunctionDefn" };

struct Definition : public Atom {
    REFLECT_DECL()

    Symbol* m_sym = nullptr;
    Atom* m_value = nullptr;

    Definition( Symbol* sym, Atom* value) : m_sym(sym), m_value(value) {}

    virtual void codegen() {
        if( auto* def = dynamic_cast<FunctionDefn*>( m_value ) ) {
            printf( "define <rettype> %s(", m_sym->text().c_str() );
            const char* sep = "";
            for( auto* a : def->m_arg_syms ) {
                printf( "%s<dtype> %s", sep, a->text().c_str() );
                sep = ", ";
            }
            printf( ")(" );
            sep = "";
            for( auto* a : def->m_arg_syms ) {
                printf( "%s<mtype> %s, ", sep, a->text().c_str() );
                sep = ", ";
            }
            printf( ") {\n" );
            printf( "}\n" );
        }
        else {
            assert( false );
        }

    }
};

const Reflect::Field Definition::s_reflectFields[] = {
    REFLECT_FIELD( Definition,m_sym), REFLECT_FIELD( Definition,m_value)
};
REFLECT_DEFN( Definition, Atom, m_sym);



struct BuiltinDefine : public Atom {
    
    /*template<typename FUNC, typename...VISITARGS>
    void visit( FUNC func, VISITARGS...visitargs ) {
        func( m_sym, visitargs... );
        func( m_value, visitargs... );
    }*/
    Atom* _parse( State* state, AtomList* args ) override {
        Symbol* sym = nullptr;
        Atom* rhs = nullptr;
        Args::bind( state, args, &sym, &rhs );
        Atom* value = rhs->parse( state );
        state->define( sym->text().c_str(), value );
        return new Definition( sym, value );
    }
};



struct BuiltinLambda : public Atom {
    static const Reflect::Type s_reflectType;
    static const Reflect::Type& staticType() { return s_reflectType; }
    virtual const Reflect::Type& reflect() const { return s_reflectType; }

    Atom* _parse( State* state, AtomList* args ) {
        Args::ListOf< Symbol* > arg_syms;
        Atom* rbody = nullptr;
        Args::bind( state, args, &arg_syms, &rbody );
        //Env*
        for( auto* a : arg_syms.m_list ) {
            state->define( a->text().c_str(), a );
        }
        Atom* pbody = rbody->parse( state );
        return new FunctionDefn( arg_syms.m_list, pbody );
    }

    
    //    Type* retType = m_body->parse( state );
    //    auto r = new Lambda( nullptr/*state->m_env*/, m_arg_syms.m_list, m_body );
    //    r->m_loc = m_body->m_loc;
    //    r->m_type = state->newCallableType( inTypes, retType );
    //    return r;
    //}
};

const Reflect::Type BuiltinLambda::s_reflectType = {
    &Atom::s_reflectType, "BuiltinLambda" };

struct Reference : public Atom {
    Atom* m_atom;
    Reference( Atom* a ) : m_atom(a) {
    }
};

Atom* Symbol::_parse( State* state, AtomList* args ) {
    return new Reference( state->evalSym( *this ) );
}


struct BuiltinLet : public Atom {
    Args::ListOf< Args::PairOf<Symbol*, Atom*> > m_lets;
    Atom* m_body = nullptr;
    /*template<typename FUNC, typename...VISITARGS>
    void visit( FUNC func, VISITARGS...visitargs ) {
        func( m_lets, visitargs... );
        func( m_body, visitargs... );
    }*/
    Atom* _parse( State* state, AtomList* args ) {
        Atom* body;
        Args::bind( state, args, &m_lets, &body );
        for( const auto& pair : m_lets.m_list ) {
            Atom* a = pair.second->parse(state);
            state->define( pair.first->text().c_str(), a );
        }
        m_body = body->parse( state );
        return this;
    }
};

struct BuiltinModule {
    Args::ListOf< List* > m_contents;
    /*template<typename FUNC, typename...VISITARGS>
    void visit( FUNC func, VISITARGS...visitargs ) {
        func( m_contents, visitargs... );
    }*/

    Atom* parse( State* state, List* syntax ) {
        for( Atom* a : syntax->items ) {
            a->parse(state);
        }
        return nullptr;
    }
};


//void Atom::print_field( int ind, const char* name, const void* addr, const type_info& type ) const {
//    printf( "%s%s", indent(ind), name );
//    if( type == typeid( Symbol* ) ) {
//        printf( "=%s ", (*static_cast<const Symbol*const*>( addr ))->text().c_str() );
//    }
//    else {
//        printf( "=(%s)%p ", type.name(), addr );
//    }
//}

static Type s_intType;
static Type s_doubleType;

void parse( List* syntax ) {
    State state;
    state.define( "define", new BuiltinDefine() );
    state.define( "lambda", new BuiltinLambda() );
    state.define( "let", new BuiltinLet() );
    state.define( "int", &s_intType );
    state.define( "double", &s_doubleType );
    //begin for range set! add_d div_d
    for( Atom* a : syntax->items ) {
        Atom* b = a->parse( &state );
        b->print( 0 );
        b->codegen();
    }
    //module.parse( &state, &items );
}
#endif

int main( int argc, const char* argv[] ) {
    if( argc < 2 ) {
        Error.fmt( "Need a script to run" );
        return 1;
    }
    try {
        Syntax::SourceManager smanager;
        if( Syntax::List* syntax = Syntax::parse_file( smanager, argv[1] ) ) {
            syntax->print( 0 );
            return 0;
        }
    }
    catch( float ) {
        return 1;
    }
}
