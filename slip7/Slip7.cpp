
// 1. lex + tree
// 2. tree shape check
// 3. syntax basics
// 4. syntax 
#include "Pch.h"
#include "SourceManager.h"
#include "Syntax.h"

namespace Sema {
    struct Node;
    struct Type;
    struct State;

    typedef Iter<Syntax::Atom*> Args;

    struct Node {
        virtual ~Node() {}
        virtual Node* eval( State* state, Args& args ) {
            verify( false );
            return nullptr;
        }

        Type* m_type{ nullptr };
    };
    //const Reflect::Field Node::s_reflectFields[] = {};
    //const Reflect::Type Node::s_reflectType = { Reflect::Kind::Record, nullptr, "Node" };

    struct Sema {

    };

    struct Type : Node {
    };

    struct Decl : Node {
    };

    struct Module : Node {
        std::vector<Node*> items;
    };



    struct State {
        
        void define( const char* sym, Node* value ) {
            syms[intern( sym )] = value;
        }

        Node* eval( Syntax::Atom* atom ) {
            if( auto sym = dynamic_cast<Syntax::Symbol*>( atom ) ) {
                auto it = syms.find( intern( sym->text().c_str() ) );
                assert2( it != syms.end(), sym->text().c_str() );
                return it->second;
            }
            else if( Syntax::List* list = dynamic_cast<Syntax::List*>( atom ) ) {
                assert( list );
                assert( list->items.size() );
                auto& items = list->items;
                Node* n = eval( items[0] );
                Args args{ &items[1], &items[items.size()-1] };
                return n->eval( this, args );
            }
            verify( false );
            return nullptr;
        }

        Module* module( Syntax::List* list ) {
            auto* module = new Module();
            for( auto c : list->items ) {
                Node* n = eval(c);
                module->items.push_back( n );
            }
            return module;
        }

        std::map < const char*, Node* > syms;

    protected:

        std::unordered_set<std::string> m_interns;
        const char* intern( const char* s ) {
            auto it = m_interns.insert( s );
            return it.first->c_str();
        }
        #if 0
        Atom* evalSym( Symbol& sym ) {
            auto it = syms.find( intern( sym.text().c_str() ) );
            assert( it != syms.end() );
            return it->second;
        }
        Type* newCallableType( array_view<Type*> inTypes, Type* retType ) {
            return nullptr;
        }
        #endif

    };
    #if 0
    const Reflect::Type Atom::s_reflectType = { nullptr, "Atom" };

    Atom* State_evalSym( State* s, std::string& sym ) {
        auto it = s->syms.find( s->intern( sym.c_str() ) );
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
    #endif


    struct Symbol : public Node {
        std::string m_name;
        Syntax::Symbol* m_sym;

        static Symbol* parse1( State* state, Syntax::Atom* atom ) {
            auto sym = dynamic_cast<Syntax::Symbol*>( atom );
            verify( sym );
            auto r = new Symbol();
            r->m_name = sym->text();
            r->m_sym = sym;
            if( sym->m_type ) {
                Node* n = state->eval( sym->m_type );
                auto t = dynamic_cast<Type*>( n );
                r->m_type = t;
            }
            return r;
        }

        static Symbol* parse( State* state, Args& args ) {
            auto r = parse1( state, args.cur() );
            args.advance();
            return r;
        }
        //    //auto val = Expr::bind( state, args );
        //    //Atom* value = rhs->parse( state );
        //    return new Symbol{ name, sym };
        //}
    };

    struct FunctionDecl : public Node {

        std::vector< Symbol* > m_arg_syms;
        Node* m_body = nullptr;

        FunctionDecl( std::vector<Symbol*>& args, Node* body ) {
            m_arg_syms.swap( args );
            m_body = body;
        }

        /*Atom* _parse( State* state, AtomList* args ) {
        std::vector<Atom*> vals;
        while( args->size() ) {
        Atom* arg = args->pop_front();
        vals.push_back( arg->parse( state ) );
        }
        return new FunctionCall( vals );
        }*/
    };


    struct Definition : public Node {

        Symbol* m_sym = nullptr;
        Node* m_value = nullptr;

        Definition() {}
        Definition( Symbol* sym, Node* value ) : m_sym( sym ), m_value( value ) {}


        #if 0
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
        #endif
    };


    struct BuiltinDefine : public Node {

        /*template<typename FUNC, typename...VISITARGS>
        void visit( FUNC func, VISITARGS...visitargs ) {
            func( m_sym, visitargs... );
            func( m_value, visitargs... );
        }*/

        Node* eval( State* state, Args& args ) override {
            auto sym = Symbol::parse( state, args );
            auto val = state->eval( args.cur() );
            args.advance();
            verify( args.used() );
            state->define( sym->m_name.c_str(), val );
            return new Definition( sym, val );
        }
    };


    struct BuiltinLambda : public Node {

        Node* eval( State* state, Args& args ) override {
            auto ain = dynamic_cast<Syntax::List*>(args.cur());
            args.advance();
            std::vector< Symbol* > arg_syms;
            for( auto i : ain->items ) {
                Symbol::parse1( state, i );
            }

            //Args::bind( state, args, &arg_syms );
            Syntax::Atom* rhs = args.cur();
            args.advance();
            
            for( auto a : arg_syms ) {
                state->define( a->m_name.c_str(), a );
            }
            Node* body = state->eval( rhs );
            return new FunctionDecl( arg_syms, body );
        }


        //    Type* retType = m_body->parse( state );
        //    auto r = new Lambda( nullptr/*state->m_env*/, m_arg_syms.m_list, m_body );
        //    r->m_loc = m_body->m_loc;
        //    r->m_type = state->newCallableType( inTypes, retType );
        //    return r;
        //}
    };

    #if 0
    struct Reference : public Atom {
        Atom* m_atom;
        Reference( Atom* a ) : m_atom( a ) {
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
                Atom* a = pair.second->parse( state );
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
                a->parse( state );
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

    #endif


    Sema* analyse( Syntax::List* syntax ) {
        State state;
        state.define( "define", new BuiltinDefine() );
        //state.define( "lambda", new BuiltinLambda() );
        //state.define( "let", new BuiltinLet() );
        //state.define( "int", &s_intType );
        //state.define( "double", &s_doubleType );
        Module* module = state.module( syntax );
        ////begin for range set! add_d div_d
        //for( Atom* a : syntax->items ) {
        //    Atom* b = a->parse( &state );
        //    b->print( 0 );
        //    b->codegen();
        //}
        return nullptr;
    }
}

int main( int argc, const char* argv[] ) {
    if( argc < 2 ) {
        Error.fmt( "Need a script to run" );
        return 1;
    }
    try {
        Syntax::SourceManager smanager;
        Syntax::List* syntax = Syntax::parse_file( smanager, argv[1] );
        verify( syntax );
        Sema::Sema* sema = Sema::analyse( syntax );
        verify( sema );
    }
    catch( float ) {
        return 1;
    }
}
