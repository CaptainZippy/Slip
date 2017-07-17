
// 1. lex + tree
// 2. tree shape check
// 3. syntax basics
// 4. syntax 
#include "Pch.h"
#include "SourceManager.h"
#include "Syntax.h"
#include "Reflect.h"
namespace Reflect {
    namespace Detail {
        struct Output {
            void begin(const char* s) {
                printf("%s", s);
                m_indent.push_back(' ');
            }
            void write(const char* s) {
                if(s) printf("%s", s);
            }
            void field(const char* s) {
                printf("%s = ", s);
            }
            void write(const void* s) {
                printf("%p", s);
            }
            void end(const char* s=nullptr) {
                m_indent.erase(m_indent.size() - 1);
                write(s);
            }
            void nl() {
                printf("\n%s", m_indent.c_str());
            }
            std::string m_indent;
        };
        void printVar( Output& out, Var top ) {
            if( top.addr == nullptr ) {
                out.write("null");
                return;
            }
            if (top.type->toString) {
                out.write( top.type->toString(top.addr).c_str() );
                return;
            }
            switch( top.type->kind ) {
                case Kind::Pointer: {
                    auto obj = *(void**)top.addr;
                    //out.write(obj);
                    if (obj) {
                        auto sub = top.type->sub;
                        Var e{ obj, sub->dynamicType(obj) };
                        printVar(out, e);
                    }
                    else {
                        out.write("null");
                    }
                    break;
                }
                case Kind::Record: {
                    out.begin( top.type->name );
                    out.write("{");
                    std::vector<const Reflect::Type*> chain;
                    for (auto c = top.type; c; c = c->parent) {
                        if (c->fields.size()) {
                            chain.push_back(c);
                        }
                    }
                    for( auto c : reversed(chain) ) {
                        for( auto f : c->fields ) {
                            out.nl();
                            out.field(f.name);
                            Var v = top[f];
                            printVar( out, v );
                        }
                    }
                    out.end("}");
                    break;
                }
                case Kind::Array: {
                    auto et = top.type->sub;
                    char* s = ( (char**) top.addr )[0];
                    char* e = ( (char**) top.addr )[1];
                    unsigned count = unsigned(( e - s ) / et->size);
                    out.begin("[");
                    for( unsigned i = 0; i < count; ++i ) {
                        Var e{ s + i * et->size, et };
                        out.nl();
                        printVar( out, e );
                    }
                    out.end("]");
                    break;
                }
                case Kind::String: {
                    std::string s = top.type->toString(top.addr);
                    out.write(s.c_str());
                    break;
                }
                default:
                    out.write( "???" );
                    break;
            }
        }
    }
    void printVar(Var top) {
        Detail::Output output;
        Detail::printVar(output, top);
        output.nl();
    }
}

namespace Sema {
    struct Node;
    struct Type;
    struct State;
    struct FunctionCall;
    struct FunctionDecl;
    struct Number;

    typedef Iter<Syntax::Atom*> Args;

    struct Node : Reflect::AbstractReflected {
        REFLECT_DECL();
        virtual Node* eval( State* state, Args& args ) const {
            verify( false );
            return nullptr;
        }
        void print() const {
            Reflect::Var self{ const_cast<Node*>(this), dynamicType() };
            Reflect::printVar( self );
        }

        Type* m_type{ nullptr };
    };
    REFLECT_BEGIN(Node)
        REFLECT_FIELDS(m_type)
    REFLECT_END()


    struct Type : Node {
    };

    struct Decl : Node {
    };

    struct Number : Node {
        Number( Syntax::Number* n) : m_num(n) {}
        Syntax::Number* m_num;
    };

    struct Module : Node {
        REFLECT_DECL();
        std::vector<Node*> m_items;
    };

    REFLECT_BEGIN(Module)
        REFLECT_PARENT(Node)
        REFLECT_FIELDS(m_items)
    REFLECT_END()

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
            else if( auto list = dynamic_cast<Syntax::List*>( atom ) ) {
                assert( list );
                assert( list->items.size() );
                auto& items = list->items;
                Node* n = eval( items[0] );
                Args args{ items }; args.advance();
                return n->eval( this, args );
            }
            else if( auto num = dynamic_cast<Syntax::Number*>( atom ) ) {
                return new Number( num );
            }

            verify( false );
            return nullptr;
        }

        Module* module( Syntax::List* list ) {
            auto* module = new Module();
            for( auto c : list->items ) {
                Node* n = eval(c);
                module->m_items.push_back( n );
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
    #endif

    struct FunctionCall : public Node {
        REFLECT_DECL();
        const FunctionDecl* m_func{ nullptr };
        std::vector< Node* > m_args;
        FunctionCall( const FunctionDecl* func, std::vector< Node* >&& args )
            : m_func(func), m_args(args) {
        }
    };

    REFLECT_BEGIN(FunctionCall)
        REFLECT_FIELDS(m_func)
        //REFLECT_FIELD(m_args)
    REFLECT_END()

    struct Symbol : public Node {
        REFLECT_DECL();

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

        static std::string toString(const void* obj) {
            auto sym = static_cast<const Symbol*>(obj);
            return string_format("%s{\"%s\"}", sym->dynamicType()->name, sym->m_name.c_str());
        }

        //    //auto val = Expr::bind( state, args );
        //    //Atom* value = rhs->parse( state );
        //    return new Symbol{ name, sym };
        //}
    };

    REFLECT_BEGIN(Symbol)
        REFLECT_FIELDS(m_name)
        REFLECT_TO_STRING(Symbol::toString)
        //REFLECT_FIELD(Symbol, m_sym)
    REFLECT_END()

    struct FunctionDecl : public Node {

        REFLECT_DECL();

        std::vector< Symbol* > m_arg_syms;
        Node* m_body = nullptr;

        FunctionDecl() {}
        FunctionDecl( std::vector<Symbol*>& args, Node* body ) {
            m_arg_syms.swap( args );
            m_body = body;
        }

        Node* eval( State* state, Args& args ) const override {
            std::vector<Node*> vals;
            verify( args.size() == m_arg_syms.size() );
            for( ; args.size(); args.advance() ) {
                vals.push_back( state->eval(args.cur()) );
            }
            return new FunctionCall( this, std::move(vals) );
        }
    };

    REFLECT_BEGIN(FunctionDecl)
        REFLECT_FIELDS(m_arg_syms)
        //REFLECT_FIELD(FunctionDecl, m_body)
    REFLECT_END()


    struct Sequence : public Node {
        std::vector<Node*> m_items;
    };

    struct Scope : public Node {
        Node* m_child{ nullptr };
    };



    struct Definition : public Node {

        REFLECT_DECL();

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

    REFLECT_BEGIN(Definition)
        REFLECT_FIELDS(m_sym)
//		REFLECT_FIELD(m_value)
    REFLECT_END()

    struct BuiltinDefine : public Node {

        /*template<typename FUNC, typename...VISITARGS>
        void visit( FUNC func, VISITARGS...visitargs ) {
            func( m_sym, visitargs... );
            func( m_value, visitargs... );
        }*/

        Node* eval( State* state, Args& args ) const override {
            auto sym = Symbol::parse( state, args );
            auto val = state->eval( args.cur() );
            args.advance();
            verify( args.used() );
            state->define( sym->m_name.c_str(), val );
            return new Definition( sym, val );
        }
    };


    struct BuiltinLambda : public Node {

        Node* eval( State* state, Args& args ) const override {
            auto ain = dynamic_cast<Syntax::List*>(args.cur());
            args.advance();
            std::vector< Symbol* > arg_syms;
            for( auto i : ain->items ) {
                arg_syms.push_back( Symbol::parse1( state, i ) );
            }

            //Args::bind( state, args, &arg_syms );
            Syntax::Atom* rhs = args.cur();
            args.advance();
            verify( args.used() );

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
    #endif


    struct BuiltinLet : public Node {
        Node* eval( State* state, Args& args ) const override {
            auto lets = dynamic_cast<Syntax::List*>( args.cur() );
            args.advance();
            auto body = args.cur();
            args.advance();
            verify(args.used());
            auto seq = new Sequence();
            for( auto pair : lets->items) {
                auto let = dynamic_cast<Syntax::List*>( pair );
                verify( let->size() == 2 );
                //Atom* a = pair.second->parse( state );
                //state->define( pair.first->text().c_str(), a );
            }
            return new Scope();
        }
    };
    #if 0

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
    #endif

    static Type s_intType;
    static Type s_doubleType;



    Node* analyse( Syntax::List* syntax ) {
        State state;
        state.define( "define", new BuiltinDefine() );
        state.define( "lambda", new BuiltinLambda() );
        state.define( "let", new BuiltinLet() );
        //state.define( "int", &s_intType );
        state.define( "double", &s_doubleType );
        Module* module = state.module( syntax );
        ////begin for range set! add_d div_d
        //for( Atom* a : syntax->items ) {
        //    Atom* b = a->parse( &state );
        //    b->print( 0 );
        //    b->codegen();
        //}
        return module;
    }
    Node* typecheck( Node* node ) {
        return node;
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
        Sema::Node* sema = Sema::analyse( syntax );
        verify( sema );
        sema->print();
        auto ok = Sema::typecheck( sema );
        verify( ok );
    }
    catch( float ) {
        return 1;
    }
}
