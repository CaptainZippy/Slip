#include "pch/Pch.h"

#include "Ast.h"
#include "Lex.h"
#include "Slip.h"

#define WITH( ... ) [&]( auto& _ ) { __VA_ARGS__; }

namespace Slip::Parse {

    template <typename T>
    struct Iter {
        Iter() = default;

        Iter( std::vector<T>& v ) {
            T* t = v.size() ? &v[0] : nullptr;
            m_begin = t;
            m_end = t + v.size();
        }

        Iter( array_view<T> v ) {
            m_begin = v.begin();
            m_end = v.end();
        }

        T cur() const {
            assert( size() );
            return *m_begin;
        }

        bool advance() {
            assert( m_begin < m_end );
            m_begin += 1;
            return m_begin < m_end;
        }

        bool empty() const { return m_begin == m_end; }

        size_t size() const { return m_end - m_begin; }

        T* begin() { return m_begin; }
        T* end() { return m_end; }

        T* m_begin{nullptr};
        T* m_end{nullptr};
    };
    typedef Iter<Lex::Atom*> Args;

    struct Define;
    struct Func;
    struct Let;
    struct If;
    struct While;
    struct Cond;
    struct Begin;
    struct Block;
    struct Break;
    struct Var;
    struct Set;
    struct Scope;
    struct Macro;
    struct Now;
    struct ArrayView;
    struct ArrayConst;

}  // namespace Slip::Parse

using namespace Slip;
enum class Ellipsis { ZeroOrMore, OneOrMore };

static Result matchLex( Parse::Args& args ) {
    RETURN_RES_IF( Result::ERR, !args.empty() );
    return Result::OK;
}

static Result matchLex( Parse::Args& args, std::vector<Lex::Atom*>* list, Ellipsis ellipsis ) {
    RETURN_RES_IF( Result::ERR, ellipsis == Ellipsis::OneOrMore && args.empty() );
    if( args.empty() ) {
        return Result::OK;
    }
    // auto sl = args.cur()->m_loc;
    // sl.m_end = ( args.end() - 1 )[0]->m_loc.m_end;
    do {
        list->emplace_back( args.cur() );
    } while( args.advance() );
    return Result::OK;
}

/// Match and consume the entire argument list exactly
template <typename HEAD, typename... REST>
static Result matchLex( Parse::Args& args, HEAD** head, REST... rest ) {
    RETURN_RES_IF( Result::ERR, args.empty() );
    auto h = dynamic_cast<HEAD*>( args.cur() );
    RETURN_RES_IF( Result::ERR, !h );
    args.advance();
    *head = h;
    return matchLex( args, rest... );
}

template <typename... REST>
static Result matchLex( Lex::List* list, REST... rest ) {
    Parse::Args args{list->items()};
    RETURN_RES_IF( Result::ERR, args.empty() );
    args.advance();
    return matchLex( args, rest... );
}

static Result parse1( Ast::Environment* env, Lex::Atom* atom, Ast::Node** out );

static Result macroExpand1( Ast::Environment* env, Lex::List* args, void* context, Ast::Node** out ) {
    RETURN_RES_IF( Result::ERR, args->size() < 2 || args->size() > 3 );
    Lex::Symbol* larg;
    Lex::Symbol* lenv = nullptr;
    switch( args->size() ) {
        case 2:
            RETURN_IF_FAILED( matchLex( args, &larg ) );
            break;
        case 3:
            RETURN_IF_FAILED( matchLex( args, &larg, &lenv ) );
            break;
    }

    Ast::Node* repl;
    RETURN_IF_FAILED( env->lookup( larg->text(), &repl ) );
    auto text = dynamic_cast<Ast::Syntax*>( repl );
    RETURN_RES_IF( Result::ERR, text == nullptr );

    Ast::Environment* xenv;
    if( lenv ) {
        Ast::Node* val;
        RETURN_IF_FAILED( env->lookup( lenv->text(), &val ) );
        xenv = dynamic_cast<Ast::Environment*>( val );
        RETURN_RES_IF( Result::ERR, xenv == nullptr );
    } else {
        xenv = env;
    }

    RETURN_IF_FAILED( parse1( xenv, text->m_atom, out ) );
    return Result::OK;
}

static Result macroExpand( Ast::MacroDecl* macro, Ast::Environment* env, Lex::List* list, Ast::Node** out ) {
    auto args = list->items().ltrim( 1 );
    RETURN_RES_IF( Result::ERR, args.size() != macro->m_params.size() );
    auto localEnv = new Ast::Environment( macro->m_staticEnv );
    for( unsigned i = 0; i < macro->m_params.size(); ++i ) {
        localEnv->bind( macro->m_params[i]->name(), new Ast::Syntax( args[i] ) );
    }
    localEnv->bind( macro->m_dynEnvSym, env );
    static Ast::Builtin expander{"expand"sv, &macroExpand1, nullptr};
    localEnv->bind( "expand"sv, &expander );
    RETURN_IF_FAILED( parse1( localEnv, macro->m_body, out ) );
    return Result::OK;
}

static Result parse1( Ast::Environment* env, Lex::Atom* atom, Ast::Node** out ) {
    *out = nullptr;
    if( atom == nullptr ) {
        return Result::OK;
    } else if( auto sym = dynamic_cast<Lex::Symbol*>( atom ) ) {
        Ast::Node* node;
        RETURN_IF_FAILED( env->lookup( sym->text(), &node ), "Symbol '%.*s' not found", sym->text().length(), sym->text().data() );

        // TODO: detect symbol is decl or const.
        if( auto ty = dynamic_cast<Ast::Type*>( node ) ) {
            *out = ty;
        } else if( auto nu = dynamic_cast<Ast::Number*>( node ) ) {
            *out = nu;
        } else {
            *out = new Ast::Reference( node );
        }
        return Result::OK;
    } else if( auto list = dynamic_cast<Lex::List*>( atom ) ) {
        RETURN_RES_IF( Result::ERR, !list );
        RETURN_RES_IF( Result::ERR, list->size() == 0 );
        auto items = list->items();
        auto first = items[0];
        auto sym = dynamic_cast<Lex::Symbol*>( first );
        RETURN_RES_IF( Result::ERR, !sym, "Symbol expected in first list position" );

        // sym can resolve to a builtin or macro or function. Only functions can be overloaded.
        Ast::Node* p;
        auto isym = istring::make( sym->text() );
        std::vector<Ast::Node*> candidates;
        Ast::Environment::LookupIter iter;
        while( env->lookup_iter( isym, &p, iter ) ) {
            if( auto b = dynamic_cast<Ast::Builtin*>( p ) ) {
                RETURN_RES_IF( Result::ERR, !candidates.empty(), "Builtins can't be overloaded" );
                RETURN_RES_IF( Result::ERR, env->lookup_iter( isym, &p, iter ), "Builtins can't be overloaded" );
                return b->parse( env, list, out );
            } else if( auto m = dynamic_cast<Ast::MacroDecl*>( p ) ) {
                RETURN_RES_IF( Result::ERR, !candidates.empty(), "Builtins can't be overloaded" );
                RETURN_RES_IF( Result::ERR, env->lookup_iter( isym, &p, iter ), "Builtins can't be overloaded" );
                return macroExpand( m, env, list, out );
            } else {
                candidates.emplace_back( p );
            }
        }

        // eval args
        vector<Ast::Node*> fa;
        for( auto a : list->items().ltrim( 1 ) ) {
            Ast::Node* n;
            RETURN_IF_FAILED( parse1( env, a, &n ) );
            fa.push_back( n );
        }
        *out = new Ast::NamedFunctionCall( isym, std::move( candidates ), std::move( fa ), WITH( _.m_loc = list->m_loc ) );
        return Result::OK;
    } else if( auto num = dynamic_cast<Lex::Number*>( atom ) ) {
        Ast::Node* te;
        RETURN_IF_FAILED( parse1( env, num->m_decltype, &te ) );
        auto r = new Ast::Number( num->text(), WITH( _.m_loc = num->m_loc, _.m_declTypeExpr = te ) );
        *out = r;
        return Result::OK;
    } else if( auto str = dynamic_cast<Lex::String*>( atom ) ) {
        Ast::Node* te;
        RETURN_IF_FAILED( parse1( env, str->m_decltype, &te ) );
        auto r = new Ast::String( str->text(), WITH( _.m_loc = str->m_loc, _.m_declTypeExpr = te ) );
        *out = r;
        return Result::OK;
    }
    RETURN_RES_IF( Result::ERR, true );
}

struct Parse::Define {
    static Result parse( Ast::Environment* env, Lex::List* args, void* context, Ast::Node** out ) {
        Lex::Symbol* lname;
        Lex::Atom* lval;
        RETURN_IF_FAILED( matchLex( args, &lname, &lval ) );
        Ast::Node* nval;
        RETURN_IF_FAILED( parse1( env, lval, &nval ) );

        auto ret = new Ast::Definition( lname->text(), nval, WITH( _.m_loc = lname->m_loc ) );
        env->bind( lname->text(), ret );

        *out = ret;
        return Result::OK;
    }
};

struct Parse::Func {
    static Result parse( Ast::Environment* env, Lex::List* args, void* context, Ast::Node** out ) {
        *out = nullptr;
        RETURN_RES_IF( Result::ERR, args->size() < 3 );
        Lex::Symbol* lname;
        Lex::List* largs;
        std::vector<Lex::Atom*> lbody;
        RETURN_IF_FAILED( matchLex( args, &lname, &largs, &lbody, Ellipsis::OneOrMore ) );
        auto func = new Ast::FunctionDecl( lname->text(), WITH( _.m_loc = lname->m_loc ) );
        env->bind( func->m_name, func );
        auto inner = new Ast::Environment( env );
        if( auto a = largs->m_decltype ) {
            Ast::Node* te;
            RETURN_IF_FAILED( parse1( inner, a, &te ) );
            func->m_declReturnTypeExpr = te;
        }
        for( auto item : largs->items() ) {
            auto sym = dynamic_cast<Lex::Symbol*>( item );
            RETURN_RES_IF( Result::ERR, sym == nullptr );
            Ast::Node* te;
            RETURN_IF_FAILED( parse1( inner, item->m_decltype, &te ) );
            auto param = new Ast::Parameter( sym->text(), WITH( _.m_loc = sym->m_loc, _.m_declTypeExpr = te ) );
            func->m_params.push_back( param );
        }
        // TODO check unique names
        for( auto p : func->m_params ) {
            inner->bind( p->m_name, p );
        }
        Ast::Node* body;
        if( lbody.size() == 1 ) {
            RETURN_IF_FAILED( parse1( inner, lbody[0], &body ) );
        } else {
            auto bd = new Ast::Sequence();
            for( auto&& l : lbody ) {
                Ast::Node* b;
                RETURN_IF_FAILED( parse1( inner, l, &b ) );
                bd->m_items.emplace_back( b );
            }
            body = bd;
        }
        func->m_body = body;
        *out = func;
        return Result::OK;
    }
};

struct Parse::Let {
    static Result parse( Ast::Environment* env, Lex::List* args, void* context, Ast::Node** out ) {
        Lex::Symbol* lname;
        Lex::Atom* lbody;
        if( matchLex( args, &lname, &lbody ).isOk() ) {  // short form (let foo 0)

            Ast::Node* aval;
            RETURN_IF_FAILED( parse1( env, lbody, &aval ) );
            Ast::Node* te;
            RETURN_IF_FAILED( parse1( env, lname->m_decltype, &te ) );
            auto def = new Ast::Definition( lname->text(), aval, WITH( _.m_loc = lbody->m_loc, _.m_declTypeExpr = te ) );
            RETURN_IF_FAILED( env->bind( lname->text(), def ) );
            *out = def;
            return Result::OK;
        }

        Lex::List* llets;  // long form (let ((a 0) (b 1)) (+ a b))
        RETURN_IF_FAILED( matchLex( args, &llets, &lbody ) );

        auto inner = new Ast::Environment( env );
        auto seq = new Ast::Sequence();
        for( auto litem : llets->items() ) {
            Lex::List* lpair = dynamic_cast<Lex::List*>( litem );
            RETURN_RES_IF( Result::ERR, !lpair );
            Lex::Symbol* lsym;
            Lex::Atom* lval;
            Args tmpargs{lpair->items()};
            RETURN_IF_FAILED( matchLex( tmpargs, &lsym, &lval ) );
            Ast::Node* aval;
            RETURN_IF_FAILED( parse1( inner, lval, &aval ) );
            Ast::Node* te;
            RETURN_IF_FAILED( parse1( inner, lsym->m_decltype, &te ) );

            auto def = new Ast::Definition( lsym->text(), aval, WITH( _.m_loc = lsym->m_loc, _.m_declTypeExpr = te ) );
            inner->bind( lsym->text(), def );
            seq->m_items.push_back( def );
        }
        Ast::Node* abody;
        RETURN_IF_FAILED( parse1( inner, lbody, &abody ) );
        seq->m_items.push_back( abody );

        *out = seq;
        return Result::OK;
    }
};

struct Parse::If {
    static Result parse( Ast::Environment* env, Lex::List* args, void* context, Ast::Node** out ) {
        Lex::Atom* lcond;
        Lex::Atom* ltrue;
        Lex::Atom* lfalse;
        RETURN_IF_FAILED( matchLex( args, &lcond, &ltrue, &lfalse ) );

        Ast::Node* ncond;
        Ast::Node* ntrue;
        Ast::Node* nfalse;
        RETURN_IF_FAILED( parse1( env, lcond, &ncond ) );
        RETURN_IF_FAILED( parse1( env, ltrue, &ntrue ) );
        RETURN_IF_FAILED( parse1( env, lfalse, &nfalse ) );

        *out = new Ast::If( ncond, ntrue, nfalse, WITH( _.m_loc = lcond->m_loc ) );
        return Result::OK;
    }
};

struct Parse::While {
    static Result parse( Ast::Environment* env, Lex::List* args, void* context, Ast::Node** out ) {
        Lex::Atom* lcond;
        std::vector<Lex::Atom*> lbody;
        RETURN_IF_FAILED( matchLex( args, &lcond, &lbody, Ellipsis::OneOrMore ) );
        Ast::Node* ncond;
        RETURN_IF_FAILED( parse1( env, lcond, &ncond ) );
        Ast::Node* nbody;
        if( lbody.size() == 1 ) {
            RETURN_IF_FAILED( parse1( env, lbody[0], &nbody ) );
        } else {
            auto bd = new Ast::Sequence();
            for( auto&& l : lbody ) {
                Ast::Node* b;
                RETURN_IF_FAILED( parse1( env, l, &b ) );
                bd->m_items.emplace_back( b );
            }
            nbody = bd;
        }
        *out = new Ast::While( ncond, nbody, WITH( _.m_loc = lcond->m_loc ) );
        return Result::OK;
    }
};

struct Parse::Cond {
    static Result parse( Ast::Environment* env, Lex::List* args, void* context, Ast::Node** out ) {
        *out = nullptr;
        RETURN_RES_IF( Result::ERR, args->size() < 2 );
        vector<pair<Ast::Node*, Ast::Node*>> cases;
        for( auto&& arg : args->items().ltrim( 1 ) ) {
            auto pair = dynamic_cast<Lex::List*>( arg );
            RETURN_RES_IF( Result::ERR, pair == nullptr );
            RETURN_RES_IF( Result::ERR, pair->size() != 2 );

            Ast::Node* cond;
            RETURN_IF_FAILED( parse1( env, pair->at( 0 ), &cond ) );
            Ast::Node* iftrue;
            RETURN_IF_FAILED( parse1( env, pair->at( 1 ), &iftrue ) );
            cases.emplace_back( cond, iftrue );
        }
        auto cond = new Ast::Cond( WITH( /*TODO loc*/ ) );
        cond->m_cases.swap( cases );
        *out = cond;
        return Result::OK;
    }
};

struct Parse::Begin {
    static Result parse( Ast::Environment* env, Lex::List* args, void* context, Ast::Node** out ) {
        *out = nullptr;
        auto ret = new Ast::Sequence;
        for( auto arg : args->items().ltrim( 1 ) ) {
            Ast::Node* n;
            RETURN_IF_FAILED( parse1( env, arg, &n ) );
            ret->m_items.push_back( n );
        }
        *out = ret;
        return Result::OK;
    }
};

struct Parse::Scope {
    static Result parse( Ast::Environment* env, Lex::List* args, void* context, Ast::Node** out ) {
        *out = nullptr;
        auto ret = new Ast::Sequence;
        auto inner = new Ast::Environment( env );
        for( auto arg : args->items().ltrim( 1 ) ) {
            Ast::Node* n;
            RETURN_IF_FAILED( parse1( inner, arg, &n ) );
            ret->m_items.push_back( n );
        }
        *out = new Ast::Scope( ret );
        return Result::OK;
    }
};

struct Parse::Block {
    static Result parse( Ast::Environment* env, Lex::List* args, void* context, Ast::Node** out ) {
        *out = nullptr;
        Lex::Symbol* name;
        std::vector<Lex::Atom*> contents;
        RETURN_IF_FAILED( matchLex( args, &name, &contents, Ellipsis::ZeroOrMore ) );
        auto seq = new Ast::Sequence;
        auto ret = new Ast::Block( istring::make( name->text() ), seq );
        auto inner = new Ast::Environment( env );
        inner->bind( name->text(), ret );
        for( auto c : contents ) {
            Ast::Node* n;
            RETURN_IF_FAILED( parse1( inner, c, &n ) );
            seq->m_items.push_back( n );
        }
        *out = ret;
        return Result::OK;
    }
};

struct Parse::Break {
    static Result parse( Ast::Environment* env, Lex::List* args, void* context, Ast::Node** out ) {
        *out = nullptr;
        Lex::Symbol* llabel;
        Lex::Atom* lval;
        RETURN_IF_FAILED( matchLex( args, &llabel, &lval ) );
        Ast::Node* nlabel;
        RETURN_IF_FAILED( parse1( env, llabel, &nlabel ) );
        auto blockr = dynamic_cast<Ast::Reference*>( nlabel );  // TODO tidy
        auto blocka = dynamic_cast<Ast::Block*>( blockr->m_target );
        RETURN_RES_IF( Result::ERR, blocka == nullptr );
        Ast::Node* nval;
        RETURN_IF_FAILED( parse1( env, lval, &nval ) );
        *out = new Ast::Break( blocka, nval );
        return Result::OK;
    }
};

struct Parse::Var {
    static Result parse( Ast::Environment* env, Lex::List* args, void* context, Ast::Node** out ) {
        *out = nullptr;

        Lex::Symbol* sym;
        std::vector<Lex::Atom*> inits;
        RETURN_IF_FAILED( matchLex( args, &sym, &inits, Ellipsis::ZeroOrMore ) );

        Ast::Node* varType;
        RETURN_IF_FAILED( parse1( env, sym->m_decltype, &varType ) );

        std::vector<Ast::Node*> vals;
        for( auto&& i : inits ) {
            Ast::Node* n;
            RETURN_IF_FAILED( parse1( env, i, &n ) );
            vals.emplace_back( n );
        }

        auto ret = new Ast::VariableDecl( istring::make( sym->text() ), WITH( _.m_declTypeExpr = varType, _.m_loc = sym->m_loc ) );
        ret->m_initializer.swap( vals );
        RETURN_IF_FAILED( env->bind( sym->text(), ret ) );
        *out = ret;
        return Result::OK;
    }
};

struct Parse::Set {
    static Result parse( Ast::Environment* env, Lex::List* args, void* context, Ast::Node** out ) {
        *out = nullptr;
        Lex::Symbol* sym;
        Lex::Atom* expr;
        RETURN_IF_FAILED( matchLex( args, &sym, &expr ) );
        Ast::Node* value = nullptr;
        RETURN_IF_FAILED( env->lookup( sym->text(), &value ), "Symbol '%.*s' not found", sym->text().length(), sym->text().data() );
        Ast::Node* rhs;
        RETURN_IF_FAILED( parse1( env, expr, &rhs ) );
        *out = new Ast::Assignment( value, rhs, WITH( _.m_loc = sym->m_loc ) );
        return Result::OK;
    }
};

struct Parse::Macro {
    static Result parse( Ast::Environment* env, Lex::List* args, void* context, Ast::Node** out ) {
        *out = nullptr;
        Lex::Symbol* lname;
        Lex::List* largs;
        Lex::Symbol* lenv;
        Lex::Atom* lbody;
        RETURN_IF_FAILED( matchLex( args, &lname, &largs, &lenv, &lbody ) );
        auto macro = new Ast::MacroDecl( lname->text(), lenv->text(), env, WITH( _.m_loc = lname->m_loc ) );
        for( auto item : largs->items() ) {
            auto sym = dynamic_cast<Lex::Symbol*>( item );
            RETURN_RES_IF( Result::ERR, sym == nullptr );
            auto param = new Ast::Parameter( sym->text(), WITH( _.m_loc = sym->m_loc ) );
            macro->m_params.push_back( param );
        }
        env->bind( macro->m_name, macro );
        macro->m_body = lbody;
        *out = macro;
        return Result::OK;
    }
};

struct Parse::Now {
    static Result parse( Ast::Environment* env, Lex::List* args, void* context, Ast::Node** out ) {
        *out = nullptr;
        Lex::List rest{args->m_loc};
        for( auto p : args->items().ltrim( 1 ) ) {
            rest.append( p );
        }
        Ast::Node* expr;
        RETURN_IF_FAILED( parse1( env, &rest, &expr ) );
        return Eval::evaluate( env, expr, out );
    }
};

template <typename... Args>
static Ast::Type* _makeFuncType( string_view name, Args&&... args ) {
    auto r = new Ast::Type( name );
    r->m_callable = {args...};
    return r;
}

struct Parse::ArrayView {
    struct Cache {
        string _generic_root;
        Cache( string&& root ) : _generic_root( root ) {}

        Result instantiate( Ast::Type* t, Ast::Type** out ) {
            *out = nullptr;
            auto name = string_format( "%s__%s__", _generic_root.c_str(), t->name().c_str() );
            if( auto it = types_.find( name ); it != types_.end() ) {
                *out = it->second;
                return Result::OK;
            }
            auto r = new Ast::Type( name );
            r->m_array = t;

            auto ftype = _makeFuncType( string_format( "(%s)->int", name.c_str() ), &Ast::s_typeInt, r );
            auto fdecl = new Ast::FunctionDecl( "size", WITH( _.m_type = ftype ) );
            fdecl->m_params.emplace_back( new Ast::Parameter( "self", WITH( _.m_type = r ) ) );
            r->m_methods.emplace_back( fdecl );

            auto attype = _makeFuncType( string_format( "(%s,int)->%s", name.c_str(), t->name().c_str() ), t, r, &Ast::s_typeInt );
            auto atdecl = new Ast::FunctionDecl( "at", WITH( _.m_type = attype ) );
            atdecl->m_params.emplace_back( new Ast::Parameter( "self", WITH( _.m_type = r ) ) );
            atdecl->m_params.emplace_back( new Ast::Parameter( "idx", WITH( _.m_type = &Ast::s_typeInt ) ) );
            r->m_methods.emplace_back( atdecl );

            auto puttype = _makeFuncType( string_format( "(%s,int,%s)->void", name.c_str(), t->name().c_str() ), &Ast::s_typeVoid, r,
                                          &Ast::s_typeInt, t );
            auto putdecl = new Ast::FunctionDecl( "put!", WITH( _.m_type = puttype ) );
            putdecl->m_params.emplace_back( new Ast::Parameter( "self", WITH( _.m_type = r ) ) );
            putdecl->m_params.emplace_back( new Ast::Parameter( "idx", WITH( _.m_type = &Ast::s_typeInt ) ) );
            putdecl->m_params.emplace_back( new Ast::Parameter( "val", WITH( _.m_type = t ) ) );
            r->m_methods.emplace_back( putdecl );

            if( _generic_root == "array_heap" ) {
                auto retype = _makeFuncType( string_format( "(%s,int)->void", name.c_str() ), &Ast::s_typeVoid, r, &Ast::s_typeInt );
                auto redecl = new Ast::FunctionDecl( "resize", WITH( _.m_type = retype ) );
                redecl->m_params.emplace_back( new Ast::Parameter( "self", WITH( _.m_type = r ) ) );
                redecl->m_params.emplace_back( new Ast::Parameter( "size", WITH( _.m_type = &Ast::s_typeInt ) ) );
                r->m_methods.emplace_back( redecl );
            }

            types_.emplace( name, r );
            *out = r;
            return Result::OK;
        }
        std::map<std::string, Ast::Type*> types_;
    };

    static Result parse( Ast::Environment* env, Lex::List* args, void* context, Ast::Node** out ) {
        *out = nullptr;
        Lex::Symbol* lparam;
        RETURN_IF_FAILED( matchLex( args, &lparam ) );
        Ast::Node* param = env->lookup( lparam->text() );  // TODO
        auto type = dynamic_cast<Ast::Type*>( param );
        assert( type );
        auto cache = static_cast<Cache*>( context );
        Ast::Type* r;
        RETURN_IF_FAILED( cache->instantiate( type, &r ) );
        *out = r;
        return Result::OK;
    }
};

static void addBuiltin( Ast::Environment* env, string_view name, Ast::Builtin::ParseFunc func, void* ctx = nullptr ) {
    env->bind( name, new Ast::Builtin( name, func, ctx ) );
}

static void addIntrinsic( Ast::Environment* env, string_view name, Ast::Type* type ) {
    auto n = istring::make( name );
    auto f = new Ast::FunctionDecl( n, WITH( _.m_type = type ) );
    char pname[2] = {'a', 0};
    for( auto&& at : array_view( type->m_callable ).ltrim( 1 ) ) {
        auto p = new Ast::Parameter( pname, WITH( _.m_type = at ) );
        f->m_params.emplace_back( p );
        pname[0] += 1;
    }
    env->bind( n, f );
}

Slip::Result Parse::module( Lex::List& lex, Slip::unique_ptr_del<Ast::Module>& mod ) {
    auto env = new Ast::Environment( nullptr );
    addBuiltin( env, "define"sv, &Define::parse );
    addBuiltin( env, "func"sv, &Func::parse );
    addBuiltin( env, "if"sv, &If::parse );
    addBuiltin( env, "while"sv, &While::parse );
    addBuiltin( env, "cond"sv, &Cond::parse );
    addBuiltin( env, "let"sv, &Let::parse );
    addBuiltin( env, "begin"sv, &Begin::parse );
    addBuiltin( env, "block"sv, &Block::parse );
    addBuiltin( env, "break"sv, &Break::parse );
    addBuiltin( env, "var"sv, &Var::parse );
    addBuiltin( env, "set!"sv, &Set::parse );
    addBuiltin( env, "scope"sv, &Scope::parse );
    addBuiltin( env, "macro"sv, &Macro::parse );
    addBuiltin( env, "#"sv, &Now::parse );
    addBuiltin( env, "array_view"sv, &ArrayView::parse, new Parse::ArrayView::Cache( "array_view" ) );
    addBuiltin( env, "array_const"sv, &ArrayView::parse, new Parse::ArrayView::Cache( "array_const" ) );
    addBuiltin( env, "array_heap"sv, &ArrayView::parse, new Parse::ArrayView::Cache( "array_heap" ) );

    env->bind( "int"sv, &Ast::s_typeInt );
    env->bind( "float"sv, &Ast::s_typeFloat );
    env->bind( "double"sv, &Ast::s_typeDouble );
    env->bind( "void"sv, &Ast::s_typeVoid );
    env->bind( "string"sv, &Ast::s_typeString );
    env->bind( "bool"sv, &Ast::s_typeBool );

    env->bind( "true"sv, new Ast::Number( "true", WITH( _.m_type = &Ast::s_typeBool ) ) );
    env->bind( "false"sv, new Ast::Number( "false", WITH( _.m_type = &Ast::s_typeBool ) ) );

    auto b_ii = _makeFuncType( "(int, int)->bool", &Ast::s_typeBool, &Ast::s_typeInt, &Ast::s_typeInt );
    auto i_ii = _makeFuncType( "(int, int)->int", &Ast::s_typeInt, &Ast::s_typeInt, &Ast::s_typeInt );
    auto v_s = _makeFuncType( "(string)->void", &Ast::s_typeVoid, &Ast::s_typeString );
    auto v_i = _makeFuncType( "(int)->void", &Ast::s_typeVoid, &Ast::s_typeInt );
    auto v_d = _makeFuncType( "(double)->void", &Ast::s_typeVoid, &Ast::s_typeDouble );
    auto d_dd = _makeFuncType( "(double, double)->double", &Ast::s_typeDouble, &Ast::s_typeDouble, &Ast::s_typeDouble );
    auto d_i = _makeFuncType( "(int)->double", &Ast::s_typeDouble, &Ast::s_typeInt );
    auto v_ss = _makeFuncType( "(string, string)->void", &Ast::s_typeVoid, &Ast::s_typeString, &Ast::s_typeString );
    auto i_s = _makeFuncType( "(string)->void", &Ast::s_typeInt, &Ast::s_typeString );
    // auto t_t = _makeFuncType( "(type)->type", &Ast::s_typeType, &Ast::s_typeType );
    // auto v_v = _makeFuncType( "(void)->void", &Ast::s_typeVoid, &Ast::s_typeVoid );

    addIntrinsic( env, "eq?", b_ii );
    addIntrinsic( env, "lt?", b_ii );
    addIntrinsic( env, "add", i_ii );
    addIntrinsic( env, "sub", i_ii );
    addIntrinsic( env, "puts", v_s );
    addIntrinsic( env, "puti", v_i );
    addIntrinsic( env, "putd", v_d );
    addIntrinsic( env, "addd", d_dd );
    addIntrinsic( env, "divd", d_dd );
    addIntrinsic( env, "dfromi", d_i );
    addIntrinsic( env, "atoi", i_s );
    addIntrinsic( env, "strcat!", v_ss );

    auto module = make_unique_del<Ast::Module>();
    for( auto c : lex.items() ) {
        Ast::Node* n;
        RETURN_IF_FAILED( parse1( env, c, &n ), "Failed to parse" );
        module->m_items.push_back( n );
    }
    mod = std::move( module );
    return Result::OK;
}
