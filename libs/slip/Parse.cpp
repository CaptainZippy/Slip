#include "slip/pch/Pch.h"

#include "Ast.h"
#include "Errors.h"
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
            m_start = t;
        }

        Iter( array_view<T> v ) {
            m_begin = v.begin();
            m_end = v.end();
            m_start = v.begin();
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

        Io::SourceLocation loc() const {
            if( m_begin < m_end ) {
                return m_begin[0]->m_loc;
            } else if( m_start != m_end ) {
                return m_end[-1]->m_loc;
            }
            return Io::SourceLocation();
        }

        T* begin() { return m_begin; }
        T* end() { return m_end; }

        T* m_begin{ nullptr };
        T* m_end{ nullptr };
        T* m_start{ nullptr };
    };
    typedef Iter<Ast::LexNode*> Args;

    struct ArrayView;
    struct ArrayConst;
    struct ResultT;
    Result ResultT_instantiate( Ast::Module* env, Ast::Type* t, Ast::Type** out );

    template <typename... Args>
    static Ast::Type* _makeFuncType( string_view name, Args&&... args ) {
        auto r = new Ast::Type( istring::make( name ), Ast::fixMeDeclContext );
        r->m_callable = { args... };
        return r;
    }

    static void addBuiltin( Ast::Environment* env, istring name, Ast::Expr* declContext, Ast::Builtin::ParseFunc&& fun ) {
        env->bind( name, new Ast::Builtin( name, declContext, std::move( fun ) ) );
    }

    static auto addIntrinsic( Ast::Environment* env, string_view name, Ast::Type* type ) -> Ast::FunctionDecl* {
        auto n = istring::make( name );
        auto f = new Ast::FunctionDecl( n, Ast::fixMeDeclContext,
                                        WITH( _.m_type = type, _.m_intrinsic = Ast::FunctionDecl::NotImplemented, _.environment_ = env ) );
        char pname[2] = { 'a', 0 };
        for( auto&& at : array_view( type->m_callable ).ltrim( 1 ) ) {
            auto p = new Ast::Parameter( istring::make( pname ), f, WITH( _.m_type = at ) );
            f->m_params.emplace_back( p );
            pname[0] += 1;
        }
        env->bind( n, f );
        return f;
    }

    enum class Flags { None, LValue, RValue };
}  // namespace Slip::Parse

using namespace Slip;
enum class Ellipsis { ZeroOrMore, OneOrMore };

static Result parse1( Ast::Environment* env, Ast::LexNode* atom, Parse::Flags flags, Ast::Expr** out );
static Result parse_Var( Ast::Environment* env, Ast::LexList* args, Ast::Expr** out );

static Result matchLex( Ast::Environment* env, Parse::Args& args ) {
    RETURN_ERROR_IF( !args.empty(), Error::TooManyArguments, args.cur()->m_loc );
    return Result::OK;
}

static Result matchLex( Ast::Environment* env, Parse::Args& args, std::vector<Ast::LexNode*>* list, Ellipsis ellipsis ) {
    RETURN_ERROR_IF( ellipsis == Ellipsis::OneOrMore && args.empty(), Error::TooFewArguments, args.loc() );
    if( args.empty() ) {
        return Result::OK;
    }
    do {
        list->emplace_back( args.cur() );
    } while( args.advance() );
    return Result::OK;
}

/// Match and consume the entire argument list exactly
template <typename HEAD, typename... REST>
static Result matchLex( Ast::Environment* env, Parse::Args& args, HEAD** head, REST... rest ) {
    RETURN_ERROR_IF( args.empty(), Error::TooFewArguments, args.loc() );
    auto h = dynamic_cast<HEAD*>( args.cur() );
    if( !h ) {
        if( auto now = dynamic_cast<Ast::LexNowExpr*>( args.cur() ) ) {
            Ast::Expr* n;
            RETURN_IF_FAILED( parse1( env, now, Parse::Flags::None, &n ) );
            h = dynamic_cast<HEAD*>( args.cur() );
        }
    }
    RETURN_ERROR_IF( !h, Error::LexUnexpected, args.cur()->m_loc, "Expected '%s', got '%s'", Reflect::getType<HEAD>()->name,
                     args.cur()->dynamicType()->name );
    args.advance();
    *head = h;
    return matchLex( env, args, rest... );
}

template <typename... REST>
static Result matchLex( Ast::Environment* env, Ast::LexList* list, REST... rest ) {
    Parse::Args args{ list->items() };
    RETURN_ERROR_IF( args.empty(), Error::TooFewArguments, list->m_loc );
    args.advance();
    return matchLex( env, args, rest... );
}

static Result macroExpand1( Ast::Environment* env, Ast::LexList* args, Ast::Expr** out ) {
    RETURN_ERROR_IF( args->size() < 2 || args->size() > 3, Error::WrongNumberOfArguments, args->m_loc );
    Ast::LexIdent* larg;
    Ast::LexIdent* lenv = nullptr;
    switch( args->size() ) {
        case 2:
            RETURN_IF_FAILED( matchLex( env, args, &larg ) );
            break;
        case 3:
            RETURN_IF_FAILED( matchLex( env, args, &larg, &lenv ) );
            break;
    }

    Ast::Expr* repl;
    RETURN_IF_FAILED( env->lookup( larg->text(), &repl ) );
    auto text = dynamic_cast<Ast::LexNode*>( repl );
    RETURN_ERROR_IF( text == nullptr, Error::MacroExpansionError, repl->m_loc, "Replacement expected" );

    Ast::Environment* xenv;
    if( lenv ) {
        Ast::Expr* val;
        RETURN_IF_FAILED( env->lookup( lenv->text(), &val ) );
        xenv = dynamic_cast<Ast::Environment*>( val );
        RETURN_ERROR_IF( xenv == nullptr, Error::MacroExpansionError, val->m_loc, "Environment expected" );
    } else {
        xenv = env;
    }

    // run macros in a sub environment
    xenv = new Ast::Environment( xenv );

    RETURN_IF_FAILED( parse1( xenv, text, Parse::Flags::None, out ) );
    return Result::OK;
}

static Result macroExpand( Ast::MacroDecl* macro, Ast::Environment* env, Ast::LexList* list, Ast::Expr** out ) {
    auto args = list->items().ltrim( 1 );
    RETURN_ERROR_IF( args.size() != macro->m_params.size(), Error::WrongNumberOfArguments, list->m_loc, "Got %i, expected %i", args.size(),
                     macro->m_params.size() );
    auto localEnv = new Ast::Environment( macro->m_staticEnv );
    for( unsigned i = 0; i < macro->m_params.size(); ++i ) {
        localEnv->bind( macro->m_params[i]->name(), args[i] );
    }
    localEnv->bind( macro->m_dynEnvSym, env );
    static Ast::Builtin expander{ "expand"_istr, Ast::fixMeDeclContext, &macroExpand1 };
    localEnv->bind( "expand"_istr, &expander );
    std::vector<Ast::Expr*> body;
    for( auto&& b : macro->m_body ) {
        Ast::Expr* p;
        RETURN_IF_FAILED( parse1( localEnv, b, Parse::Flags::None, &p ) );
        body.push_back( p );
    }
    if( body.size() == 1 ) {
        *out = body[0];
    } else {
        *out = new Ast::Sequence( std::move( body ) );
    }
    return Result::OK;
}

static Result parse_Stringize( Ast::Environment* env, Ast::LexList* args, Ast::Expr** out ) {
    *out = nullptr;
    RETURN_ERROR_IF( args->size() != 2, Error::Failed, args->m_loc );
    auto expr1 = dynamic_cast<Ast::LexIdent*>( args->at( 1 ) );
    Ast::Expr* node1;
    RETURN_IF_FAILED( env->lookup( expr1->text(), &node1 ) );
    auto lex1 = dynamic_cast<Ast::LexNode*>( node1 );

    std::vector<char> txt;
    Io::TextOutput txtout( &txt );
    Ast::print( lex1, txtout );
    *out = new Ast::String( { txt.data(), txt.size() }, WITH( _.m_loc = expr1->m_loc ) );
    return Result::OK;
}

static Result parse_Bind( Ast::Environment* env, Ast::LexList* args, Ast::Expr** out ) {
    Ast::LexIdent* lenv;
    Ast::LexIdent* lname;
    Ast::LexIdent* lval;
    RETURN_IF_FAILED( matchLex( env, args, &lenv, &lname, &lval ) );
    Ast::Expr* nenv;
    RETURN_IF_FAILED( env->lookup( lenv->text(), &nenv ) );
    auto env2 = dynamic_cast<Ast::Environment*>( nenv );

    Ast::Expr* nname;
    RETURN_IF_FAILED( env->lookup( lname->text(), &nname ) );
    Ast::Expr* nval;
    RETURN_IF_FAILED( env->lookup( lval->text(), &nval ) );
    RETURN_IF_FAILED( env2->bind( dynamic_cast<Ast::LexIdent*>( nname )->text(), nval ) );

    *out = new Ast::Nop();
    return Result::OK;
}

static Result parse1( Ast::Environment* env, Ast::LexNode* atom, Parse::Flags flags, Ast::Expr** out ) {
    *out = nullptr;
    if( atom == nullptr ) {
        return Result::OK;
    } else if( auto sym = dynamic_cast<Ast::LexIdent*>( atom ) ) {
        Ast::Expr* node;
        RETURN_ERROR_IF_FAILED( env->lookup( sym->text(), &node ), Error::SymbolNotFound, atom->m_loc, "Symbol '%.*s' not found",
                                STRING_VIEW_VARG( sym->text() ) );
        switch( flags ) {
            case Parse::Flags::None:
                *out = node;
                break;
            case Parse::Flags::LValue:
            case Parse::Flags::RValue:
                *out = new Ast::Reference( node );
                break;
        }
        return Result::OK;
    } else if( auto list = dynamic_cast<Ast::LexList*>( atom ) ) {
        RETURN_ERROR_IF( list->size() == 0, Error::UnexpectedEmptyList, atom->m_loc );
        std::vector<Ast::Expr*> candidates;
        istring isym;

        auto items = list->items();
        auto first = items[0];
        if( auto dot = dynamic_cast<Ast::LexDot*>( first ) ) {
            std::string name;
            Ast::LexIdent* ident;
            RETURN_IF_FAILED( dynCast( dot->m_lhs, &ident ) );
            Ast::Expr* out;
            name += ident->text();
            RETURN_IF_FAILED( env->lookup( ident->text(), &out ) );
            Ast::Dictlike* dict;
            RETURN_IF_FAILED( dynCast( out, &dict ) );
            Ast::LexIdent* identr;
            RETURN_IF_FAILED( dynCast( dot->m_rhs, &identr ) );
            Ast::Expr* e;
            RETURN_IF_FAILED( dict->lookup( identr->text(), &e ) );
            name += "_";
            name += identr->text();
            candidates.emplace_back( e );
            isym = istring::make( name );
        } else {
            auto sym = dynamic_cast<Ast::LexIdent*>( first );
            RETURN_ERROR_IF( !sym, Error::SymbolExpectedAtListHead, list->m_loc );

            // sym can resolve to a builtin or macro or function. Only functions can be overloaded.
            env->lookupAll( istring::make( sym->text() ), candidates );
            for( Ast::Expr* p : candidates ) {
                if( auto b = dynamic_cast<Ast::Builtin*>( p ) ) {
                    RETURN_ERROR_IF( candidates.size() != 1, Error::CannotOverload, sym->m_loc, "Builtins can't be overloaded" );
                    return b->parse( env, list, out );
                } else if( auto m = dynamic_cast<Ast::MacroDecl*>( p ) ) {
                    RETURN_ERROR_IF( candidates.size() != 1, Error::CannotOverload, sym->m_loc, "Builtins can't be overloaded" );
                    return macroExpand( m, env, list, out );
                } else {
                    // candidates.emplace_back( p );
                }
            }
        }

        // eval args
        std::vector<Ast::Expr*> fa;
        for( auto a : list->items().ltrim( 1 ) ) {
            Ast::Expr* n;
            RETURN_IF_FAILED( parse1( env, a, Parse::Flags::RValue, &n ) );
            fa.push_back( n );
        }
        *out = new Ast::NamedFunctionCall( isym, Ast::fixMeDeclContext, std::move( candidates ), std::move( fa ),
                                           WITH( _.m_loc = list->m_loc ) );
        return Result::OK;
    } else if( auto num = dynamic_cast<Ast::LexNumber*>( atom ) ) {
        Ast::Expr* te;
        RETURN_IF_FAILED( parse1( env, num->m_declTypeLex, Parse::Flags::None, &te ) );
        auto r = new Ast::Number( num->istr(), WITH( _.m_loc = num->m_loc, _.m_declTypeExpr = te ) );
        *out = r;
        return Result::OK;
    } else if( auto str = dynamic_cast<Ast::LexString*>( atom ) ) {
        Ast::Expr* te;
        RETURN_IF_FAILED( parse1( env, str->m_declTypeLex, Parse::Flags::None, &te ) );
        auto r = new Ast::String( str->text(), WITH( _.m_loc = str->m_loc, _.m_declTypeExpr = te ) );
        *out = r;
        return Result::OK;
    } else if( auto dot = dynamic_cast<Ast::LexDot*>( atom ) ) {
        Ast::Expr* lhs;
        RETURN_IF_FAILED( parse1( env, dot->m_lhs, Parse::Flags::None, &lhs ) );
        auto rhs = dynamic_cast<Ast::LexIdent*>( dot->m_rhs );
        RETURN_ERROR_IF( !rhs, Error::SymbolExpectedAfterDot, dot->m_rhs->m_loc );  // todo:now
        auto r = new Ast::Selector( lhs, rhs, WITH( _.m_loc = dot->m_loc ) );
        *out = r;
        return Result::OK;
    } else if( auto now = dynamic_cast<Ast::LexNowExpr*>( atom ) ) {
        auto inner = new Ast::Environment( env );
        Parse::addBuiltin( inner, "stringize"_istr, Ast::fixMeDeclContext, &parse_Stringize );
        Parse::addBuiltin( inner, "bind"_istr, Ast::fixMeDeclContext, &parse_Bind );
        Ast::Expr* parsed;
        RETURN_IF_FAILED( parse1( inner, now->m_expr, Parse::Flags::None, &parsed ) );
        Ast::Expr* replacement = nullptr;
        RETURN_IF_FAILED( Eval::evaluate( env, parsed, &replacement ) );
        *out = replacement;
        return Result::OK;
    }
    RETURN_ERROR( Error::NotImplemented, atom->m_loc );
}

Result Slip::Parse::parse( Ast::Environment* env, Ast::LexNode* atom, Ast::Expr** out ) {
    return parse1( env, atom, Parse::Flags::RValue, out );
}

static Result parse_Coroutine( Ast::Environment* env, Ast::LexList* args, Ast::Expr** out ) {
    *out = nullptr;
    RETURN_ERROR_IF( args->size() < 3, Error::TooFewArguments, args->m_loc );
    Ast::LexIdent* lname;
    Ast::LexList* largs;
    std::vector<Ast::LexNode*> lbody;
    RETURN_IF_FAILED( matchLex( env, args, &lname, &largs, &lbody, Ellipsis::OneOrMore ) );
    auto coro = new Ast::CoroutineDecl( istring::make( lname->text() ), Ast::fixMeDeclContext, WITH( _.m_loc = lname->m_loc ) );
    env->bind( coro->m_name, coro );
    auto inner = new Ast::Environment( env );
    Parse::addBuiltin( inner, "yield"_istr, coro, [coro]( auto env, auto args, auto out ) -> Result {
        RETURN_ERROR_IF( args->size() != 2, Error::WrongNumberOfArguments, args->m_loc, "Expected 2, got %i", args->size() );
        auto ret = new Ast::CoroutineYield();
        RETURN_IF_FAILED( parse1( env, args->at( 1 ), Parse::Flags::RValue, &ret->m_expr ) );
        ret->m_coro = coro;
        *out = ret;
        return Result::OK;
    } );

    if( auto a = largs->m_declTypeLex ) {
        Ast::Expr* te;
        RETURN_IF_FAILED( parse1( inner, a, Parse::Flags::None, &te ) );
        coro->m_declReturnTypeExpr = te;
    }
    for( auto item : largs->items() ) {
        auto sym = dynamic_cast<Ast::LexIdent*>( item );
        RETURN_ERROR_IF( sym == nullptr, Error::ParameterIdentifierExpected, item->m_loc );
        Ast::Expr* te;
        RETURN_IF_FAILED( parse1( inner, item->m_declTypeLex, Parse::Flags::None, &te ) );
        auto param = new Ast::Parameter( sym->istr(), coro, WITH( _.m_loc = sym->m_loc, _.m_declTypeExpr = te ) );
        coro->m_params.push_back( param );
    }
    // TODO check unique names
    for( auto p : coro->m_params ) {
        inner->bind( p->m_name, p );
    }
    Ast::Expr* body;
    if( lbody.size() == 1 ) {
        RETURN_IF_FAILED( parse1( inner, lbody[0], Parse::Flags::None, &body ) );
    } else {
        auto bd = new Ast::Sequence();
        for( auto&& l : lbody ) {
            Ast::Expr* b;
            RETURN_IF_FAILED( parse1( inner, l, Parse::Flags::None, &b ) );
            bd->m_items.emplace_back( b );
        }
        body = bd;
    }
    coro->m_body = body;
    *out = coro;
    return Result::OK;
}

static Result parse_Define( Ast::Environment* env, Ast::LexList* args, Ast::Expr** out ) {
    Ast::LexIdent* lname;
    Ast::LexNode* lval;
    RETURN_IF_FAILED( matchLex( env, args, &lname, &lval ) );
    Ast::Expr* nval;
    RETURN_IF_FAILED( parse1( env, lval, Parse::Flags::RValue, &nval ) );

    auto ret = new Ast::Definition( lname->istr(), Ast::fixMeDeclContext, nval, WITH( _.m_loc = lname->m_loc ) );
    env->bind( lname->text(), ret );

    *out = ret;
    return Result::OK;
}

static Result parse_Func( Ast::Environment* env, Ast::LexList* args, Ast::Expr** out ) {
    *out = nullptr;
    RETURN_ERROR_IF( args->size() < 3, Error::TooFewArguments, args->m_loc, "Expected 3, got %i", args->size() );
    Ast::LexIdent* lname;
    Ast::LexList* largs;
    std::vector<Ast::LexNode*> lbody;
    RETURN_IF_FAILED( matchLex( env, args, &lname, &largs, &lbody, Ellipsis::OneOrMore ) );
    auto func = new Ast::FunctionDecl( lname->istr(), Ast::fixMeDeclContext, WITH( _.m_loc = lname->m_loc, _.environment_ = env ) );
    env->bind( func->m_name, func );
    auto inner = new Ast::Environment( env );
    if( auto a = largs->m_declTypeLex ) {
        Ast::Expr* te;
        RETURN_IF_FAILED( parse1( inner, a, Parse::Flags::None, &te ) );
        func->m_declReturnTypeExpr = te;
    }
    for( auto item : largs->items() ) {
        auto sym = dynamic_cast<Ast::LexIdent*>( item );
        RETURN_ERROR_IF( sym == nullptr, Error::ParameterIdentifierExpected, item->m_loc );
        Ast::Expr* te;
        RETURN_IF_FAILED( parse1( inner, item->m_declTypeLex, Parse::Flags::None, &te ) );
        auto param = new Ast::Parameter( sym->istr(), func, WITH( _.m_loc = sym->m_loc, _.m_declTypeExpr = te ) );
        auto it = rng::find_if( item->m_declTypeLex->m_attrs, []( Ast::LexNode* n ) {
            if( auto i = dynamic_cast<Ast::LexIdent*>( n ) ) {
                return i->text() == "ref";
            }
            return false;
        } );
        if( !it.empty() ) {
            param->m_ref = true;
        }
        func->m_params.push_back( param );
    }
    // TODO check unique names
    for( auto p : func->m_params ) {
        inner->bind( p->m_name, p );
    }
    Ast::Expr* body;
    if( lbody.size() == 1 ) {
        RETURN_IF_FAILED( parse1( inner, lbody[0], Parse::Flags::RValue, &body ) );
    } else {
        auto bd = new Ast::Sequence();
        for( auto&& l : lbody ) {
            Ast::Expr* b;
            RETURN_IF_FAILED( parse1( inner, l, Parse::Flags::RValue, &b ) );
            bd->m_items.emplace_back( b );
        }
        body = bd;
    }
    func->m_body = body;
    *out = func;
    return Result::OK;
}

static Result parse_Let( Ast::Environment* env, Ast::LexList* args, Ast::Expr** out ) {
    Ast::Expr* n;
    Ast::VariableDecl* v;
    RETURN_IF_FAILED( parse_Var( env, args, &n ) );
    RETURN_IF_FAILED( dynCast( n, &v ) );
    v->m_kind = Ast::VariableDecl::Kind::Immutable;
    *out = v;
    return Result::OK;
#if 0
    Ast::LexIdent* lname;
    Ast::LexNode* lbody;
    if( matchLex( env, args, &lname, &lbody ).isOk() ) {  // short form (let foo 0)

        Ast::Expr* aval;
        RETURN_IF_FAILED( parse1( env, lbody, &aval ) );
        Ast::Expr* te;
        RETURN_IF_FAILED( parse1( env, lname->m_decltype, &te ) );
        auto def = new Ast::Definition( lname->text(), aval, WITH( _.m_loc = lbody->m_loc, _.m_declTypeExpr = te ) );
        RETURN_IF_FAILED( env->bind( lname->text(), def ) );
        *out = def;
        return Result::OK;
    }

    Ast::LexList* llets;  // long form (let ((a 0) (b 1)) (+ a b))
    RETURN_IF_FAILED( matchLex( env, args, &llets, &lbody ) );

    auto inner = new Ast::Environment( env );
    auto seq = new Ast::Sequence();
    for( auto litem : llets->items() ) {
        Ast::LexList* lpair = dynamic_cast<Ast::LexList*>( litem );
        RETURN_ERROR_IF( !lpair );
        Ast::LexIdent* lsym;
        Ast::LexNode* lval;
        Parse::Args tmpargs{lpair->items()};
        RETURN_IF_FAILED( matchLex( env, tmpargs, &lsym, &lval ) );
        Ast::Expr* aval;
        RETURN_IF_FAILED( parse1( inner, lval, &aval ) );
        Ast::Expr* te;
        RETURN_IF_FAILED( parse1( inner, lsym->m_decltype, &te ) );

        auto def = new Ast::Definition( lsym->text(), aval, WITH( _.m_loc = lsym->m_loc, _.m_declTypeExpr = te ) );
        inner->bind( lsym->text(), def );
        seq->m_items.push_back( def );
    }
    Ast::Expr* abody;
    RETURN_IF_FAILED( parse1( inner, lbody, &abody ) );
    seq->m_items.push_back( abody );

    *out = seq;
    return Result::OK;
#endif
}

static Result parse_If( Ast::Environment* env, Ast::LexList* args, Ast::Expr** out ) {
    Ast::LexNode* lcond;
    Ast::LexNode* ltrue;
    Ast::LexNode* lfalse;
    RETURN_IF_FAILED( matchLex( env, args, &lcond, &ltrue, &lfalse ) );

    Ast::Expr* ncond;
    Ast::Expr* ntrue;
    Ast::Expr* nfalse;
    RETURN_IF_FAILED( parse1( env, lcond, Parse::Flags::RValue, &ncond ) );
    RETURN_IF_FAILED( parse1( env, ltrue, Parse::Flags::RValue, &ntrue ) );
    RETURN_IF_FAILED( parse1( env, lfalse, Parse::Flags::RValue, &nfalse ) );

    *out = new Ast::If( ncond, ntrue, nfalse, WITH( _.m_loc = lcond->m_loc ) );
    return Result::OK;
}

static Result parse_While( Ast::Environment* env, Ast::LexList* args, Ast::Expr** out ) {
    Ast::LexNode* lcond;
    std::vector<Ast::LexNode*> lbody;
    RETURN_IF_FAILED( matchLex( env, args, &lcond, &lbody, Ellipsis::OneOrMore ) );
    Ast::Expr* ncond;
    RETURN_IF_FAILED( parse1( env, lcond, Parse::Flags::RValue, &ncond ) );
    Ast::Expr* nbody;
    if( lbody.size() == 1 ) {
        RETURN_IF_FAILED( parse1( env, lbody[0], Parse::Flags::RValue, &nbody ) );
    } else {
        auto bd = new Ast::Sequence();
        for( auto&& l : lbody ) {
            Ast::Expr* b;
            RETURN_IF_FAILED( parse1( env, l, Parse::Flags::RValue, &b ) );
            if( b )
                bd->m_items.emplace_back( b );
        }
        nbody = bd;
    }
    *out = new Ast::While( ncond, nbody, WITH( _.m_loc = lcond->m_loc ) );
    return Result::OK;
}

static Result parse_Cond( Ast::Environment* env, Ast::LexList* args, Ast::Expr** out ) {
    *out = nullptr;
    RETURN_ERROR_IF( args->size() < 2, Error::TooFewArguments, args->m_loc, "Expected 2, got %i", args->size() );
    std::vector<std::pair<Ast::Expr*, Ast::Expr*>> cases;
    for( auto&& arg : args->items().ltrim( 1 ) ) {
        auto pair = dynamic_cast<Ast::LexList*>( arg );
        RETURN_ERROR_IF( pair == nullptr, Error::ListExpected, arg->m_loc );
        RETURN_ERROR_IF( pair->size() != 2, Error::WrongNumberOfArguments, arg->m_loc, "Expected 2, got %i", pair->size() );

        Ast::Expr* cond;
        RETURN_IF_FAILED( parse1( env, pair->at( 0 ), Parse::Flags::RValue, &cond ) );
        Ast::Expr* iftrue;
        RETURN_IF_FAILED( parse1( env, pair->at( 1 ), Parse::Flags::RValue, &iftrue ) );
        cases.emplace_back( cond, iftrue );
    }
    auto cond = new Ast::Cond( WITH( /*TODO loc*/ ) );
    cond->m_cases.swap( cases );
    *out = cond;
    return Result::OK;
}

static Result parse_Begin( Ast::Environment* env, Ast::LexList* args, Ast::Expr** out ) {
    *out = nullptr;
    auto ret = new Ast::Sequence;
    for( auto arg : args->items().ltrim( 1 ) ) {
        Ast::Expr* n;
        RETURN_IF_FAILED( parse1( env, arg, Parse::Flags::RValue, &n ) );
        ret->m_items.push_back( n );
    }
    *out = ret;
    return Result::OK;
}

static Result parse_Scope( Ast::Environment* env, Ast::LexList* args, Ast::Expr** out ) {
    *out = nullptr;
    auto ret = new Ast::Sequence;
    auto inner = new Ast::Environment( env );
    for( auto arg : args->items().ltrim( 1 ) ) {
        Ast::Expr* n;
        RETURN_IF_FAILED( parse1( inner, arg, Parse::Flags::RValue, &n ) );
        ret->m_items.push_back( n );
    }
    *out = new Ast::Scope( ret );
    return Result::OK;
}

static Result parse_Block( Ast::Environment* env, Ast::LexList* args, Ast::Expr** out ) {
    *out = nullptr;
    Ast::LexIdent* name;
    std::vector<Ast::LexNode*> contents;
    RETURN_IF_FAILED( matchLex( env, args, &name, &contents, Ellipsis::ZeroOrMore ) );
    auto seq = new Ast::Sequence;
    auto ret = new Ast::Block( name->istr(), Ast::fixMeDeclContext, seq );
    auto inner = new Ast::Environment( env );
    inner->bind( name->text(), ret );
    for( auto c : contents ) {
        Ast::Expr* n;
        RETURN_IF_FAILED( parse1( inner, c, Parse::Flags::RValue, &n ) );
        seq->m_items.push_back( n );
    }
    *out = ret;
    return Result::OK;
}

static Result parse_Break( Ast::Environment* env, Ast::LexList* args, Ast::Expr** out ) {
    *out = nullptr;
    Ast::LexIdent* llabel;
    Ast::LexNode* lval;
    RETURN_IF_FAILED( matchLex( env, args, &llabel, &lval ) );
    Ast::Expr* nlabel;
    RETURN_IF_FAILED( parse1( env, llabel, Parse::Flags::RValue, &nlabel ) );
    auto blockr = dynamic_cast<Ast::Reference*>( nlabel );  // TODO tidy
    auto blocka = dynamic_cast<Ast::Block*>( blockr->m_target );
    RETURN_ERROR_IF( blocka == nullptr, Error::BreakTargetMustBeBlock, llabel->m_loc );
    Ast::Expr* nval;
    RETURN_IF_FAILED( parse1( env, lval, Parse::Flags::RValue, &nval ) );
    *out = new Ast::Break( blocka, nval );
    return Result::OK;
}

static Result parse_Var( Ast::Environment* env, Ast::LexList* args, Ast::Expr** out ) {
    *out = nullptr;

    Ast::LexIdent* sym;
    std::vector<Ast::LexNode*> inits;
    // 'var' sym inits* e.g. (var foo 1 2)
    RETURN_IF_FAILED( matchLex( env, args, &sym, &inits, Ellipsis::ZeroOrMore ) );

    Ast::Expr* varType;
    RETURN_IF_FAILED( parse1( env, sym->m_declTypeLex, Parse::Flags::None, &varType ) );

    std::vector<Ast::Expr*> vals;
    for( auto&& i : inits ) {
        Ast::Expr* n;
        RETURN_IF_FAILED( parse1( env, i, Parse::Flags::RValue, &n ) );
        vals.emplace_back( n );
    }

    auto ret = new Ast::VariableDecl( sym->istr(), Ast::fixMeDeclContext, WITH( _.m_declTypeExpr = varType, _.m_loc = sym->m_loc ) );
    ret->m_kind = Ast::VariableDecl::Kind::Mutable;
    ret->m_initializer.swap( vals );
    RETURN_IF_FAILED( env->bind( sym->text(), ret ) );
    *out = ret;
    return Result::OK;
}

static Result parse_Const( Ast::Environment* env, Ast::LexList* args, Ast::Expr** out ) {
    Ast::Expr* n;
    Ast::VariableDecl* v;
    RETURN_IF_FAILED( parse_Var( env, args, &n ) );
    RETURN_IF_FAILED( dynCast( n, &v ) );
    v->m_kind = Ast::VariableDecl::Kind::Constant;
    *out = v;
    return Result::OK;
}

static Result parse_Set( Ast::Environment* env, Ast::LexList* args, Ast::Expr** out ) {
    *out = nullptr;
    Ast::LexNode* lex_dst;
    Ast::LexNode* lex_src;
    RETURN_IF_FAILED( matchLex( env, args, &lex_dst, &lex_src ) );
    Ast::Expr* dst;
    RETURN_IF_FAILED( parse1( env, lex_dst, Parse::Flags::LValue, &dst ) );
    Ast::Expr* src;
    RETURN_IF_FAILED( parse1( env, lex_src, Parse::Flags::RValue, &src ) );
    *out = new Ast::Assignment( dst, src, WITH( _.m_loc = lex_src->m_loc ) );
    return Result::OK;
}

static Result parse_Try( Ast::Environment* env, Ast::LexList* args, Ast::Expr** out ) {
    *out = nullptr;
    Ast::LexNode* lexpr{};
    RETURN_ERROR_IF_FAILED( matchLex( env, args, &lexpr ), Error::WrongNumberOfArguments, args->m_loc, "try expects 1 argument" );
    Ast::Expr* expr;
    RETURN_IF_FAILED( parse1( env, lexpr, Parse::Flags::RValue, &expr ) );
    *out = new Ast::TryExpr( expr, WITH( _.m_loc = lexpr->m_loc ) );
    return Result::OK;
}

static Result parse_Catch( Ast::Environment* env, Ast::LexList* args, Ast::Expr** out ) {
    *out = nullptr;
    Ast::LexNode* lexpr{};
    Ast::LexNode* lfail{};
    RETURN_ERROR_IF_FAILED( matchLex( env, args, &lexpr, &lfail ), Error::WrongNumberOfArguments, args->m_loc,
                            "catch expects 2 arguments" );

    Ast::Expr* expr;
    RETURN_IF_FAILED( parse1( env, lexpr, Parse::Flags::RValue, &expr ) );
    Ast::Expr* fail{};
    RETURN_IF_FAILED( parse1( env, lfail, Parse::Flags::RValue, &fail ) );

    *out = new Ast::CatchExpr( expr, fail, WITH( _.m_loc = lexpr->m_loc ) );
    return Result::OK;
}

static Result parse_Macro( Ast::Environment* env, Ast::LexList* args, Ast::Expr** out ) {
    *out = nullptr;
    Ast::LexIdent* lname;
    Ast::LexList* largs;
    Ast::LexIdent* lenv;
    std::vector<Ast::LexNode*> lbody;
    RETURN_IF_FAILED( matchLex( env, args, &lname, &largs, &lenv, &lbody, Ellipsis::OneOrMore ) );
    auto macro = new Ast::MacroDecl( lname->istr(), Ast::fixMeDeclContext, lenv->text(), env, WITH( _.m_loc = lname->m_loc ) );
    for( auto item : largs->items() ) {
        auto sym = dynamic_cast<Ast::LexIdent*>( item );
        RETURN_ERROR_IF( sym == nullptr, Error::ParameterIdentifierExpected, item->m_loc );
        auto param = new Ast::Parameter( sym->istr(), macro, WITH( _.m_loc = sym->m_loc ) );
        macro->m_params.push_back( param );
    }
    RETURN_IF_FAILED( env->bind( macro->m_name, macro ) );
    macro->m_body = std::move( lbody );
    *out = macro;
    return Result::OK;
}

static Result parse_Pipe( Ast::Environment* env, Ast::LexList* args, Ast::Expr** out ) {
    *out = nullptr;
    std::vector<Ast::LexNode*> lparts;
    RETURN_IF_FAILED( matchLex( env, args, &lparts, Ellipsis::OneOrMore ) );
    auto pipe = new Ast::PipelineExpr( WITH( _.m_loc = args->m_loc ) );
    for( auto&& p : lparts ) {
        Ast::Expr* n;
        RETURN_IF_FAILED( parse1( env, p, Parse::Flags::RValue, &n ) );
        pipe->addStage( n );
    }
    *out = pipe;
    return Result::OK;
}

static Result parse_Fmt( Ast::Environment* env, Ast::LexList* args, Ast::Expr** out ) {
    *out = nullptr;
    Ast::LexString* fmtLex;
    RETURN_IF_FAILED( matchLex( env, args, &fmtLex ) );
    auto fmt = fmtLex->text();
    size_t cur = 0;
    std::vector<Ast::Expr*> parts;
    while( cur != fmt.size() ) {
        auto sp = fmt.find( '{', cur );
        if( sp != std::string::npos ) {
            auto ep = fmt.find( '}', sp );
            RETURN_ERROR_IF( ep == std::string::npos, Error::LexPrematureEndOfFile, fmtLex->m_loc, "Missing end '}'" );
            if( cur != sp ) {
                auto loc = fmtLex->m_loc;
                loc.m_end = loc.m_start + sp;
                loc.m_start += cur;
                parts.emplace_back( new Ast::String( fmt.substr( cur, sp - cur ), WITH( _.m_loc = loc ) ) );
            }
            Io::TextInput input( fmtLex->m_loc.m_file->m_contents.data(), fmt.data() + sp + 1, fmt.data() + ep, fmtLex->m_loc.m_file );
            Ast::LexNode* node;
            RETURN_IF_FAILED( Ast::lex_atom( input, &node ) );
            Ast::Expr* expr;
            RETURN_IF_FAILED( parse1( env, node, Parse::Flags::RValue, &expr ) );
            parts.emplace_back( expr );
            cur = ep + 1;
        } else {
            auto loc = fmtLex->m_loc;
            loc.m_start += cur;
            parts.emplace_back( new Ast::String( fmt.substr( cur ), WITH( _.m_loc = loc ) ) );
            break;
        }
    }

    std::vector<Ast::Expr*> tostringExprs;
    auto itostring = "tostring"_istr;
    env->lookupAll( itostring, tostringExprs );
    // TODO: don't tostring & stringjoin everything inline, pass literal string fragments
    // and reflected pointers to the expression values
    std::vector<Ast::Expr*> sparts;
    for( auto part : parts ) {
        if( dynamic_cast<Ast::String*>( part ) ) {
            sparts.push_back( part );
        } else {
            sparts.push_back( new Ast::NamedFunctionCall( itostring, Ast::fixMeDeclContext, tostringExprs, std::vector<Ast::Expr*>{ part },
                                                          WITH( _.m_loc = args->m_loc ) ) );
        }
    }

    std::vector<Ast::Expr*> strjoinExprs;
    auto istrjoin = istring::make( "strjoin"_sv );
    env->lookupAll( istrjoin, strjoinExprs );
    *out = new Ast::NamedFunctionCall( istrjoin, Ast::fixMeDeclContext, std::move( strjoinExprs ), std::move( sparts ),
                                       WITH( _.m_loc = args->m_loc ) );

    //*out = new Ast::String( fmt, WITH( _.m_loc = args->m_loc ) ); // debug: return the input string
    return Result::OK;
}

static Result parse_List( Ast::Environment* env, Ast::LexList* args, Ast::Expr** out ) {
    *out = nullptr;
    std::vector<Ast::LexNode*> lparts;

    RETURN_IF_FAILED( matchLex( env, args, &lparts, Ellipsis::OneOrMore ) );
    auto r = new Ast::DataList( WITH(_.m_loc = args->m_loc ) );
    r->m_loc = args->m_loc;
    for( auto&& p : args->items().ltrim( 1 ) ) {
        Ast::Expr* n;
        RETURN_IF_FAILED( parse1( env, p, Parse::Flags::RValue, &n ) );
        r->m_items.push_back( n );
    }
    *out = r;
    return Result::OK;
}

static Result parse_Struct( Ast::Environment* env, Ast::LexList* args, Ast::Expr** out ) {
    *out = nullptr;
    Ast::LexIdent* lname;
    std::vector<Ast::LexNode*> lfields;
    RETURN_IF_FAILED( matchLex( env, args, &lname, &lfields, Ellipsis::ZeroOrMore ) );
    auto decl =
        new Ast::StructDecl( lname->istr(), Ast::fixMeDeclContext, WITH( _.m_loc = lname->m_loc ) );  // TODO separate decl & instantiation
    auto type = new Ast::Type( istring::make( string_concat( lname->text(), env->nameSuffix() ) ), Ast::fixMeDeclContext );
    type->m_struct = decl;
    RETURN_IF_FAILED( env->bind( decl->m_name, type ) );
    for( auto&& lf : lfields ) {
        auto li = dynamic_cast<Ast::LexIdent*>( lf );
        RETURN_ERROR_IF( li == nullptr, Error::FieldIdentifierExpected, lf->m_loc );
        RETURN_ERROR_IF( li->m_declTypeLex == nullptr, Error::FieldTypeExpected, lf->m_loc );
        Ast::Expr* ft;
        RETURN_IF_FAILED( parse1( env, li->m_declTypeLex, Parse::Flags::None, &ft ) );
        decl->m_fields.emplace_back( new Ast::StructField( li->istr(), decl, WITH( _.m_loc = li->m_loc; _.m_declTypeExpr = ft; ) ) );
    }
    *out = decl;
    return Result::OK;
}

struct Parse::ArrayView {
    enum class Kind { View, Fixed, Heap };
    static Result parse_ArrayView( Ast::Environment* env, Ast::LexList* args, Ast::Expr** out ) {
        static Ast::GenericDecl* decl = makeGeneric( Kind::View );
        return parse_internal( env, args, out, decl, Kind::View );
    }
    static Result parse_ArrayFixed( Ast::Environment* env, Ast::LexList* args, Ast::Expr** out ) {
        static Ast::GenericDecl* decl = makeGeneric( Kind::Fixed );
        return parse_internal( env, args, out, decl, Kind::Fixed );
    }
    static Result parse_ArrayHeap( Ast::Environment* env, Ast::LexList* args, Ast::Expr** out ) {
        static Ast::GenericDecl* decl = makeGeneric( Kind::Heap );
        return parse_internal( env, args, out, decl, Kind::Heap );
    }

   protected:
    // TODO add declcontext to decls
    static inline istring s_nameFromKind[] = { "builtin_array_view"_istr, "builtin_array_fixed"_istr, "builtin_array_heap"_istr };

    static Ast::GenericDecl* makeGeneric( Kind kind ) {
        auto g = new Ast::GenericDecl( s_nameFromKind[(int)kind], Ast::fixMeDeclContext, WITH() );
        g->params_.push_back( new Ast::Parameter( "T"_istr, g, WITH( _.m_type = &Ast::s_typeType ) ) );
        if( kind == Kind::Fixed ) {
            g->params_.push_back( new Ast::Parameter( "N"_istr, g, WITH( _.m_type = &Ast::s_typeInt ) ) );
        }
        return g;
    }

    static Result instantiate_internal( istring name, Ast::Environment* env, Kind kind /*fixme*/, Ast::Type* t, Ast::LexNumber* count,
                                        Ast::GenericDecl* genericDecl, Ast::Type** out ) {
        *out = nullptr;
        auto r = new Ast::Type( name, Ast::fixMeDeclContext );
        r->m_array = t;
        r->generic_ = new Ast::GenericInstantiation( WITH( _.decl_ = genericDecl ) );
        r->generic_->args_.push_back( t );
        if( count ) {
            r->generic_->args_.push_back( count );
        }
        // count?

        {
            auto ftype = _makeFuncType( string_format( "(%s)->int", name.c_str() ), &Ast::s_typeInt, r );
            auto fdecl =
                new Ast::FunctionDecl( "size"_istr, r, WITH( _.m_type = ftype, _.m_intrinsic = Ast::FunctionDecl::NotImplemented ) );
            fdecl->m_params.emplace_back( new Ast::Parameter( "self"_istr, fdecl, WITH( _.m_type = r ) ) );
            r->m_methods.emplace_back( fdecl );
        }

        {
            auto attype = _makeFuncType( string_format( "(%s,int)->%s", name.c_str(), t->name().c_str() ), t, r, &Ast::s_typeInt );
            auto atdecl =
                new Ast::FunctionDecl( "at"_istr, r, WITH( _.m_type = attype, _.m_intrinsic = Ast::FunctionDecl::NotImplemented ) );
            atdecl->m_params.emplace_back( new Ast::Parameter( "self"_istr, atdecl, WITH( _.m_type = r ) ) );
            atdecl->m_params.emplace_back( new Ast::Parameter( "idx"_istr, atdecl, WITH( _.m_type = &Ast::s_typeInt ) ) );
            r->m_methods.emplace_back( atdecl );
        }

        {
            Ast::Type* resultT;
            RETURN_IF_FAILED( ResultT_instantiate( env->module(), t, &resultT ) );
            auto gettype =
                _makeFuncType( string_format( "(%s,int)->{result %s}", name.c_str(), t->name().c_str() ), resultT, r, &Ast::s_typeInt );
            auto getdecl =
                new Ast::FunctionDecl( "get"_istr, r, WITH( _.m_type = gettype, _.m_intrinsic = Ast::FunctionDecl::NotImplemented ) );
            getdecl->m_params.emplace_back( new Ast::Parameter( "self"_istr, getdecl, WITH( _.m_type = r ) ) );
            getdecl->m_params.emplace_back( new Ast::Parameter( "idx"_istr, getdecl, WITH( _.m_type = &Ast::s_typeInt ) ) );
            r->m_methods.emplace_back( getdecl );
        }

        {
            auto puttype = _makeFuncType( string_format( "(%s,int,%s)->void", name.c_str(), t->name().c_str() ), &Ast::s_typeVoid, r,
                                          &Ast::s_typeInt, t );
            auto putdecl =
                new Ast::FunctionDecl( "put!"_istr, r, WITH( _.m_type = puttype, _.m_intrinsic = Ast::FunctionDecl::NotImplemented ) );
            putdecl->m_params.emplace_back( new Ast::Parameter( "self"_istr, putdecl, WITH( _.m_type = r ) ) );
            putdecl->m_params.emplace_back( new Ast::Parameter( "idx"_istr, putdecl, WITH( _.m_type = &Ast::s_typeInt ) ) );
            putdecl->m_params.emplace_back( new Ast::Parameter( "val"_istr, putdecl, WITH( _.m_type = t ) ) );
            r->m_methods.emplace_back( putdecl );
        }

        if( kind == Kind::Heap ) {
            auto retype = _makeFuncType( string_format( "(%s,int)->void", name.c_str() ), &Ast::s_typeVoid, r, &Ast::s_typeInt );
            auto redecl =
                new Ast::FunctionDecl( "resize"_istr, r, WITH( _.m_type = retype, _.m_intrinsic = Ast::FunctionDecl::NotImplemented ) );
            redecl->m_params.emplace_back( new Ast::Parameter( "self"_istr, redecl, WITH( _.m_type = r ) ) );
            redecl->m_params.emplace_back( new Ast::Parameter( "size"_istr, redecl, WITH( _.m_type = &Ast::s_typeInt ) ) );
            r->m_methods.emplace_back( redecl );
        }

        *out = r;
        return Result::OK;
    }

    static Result parse_internal( Ast::Environment* env, Ast::LexList* args, Ast::Expr** out, Ast::GenericDecl* genericDecl, Kind kind ) {
        *out = nullptr;
        Ast::LexIdent* lparam;
        Ast::LexNumber* lcount{ nullptr };
        std::string count;
        if( kind == Kind::Fixed ) {
            RETURN_IF_FAILED( matchLex( env, args, &lparam, &lcount ) );
            count = lcount->text();
            count.insert( 0, " " );
        } else {
            RETURN_IF_FAILED( matchLex( env, args, &lparam ) );
        }
        Ast::Expr* param;
        RETURN_IF_FAILED( env->lookup( lparam->text(), &param ) );
        auto type = dynamic_cast<Ast::Type*>( param );
        RETURN_ERROR_IF( type == nullptr, Error::TypeExpected, param->m_loc, "Array of non-type" );

        auto name = istring::make( string_format( "{%s %s%s}", s_nameFromKind[(int)kind].c_str(), type->name(), count.c_str() ) );

        Ast::Type* r;
        RETURN_IF_FAILED( env->module()->instantiate(
            name, [=]( Ast::Type** out ) { return instantiate_internal( name, env, kind, type, lcount, genericDecl, out ); }, &r ) );
        *out = r;
        return Result::OK;
    }
};

static Ast::GenericDecl* s_resultTDecl{};

Slip::Result Slip::Parse::ResultT_instantiate( Ast::Module* mod, Ast::Type* t, Ast::Type** out ) {
    auto name = istring::make( string_format( "{result %s}", t->name().c_str() ) );
    RETURN_IF_FAILED( mod->instantiate(
        name,
        [&]( Ast::Type** ret ) {
            auto r = new Ast::Type( name, Ast::fixMeDeclContext );
            r->m_sum.emplace_back( t );
            r->m_sum.emplace_back( &Ast::s_typeError );
            r->generic_ = new Ast::GenericInstantiation( WITH( _.decl_ = s_resultTDecl ) );
            r->generic_->args_.push_back( new Ast::Parameter( t->name(), r, WITH( _.m_type = t ) ) );
            *ret = r;
            return Result::OK;
        },
        out ) );
    return Result::OK;
}

static Result parse_ResultT( Ast::Environment* env, Ast::LexList* args, Ast::Expr** out ) {
    *out = nullptr;
    Ast::LexIdent* lparam;
    RETURN_IF_FAILED( matchLex( env, args, &lparam ) );
    Ast::Expr* param;
    RETURN_IF_FAILED( env->lookup( lparam->text(), &param ) );
    auto type = dynamic_cast<Ast::Type*>( param );
    assert( type );
    Ast::Type* r;
    RETURN_IF_FAILED( Slip::Parse::ResultT_instantiate( env->module(), type, &r ) );
    *out = r;
    return Result::OK;
}

Slip::Result Parse::module( string_view name, Ast::LexList& lex, Slip::unique_ptr_del<Ast::Module>& mod ) {
    auto module = make_unique_del<Ast::Module>( istring::make( name ), Ast::fixMeDeclContext );

    auto lang0 = new Ast::Module( "lang0"_istr, Ast::fixMeDeclContext );
    {
        auto env = lang0->env();
        addBuiltin( env, "coro"_istr, lang0, &parse_Coroutine );
        addBuiltin( env, "define"_istr, lang0, &parse_Define );
        addBuiltin( env, "func"_istr, lang0, &parse_Func );
        addBuiltin( env, "if"_istr, lang0, &parse_If );
        addBuiltin( env, "while"_istr, lang0, &parse_While );
        addBuiltin( env, "cond"_istr, lang0, &parse_Cond );
        addBuiltin( env, "let"_istr, lang0, &parse_Let );
        addBuiltin( env, "begin"_istr, lang0, &parse_Begin );
        addBuiltin( env, "block"_istr, lang0, &parse_Block );
        addBuiltin( env, "break"_istr, lang0, &parse_Break );
        addBuiltin( env, "var"_istr, lang0, &parse_Var );
        addBuiltin( env, "const"_istr, lang0, &parse_Const );
        addBuiltin( env, "set!"_istr, lang0, &parse_Set );
        addBuiltin( env, "try"_istr, lang0, &parse_Try );
        addBuiltin( env, "catch"_istr, lang0, &parse_Catch );
        addBuiltin( env, "scope"_istr, lang0, &parse_Scope );
        addBuiltin( env, "macro"_istr, lang0, &parse_Macro );
        addBuiltin( env, "struct"_istr, lang0, &parse_Struct );
        addBuiltin( env, "pipe"_istr, lang0, &parse_Pipe );
        addBuiltin( env, "fmt"_istr, lang0, &parse_Fmt );
        addBuiltin( env, "list"_istr, lang0, &parse_List );
        addBuiltin( env, "array_view"_istr, lang0, &ArrayView::parse_ArrayView );
        addBuiltin( env, "array_fixed"_istr, lang0, &ArrayView::parse_ArrayFixed );
        addBuiltin( env, "array_heap"_istr, lang0, &ArrayView::parse_ArrayHeap );
        addBuiltin( env, "result"_istr, lang0, &parse_ResultT );
        {
            auto d = new Ast::GenericDecl( "builtin_Result"_istr, lang0, WITH() );
            d->params_.push_back( new Ast::Parameter( "T"_istr, d, WITH( _.m_type = &Ast::s_typeType ) ) );
            d->environment_ = env;
            s_resultTDecl = d;
        }
        {
            static const Io::SourceNameAndContents generic{ "generic.slip",
                                                            "(generic (TYPE:Type)\n"
                                                            "  (union result\n"
                                                            "    ok:TYPE\n"
                                                            "    fail:i32))\n" };
            const auto& s = generic.m_contents;
            Io::TextInput input{ s.c_str(), s.c_str() + s.size(), &generic };
            Ast::LexNode* lex;
            Slip::Ast::lex_atom( input, &lex );
            Ast::LexList* list = dynamic_cast<Ast::LexList*>( lex );
            assert( list );
            Ast::LexList* args;
            Ast::LexNode* body;
            matchLex( env, list, &args, &body );
            auto decl = new Ast::GenericDecl( "result"_istr, lang0,
                                              WITH(  // TODO
                                                  _.environment_ = env, _.body_ = body ) );
            std::vector<Ast::Parameter*>& params = decl->params_;
            for( auto a : args->m_items ) {
                auto li = dynamic_cast<Ast::LexIdent*>( a );
                assert( li );
                params.push_back( new Ast::Parameter( li->istr(), decl, WITH( _.m_type = &Ast::s_typeType ) ) );  // todo parse type
            }
            // env->bind( "result"_sv, decl );
        }

        env->bind( "int"_sv, &Ast::s_typeInt );
        env->bind( "i32"_sv, &Ast::s_typeInt );
        env->bind( "float"_sv, &Ast::s_typeFloat );
        env->bind( "double"_sv, &Ast::s_typeDouble );
        env->bind( "f64"_sv, &Ast::s_typeDouble );
        env->bind( "f32"_sv, &Ast::s_typeFloat );
        env->bind( "void"_sv, &Ast::s_typeVoid );
        env->bind( "string"_sv, &Ast::s_typeString );
        env->bind( "bool"_sv, &Ast::s_typeBool );

        env->bind( "true"_sv, new Ast::Number( "true"_istr, WITH( _.m_type = &Ast::s_typeBool ) ) );
        env->bind( "false"_sv, new Ast::Number( "false"_istr, WITH( _.m_type = &Ast::s_typeBool ) ) );
        env->bind( "failed"_sv, new Ast::Number( "failed"_istr, WITH( _.m_type = &Ast::s_typeError ) ) );
    }

    auto b_ii = _makeFuncType( "(int, int)->bool", &Ast::s_typeBool, &Ast::s_typeInt, &Ast::s_typeInt );
    auto i_ii = _makeFuncType( "(int, int)->int", &Ast::s_typeInt, &Ast::s_typeInt, &Ast::s_typeInt );
    auto v_s = _makeFuncType( "(string)->void", &Ast::s_typeVoid, &Ast::s_typeString );
    auto v_i = _makeFuncType( "(int)->void", &Ast::s_typeVoid, &Ast::s_typeInt );
    auto v_d = _makeFuncType( "(double)->void", &Ast::s_typeVoid, &Ast::s_typeDouble );
    auto s_d = _makeFuncType( "(double)->string", &Ast::s_typeString, &Ast::s_typeDouble );
    auto d_dd = _makeFuncType( "(double, double)->double", &Ast::s_typeDouble, &Ast::s_typeDouble, &Ast::s_typeDouble );
    auto d_i = _makeFuncType( "(int)->double", &Ast::s_typeDouble, &Ast::s_typeInt );
    auto s_i = _makeFuncType( "(int)->string", &Ast::s_typeString, &Ast::s_typeInt );
    auto v_ss = _makeFuncType( "(string, string)->void", &Ast::s_typeVoid, &Ast::s_typeString, &Ast::s_typeString );
    auto s_s = _makeFuncType( "(string)->string", &Ast::s_typeString, &Ast::s_typeString );
    auto s_ss = _makeFuncType( "(string,string)->string", &Ast::s_typeString, &Ast::s_typeString, &Ast::s_typeString );
    auto s_sss =
        _makeFuncType( "(string, string, string)->string", &Ast::s_typeString, &Ast::s_typeString, &Ast::s_typeString, &Ast::s_typeString );
    auto s_ssss = _makeFuncType( "(string, string, string, string)->string", &Ast::s_typeString, &Ast::s_typeString, &Ast::s_typeString,
                                 &Ast::s_typeString, &Ast::s_typeString );
    auto s_sssss = _makeFuncType( "(string, string, string, string, string)->string", &Ast::s_typeString, &Ast::s_typeString,
                                  &Ast::s_typeString, &Ast::s_typeString, &Ast::s_typeString, &Ast::s_typeString );
    auto i_s = _makeFuncType( "(string)->void", &Ast::s_typeInt, &Ast::s_typeString );
    auto t_t = _makeFuncType( "(type)->type", &Ast::s_typeType, &Ast::s_typeType );
    // auto v_v = _makeFuncType( "(void)->void", &Ast::s_typeVoid, &Ast::s_typeVoid );
    Ast::Type* Ri;
    RETURN_IF_FAILED( ResultT_instantiate( module.get(), &Ast::s_typeInt, &Ri ) );
    auto Ri_s = _makeFuncType( "(string)->{Result int}", Ri, &Ast::s_typeString );

    auto bitops = make_unique_del<Ast::Module>( "bitops"_istr, lang0 );
    {
        auto env = bitops->env();
        addIntrinsic( env, "asl", i_ii );  // arith shift left
        addIntrinsic( env, "lsl", i_ii );  // logical shift left
        addIntrinsic( env, "asr", i_ii );
        addIntrinsic( env, "lsr", i_ii );
    }

    auto builtin = make_unique_del<Ast::Module>( "builtin"_istr, lang0 );
    {
        auto env = builtin->env();
        addIntrinsic( env, "eq?", b_ii );
        addIntrinsic( env, "lt?", b_ii );
        addIntrinsic( env, "ge?", b_ii );
        addIntrinsic( env, "add", i_ii );
        addIntrinsic( env, "mod", i_ii );
        addIntrinsic( env, "mul", i_ii );
        addIntrinsic( env, "div", i_ii );
        addIntrinsic( env, "sub", i_ii );
        addIntrinsic( env, "puts", v_s );
        addIntrinsic( env, "puti", v_i );
        addIntrinsic( env, "putd", v_d );
        addIntrinsic( env, "addd", d_dd );
        addIntrinsic( env, "muld", d_dd );
        addIntrinsic( env, "modd", d_dd );
        addIntrinsic( env, "divd", d_dd );
        addIntrinsic( env, "dfromi", d_i );
        addIntrinsic( env, "parsei", Ri_s );
        addIntrinsic( env, "strcat!", v_ss );
        addIntrinsic( env, "strjoin", s_s );
        addIntrinsic( env, "strjoin", s_ss );
        addIntrinsic( env, "strjoin", s_sss );
        addIntrinsic( env, "strjoin", s_ssss );
        addIntrinsic( env, "strjoin", s_sssss );
        addIntrinsic( env, "tostring", s_s );
        addIntrinsic( env, "tostring", s_i );
        addIntrinsic( env, "tostring", s_d );
    }

    module->env()->addUsing( builtin.get(), istring{} );
    module->env()->addUsing( lang0, istring{} );

    for( auto c : lex.items() ) {
        Ast::Expr* e;
        RETURN_IF_FAILED( parse1( module->env(), c, Parse::Flags::RValue, &e ), "Failed to parse" );
        Ast::NamedDecl* n;
        RETURN_IF_FAILED( dynCast( e, &n ) );
        module->addExport( n->name(), n );
    }
    builtin.release();  //< FIXME leak
    bitops.release();   //< FIXME leak
    mod = std::move( module );
    return Result::OK;
}
