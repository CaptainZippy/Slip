#include "pch/Pch.h"

#include "Ast.h"
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
    typedef Iter<Ast::LexNode*> Args;

    struct ArrayView;
    struct ArrayConst;
    struct ResultT;

    template <typename... Args>
    static Ast::Type* _makeFuncType( string_view name, Args&&... args ) {
        auto r = new Ast::Type( name );
        r->m_callable = {args...};
        return r;
    }

    static void addBuiltin( Ast::Environment* env, string_view name, Ast::Builtin::ParseFunc&& fun ) {
        env->bind( name, new Ast::Builtin( name, std::move( fun ) ) );
    }

    static auto addIntrinsic( Ast::Environment* env, string_view name, Ast::Type* type ) -> Ast::FunctionDecl* {
        auto n = istring::make( name );
        auto f = new Ast::FunctionDecl( n, WITH( _.m_type = type ) );
        char pname[2] = {'a', 0};
        for( auto&& at : array_view( type->m_callable ).ltrim( 1 ) ) {
            auto p = new Ast::Parameter( pname, WITH( _.m_type = at ) );
            f->m_params.emplace_back( p );
            pname[0] += 1;
        }
        env->bind( n, f );
        return f;
    }
}  // namespace Slip::Parse

using namespace Slip;
enum class Ellipsis { ZeroOrMore, OneOrMore };

static Result parse1( Ast::Environment* env, Ast::LexNode* atom, Ast::Node** out );
static Result parse_Var( Ast::Environment* env, Ast::LexList* args, Ast::Node** out );

static Result matchLex( Ast::Environment* env, Parse::Args& args ) {
    RETURN_ERR_IF( !args.empty() );
    return Result::OK;
}

static Result matchLex( Ast::Environment* env, Parse::Args& args, std::vector<Ast::LexNode*>* list, Ellipsis ellipsis ) {
    RETURN_ERR_IF( ellipsis == Ellipsis::OneOrMore && args.empty() );
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
static Result matchLex( Ast::Environment* env, Parse::Args& args, HEAD** head, REST... rest ) {
    RETURN_ERR_IF( args.empty() );
    auto h = dynamic_cast<HEAD*>( args.cur() );
    if( !h ) {
        if( auto now = dynamic_cast<Ast::LexNowExpr*>( args.cur() ) ) {
            Ast::Node* n;
            RETURN_IF_FAILED( parse1( env, now, &n ) );
            h = dynamic_cast<HEAD*>( args.cur() );
        }
    }
    RETURN_ERR_IF( !h );
    args.advance();
    *head = h;
    return matchLex( env, args, rest... );
}

template <typename... REST>
static Result matchLex( Ast::Environment* env, Ast::LexList* list, REST... rest ) {
    Parse::Args args{list->items()};
    RETURN_ERR_IF( args.empty() );
    args.advance();
    return matchLex( env, args, rest... );
}

static Result macroExpand1( Ast::Environment* env, Ast::LexList* args, Ast::Node** out ) {
    RETURN_ERR_IF( args->size() < 2 || args->size() > 3 );
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

    Ast::Node* repl;
    RETURN_IF_FAILED( env->lookup( larg->text(), &repl ) );
    auto text = dynamic_cast<Ast::LexNode*>( repl );
    RETURN_ERR_IF( text == nullptr );

    Ast::Environment* xenv;
    if( lenv ) {
        Ast::Node* val;
        RETURN_IF_FAILED( env->lookup( lenv->text(), &val ) );
        xenv = dynamic_cast<Ast::Environment*>( val );
        RETURN_ERR_IF( xenv == nullptr );
    } else {
        xenv = env;
    }

    RETURN_IF_FAILED( parse1( xenv, text, out ) );
    return Result::OK;
}

static Result macroExpand( Ast::MacroDecl* macro, Ast::Environment* env, Ast::LexList* list, Ast::Node** out ) {
    auto args = list->items().ltrim( 1 );
    RETURN_ERR_IF( args.size() != macro->m_params.size() );
    auto localEnv = new Ast::Environment( macro->m_staticEnv );
    for( unsigned i = 0; i < macro->m_params.size(); ++i ) {
        localEnv->bind( macro->m_params[i]->name(), args[i] );
    }
    localEnv->bind( macro->m_dynEnvSym, env );
    static Ast::Builtin expander{"expand"sv, &macroExpand1};
    localEnv->bind( "expand"sv, &expander );
    std::vector<Ast::Node*> body;
    for( auto&& b : macro->m_body ) {
        Ast::Node* p;
        RETURN_IF_FAILED( parse1( localEnv, b, &p ) );
        body.push_back( p );
    }
    if( body.size() == 1 ) {
        *out = body[0];
    } else {
        *out = new Ast::Sequence( std::move( body ) );
    }
    return Result::OK;
}

static Result parse1( Ast::Environment* env, Ast::LexNode* atom, Ast::Node** out ) {
    *out = nullptr;
    if( atom == nullptr ) {
        return Result::OK;
    } else if( auto sym = dynamic_cast<Ast::LexIdent*>( atom ) ) {
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
    } else if( auto list = dynamic_cast<Ast::LexList*>( atom ) ) {
        RETURN_ERR_IF( !list );
        RETURN_ERR_IF( list->size() == 0 );
        auto items = list->items();
        auto first = items[0];
        auto sym = dynamic_cast<Ast::LexIdent*>( first );
        RETURN_ERR_IF( !sym, "Symbol expected in first list position" );

        // sym can resolve to a builtin or macro or function. Only functions can be overloaded.
        Ast::Node* p;
        auto isym = istring::make( sym->text() );
        std::vector<Ast::Node*> candidates;
        Ast::Environment::LookupIter iter;
        while( env->lookup_iter( isym, &p, iter ) ) {
            if( auto b = dynamic_cast<Ast::Builtin*>( p ) ) {
                RETURN_ERR_IF( !candidates.empty(), "Builtins can't be overloaded" );
                RETURN_ERR_IF( env->lookup_iter( isym, &p, iter ), "Builtins can't be overloaded" );
                return b->parse( env, list, out );
            } else if( auto m = dynamic_cast<Ast::MacroDecl*>( p ) ) {
                RETURN_ERR_IF( !candidates.empty(), "Builtins can't be overloaded" );
                RETURN_ERR_IF( env->lookup_iter( isym, &p, iter ), "Builtins can't be overloaded" );
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
    } else if( auto num = dynamic_cast<Ast::LexNumber*>( atom ) ) {
        Ast::Node* te;
        RETURN_IF_FAILED( parse1( env, num->m_decltype, &te ) );
        auto r = new Ast::Number( num->text(), WITH( _.m_loc = num->m_loc, _.m_declTypeExpr = te ) );
        *out = r;
        return Result::OK;
    } else if( auto str = dynamic_cast<Ast::LexString*>( atom ) ) {
        Ast::Node* te;
        RETURN_IF_FAILED( parse1( env, str->m_decltype, &te ) );
        auto r = new Ast::String( str->text(), WITH( _.m_loc = str->m_loc, _.m_declTypeExpr = te ) );
        *out = r;
        return Result::OK;
    } else if( auto dot = dynamic_cast<Ast::LexDot*>( atom ) ) {
        Ast::Node* lhs;
        RETURN_IF_FAILED( parse1( env, dot->m_lhs, &lhs ) );
        auto rhs = dynamic_cast<Ast::LexIdent*>( dot->m_rhs );
        RETURN_ERR_IF( !rhs, "Symbol expected in first list position" );  // todo:now
        auto r = new Ast::Selector( lhs, rhs, WITH( _.m_loc = dot->m_loc ) );
        *out = r;
        return Result::OK;
    } else if( auto now = dynamic_cast<Ast::LexNowExpr*>( atom ) ) {
        auto expr = dynamic_cast<Ast::LexList*>( now->m_expr );
        RETURN_ERR_IF( expr == nullptr || expr->size() < 1, "fixme" );
        auto expr0 = dynamic_cast<Ast::LexIdent*>( expr->at( 0 ) );
        Ast::Node* replacement = nullptr;

        if( expr0->text() == "env" ) {
            RETURN_ERR_IF( expr->size() != 2, "fixme" );
            Ast::Node* node0;
            RETURN_IF_FAILED( env->lookup( expr0->text(), &node0 ) );
            auto env0 = dynamic_cast<Ast::Environment*>( node0 );

            auto expr1 = dynamic_cast<Ast::LexIdent*>( expr->at( 1 ) );
            Ast::Node* node1;
            RETURN_IF_FAILED( env->lookup( expr1->text(), &node1 ) );
            auto lex1 = dynamic_cast<Ast::LexNode*>( node1 );
            RETURN_IF_FAILED( parse1( env0, lex1, &replacement ) );
        } else if( expr0->text() == "stringize" ) {
            RETURN_ERR_IF( expr->size() != 2, "fixme" );
            auto expr1 = dynamic_cast<Ast::LexIdent*>( expr->at( 1 ) );
            Ast::Node* node1;
            RETURN_IF_FAILED( env->lookup( expr1->text(), &node1 ) );
            auto lex1 = dynamic_cast<Ast::LexNode*>( node1 );

            std::vector<char> txt;
            Io::TextOutput out( &txt );
            Ast::print( lex1, out );
            replacement = new Ast::String( {txt.data(), txt.size()}, WITH( _.m_loc = expr1->m_loc ) );
        } else if( expr0->text() == "with_env" ) {
            Ast::LexIdent* lname;
            Ast::LexIdent* lparent;
            std::vector<Ast::LexNode*> lbody;
            RETURN_IF_FAILED( matchLex( env, expr, &lname, &lparent, &lbody, Ellipsis::OneOrMore ) );
            Ast::Node* parent;
            RETURN_IF_FAILED( env->lookup( lparent->text(), &parent ) );
            auto parentEnv = dynamic_cast<Ast::Environment*>( parent );
            auto inner = new Ast::Environment( parentEnv );
            env->bind( lname->text(), inner );
            if( lbody.size() == 1 ) {
                RETURN_IF_FAILED( parse1( env, lbody[0], &replacement ) );
            } else {
                auto bd = new Ast::Sequence();
                for( auto&& l : lbody ) {
                    Ast::Node* b;
                    RETURN_IF_FAILED( parse1( env, l, &b ) );
                    if( b )
                        bd->m_items.emplace_back( b );
                }
                replacement = bd;
            }
        } else if( expr0->text() == "expand" ) {
            Ast::Node* n;
            RETURN_IF_FAILED( macroExpand1( env, expr, &n ) );
        } else if( expr0->text() == "bind" ) {
            Ast::LexIdent* lenv;
            Ast::LexIdent* lname;
            Ast::LexIdent* lval;
            RETURN_IF_FAILED( matchLex( env, expr, &lenv, &lname, &lval ) );
            Ast::Node* nenv;
            RETURN_IF_FAILED( env->lookup( lenv->text(), &nenv ) );
            auto env2 = dynamic_cast<Ast::Environment*>( nenv );

            Ast::Node* nname;
            RETURN_IF_FAILED( env->lookup( lname->text(), &nname ) );
            Ast::Node* nval;
            RETURN_IF_FAILED( env->lookup( lval->text(), &nval ) );
            RETURN_IF_FAILED( env2->bind( dynamic_cast<Ast::LexIdent*>( nname )->text(), nval ) );
        } else {
            RETURN_ERR_IF( true );
        }

        if( replacement == nullptr ) {
            int x;
            x = 0;
        }

        *out = replacement;
        return Result::OK;
    }
    RETURN_ERR_IF( true );
}

static Result parse_Coroutine( Ast::Environment* env, Ast::LexList* args, Ast::Node** out ) {
    *out = nullptr;
    RETURN_ERR_IF( args->size() < 3 );
    Ast::LexIdent* lname;
    Ast::LexList* largs;
    std::vector<Ast::LexNode*> lbody;
    RETURN_IF_FAILED( matchLex( env, args, &lname, &largs, &lbody, Ellipsis::OneOrMore ) );
    auto coro = new Ast::CoroutineDecl( lname->text(), WITH( _.m_loc = lname->m_loc ) );
    env->bind( coro->m_name, coro );
    auto inner = new Ast::Environment( env );
    Slip::Parse::addBuiltin( inner, "yield"sv, [coro]( auto env, auto args, auto out ) -> Result {
        RETURN_ERR_IF( args->size() != 2 );
        auto ret = new Ast::CoroutineYield();
        RETURN_IF_FAILED( parse1( env, args->at( 1 ), &ret->m_expr ) );
        ret->m_coro = coro;
        *out = ret;
        return Result::OK;
    } );

    if( auto a = largs->m_decltype ) {
        Ast::Node* te;
        RETURN_IF_FAILED( parse1( inner, a, &te ) );
        coro->m_declReturnTypeExpr = te;
    }
    for( auto item : largs->items() ) {
        auto sym = dynamic_cast<Ast::LexIdent*>( item );
        RETURN_ERR_IF( sym == nullptr );
        Ast::Node* te;
        RETURN_IF_FAILED( parse1( inner, item->m_decltype, &te ) );
        auto param = new Ast::Parameter( sym->text(), WITH( _.m_loc = sym->m_loc, _.m_declTypeExpr = te ) );
        coro->m_params.push_back( param );
    }
    // TODO check unique names
    for( auto p : coro->m_params ) {
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
    coro->m_body = body;
    *out = coro;
    return Result::OK;
}

static Result parse_Define( Ast::Environment* env, Ast::LexList* args, Ast::Node** out ) {
    Ast::LexIdent* lname;
    Ast::LexNode* lval;
    RETURN_IF_FAILED( matchLex( env, args, &lname, &lval ) );
    Ast::Node* nval;
    RETURN_IF_FAILED( parse1( env, lval, &nval ) );

    auto ret = new Ast::Definition( lname->text(), nval, WITH( _.m_loc = lname->m_loc ) );
    env->bind( lname->text(), ret );

    *out = ret;
    return Result::OK;
}

static Result parse_Func( Ast::Environment* env, Ast::LexList* args, Ast::Node** out ) {
    *out = nullptr;
    RETURN_ERR_IF( args->size() < 3 );
    Ast::LexIdent* lname;
    Ast::LexList* largs;
    std::vector<Ast::LexNode*> lbody;
    RETURN_IF_FAILED( matchLex( env, args, &lname, &largs, &lbody, Ellipsis::OneOrMore ) );
    auto func = new Ast::FunctionDecl( lname->text(), WITH( _.m_loc = lname->m_loc ) );
    env->bind( func->m_name, func );
    auto inner = new Ast::Environment( env );
    if( auto a = largs->m_decltype ) {
        Ast::Node* te;
        RETURN_IF_FAILED( parse1( inner, a, &te ) );
        func->m_declReturnTypeExpr = te;
    }
    for( auto item : largs->items() ) {
        auto sym = dynamic_cast<Ast::LexIdent*>( item );
        RETURN_ERR_IF( sym == nullptr );
        Ast::Node* te;
        RETURN_IF_FAILED( parse1( inner, item->m_decltype, &te ) );
        auto param = new Ast::Parameter( sym->text(), WITH( _.m_loc = sym->m_loc, _.m_declTypeExpr = te ) );
        auto it = find_if( item->m_decltype->m_attrs, []( Ast::LexNode* n ) {
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

static Result parse_Let( Ast::Environment* env, Ast::LexList* args, Ast::Node** out ) {
    Ast::Node* n;
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

        Ast::Node* aval;
        RETURN_IF_FAILED( parse1( env, lbody, &aval ) );
        Ast::Node* te;
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
        RETURN_ERR_IF( !lpair );
        Ast::LexIdent* lsym;
        Ast::LexNode* lval;
        Parse::Args tmpargs{lpair->items()};
        RETURN_IF_FAILED( matchLex( env, tmpargs, &lsym, &lval ) );
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
#endif
}

static Result parse_If( Ast::Environment* env, Ast::LexList* args, Ast::Node** out ) {
    Ast::LexNode* lcond;
    Ast::LexNode* ltrue;
    Ast::LexNode* lfalse;
    RETURN_IF_FAILED( matchLex( env, args, &lcond, &ltrue, &lfalse ) );

    Ast::Node* ncond;
    Ast::Node* ntrue;
    Ast::Node* nfalse;
    RETURN_IF_FAILED( parse1( env, lcond, &ncond ) );
    RETURN_IF_FAILED( parse1( env, ltrue, &ntrue ) );
    RETURN_IF_FAILED( parse1( env, lfalse, &nfalse ) );

    *out = new Ast::If( ncond, ntrue, nfalse, WITH( _.m_loc = lcond->m_loc ) );
    return Result::OK;
}

static Result parse_While( Ast::Environment* env, Ast::LexList* args, Ast::Node** out ) {
    Ast::LexNode* lcond;
    std::vector<Ast::LexNode*> lbody;
    RETURN_IF_FAILED( matchLex( env, args, &lcond, &lbody, Ellipsis::OneOrMore ) );
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
            if( b )
                bd->m_items.emplace_back( b );
        }
        nbody = bd;
    }
    *out = new Ast::While( ncond, nbody, WITH( _.m_loc = lcond->m_loc ) );
    return Result::OK;
}

static Result parse_Cond( Ast::Environment* env, Ast::LexList* args, Ast::Node** out ) {
    *out = nullptr;
    RETURN_ERR_IF( args->size() < 2 );
    vector<pair<Ast::Node*, Ast::Node*>> cases;
    for( auto&& arg : args->items().ltrim( 1 ) ) {
        auto pair = dynamic_cast<Ast::LexList*>( arg );
        RETURN_ERR_IF( pair == nullptr );
        RETURN_ERR_IF( pair->size() != 2 );

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

static Result parse_Begin( Ast::Environment* env, Ast::LexList* args, Ast::Node** out ) {
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

static Result parse_Scope( Ast::Environment* env, Ast::LexList* args, Ast::Node** out ) {
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

static Result parse_Block( Ast::Environment* env, Ast::LexList* args, Ast::Node** out ) {
    *out = nullptr;
    Ast::LexIdent* name;
    std::vector<Ast::LexNode*> contents;
    RETURN_IF_FAILED( matchLex( env, args, &name, &contents, Ellipsis::ZeroOrMore ) );
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

static Result parse_Break( Ast::Environment* env, Ast::LexList* args, Ast::Node** out ) {
    *out = nullptr;
    Ast::LexIdent* llabel;
    Ast::LexNode* lval;
    RETURN_IF_FAILED( matchLex( env, args, &llabel, &lval ) );
    Ast::Node* nlabel;
    RETURN_IF_FAILED( parse1( env, llabel, &nlabel ) );
    auto blockr = dynamic_cast<Ast::Reference*>( nlabel );  // TODO tidy
    auto blocka = dynamic_cast<Ast::Block*>( blockr->m_target );
    RETURN_ERR_IF( blocka == nullptr );
    Ast::Node* nval;
    RETURN_IF_FAILED( parse1( env, lval, &nval ) );
    *out = new Ast::Break( blocka, nval );
    return Result::OK;
}

static Result parse_Var( Ast::Environment* env, Ast::LexList* args, Ast::Node** out ) {
    *out = nullptr;

    Ast::LexIdent* sym;
    std::vector<Ast::LexNode*> inits;
    RETURN_IF_FAILED( matchLex( env, args, &sym, &inits, Ellipsis::ZeroOrMore ) );

    Ast::Node* varType;
    RETURN_IF_FAILED( parse1( env, sym->m_decltype, &varType ) );

    std::vector<Ast::Node*> vals;
    for( auto&& i : inits ) {
        Ast::Node* n;
        RETURN_IF_FAILED( parse1( env, i, &n ) );
        vals.emplace_back( n );
    }

    auto ret = new Ast::VariableDecl( istring::make( sym->text() ), WITH( _.m_declTypeExpr = varType, _.m_loc = sym->m_loc ) );
    ret->m_kind = Ast::VariableDecl::Kind::Mutable;
    ret->m_initializer.swap( vals );
    RETURN_IF_FAILED( env->bind( sym->text(), ret ) );
    *out = ret;
    return Result::OK;
}

static Result parse_Const( Ast::Environment* env, Ast::LexList* args, Ast::Node** out ) {
    Ast::Node* n;
    Ast::VariableDecl* v;
    RETURN_IF_FAILED( parse_Var( env, args, &n ) );
    RETURN_IF_FAILED( dynCast( n, &v ) );
    v->m_kind = Ast::VariableDecl::Kind::Constant;
    *out = v;
    return Result::OK;
}

static Result parse_Set( Ast::Environment* env, Ast::LexList* args, Ast::Node** out ) {
    *out = nullptr;
    Ast::LexNode* lex_dst;
    Ast::LexNode* lex_src;
    RETURN_IF_FAILED( matchLex( env, args, &lex_dst, &lex_src ) );
    Ast::Node* dst;
    RETURN_IF_FAILED( parse1( env, lex_dst, &dst ) );
    Ast::Node* src;
    RETURN_IF_FAILED( parse1( env, lex_src, &src ) );
    *out = new Ast::Assignment( dst, src, WITH( _.m_loc = lex_src->m_loc ) );
    return Result::OK;
}

static Result parse_Macro( Ast::Environment* env, Ast::LexList* args, Ast::Node** out ) {
    *out = nullptr;
    Ast::LexIdent* lname;
    Ast::LexList* largs;
    Ast::LexIdent* lenv;
    std::vector<Ast::LexNode*> lbody;
    RETURN_IF_FAILED( matchLex( env, args, &lname, &largs, &lenv, &lbody, Ellipsis::OneOrMore ) );
    auto macro = new Ast::MacroDecl( lname->text(), lenv->text(), env, WITH( _.m_loc = lname->m_loc ) );
    for( auto item : largs->items() ) {
        auto sym = dynamic_cast<Ast::LexIdent*>( item );
        RETURN_ERR_IF( sym == nullptr );
        auto param = new Ast::Parameter( sym->text(), WITH( _.m_loc = sym->m_loc ) );
        macro->m_params.push_back( param );
    }
    RETURN_IF_FAILED( env->bind( macro->m_name, macro ) );
    macro->m_body = std::move( lbody );
    *out = macro;
    return Result::OK;
}

static Result parse_Struct( Ast::Environment* env, Ast::LexList* args, Ast::Node** out ) {
    *out = nullptr;
    Ast::LexIdent* lname;
    std::vector<Ast::LexNode*> lfields;
    RETURN_IF_FAILED( matchLex( env, args, &lname, &lfields, Ellipsis::ZeroOrMore ) );
    auto decl = new Ast::StructDecl( lname->text(), WITH( _.m_loc = lname->m_loc ) );
    auto type = new Ast::Type( decl->name() );
    type->m_struct = decl;
    RETURN_IF_FAILED( env->bind( decl->m_name, type ) );
    for( auto&& lf : lfields ) {
        auto li = dynamic_cast<Ast::LexIdent*>( lf );
        RETURN_ERR_IF( li == nullptr, "Expected identifier for field" );
        RETURN_ERR_IF( li->m_decltype == nullptr, "Missing type for field" );
        Ast::Node* ft;
        RETURN_IF_FAILED( parse1( env, li->m_decltype, &ft ) );
        decl->m_fields.emplace_back( new Ast::StructField( li->text(), WITH( _.m_loc = li->m_loc; _.m_declTypeExpr = ft; ) ) );
    }
    *out = decl;
    return Result::OK;
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

    static Result parse( Cache* cache, Ast::Environment* env, Ast::LexList* args, Ast::Node** out ) {
        *out = nullptr;
        Ast::LexIdent* lparam;
        RETURN_IF_FAILED( matchLex( env, args, &lparam ) );
        Ast::Node* param = env->lookup( lparam->text() );  // TODO
        auto type = dynamic_cast<Ast::Type*>( param );
        assert( type );
        Ast::Type* r;
        RETURN_IF_FAILED( cache->instantiate( type, &r ) );
        *out = r;
        return Result::OK;
    }
};

struct Parse::ResultT {
    struct Cache {
        Result instantiate( Ast::Type* t, Ast::Type** out ) {
            *out = nullptr;
            auto name = string_format( "Result<%s>", t->name().c_str() );
            if( auto it = types_.find( name ); it != types_.end() ) {
                *out = it->second;
                return Result::OK;
            }
            auto r = new Ast::Type( name );
            r->m_sum.emplace_back( t );
            r->m_sum.emplace_back( &Ast::s_typeError );
            types_.emplace( name, r );
            *out = r;
            return Result::OK;
        }
        std::map<std::string, Ast::Type*> types_;
    };

    static Result parse( Cache* cache, Ast::Environment* env, Ast::LexList* args, Ast::Node** out ) {
        *out = nullptr;
        Ast::LexIdent* lparam;
        RETURN_IF_FAILED( matchLex( env, args, &lparam ) );
        Ast::Node* param = env->lookup( lparam->text() );  // TODO
        auto type = dynamic_cast<Ast::Type*>( param );
        assert( type );
        Ast::Type* r;
        RETURN_IF_FAILED( cache->instantiate( type, &r ) );
        *out = r;
        return Result::OK;
    }
};

Slip::Result Parse::module( Ast::LexList& lex, Slip::unique_ptr_del<Ast::Module>& mod ) {
    auto env = new Ast::Environment( nullptr );
    addBuiltin( env, "coro"sv, &parse_Coroutine );
    addBuiltin( env, "define"sv, &parse_Define );
    addBuiltin( env, "func"sv, &parse_Func );
    addBuiltin( env, "if"sv, &parse_If );
    addBuiltin( env, "while"sv, &parse_While );
    addBuiltin( env, "cond"sv, &parse_Cond );
    addBuiltin( env, "let"sv, &parse_Let );
    addBuiltin( env, "begin"sv, &parse_Begin );
    addBuiltin( env, "block"sv, &parse_Block );
    addBuiltin( env, "break"sv, &parse_Break );
    addBuiltin( env, "var"sv, &parse_Var );
    addBuiltin( env, "const"sv, &parse_Const );
    addBuiltin( env, "set!"sv, &parse_Set );
    addBuiltin( env, "scope"sv, &parse_Scope );
    addBuiltin( env, "macro"sv, &parse_Macro );
    addBuiltin( env, "struct"sv, &parse_Struct );
    addBuiltin( env, "array_view"sv, [cache = new Parse::ArrayView::Cache( "array_view" )]( auto e, auto a, auto o ) {
        return ArrayView::parse( cache, e, a, o );
    } );
    addBuiltin( env, "array_const"sv, [cache = new Parse::ArrayView::Cache( "array_const" )]( auto e, auto a, auto o ) {
        return ArrayView::parse( cache, e, a, o );
    } );
    addBuiltin( env, "array_heap"sv, [cache = new Parse::ArrayView::Cache( "array_heap" )]( auto e, auto a, auto o ) {
        return ArrayView::parse( cache, e, a, o );
    } );
    addBuiltin( env, "result"sv,
                [cache = new Parse::ResultT::Cache()]( auto e, auto a, auto o ) { return ResultT::parse( cache, e, a, o ); } );

    env->bind( "int"sv, &Ast::s_typeInt );
    env->bind( "float"sv, &Ast::s_typeFloat );
    env->bind( "double"sv, &Ast::s_typeDouble );
    env->bind( "void"sv, &Ast::s_typeVoid );
    env->bind( "string"sv, &Ast::s_typeString );
    env->bind( "bool"sv, &Ast::s_typeBool );

    env->bind( "true"sv, new Ast::Number( "true", WITH( _.m_type = &Ast::s_typeBool ) ) );
    env->bind( "false"sv, new Ast::Number( "false", WITH( _.m_type = &Ast::s_typeBool ) ) );
    env->bind( "failed"sv, new Ast::Number( "failed", WITH( _.m_type = &Ast::s_typeError ) ) );

    auto b_ii = _makeFuncType( "(int, int)->bool", &Ast::s_typeBool, &Ast::s_typeInt, &Ast::s_typeInt );
    auto i_ii = _makeFuncType( "(int, int)->int", &Ast::s_typeInt, &Ast::s_typeInt, &Ast::s_typeInt );
    auto v_s = _makeFuncType( "(string)->void", &Ast::s_typeVoid, &Ast::s_typeString );
    auto v_i = _makeFuncType( "(int)->void", &Ast::s_typeVoid, &Ast::s_typeInt );
    auto v_d = _makeFuncType( "(double)->void", &Ast::s_typeVoid, &Ast::s_typeDouble );
    auto d_dd = _makeFuncType( "(double, double)->double", &Ast::s_typeDouble, &Ast::s_typeDouble, &Ast::s_typeDouble );
    auto d_i = _makeFuncType( "(int)->double", &Ast::s_typeDouble, &Ast::s_typeInt );
    auto v_ss = _makeFuncType( "(string, string)->void", &Ast::s_typeVoid, &Ast::s_typeString, &Ast::s_typeString );
    auto i_s = _makeFuncType( "(string)->void", &Ast::s_typeInt, &Ast::s_typeString );
    auto t_t = _makeFuncType( "(type)->type", &Ast::s_typeType, &Ast::s_typeType );
    // auto v_v = _makeFuncType( "(void)->void", &Ast::s_typeVoid, &Ast::s_typeVoid );

    addIntrinsic( env, "eq?", b_ii );
    addIntrinsic( env, "lt?", b_ii );
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
