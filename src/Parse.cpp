#include "pch/Pch.h"

#include "Ast.h"
#include "Lex.h"
#include "Parse.h"
#include "Slip.h"

#define WITH( ... ) [&]( auto& _ ) { __VA_ARGS__; }

namespace Slip::Parse {
    struct State;

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

        bool used() const { return m_begin == m_end; }

        size_t size() const { return m_end - m_begin; }

        T* begin() { return m_begin; }
        T* end() { return m_end; }

        T* m_begin{nullptr};
        T* m_end{nullptr};
    };
    typedef Iter<Lex::Atom*> Args;

    struct Parser {
        virtual ~Parser() {}
        Result parse( State* state, Args& args, Ast::Node** out ) const;

       protected:
        virtual Result _parse( State* state, Args& args, Ast::Node** out ) const = 0;
    };

    struct Environment {
        Environment( std::shared_ptr<Environment>&& parent ) : parent_( parent ) {}

        using SymbolBase = std::variant<Parser*, Ast::Node*, vector<Ast::Node*>>;
        struct SymbolValue : public SymbolBase {
            using SymbolBase::SymbolBase;
            Parser* isBuiltin() const { return index() == 0 ? get<0>( *this ) : nullptr; }
            Ast::Node* isNode() const { return index() == 1 ? get<1>( *this ) : nullptr; }
            bool isOverload() const { return index() == 2; }
            const vector<Ast::Node*>& overloads() const { return get<2>( *this ); }
            Result addOverload( Ast::Node* n );
        };

        Result lookup( string_view sym, const SymbolValue** out ) const {
            auto s = istring::make( sym );

            for( auto cur = this; cur != nullptr; cur = cur->parent_.get() ) {
                auto x = cur->syms_.find( s );
                if( x != cur->syms_.end() ) {
                    *out = &x->second;
                    return Result::OK;
                }
            }
            RETURN_RES_IF( Result::ERR, true, "symbol not found '%s'", s.c_str() );
        }

        template <typename T>
        auto bind( istring sym, T value ) {
            return syms_.emplace( sym, value );
        }

        std::shared_ptr<Environment> parent_;
        std::map<istring, SymbolValue> syms_;
    };

    struct State {
        State() { enterScope(); }

        ~State() { leaveScope(); }

        void enterScope() { env_ = std::make_shared<Environment>( std::move( env_ ) ); }

        void leaveScope() { env_ = env_->parent_; }

        void addParser( string_view sym, Parser* value ) {
            auto s = istring::make( sym );
            auto p = env_->bind( s, value );
            assert( p.second );  // new entry
        }

        Result addSym( string_view sym, Ast::Node* node ) {
            auto s = istring::make( sym );
            auto p = env_->bind( s, node );
            if( p.second ) {  // new entry
                return Result::OK;
            }
            // Function overload?
            assert( false );  // TODO
            // SymbolValue& existing = p.first->second;
            // RETURN_IF_FAILED( existing.addOverload( node ) );
            return Result::OK;
        }

        auto addIntrinsic( string_view sym, Ast::Type* type ) -> Ast::FunctionDecl* {
            auto s = istring::make( sym );
            auto f = new Ast::FunctionDecl( sym, WITH( _.m_type = type ) );
            char name[2] = {'a', '0'};
            for( auto&& at : array_view( type->m_callable ).ltrim( 1 ) ) {
                auto a = new Ast::Argument( name, WITH( _.m_type = at ) );
                f->m_args.emplace_back( a );
                name[0] += 1;
            }
            auto p = env_->bind( s, f );
            assert( p.second );
            return f;
        }

        Lex::Symbol* symbol( Lex::Atom* atom ) {
            if( auto sym = dynamic_cast<Lex::Symbol*>( atom ) ) {
                return sym;
            }
            return nullptr;
        }

        using SymbolValue = Environment::SymbolValue;
        Result parse( Lex::Atom* atom, Ast::Node** out );

        Result macroExpand( Ast::MacroDecl* macro, Args args, Ast::Node** out );

        Result lookup( string_view sym, const Environment::SymbolValue** out ) const { return env_->lookup( sym, out ); }

        std::shared_ptr<Environment> env_;
    };

    struct Define;
    struct Func;
    struct Let;
    struct If;
    struct While;
    struct Cond;
    struct Begin;
    struct Var;
    struct Set;
    struct Macro;

    Result Environment::SymbolValue::addOverload( Ast::Node* n ) {
        RETURN_RES_IF( Result::ERR, isBuiltin(), "Fixme" );
        auto newFunc = dynamic_cast<Ast::FunctionDecl*>( n );
        RETURN_RES_IF( Result::ERR, newFunc == nullptr, "Only functions can be overloads" );
        if( isOverload() ) {
            // TODO check not duplicate
            get<2>( *this ).push_back( newFunc );
            return Result::OK;
        }
        // this changes to overload
        auto oldFunc = isNode();
        RETURN_RES_IF( Result::ERR, oldFunc == nullptr, "Existing symbol" );
        std::vector<Ast::Node*> decls;
        decls.emplace_back( oldFunc );
        decls.emplace_back( newFunc );
        *this = std::move( decls );
        return Result::OK;
    }
}  // namespace Slip::Parse

using namespace Slip;

static Result matchLex( Parse::Args& args ) {
    RETURN_RES_IF( Result::ERR, !args.used() );
    return Result::OK;
}

/// Match and consume the entire argument list exactly
template <typename HEAD, typename... REST>
static Result matchLex( Parse::Args& args, HEAD** head, REST**... rest ) {
    auto h = dynamic_cast<HEAD*>( args.cur() );
    RETURN_RES_IF( Result::ERR, !h );
    args.advance();
    *head = h;
    return matchLex( args, rest... );
}

Result Parse::State::parse( Lex::Atom* atom, Ast::Node** out ) {
    *out = nullptr;
    if( atom == nullptr ) {
        return Result::OK;
    } else if( auto sym = dynamic_cast<Lex::Symbol*>( atom ) ) {
        const SymbolValue* p;
        RETURN_IF_FAILED( env_->lookup( sym->text(), &p ) );
        RETURN_RES_IF( Result::ERR, p->isBuiltin() );
        RETURN_RES_IF( Result::ERR, p->isOverload() );

        // TODO: detect symbol is decl or const.
        if( auto node = p->isNode() ) {
            if( auto ty = dynamic_cast<Ast::Type*>( node ) ) {
                *out = ty;
            } else if( auto nu = dynamic_cast<Ast::Number*>( node ) ) {
                *out = nu;
            } else {
                *out = new Ast::Reference( node );
            }
            return Result::OK;
        }

    } else if( auto list = dynamic_cast<Lex::List*>( atom ) ) {
        RETURN_RES_IF( Result::ERR, !list );
        RETURN_RES_IF( Result::ERR, list->size() == 0 );
        auto items = list->items();
        auto first = items[0];
        const SymbolValue* p = nullptr;
        if( auto sym = dynamic_cast<Lex::Symbol*>( first ) ) {
            RETURN_IF_FAILED( lookup( sym->text(), &p ), "Symbol '%.*s' not found", sym->text().length(), sym->text().data() );
        } else {
            RETURN_RES_IF_REACHED( Result::ERR );
        }
        Args args{items};
        args.advance();
        if( auto b = p->isBuiltin() ) {
            return b->parse( this, args, out );
        } else if( auto macro = dynamic_cast<Ast::MacroDecl*>( p->isNode() ) ) {
            return macroExpand( macro, args, out );
        }
        vector<Ast::Node*> fa;
        for( auto a : args ) {
            Ast::Node* n;
            RETURN_IF_FAILED( parse( a, &n ) );
            fa.push_back( n );
        }
        if( auto node = p->isNode() ) {
            *out = new Ast::FunctionCall( new Ast::Reference( node ), move( fa ), WITH( _.m_loc = list->m_loc ) );
        } else if( p->isOverload() ) {
            *out = new Ast::UnresolvedCall( sym->text(), p->overloads(), move( fa ), WITH( _.m_loc = list->m_loc ) );
        }
        return Result::OK;
    } else if( auto num = dynamic_cast<Lex::Number*>( atom ) ) {
        Ast::Node* te;
        RETURN_IF_FAILED( this->parse( num->m_decltype, &te ) );
        auto r = new Ast::Number( num->text(), WITH( _.m_loc = num->m_loc, _.m_declTypeExpr = te ) );
        *out = r;
        return Result::OK;
    } else if( auto str = dynamic_cast<Lex::String*>( atom ) ) {
        Ast::Node* te;
        RETURN_IF_FAILED( this->parse( str->m_decltype, &te ) );
        auto r = new Ast::String( str->text(), WITH( _.m_loc = str->m_loc, _.m_declTypeExpr = te ) );
        *out = r;
        return Result::OK;
    }
    RETURN_RES_IF( Result::ERR, true );
}

namespace {
    using namespace Ast;
    struct ExpandWalker {
        Parse::State* m_state;
        unordered_map<istring, Lex::Atom*> m_args;

        Result setArgs( const vector<Argument*>& names, Slip::Parse::Args& values ) {
            RETURN_RES_IF( Result::ERR, names.size() != values.size() );
            for( unsigned i = 0; i < names.size(); ++i ) {
                m_args.emplace( names[i]->name(), values.cur() );
                values.advance();
            }
            return Result::OK;
        }

        template <typename T>
        T* dispatch( T* n ) {
            return (T*)Ast::dispatch<Ast::Node*>( n, *this );
        }

        Ast::Node* operator()( Ast::Node* n ) {
            __debugbreak();
            return n;
        }
        Ast::Node* operator()( Ast::Number* n ) { return n; }
        Ast::Node* operator()( Ast::String* n ) { return n; }

        Ast::Node* operator()( Ast::FunctionCall* call ) {
            if( auto fref = dynamic_cast<Ast::Reference*>( call->m_func ) ) {
                if( auto decl = dynamic_cast<Ast::FunctionDecl*>( fref->m_target ) ) {
                    if( decl->name().view() == "expand"sv ) {
                        if( auto aref = dynamic_cast<Ast::Reference*>( call->m_args[0] ) ) {
                            if( auto aname = dynamic_cast<Ast::Named*>( aref->m_target ) ) {
                                auto iter = m_args.find( aname->name() );
                                if( iter != m_args.end() ) {
                                    Ast::Node* out;
                                    m_state->parse( iter->second, &out );
                                    return out;
                                }
                            }
                        }
                    }
                }
            }
            auto func = dispatch( call->m_func );
            vector<Ast::Node*> args;
            for( auto&& c : call->m_args ) {
                args.push_back( dispatch( c ) );
            }
            return new FunctionCall( func, move( args ), WITH( _.m_loc = call->m_loc ) );
        }

        Ast::Node* operator()( Ast::Cond* cond ) {
            auto ret = new Cond( WITH( _.m_loc = cond->m_loc ) );
            for( auto&& c : cond->m_cases ) {
                ret->m_cases.emplace_back( dispatch( c.first ), dispatch( c.second ) );
            }
            return ret;
        }
    };
}  // namespace

Result Parse::State::macroExpand( Ast::MacroDecl* macro, Args args, Ast::Node** out ) {
    ExpandWalker expander{this};
    RETURN_IF_FAILED( expander.setArgs( macro->m_args, args ) );
    *out = Ast::dispatch<Ast::Node*>( macro->m_body, expander );
    return Result::OK;
}

struct Parse::Define : Parse::Parser {
    Result _parse( State* state, Args& args, Ast::Node** out ) const override {
        Lex::Symbol* lname;
        Lex::Atom* lval;
        RETURN_IF_FAILED( matchLex( args, &lname, &lval ) );
        Ast::Node* nval;
        RETURN_IF_FAILED( state->parse( lval, &nval ) );

        auto ret = new Ast::Definition( lname->text(), nval, WITH( _.m_loc = lname->m_loc ) );
        state->addSym( lname->text(), ret );

        *out = ret;
        return Result::OK;
    }
};

struct Parse::Func : Parse::Parser {
    Result _parse( State* state, Args& args, Ast::Node** out ) const override {
        *out = nullptr;
        Lex::Symbol* lname;
        Lex::List* largs;
        Lex::Atom* lbody;
        RETURN_IF_FAILED( matchLex( args, &lname, &largs, &lbody ) );
        auto func = new Ast::FunctionDecl( lname->text(), WITH( _.m_loc = lname->m_loc ) );
        state->addSym( func->m_name, func );
        state->enterScope();
        if( auto a = largs->m_decltype ) {
            Ast::Node* te;
            RETURN_IF_FAILED( state->parse( a, &te ) );
            func->m_declReturnTypeExpr = te;
        }
        for( auto item : largs->items() ) {
            auto sym = state->symbol( item );
            RETURN_RES_IF( Result::ERR, sym == nullptr );
            Ast::Node* te;
            RETURN_IF_FAILED( state->parse( item->m_decltype, &te ) );
            auto arg = new Ast::Argument( sym->text(), WITH( _.m_loc = sym->m_loc, _.m_declTypeExpr = te ) );
            func->m_args.push_back( arg );
        }
        // TODO check unique names
        for( auto a : func->m_args ) {
            state->addSym( a->m_name, a );
        }
        Ast::Node* body;
        RETURN_IF_FAILED( state->parse( lbody, &body ) );
        state->leaveScope();
        func->m_body = body;
        *out = func;
        return Result::OK;
    }
};

struct Parse::Let : Parse::Parser {
    Result _parse( State* state, Args& args, Ast::Node** out ) const override {
        Lex::Symbol* lname;
        Lex::Atom* lbody;
        if( matchLex( args, &lname, &lbody ).isOk() ) {  // short form (let foo 0)

            Ast::Node* aval;
            RETURN_IF_FAILED( state->parse( lbody, &aval ) );
            Ast::Node* te;
            RETURN_IF_FAILED( state->parse( lname->m_decltype, &te ) );
            auto def = new Ast::Definition( lname->text(), aval, WITH( _.m_loc = lbody->m_loc, _.m_declTypeExpr = te ) );
            state->addSym( lname->text(), def );
            *out = def;
            return Result::OK;
        }

        Lex::List* llets;  // long form (let ((a 0) (b 1)) (+ a b))
        RETURN_IF_FAILED( matchLex( args, &llets, &lbody ) );

        state->enterScope();
        auto seq = new Ast::Sequence();
        for( auto litem : llets->items() ) {
            Lex::List* lpair = dynamic_cast<Lex::List*>( litem );
            RETURN_RES_IF( Result::ERR, !lpair );
            Lex::Symbol* lsym;
            Lex::Atom* lval;
            Args tmpargs{lpair->items()};
            RETURN_IF_FAILED( matchLex( tmpargs, &lsym, &lval ) );
            Ast::Node* aval;
            RETURN_IF_FAILED( state->parse( lval, &aval ) );
            Ast::Node* te;
            RETURN_IF_FAILED( state->parse( lsym->m_decltype, &te ) );

            auto def = new Ast::Definition( lsym->text(), aval, WITH( _.m_loc = lsym->m_loc, _.m_declTypeExpr = te ) );
            state->addSym( lsym->text(), def );
            seq->m_items.push_back( def );
        }
        Ast::Node* abody;
        RETURN_IF_FAILED( state->parse( lbody, &abody ) );
        seq->m_items.push_back( abody );
        state->leaveScope();

        *out = seq;
        return Result::OK;
    }
};

struct Parse::If : Parse::Parser {
    Result _parse( State* state, Args& args, Ast::Node** out ) const override {
        Lex::Atom* lcond;
        Lex::Atom* ltrue;
        Lex::Atom* lfalse;
        RETURN_IF_FAILED( matchLex( args, &lcond, &ltrue, &lfalse ) );

        Ast::Node* ncond;
        Ast::Node* ntrue;
        Ast::Node* nfalse;
        RETURN_IF_FAILED( state->parse( lcond, &ncond ) );
        RETURN_IF_FAILED( state->parse( ltrue, &ntrue ) );
        RETURN_IF_FAILED( state->parse( lfalse, &nfalse ) );

        *out = new Ast::If( ncond, ntrue, nfalse, WITH( _.m_loc = lcond->m_loc ) );
        return Result::OK;
    }
};

struct Parse::While : Parse::Parser {
    Result _parse( State* state, Args& args, Ast::Node** out ) const override {
        Lex::Atom* lcond;
        Lex::Atom* lbody;
        RETURN_IF_FAILED( matchLex( args, &lcond, &lbody ) );

        Ast::Node* ncond;
        Ast::Node* nbody;
        RETURN_IF_FAILED( state->parse( lcond, &ncond ) );
        RETURN_IF_FAILED( state->parse( lbody, &nbody ) );

        *out = new Ast::While( ncond, nbody, WITH( _.m_loc = lcond->m_loc ) );
        return Result::OK;
    }
};

struct Parse::Cond : Parse::Parser {
    Result _parse( State* state, Args& args, Ast::Node** out ) const override {
        *out = nullptr;
        RETURN_RES_IF( Result::ERR, args.size() < 1 );
        vector<pair<Ast::Node*, Ast::Node*>> cases;
        for( auto&& arg : args ) {
            auto pair = dynamic_cast<Lex::List*>( arg );
            RETURN_RES_IF( Result::ERR, pair == nullptr );
            RETURN_RES_IF( Result::ERR, pair->size() != 2 );

            Ast::Node* cond;
            RETURN_IF_FAILED( state->parse( pair->at( 0 ), &cond ) );
            Ast::Node* iftrue;
            RETURN_IF_FAILED( state->parse( pair->at( 1 ), &iftrue ) );
            cases.emplace_back( cond, iftrue );
        }
        auto cond = new Ast::Cond( WITH( /*TODO loc*/ ) );
        cond->m_cases.swap( cases );
        *out = cond;
        return Result::OK;
    }
};

struct Parse::Begin : Parse::Parser {
    Result _parse( State* state, Args& args, Ast::Node** out ) const override {
        *out = nullptr;
        auto ret = new Ast::Sequence;
        for( auto arg : args ) {
            Ast::Node* n;
            RETURN_IF_FAILED( state->parse( arg, &n ) );
            ret->m_items.push_back( n );
        }
        *out = ret;
        return Result::OK;
    }
};

struct Parse::Var : Parse::Parser {
    Result _parse( State* state, Args& args, Ast::Node** out ) const override {
        *out = nullptr;
        RETURN_RES_IF( Result::ERR, args.size() < 1 );
        RETURN_RES_IF( Result::ERR, args.size() > 2 );
        Lex::Symbol* sym;
        Ast::Node* expr{nullptr};
        if( args.size() == 1 ) {
            RETURN_IF_FAILED( matchLex( args, &sym ) );
        } else {
            Lex::Atom* init;
            RETURN_IF_FAILED( matchLex( args, &sym, &init ) );
            RETURN_IF_FAILED( state->parse( init, &expr ) );
        }
        Ast::Node* te;
        RETURN_IF_FAILED( state->parse( sym->m_decltype, &te ) );
        auto ret = new Ast::VariableDecl( istring::make( sym->text() ), WITH( _.m_declTypeExpr = te, _.m_loc = sym->m_loc ) );
        ret->m_initializer = expr;
        state->addSym( sym->text(), ret );
        *out = ret;
        return Result::OK;
    }
};

struct Parse::Set : Parse::Parser {
    Result _parse( State* state, Args& args, Ast::Node** out ) const override {
        *out = nullptr;
        Lex::Symbol* sym;
        Lex::Atom* expr;
        RETURN_IF_FAILED( matchLex( args, &sym, &expr ) );
        State::SymbolValue const* value = nullptr;
        RETURN_IF_FAILED( state->lookup( sym->text(), &value ) );
        RETURN_RES_IF( Result::ERR, !value->isNode() );
        Ast::Node* rhs;
        RETURN_IF_FAILED( state->parse( expr, &rhs ) );
        *out = new Ast::Assignment( value->isNode(), rhs, WITH( _.m_loc = sym->m_loc ) );
        return Result::OK;
    }
};

struct Parse::Macro : Parse::Parser {
    Result _parse( State* state, Args& args, Ast::Node** out ) const override {
        *out = nullptr;
        Lex::Symbol* lname;
        Lex::List* largs;
        Lex::Atom* lbody;
        RETURN_IF_FAILED( matchLex( args, &lname, &largs, &lbody ) );
        auto macro = new Ast::MacroDecl( lname->text(), WITH( _.m_loc = lname->m_loc ) );
        state->addSym( macro->m_name, macro );
        state->enterScope();
        for( auto item : largs->items() ) {
            auto sym = dynamic_cast<Lex::Symbol*>( item );
            RETURN_RES_IF( Result::ERR, sym == nullptr );
            auto arg = new Ast::Argument( sym->text(), WITH( _.m_loc = sym->m_loc ) );
            macro->m_args.push_back( arg );
        }
        for( auto a : macro->m_args ) {
            state->addSym( a->m_name, a );
        }
        Ast::Node* body;
        RETURN_IF_FAILED( state->parse( lbody, &body ) );
        state->leaveScope();
        macro->m_body = body;
        *out = macro;
        return Result::OK;
    }
};

template <typename... Args>
static Ast::Type* _makeFuncType( string_view name, Args&&... args ) {
    auto r = new Ast::Type( name );
    r->m_callable = {args...};
    return r;
}

static Result _makeArrayViewType( array_view<Ast::Node*> args, Ast::Node** out ) {
    assert( args.size() == 1 );
    auto type = dynamic_cast<Ast::Type*>( args[0] );
    assert( type );
    auto name = string_format( "array_view<%s>", type->name().c_str() );
    auto r = new Ast::Type( name );
    *out = r;
    return Result::OK;
}

static Result _makeArrayStaticType( array_view<Ast::Node*> args, Ast::Node** out ) {
    assert( args.size() == 1 );
    auto type = dynamic_cast<Ast::Type*>( args[0] );
    assert( type );
    auto name = string_format( "array_static<%s>", type->name().c_str() );
    auto r = new Ast::Type( name );
    *out = r;
    return Result::OK;
}

Slip::unique_ptr_del<Ast::Module> Parse::module( Lex::List& lex ) {
    State state;
    state.addParser( "define", new Define() );
    state.addParser( "func", new Func() );
    state.addParser( "if", new If() );
    state.addParser( "while", new While() );
    state.addParser( "cond", new Cond() );
    state.addParser( "let", new Let() );
    state.addParser( "begin", new Begin() );
    state.addParser( "var", new Var() );
    state.addParser( "set!", new Set() );
    state.addParser( "macro", new Macro() );
    state.addSym( "int", &Ast::s_typeInt );
    state.addSym( "float", &Ast::s_typeFloat );
    state.addSym( "double", &Ast::s_typeDouble );
    state.addSym( "void", &Ast::s_typeVoid );
    state.addSym( "string", &Ast::s_typeString );
    state.addSym( "bool", &Ast::s_typeBool );

    state.addSym( "true", new Ast::Number( "true", WITH( _.m_type = &Ast::s_typeBool ) ) );
    state.addSym( "false", new Ast::Number( "false", WITH( _.m_type = &Ast::s_typeBool ) ) );

    auto b_ii = _makeFuncType( "(int, int)->bool", &Ast::s_typeBool, &Ast::s_typeInt, &Ast::s_typeInt );
    auto i_ii = _makeFuncType( "(int, int)->int", &Ast::s_typeInt, &Ast::s_typeInt, &Ast::s_typeInt );
    auto v_s = _makeFuncType( "(string)->void", &Ast::s_typeVoid, &Ast::s_typeString );
    auto v_i = _makeFuncType( "(int)->void", &Ast::s_typeVoid, &Ast::s_typeInt );
    auto v_d = _makeFuncType( "(double)->void", &Ast::s_typeVoid, &Ast::s_typeDouble );
    auto d_dd = _makeFuncType( "(double, double)->double", &Ast::s_typeDouble, &Ast::s_typeDouble, &Ast::s_typeDouble );
    auto d_i = _makeFuncType( "(int)->double", &Ast::s_typeDouble, &Ast::s_typeInt );
    auto v_ss = _makeFuncType( "(string, string)->void", &Ast::s_typeVoid, &Ast::s_typeString, &Ast::s_typeString );
    auto t_t = _makeFuncType( "(type)->type", &Ast::s_typeType, &Ast::s_typeType );
    auto v_v = _makeFuncType( "(void)->void", &Ast::s_typeVoid, &Ast::s_typeVoid );
    state.addIntrinsic( "eq?", b_ii );
    state.addIntrinsic( "lt?", b_ii );
    state.addIntrinsic( "add", i_ii );
    state.addIntrinsic( "sub", i_ii );
    state.addIntrinsic( "puts", v_s );
    state.addIntrinsic( "puti", v_i );
    state.addIntrinsic( "putd", v_d );
    state.addIntrinsic( "addd", d_dd );
    state.addIntrinsic( "divd", d_dd );
    state.addIntrinsic( "dfromi", d_i );
    state.addIntrinsic( "strcat!", v_ss );
    state.addIntrinsic( "expand", v_v );
    {
        auto f = state.addIntrinsic( "array_view", t_t );
        f->m_intrinsic = _makeArrayViewType;
    }
    {
        auto f = state.addIntrinsic( "array_static", t_t );
        f->m_intrinsic = _makeArrayStaticType;
    }

    auto module = make_unique_del<Ast::Module>();
    for( auto c : lex.items() ) {
        Ast::Node* n;
        THROW_IF_FAILED( state.parse( c, &n ), "Failed to parse" );
        module->m_items.push_back( n );
    }
    return module;
}

Result Parse::Parser::parse( State* state, Args& args, Ast::Node** out ) const { return _parse( state, args, out ); }
