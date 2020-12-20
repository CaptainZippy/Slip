#include "slip/pch/Pch.h"

#include "Ast.h"
#include "Errors.h"
#include "Io.h"

namespace Slip::Parse {
    extern Slip::Result ResultT_instantiate( Ast::Type* t, Ast::Type** out );
}

namespace Slip::Sema {

    struct TypeInfo;
    struct FuncInfo;
    struct ConstraintBuilder;

    struct FuncInfo {
        TypeInfo* ret{nullptr};
        vector<TypeInfo*> params;
    };

    struct TypeInfo {
       private:
        friend struct ConstraintBuilder;
        Ast::Type* type{nullptr};

        int triggerCount{0};
        std::vector<TypeInfo*> triggerNotify;
        std::function<void( TypeInfo* )> triggerAction;

        FuncInfo* func{nullptr};  // TODO: Union?
       public:
        auto get_type() const {
            assert( type );
            return type;
        }
        auto get_func() const {
            assert( func );
            return func;
        }
    };

    struct VisitInfo {
        VisitInfo() = delete;
        VisitInfo( Ast::Expr* n, TypeInfo* t ) : node( n ), info( t ) {}
        Ast::Expr* node{nullptr};
        TypeInfo* info{nullptr};
    };

    struct ConstraintBuilder {
        Result operator()( Ast::Expr* n, VisitInfo& vi ) {
            RETURN_ERROR_IF( vi.info == nullptr, Error::InternalUnknownSemantic, n->m_loc, "Expression is of kind '%s'",
                             n->dynamicType()->name );
            return Result::OK;
        }

        Result operator()( Ast::LexNode* n, VisitInfo& vi ) {
            RETURN_ERROR( Error::InternalUnexpandedLexNode, n->m_loc );
            return Result::OK;
        }

        Result operator()( Ast::CatchExpr* n, VisitInfo& vi ) {
            TypeInfo* expri;
            RETURN_IF_FAILED( dispatch( n->m_expr, &expri ) );
            auto type = expri->get_type();
            RETURN_ERROR_IF( type->m_sum.size() != 2, Error::Fixme, n->m_loc, "only support 2 types in sum" );
            vi.info = _internKnownType( type->m_sum[0] );  // TODO assumes error type@1
            TypeInfo* faili;
            RETURN_IF_FAILED( dispatch( n->m_fail, &faili ) );
            _isConvertible( n, vi.info, n->m_fail, faili );

            return Result::OK;
        }

        Result operator()( Ast::CoroutineYield* n, VisitInfo& vi ) {
            vi.info = _internKnownType( &Ast::s_typeVoid );  // callable?
            TypeInfo* exprinfo;
            RETURN_IF_FAILED( dispatch( n->m_expr, &exprinfo ) );
            TypeInfo* coroinfo;
            RETURN_IF_FAILED( dispatch( n->m_coro, &coroinfo ) );
            _isConvertible( n, coroinfo->get_func()->ret, n->m_expr, exprinfo );
            return Result::OK;
        }

        Result operator()( Ast::CoroutineDecl* n, VisitInfo& vi ) {
            if( n->m_type ) {  // intrinsic?
                vi.info = _internKnownType( n->m_type );
            } else {
                vi.info = new TypeInfo{};
                auto ret = _evalTypeExpr( n->m_declReturnTypeExpr );
                std::vector<TypeInfo*> params;
                for( auto&& p : n->m_params ) {
                    TypeInfo* t;
                    RETURN_IF_FAILED( dispatch( p, &t ) );
                    params.emplace_back( t );
                }
                auto v_t = new Ast::Type( "auto"sv );  // FIXME.coro
                v_t->m_callCanFail = true;
                v_t->m_callable.push_back( ret->get_type() );
                auto ti_v_t = _internKnownType( v_t );
                _isFunction( vi.info, ti_v_t, std::move( params ) );
                TypeInfo* _;
                RETURN_IF_FAILED( dispatch( n->m_body, &_ ) );
            }
            return Result::OK;
        }

        Result operator()( Ast::Module* n, VisitInfo& vi ) {
            vi.info = _internKnownType( &Ast::s_typeVoid );  // Todo: proper type
            for( auto i : n->items() ) {
                TypeInfo* t;
                RETURN_IF_FAILED( dispatch( i, &t ) );
            }
            return Result::OK;
        }

        Result operator()( Ast::Nop* n, VisitInfo& vi ) {
            vi.info = _internKnownType( &Ast::s_typeVoid );
            return Result::OK;
        }

        Result operator()( Ast::Number* n, VisitInfo& vi ) {
            if( auto t = n->m_type ) {
                vi.info = _internKnownType( t );
            } else {
                vi.info = _evalTypeExpr( n->m_declTypeExpr );
            }
            return Result::OK;
        }

        Result operator()( Ast::String* n, VisitInfo& vi ) {
            vi.info = _internKnownType( &Ast::s_typeString );
            return Result::OK;
        }

        Result operator()( Ast::StructDecl* n, VisitInfo& vi ) {
            auto type = new Ast::Type( n->name() );
            for( auto&& f : n->m_fields ) {
                auto i = _evalTypeExpr( f->m_declTypeExpr );
                f->m_type = i->get_type();
            }
            type->m_struct = n;
            vi.info = _internKnownType( type );
            return Result::OK;
        }

        Result operator()( Ast::FunctionCall* n, VisitInfo& vi ) {
            TypeInfo* fi;
            RETURN_IF_FAILED( dispatch( n->m_func, &fi ) );
            std::vector<VisitInfo> ai;
            for( auto&& a : n->m_args ) {
                TypeInfo* t;
                RETURN_IF_FAILED( dispatch( a, &t ) );
                ai.emplace_back( a, t );
            }
            if( !fi->func ) {  // TODO extract method
                auto text = n->m_loc.text();
                RETURN_ERROR_IF( !fi->func, Error::CallNonCallable, n->m_loc, "%.*s", text.size(), text.begin() );
            }
            _isApplicable( n->m_func, fi, ai );
            vi.info = fi->get_func()->ret;
            return Result::OK;
        }

        Result operator()( Ast::Parameter* n, VisitInfo& vi ) {
            vi.info = _evalTypeExpr( n->m_declTypeExpr );
            return Result::OK;
        }

        Result operator()( Ast::MacroDecl* n, VisitInfo& vi ) {
            vi.info = _evalTypeExpr( &Ast::s_typeVoid );
            return Result::OK;
        }

        Result operator()( Ast::MacroExpansion* n, VisitInfo& vi ) {
            RETURN_IF_FAILED( dispatch( n->m_expansion, &vi.info ) );
            return Result::OK;
        }

        Result operator()( Ast::Sequence* n, VisitInfo& vi ) {
            if( n->m_items.empty() ) {
                vi.info = _internKnownType( &Ast::s_typeVoid );
            } else {
                for( auto&& i : n->m_items ) {
                    RETURN_IF_FAILED( dispatch( i, &vi.info ) );
                }
            }
            return Result::OK;
        }

        Result operator()( Ast::Block* n, VisitInfo& vi ) {
            vi.info = new TypeInfo();
            TypeInfo* ci;
            RETURN_IF_FAILED( dispatch( n->m_contents, &ci ) );
            _isConvertible( n, vi.info, n->m_contents, ci );
            return Result::OK;
        }

        Result operator()( Ast::Break* n, VisitInfo& vi ) {
            TypeInfo* blockinfo;
            RETURN_IF_FAILED( dispatch( n->m_target, &blockinfo ) );  // TODO verify already done
            TypeInfo* valinfo;
            RETURN_IF_FAILED( dispatch( n->m_value, &valinfo ) );
            _isConvertible( n->m_target, blockinfo, n->m_value, valinfo );
            vi.info = _internKnownType( &Ast::s_typeVoid );
            return Result::OK;
        }

        Result operator()( Ast::NamedFunctionCall* n, VisitInfo& vi ) {
            // dispatch to arguments
            std::vector<TypeInfo*> args;
            for( auto&& a : n->m_args ) {
                TypeInfo* t;
                RETURN_IF_FAILED( dispatch( a, &t ) );
                args.emplace_back( t );
            }
            struct Candidate {
                Candidate( Ast::Expr* n ) : node( n ) {}
                Ast::Expr* node;
                TypeInfo* info{};
                int score{};
            };

            std::vector<Candidate> candidates;
            for( auto&& n : n->m_candidates ) {
                candidates.emplace_back( n );
            }
            // add extra candidates from arg[0] if it exists
            if( args.empty() == false && args[0] ) {
                if( auto ty = args[0]->type ) {  // TODO move
                    for( auto&& f : ty->m_methods ) {
                        if( f->m_name == n->m_name ) {
                            candidates.emplace_back( f );
                        }
                    }
                }
            }

            enum Penalty {
                NotCallable = 100000,
                WrongNumArgs = 10000,
                WrongType = 1000,
                NeedsUnwrap = 10,
            };

            Candidate* bestCandidate = nullptr;
            int bestScore = 100000 + 1;
            for( auto& c : candidates ) {
                RETURN_IF_FAILED( dispatch( c.node, &c.info ) );
                if( auto f = c.info->func ) {
                    if( f->params.size() != args.size() ) {
                        c.score += WrongNumArgs;
                    }
                    auto narg = min2( f->params.size(), args.size() );
                    for( size_t i = 0; i < narg; ++i ) {
                        auto paramType = f->params[i]->type;
                        auto argType = args[i]->type;
                        // TODO order of overload resolution vs inference
                        if( argType && paramType != argType ) {
                            if( m_autoUnwrap.back() && argType->m_sum.size() && argType->m_sum[0] == paramType ) {
                                c.score += NeedsUnwrap;
                            } else {
                                c.score += WrongType;
                            }
                        }
                    }
                } else {
                    c.score += NotCallable;
                }
                if( c.score < bestScore ) {
                    bestCandidate = &c;
                    bestScore = c.score;
                }
            }
            if( bestCandidate == nullptr || bestCandidate->score >= WrongType ) {
                std::string proto = string_concat( n->name(), "( " );
                const char* sep = "";
                for( auto&& a : args ) {
                    proto += sep;
                    proto += a->get_type()->name();
                    sep = ", ";
                }
                Result::failed( Error::OverloadResolutionFailed, n->m_loc, "For function call '%s)", proto.c_str() );
                Slip::diagnostic( "Candidates are:\n" );
                for( auto&& c : candidates ) {
                    auto n = dynamic_cast<Ast::Named*>( c.node );
                    auto av = make_array_view( n->m_type->m_callable );
                    std::string x = string_concat( av[0]->name().c_str(), " ", n ? n->name().c_str() : "?", "(" );
                    sep = "";
                    for( auto&& a : av.ltrim( 1 ) ) {
                        x += string_concat( sep, a->name() );
                        sep = ", ";
                    }
                    Result::failed( Error::Continued, c.node->m_loc, "%s)", x.c_str() );
                }
                return Error::OverloadResolutionFailed;
            }

            auto fargs = n->m_args;
            n->m_resolved = new Ast::FunctionCall( new Ast::Reference( bestCandidate->node ), std::move( fargs ),
                                                   [&]( auto& _ ) { _.m_loc = n->m_loc; } );
            RETURN_IF_FAILED( dispatch( n->m_resolved, &vi.info ) );
            return Result::OK;
        }

        Result operator()( Ast::FunctionDecl* n, VisitInfo& vi ) {
            if( n->m_type ) {  // intrinsic?
                vi.info = _internKnownType( n->m_type );
            } else {
                vi.info = new TypeInfo{};
                auto ret = _evalTypeExpr( n->m_declReturnTypeExpr );
                std::vector<TypeInfo*> params;
                for( auto&& p : n->m_params ) {
                    TypeInfo* t;
                    RETURN_IF_FAILED( dispatch( p, &t ) );
                    params.emplace_back( t );
                }
                _isFunction( vi.info, ret, std::move( params ) );
                if( n->m_body ) {
                    TypeInfo* bt;
                    RETURN_IF_FAILED( dispatch( n->m_body, &bt ) );
                    _isConvertible( n, ret, n->m_body, bt );
                }
            }
            return Result::OK;
        }

        Result operator()( Ast::VariableDecl* n, VisitInfo& vi ) {
            TypeInfo* varTypeInfo{nullptr};
            if( n->m_type ) {  // known?
                varTypeInfo = _internKnownType( n->m_type );
            } else if( auto te = n->m_declTypeExpr ) {
                varTypeInfo = _evalTypeExpr( te );
            } else if( n->m_initializer.size() == 1 ) {
                RETURN_IF_FAILED( dispatch( n->m_initializer[0], &vi.info ) );
                return Result::OK;
            } else {
                assert( false );
                vi.info = new TypeInfo{};
            }

            assert( varTypeInfo );
            vi.info = varTypeInfo;
            auto varType = vi.info->get_type();
            if( auto et = varType->m_array ) {
                auto tt = _internKnownType( et );
                for( auto&& i : n->m_initializer ) {
                    TypeInfo* d;
                    RETURN_IF_FAILED( dispatch( i, &d ) );
                    _isConvertible( et, tt, i, d );
                }
            } else if( auto decl = varType->m_struct ) {
                RETURN_IF_FAILED( decl->m_fields.size() != n->m_initializer.size() );
                for( auto i = 0ul; i < decl->m_fields.size(); ++i ) {
                    auto ft = decl->m_fields[i]->m_type;
                    auto fi = _internKnownType( ft );
                    auto iv = n->m_initializer[i];
                    TypeInfo* id;
                    RETURN_IF_FAILED( dispatch( iv, &id ) );
                    _isConvertible( ft, fi, iv, id );
                }
            } else {
                RETURN_ERROR_IF( n->m_initializer.size() != 1, Error::ExpectedOneInitializer, n->m_loc );
                TypeInfo* ti;
                RETURN_IF_FAILED( dispatch( n->m_initializer[0], &ti ) );
                _isConvertible( n, varTypeInfo, n->m_initializer[0], ti );
            }
            return Result::OK;
        }

        Result operator()( Ast::Assignment* n, VisitInfo& vi ) {
            TypeInfo* tr;
            RETURN_IF_FAILED( dispatch( n->m_rhs, &tr ) );
            TypeInfo* tl;
            RETURN_IF_FAILED( dispatch( n->m_lhs, &tl ) );
            _isConvertible( n->m_lhs, tl, n->m_rhs, tr );
            // TODO check lhs is assignable
            vi.info = tl;
            return Result::OK;
        }

        Result operator()( Ast::Reference* n, VisitInfo& vi ) {
            RETURN_IF_FAILED( dispatch( n->m_target, &vi.info ) );
            return Result::OK;
        }

        Result operator()( Ast::Scope* n, VisitInfo& vi ) {
            RETURN_IF_FAILED( dispatch( n->m_child, &vi.info ) );
            return Result::OK;
        }

        Result operator()( Ast::Definition* n, VisitInfo& vi ) {
            assert( false );
            // isa(n, &Ast::s_typeVoid); //fixme
            //            if (n->m_type) {
            //                isa(n->m_value, n);
            //            }
            ////            else {
            //                isa(n, n->m_value);
            //            }
            // dispatch(n->m_value);
            return Result::OK;
        }

        Result operator()( Ast::If* n, VisitInfo& vi ) {
            // condition is a boolean
            TypeInfo* ci;
            RETURN_IF_FAILED( dispatch( n->m_cond, &ci ) );
            _isConvertible( n, _internKnownType( &Ast::s_typeBool ), n->m_cond, ci );
            // two legs have a common type
            TypeInfo* ti;
            RETURN_IF_FAILED( dispatch( n->m_true, &ti ) );
            TypeInfo* fi;
            RETURN_IF_FAILED( dispatch( n->m_false, &fi ) );
            vi.info = new TypeInfo{};
            _isConvertible( n, vi.info, {{n->m_true, ti}, {n->m_false, fi}} );
            return Result::OK;
        }

        Result operator()( Ast::While* n, VisitInfo& vi ) {
            // condition is a boolean
            TypeInfo* ci;
            RETURN_IF_FAILED( dispatch( n->m_cond, &ci ) );
            _isConvertible( n, _internKnownType( &Ast::s_typeBool ), n->m_cond, ci );
            RETURN_IF_FAILED( dispatch( n->m_body, &vi.info ) );
            return Result::OK;
        }

        Result operator()( Ast::Cond* n, VisitInfo& vi ) {
            RETURN_ERROR_IF( n->m_cases.empty(), Error::CondWithNoCases, n->m_loc );
            vi.info = new TypeInfo{};
            for( auto&& c : n->m_cases ) {
                // conditions are all booleans
                TypeInfo* ft;
                RETURN_IF_FAILED( dispatch( c.first, &ft ) );
                _isConvertible( n, _internKnownType( &Ast::s_typeBool ), c.first, ft );
                // expressions convert to a common type
                TypeInfo* st;
                RETURN_IF_FAILED( dispatch( c.second, &st ) );
                _isConvertible( n, vi.info, c.second, st );
            }
            return Result::OK;
        }

        Result operator()( Ast::PipelineExpr* n, VisitInfo& vi ) {
            RETURN_ERROR_IF( n->m_stages.empty(), Error::PipeWithNoStages, n->m_loc );
            Ast::Expr* prev{nullptr};
            m_autoUnwrap.push_back( true );
            bool usedUnwrap{false};
            TypeInfo* ti;
            for( auto&& stage : n->m_stages ) {
                Ast::NamedFunctionCall* nf;
                if( prev ) {
                    RETURN_IF_FAILED( dynCast( stage.expr, &nf ) );
                    nf->m_args.push_back( prev );
                }
                RETURN_IF_FAILED( dispatch( stage.expr, &ti ) );
                if( ti->get_type()->m_sum.empty() ) {
                    prev = stage.expr;
                } else {
                    stage.canFail = true;
                    prev = new Ast::UnwrapResult( stage.expr );
                    prev->m_type = ti->get_type()->m_sum[0];
                    TypeInfo* t;
                    RETURN_IF_FAILED( dispatch( prev, &t ) );
                    usedUnwrap = true;
                }
            }
            m_autoUnwrap.pop_back();
            if( usedUnwrap ) {
                auto origT = ti->get_type();
                Ast::Type* thisT;
                if( origT->m_sum.empty() ) {
                    RETURN_IF_FAILED( Slip::Parse::ResultT_instantiate( origT, &thisT ) );
                } else {
                    // TODO assert ResultT
                    thisT = origT;
                }
                vi.info = _internKnownType( thisT );
            } else {
                vi.info = ti;
            }
            return Result::OK;
        }

        Result operator()( Ast::Selector* n, VisitInfo& vi ) {
            TypeInfo* lhsi;
            RETURN_IF_FAILED( dispatch( n->m_lhs, &lhsi ) );
            auto type = lhsi->get_type();
            RETURN_ERROR_IF( type->m_struct == nullptr, Error::DotNotSupportedHere, n->m_loc );
            auto rhsi = dynamic_cast<Ast::LexIdent*>( n->m_rhs );
            auto id = istring::make( rhsi->text() );
            auto decl = type->m_struct;
            auto it = std::find_if( decl->m_fields.begin(), decl->m_fields.end(), [id]( auto a ) { return a->name() == id; } );
            RETURN_ERROR_IF( it == decl->m_fields.end(), Error::DottedAttributeNotFound, n->m_loc, "'%s'", id.c_str() );
            vi.info = _internKnownType( ( *it )->m_type );
            return Result::OK;
        }

        Result operator()( Ast::TryExpr* n, VisitInfo& vi ) {
            TypeInfo* expri;
            RETURN_IF_FAILED( dispatch( n->m_expr, &expri ) );
            auto type = expri->get_type();
            RETURN_ERROR_IF( type->m_sum.size() != 2, Error::Fixme, n->m_loc, "Try requires an error sum type" );
            vi.info = _internKnownType( type->m_sum[0] );  // TODO assumes error type@1

            return Result::OK;
        }

        Result operator()( Ast::UnwrapResult* n, VisitInfo& vi ) {
            vi.info = _internKnownType( n->m_type );
            return Result::OK;
        }

       public:
        struct Convertible {
            struct Pair {
                Ast::Expr* node;
                TypeInfo* info;
                Ast::Type* type() const { return info->type; }
            };
            Convertible( Ast::Expr* ln, TypeInfo* li, Ast::Expr* rn, TypeInfo* ri ) : lhs{ln, li} { rhs.push_back( {rn, ri} ); }
            Convertible( Ast::Expr* ln, TypeInfo* li, std::initializer_list<Pair> rl ) : lhs{ln, li}, rhs( rl ) {}
            Pair lhs;
            std::vector<Pair> rhs;
            bool todo{true};
        };
        deque<VisitInfo> m_visited;
        unordered_map<Ast::Type*, TypeInfo*> m_knownTypes;
        std::vector<Ast::Type*> m_functionTypes;
        vector<Convertible> m_convertible;
        std::vector<bool> m_autoUnwrap{false};

        Result build( Ast::Expr* node ) {
            TypeInfo* t;
            RETURN_IF_FAILED( dispatch( node, &t ) );
            return Result::OK;
        }

        Result solve() {
            while( true ) {
                bool progress = false;
                // Try propogating types RIGHT TO LEFT (in terms of assignments)
                // until we fail to make progress.
                // e.g. auto lhs = rhs; where rhs has a known type.
                for( auto&& c : m_convertible ) {
                    // If all the rhs (usually 1, more for if/switch)
                    // has a single type, we can copy it to the lhs
                    if( c.todo && c.lhs.type() == nullptr && c.rhs[0].type() ) {
                        bool allsame = true;
                        for( auto&& e : c.rhs ) {
                            if( e.type() != c.rhs[0].type() ) {
                                allsame = false;
                                break;
                            }
                        }
                        if( allsame ) {
                            progress = true;
                            _resolveType( c.lhs.info, c.rhs[0].type() );
                            c.todo = false;
                        }
                    }
                }
                // No progress made RIGHT TO LEFT, try ONE step LEFT TO RIGHT
                // before going back to the upper loop.
                // void f(int); auto x; f(x); Means x is an int.
                if( !progress ) {
                    progress = false;
                    for( auto&& c : m_convertible ) {
                        // If we know the left type
                        if( c.todo && c.lhs.type() ) {
                            std::vector<TypeInfo*> nulls;
                            // If there are a mixture of matching & null types,
                            // we promote nulls to matching
                            for( auto&& e : c.rhs ) {
                                if( e.type() == nullptr ) {
                                    nulls.push_back( e.info );
                                } else if( e.type() != c.lhs.type() ) {
                                    nulls.clear();
                                    break;
                                }
                            }
                            // Some work to do?
                            if( nulls.size() ) {
                                progress = true;
                                for( auto&& n : nulls ) {
                                    _resolveType( n, c.lhs.type() );
                                }
                                c.todo = false;
                                break;
                            }
                        }
                    }
                    if( !progress ) {
                        break;
                    }
                }
            }
            // Check we satisfied all constraints
            for( auto i = 0ul; i < m_convertible.size(); ++i ) {
                auto& c = m_convertible[i];
                for( auto&& r : c.rhs ) {
                    RETURN_ERROR_IF( !canImplicitlyConvert( c.lhs.type(), r.type() ), Error::InternalTypeConstraintFailed, r.node->m_loc );
                }
            }
            // And write our results back into the nodes
            for( auto&& v : m_visited ) {
                assert( v.node );
                auto& loc = v.node->m_loc;
                RETURN_ERROR_IF( v.info->type == nullptr, Error::TypeNotDeduced, loc, "Near '%.*s'", loc.text().size(),
                                 loc.text().begin() );
                if( v.node->m_type ) {
                    assert( v.node->m_type == v.info->type );  // type already assigned, ensure it matches deduced
                } else {
                    assert( v.info->type );
                    v.node->m_type = v.info->type;
                }
            }
            return Result::OK;
        }  // namespace Slip::Sema

        bool canImplicitlyConvert( Ast::Type* lhs, Ast::Type* rhs ) {
            if( lhs == rhs )
                return true;
            if( lhs == &Ast::s_typeVoid )
                return true;
            if( lhs->name().view() == "auto"sv )
                return true;
            for( auto&& s : lhs->m_sum ) {
                if( s == rhs )
                    return true;
            }
            return false;
        }

        Result dispatch( Ast::Expr* top, TypeInfo** out ) {
            auto& i = top->m_userData;
            if( i >= m_visited.size() || m_visited[i].node != top ) {
                top->m_userData = m_visited.size();
                auto& vi = m_visited.emplace_back( top, (TypeInfo*)nullptr );
                RETURN_IF_FAILED( Ast::dispatch<Result>( top, *this, vi ) );
                assert( vi.node );
                assert( vi.info );
            }
            *out = m_visited[i].info;
            return Result::OK;
        }

        ConstraintBuilder() {}

        ~ConstraintBuilder() {}

       protected:
        void _resolveType( TypeInfo* ti, Ast::Type* ty ) {
            ti->type = ty;
            for( auto&& a : ti->triggerNotify ) {
                assert( a->triggerCount > 0 );
                a->triggerCount -= 1;
                if( a->triggerCount == 0 ) {
                    ( a->triggerAction )( a );
                }
            }
        }

        void _isConvertible( Ast::Expr* bnode, TypeInfo* base, Ast::Expr* dnode, TypeInfo* derived ) {
            assert( bnode );
            assert( dnode );
            m_convertible.emplace_back( bnode, base, dnode, derived );
        }
        void _isConvertible( Ast::Expr* bnode, TypeInfo* base, std::initializer_list<Convertible::Pair> rhs ) {
            assert( bnode );
            for( auto&& a : rhs ) {
                assert( a.node );
            }
            m_convertible.emplace_back( bnode, base, rhs );
        }

        TypeInfo* _internKnownType( Ast::Type* t ) {
            assert( t );
            auto it = m_knownTypes.emplace( t, nullptr );
            if( it.second ) {
                auto ti = new TypeInfo;
                _resolveType( ti, t );
                it.first->second = ti;
                auto& call = t->m_callable;
                if( !call.empty() ) {  // function type?
                    auto f = new FuncInfo;
                    ti->func = f;
                    f->ret = _internKnownType( call[0] );
                    for( auto&& a : array_view( call ).ltrim( 1 ) ) {
                        f->params.emplace_back( _internKnownType( a ) );
                    }
                }
            }
            return it.first->second;
        }

        TypeInfo* _evalTypeExpr( Ast::Expr* te ) {
            if( !te ) {
                return new TypeInfo{};
            }
            if( auto t = dynamic_cast<Ast::Type*>( te ) ) {
                return _internKnownType( t );
            } else if( auto r = dynamic_cast<Ast::Reference*>( te ) ) {
                if( auto t = dynamic_cast<Ast::Type*>( r->m_target ) ) {
                    return _internKnownType( t );
                } else if( auto s = dynamic_cast<Ast::StructDecl*>( r->m_target ) ) {
                    return _internKnownType( s->m_type );
                } else {
                    assert( false );
                }
            } else if( auto call = dynamic_cast<Ast::FunctionCall*>( te ) ) {
                auto fnode = call->m_func;
                if( auto r = dynamic_cast<Ast::Reference*>( fnode ) ) {
                    fnode = r->m_target;
                }
                auto func = dynamic_cast<Ast::FunctionDecl*>( fnode );
                assert( func );
                Ast::Expr* ret;
                ( func->m_intrinsic )( call->m_args, &ret );
                auto type = dynamic_cast<Ast::Type*>( ret );
                assert( type );
                return _internKnownType( type );
            } else if( auto named = dynamic_cast<Ast::NamedFunctionCall*>( te ) ) {
                std::vector<Ast::Expr*> candidates = named->m_candidates;
                if( candidates.size() != 1 ) {
                    Result::failed( Error::UnresolvedCall, named->m_loc, "%s", named->name().c_str() );
                }
                auto func = dynamic_cast<Ast::FunctionDecl*>( candidates[0] );
                assert( func );
                Ast::Expr* ret;
                ( func->m_intrinsic )( named->m_args, &ret );
                auto type = dynamic_cast<Ast::Type*>( ret );
                assert( type );
                return _internKnownType( type );
            }
            assert( false );
            return nullptr;
        }

        void _buildFunctionType( TypeInfo* ti ) {
            assert( ti->type == nullptr );
            std::vector<Ast::Type*> args;
            FuncInfo* f = ti->func;
            args.emplace_back( f->ret->get_type() );
            for( auto&& a : f->params ) {
                args.emplace_back( a->get_type() );
            }
            for( auto&& ft : m_functionTypes ) {
                if( std::equal( ft->m_callable.begin() + 1, ft->m_callable.end(), args.begin(), args.end() ) ) {
                    ti->type = ft;
                    return;
                }
            }
            // create new
            string name;
            name.append( "(" );
            const char* sep = "";
            for( auto a : f->params ) {
                name.append( sep );
                sep = ", ";
                name.append( a->get_type()->m_name );
            }
            name.append( ") -> " );
            name.append( f->ret->get_type()->m_name );
            auto r = new Ast::Type( name );
            r->m_callable = std::move( args );
            m_functionTypes.emplace_back( r );
            _resolveType( ti, r );
        }

        void _addTriggerDep( TypeInfo* wall, TypeInfo* brick ) {
            if( brick->type == nullptr ) {
                wall->triggerCount += 1;
                brick->triggerNotify.push_back( wall );
            }
        }

        void _isFunction( TypeInfo* ti, TypeInfo* ret, vector<TypeInfo*>&& args ) {
            ti->func = new FuncInfo{ret, args};
            _addTriggerDep( ti, ret );
            for( auto&& a : args ) {
                _addTriggerDep( ti, a );
            }
            if( ti->triggerCount == 0 ) {
                _buildFunctionType( ti );
            } else {
                ti->triggerAction = [this]( TypeInfo* ti ) { _buildFunctionType( ti ); };
            }
        }

        void _isApplicable( Ast::Expr* callable, TypeInfo* ti, array_view<VisitInfo> args ) {
            auto f = ti->get_func();
            assert( f );
            assert( f->params.size() == args.size() );
            for( unsigned i = 0; i < args.size(); ++i ) {
                _isConvertible( callable, f->params[i], args[i].node, args[i].info );
            }
        }
    };  // namespace Slip::Sema

#if 0
    struct ConstraintSolver {
        Result dispatch(ConstraintBuilder& builder) {

            // apply explicit declarations in source
            for (auto&& target : builder.m_targets) {
                assert(target->type == nullptr);
                assert(target->node);
                if (auto t = target->node->m_type) {
                    target->resolve(t.get());
                }
                target = nullptr;
            }

            //
            vector<TypeInfo*> todo = builder.m_targets;
            do {
                vector<TypeInfo*> again;
                for (auto&& target : todo) {
                    //HACK: if we didn't resolve and there is exactly one type constraint
                    // then lets take the constraint as the type.
                    bool resolved = false;
                    if (target->type == nullptr) {
                        if (target->isa.size()==1) {
                            if (auto&& t = target->isa[0]->type) {
                                target->resolve( t );
                                resolved = true;
                            }
                        }
                        else { //TODO: multiple intersecting constraints
                            set<Ast::Type*> types;
                            for_each(target->isa.begin(), target->isa.end(),
                                [&types](const TypeInfo* t) { types.insert(t->type); });
                            if (types.size() == 1) {
                                if (auto t = *types.begin()) {
                                    target->resolve(t);
                                    resolved = true;
                                }
                            }
                        }

                        if (resolved == false) {
                            again.push_back(target);
                        }
                    }
                }
                if (again.size() == todo.size()) {
                    Ast::print(builder.m_topNode);
                    for (auto&& a : again) {
                        a->print();
                    }
                    RETURN_ERROR(Result::ERR, "Failed to resolve");
                }
                again.swap(todo);
            } while (todo.size());
            print(builder);

            // type checks!
            for (auto&& target : builder.m_targets) {
                for (auto is : target->isa) {
                    assert(target->type == is->type);
                }
            }

            // Write results back to the ast
            for (auto&& target : builder.m_targets) {
                if (target->node->m_type.get() != target->type) {
                    target->node->m_type = target->type;
                }
            }

            return Result::OK;
        }
    protected:

        void print(ConstraintBuilder& builder) {
            Io::TextOutput io("deps.dgml");
            io.begin("<?xml version=\"1.0\" encoding=\"utf-8\"?>\n");
            io.begin("<DirectedGraph xmlns=\"http://schemas.microsoft.com/vs/2009/dgml\">\n");
            io.begin("<Nodes>\n");
            for (auto&& src : builder.m_targets) {
                io.write(string_format("<Expr Id='0x%p' Category='Expr' Label='", src));
                io.write(src->node->dynamicType()->name);
                Ast::print(src->type, io);
                io.write("' />\n");
            }
            io.end("</Nodes>\n");
            io.begin("<Links>\n");
            for (auto&& src : builder.m_targets) {
                for (auto&& d : src->isa) {
                    io.write(string_format("<Link Source='0x%p' Target='0x%p' Category='Compatible'/>\n", src, d));
                }
            }
            /*for (auto syn : builder.m_synths) {
                io.write(string_format("<Link Source='0x%p' Target='0x%p' Category='SynthRet'/>\n", syn->target, syn->ret));
                for (auto d : syn->args) {
                    io.write(string_format("<Link Source='0x%p' Target='0x%p' Category='SynthArg'/>\n", syn->target, d));
                }
            }
            for (auto& syn : builder.m_checks) {
                io.write(string_format("<Link Source='0x%p' Target='0x%p' Category='CheckFunc'/>\n", syn->target, syn->func));
                for (auto a : syn->args) {
                    io.write(string_format("<Link Source='0x%p' Target='0x%p' Category='CheckArg'/>\n", syn->func, a));
                }
            }*/
            io.end("</Links>\n");
            io.begin("<Categories>\n");
            io.write("<Category Id=\"Compatible\"  StrokeDashArray=\"1\"/>\n");
            io.write("<Category Id=\"SynthArg\" StrokeThickness=\"3\" Stroke=\"#ff00ff00\" />\n");
            io.write("<Category Id=\"SynthRet\" StrokeThickness=\"3\" Stroke=\"#ffffff00\"/>\n");
            io.write("<Category Id=\"CheckArg\" StrokeThickness=\"1\" Stroke=\"#ff00ffff\"/>\n");
            io.end("</Categories>\n");
            io.end("</DirectedGraph>\n");
        }
    };
#endif
}  // namespace Slip::Sema

Slip::Result Slip::Sema::type_check( Slip::Ast::Expr* node ) {
    ConstraintBuilder builder;
    // RETURN_IF_FAILED(
    RETURN_IF_FAILED( builder.build( node ) );
    RETURN_IF_FAILED( builder.solve() );
    return Result::OK;
#if 0
    ConstraintSolver solver;
    ////RETURN_IF_FAILED
    solver.dispatch(builder);
#endif
}
