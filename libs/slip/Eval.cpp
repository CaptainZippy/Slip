#include "slip/pch/Pch.h"

#include "Ast.h"
#include "Errors.h"
#include "Intrinsic.h"
#include "Slip.h"

using namespace Slip;

namespace {
    struct Evaluator {
        Result dispatch( Ast::Expr* n, Ast::Expr** out ) {
            RETURN_ERROR_IF( !n->m_type, Error::InternalUncheckedEvaluation, n->m_loc, "Evaluation requires a checked expression" );
            return Ast::dispatch<Result>( n, *this, out );
        }

        Result operator()( Ast::Expr* n, Ast::Expr** out ) {
            RETURN_ERROR( Error::CompileTimeUnsupported, n->m_loc, "Cannot evaluate '%s' at compile time", n->dynamicType()->name );
        }

        Result operator()( Ast::String* n, Ast::Expr** out ) {
            *out = new Ast::String( n->m_str, [&]( auto& value ) {
                value.m_type = n->m_type;
                value.m_loc = n->m_loc;
            } );
            return Result::OK;
        }

        Result operator()( Ast::Nop* n, Ast::Expr** out ) {
            auto value = new Ast::Nop();
            value->m_type = n->m_type;
            value->m_loc = n->m_loc;
            *out = value;
            return Result::OK;
        }

        Result operator()( Ast::Number* n, Ast::Expr** out ) {
            std::string text;
            if( n->m_type == &Ast::s_typeInt ) {
                int64_t value;
                RETURN_IF_FAILED( Intrinsic::integer( n, value ) );
                text = std::to_string( value );
            } else if( n->m_type == &Ast::s_typeBool && ( n->m_num == "true" || n->m_num == "false" ) ) {
                text = n->m_num;
            } else {
                RETURN_ERROR( Error::CompileTimeUnsupported, n->m_loc, "Only integer and boolean numbers are supported" );
            }
            *out = new Ast::Number( istring::make( text ), [&]( auto& value ) {
                value.m_type = n->m_type;
                value.m_loc = n->m_loc;
            } );
            return Result::OK;
        }

        Result operator()( Ast::Reference* n, Ast::Expr** out ) { return dispatch( n->m_target, out ); }

        Result operator()( Ast::NowExpr* n, Ast::Expr** out ) {
            RETURN_ERROR_IF( !n->m_resolved, Error::InternalUnresolvedNowExpr, n->m_loc );
            return dispatch( n->m_resolved, out );
        }

        Result operator()( Ast::NamedFunctionCall* n, Ast::Expr** out ) {
            RETURN_ERROR_IF( !n->m_resolved, Error::UnresolvedCall, n->m_loc );
            return dispatch( n->m_resolved, out );
        }

        Result operator()( Ast::FunctionCall* n, Ast::Expr** out ) {
            auto function = dynamic_cast<Ast::FunctionDecl*>( n->m_func->resolve() );
            RETURN_ERROR_IF( !function || !function->m_intrinsic, Error::CompileTimeUnsupported, n->m_loc,
                             "Ordinary function calls are not supported at compile time" );
            std::vector<Ast::Expr*> args;
            for( auto arg : n->m_args ) {
                Ast::Expr* value;
                RETURN_IF_FAILED( dispatch( arg, &value ) );
                args.push_back( value );
            }
            Ast::Expr* value = nullptr;
            RETURN_IF_FAILED( function->m_intrinsic( n->m_loc, args, &value ) );
            RETURN_ERROR_IF(
                !value || value->m_type != n->m_type ||
                    ( !dynamic_cast<Ast::Number*>( value ) && !dynamic_cast<Ast::String*>( value ) && !dynamic_cast<Ast::Nop*>( value ) ),
                Error::InternalIntrinsicResult, n->m_loc, "Intrinsic must return a constant of the declared result type" );
            return dispatch( value, out );
        }
    };
}  // namespace

Result Eval::evaluate( Ast::Expr* node, Ast::Expr** out ) {
    *out = nullptr;
    Evaluator evaluator;
    return evaluator.dispatch( node, out );
}
