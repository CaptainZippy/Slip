#pragma once

#include "Ast.h"
#include "Errors.h"

namespace Slip::Intrinsic {
    inline Result integer( Ast::Expr* expr, int64_t& value ) {
        auto n = dynamic_cast<Ast::Number*>( expr );
        RETURN_ERROR_IF( !n || n->m_type != &Ast::s_typeInt, Error::InternalTypeConstraintFailed, expr->m_loc,
                         "Expected an evaluated integer argument" );
        return n->getValue( value );
    }

    template <bool Division = false, typename Operation>
    Result integerBinary( const Ast::SourceLocation& loc, array_view<Ast::Expr*> args, Ast::Expr** out, Operation operation ) {
        *out = nullptr;
        RETURN_ERROR_IF( args.size() != 2, Error::WrongNumberOfArguments, loc );
        int64_t a, b;
        RETURN_IF_FAILED( integer( args[0], a ) );
        RETURN_IF_FAILED( integer( args[1], b ) );
        if constexpr( Division ) {
            RETURN_ERROR_IF( b == 0 || ( a == INT32_MIN && b == -1 ), Error::CompileTimeArithmeticError, loc,
                             "Integer division is undefined for these operands" );
        }
        auto result = operation( a, b );
        if constexpr( !std::is_same_v<decltype( result ), bool> ) {
            RETURN_ERROR_IF( result < INT32_MIN || result > INT32_MAX, Error::CompileTimeArithmeticError, loc,
                             "Integer result is outside the signed 32-bit range" );
        }
        auto value = new Ast::Number( [&]( auto& n ) {
            n.m_type = std::is_same_v<decltype( result ), bool> ? &Ast::s_typeBool : &Ast::s_typeInt;
            n.m_loc = loc;
        } );
        if constexpr( std::is_same_v<decltype( result ), bool> ) {
            RETURN_IF_FAILED( value->setValue( result ) );
        } else {
            RETURN_IF_FAILED( value->setValue( int64_t{ result } ) );
        }
        *out = value;
        return Result::OK;
    }
}  // namespace Slip::Intrinsic
