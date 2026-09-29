#pragma once

#include "Ast.h"
#include "Errors.h"

#include <charconv>

namespace Slip::Intrinsic {
    inline Result integer( Ast::Expr* expr, int64_t& value ) {
        auto n = dynamic_cast<Ast::Number*>( expr );
        RETURN_ERROR_IF( !n || n->m_type != &Ast::s_typeInt, Error::InternalTypeConstraintFailed, expr->m_loc,
                         "Expected an evaluated integer argument" );
        auto first = n->m_num.data();
        auto last = first + n->m_num.size();
        if( first != last && *first == '+' )
            ++first;
        int32_t parsedValue;
        auto parsed = std::from_chars( first, last, parsedValue );
        RETURN_ERROR_IF( parsed.ec != std::errc{} || parsed.ptr != last, Error::CompileTimeInvalidInteger, n->m_loc,
                         "Expected a signed 32-bit decimal integer" );
        value = parsedValue;
        return Result::OK;
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
        std::string text;
        Ast::Type* type;
        if constexpr( std::is_same_v<decltype( result ), bool> ) {
            type = &Ast::s_typeBool;
            text = result ? "true" : "false";
        } else {
            RETURN_ERROR_IF( result < INT32_MIN || result > INT32_MAX, Error::CompileTimeArithmeticError, loc,
                             "Integer result is outside the signed 32-bit range" );
            type = &Ast::s_typeInt;
            text = std::to_string( result );
        }
        *out = new Ast::Number( istring::make( text ), [&]( auto& n ) {
            n.m_type = type;
            n.m_loc = loc;
        } );
        return Result::OK;
    }
}  // namespace Slip::Intrinsic
