#include "slip/Ast.h"
#include "slip/Errors.h"

#define CHECK( condition )                                  \
    do {                                                    \
        if( !( condition ) ) {                              \
            Slip::diagnostic( "Failed: %s\n", #condition ); \
            return 1;                                       \
        }                                                   \
    } while( false )

using namespace Slip;

namespace {
    int calls = 0;

    Ast::FunctionCall* call( Ast::Type* result, std::vector<Ast::Expr*> args, Ast::FunctionDecl::IntrinsicProto* callback ) {
        auto type = new Ast::Type( "callback"_istr, Ast::fixMeDeclContext );
        type->m_callable.push_back( result );
        for( auto arg : args ) type->m_callable.push_back( arg->m_type );
        auto function = new Ast::FunctionDecl( "custom"_istr, Ast::fixMeDeclContext, [&]( auto& n ) {
            n.m_type = type;
            n.m_intrinsic = callback;
        } );
        return new Ast::FunctionCall( new Ast::Reference( function ), std::move( args ), [&]( auto& n ) { n.m_type = result; } );
    }

    Ast::Number* number( const char* text ) {
        return new Ast::Number( istring::make( text ), []( auto& n ) { n.m_type = &Ast::s_typeInt; } );
    }
}  // namespace

int main() {
    static_assert( std::is_pointer_v<decltype( Ast::FunctionDecl::m_intrinsic )> );
    auto inner = call( &Ast::s_typeInt, {}, []( const Ast::SourceLocation& loc, array_view<Ast::Expr*> args, Ast::Expr** out ) -> Result {
        RETURN_ERROR_IF( !args.empty(), Error::WrongNumberOfArguments, loc );
        ++calls;
        *out = number( "7" );
        return Result::OK;
    } );
    auto outer = call( &Ast::s_typeInt, { inner, number( "2" ), number( "3" ) },
                       []( const Ast::SourceLocation& loc, array_view<Ast::Expr*> args, Ast::Expr** out ) -> Result {
                           RETURN_ERROR_IF( args.size() != 3, Error::WrongNumberOfArguments, loc );
                           const char* expected[] = { "7", "2", "3" };
                           for( size_t i = 0; i < args.size(); ++i ) {
                               auto n = dynamic_cast<Ast::Number*>( args[i] );
                               RETURN_ERROR_IF( !n || n->m_type != &Ast::s_typeInt || n->m_num != expected[i],
                                                Error::InternalTypeConstraintFailed, loc );
                           }
                           ++calls;
                           *out = number( "-9" );
                           ( *out )->m_loc = loc;
                           return Result::OK;
                       } );
    Io::SourceNameAndContents source{ "callback.slip", "(custom)" };
    outer->m_loc.m_file = &source;
    outer->m_loc.m_start = 0;
    outer->m_loc.m_end = source.m_contents.size();
    Ast::Expr* result = nullptr;
    CHECK( Eval::evaluate( nullptr, outer, &result ).isOk() );
    auto n = dynamic_cast<Ast::Number*>( result );
    CHECK( calls == 2 && n && n->m_num == "-9" && n->m_type == &Ast::s_typeInt );
    CHECK( n->m_loc.m_file == &source && n->m_loc.text() == "(custom)" );

    Ast::Number typeUse{ "0"_istr, [&]( auto& n ) { n.m_declTypeExpr = outer; } };
    CHECK( Sema::type_check( &typeUse ).code == Error::CompileTimeUnsupported );
    CHECK( calls == 2 );

    auto text = new Ast::String( std::string( "a\0b", 3 ), []( auto& n ) { n.m_type = &Ast::s_typeString; } );
    auto identity =
        call( &Ast::s_typeString, { text }, []( const Ast::SourceLocation&, array_view<Ast::Expr*> args, Ast::Expr** out ) -> Result {
            *out = args[0];
            return Result::OK;
        } );
    CHECK( Eval::evaluate( nullptr, identity, &result ).isOk() );
    auto string = dynamic_cast<Ast::String*>( result );
    CHECK( string && string->m_str == text->m_str && string->m_type == &Ast::s_typeString );

    auto fail = call( &Ast::s_typeInt, {}, []( const Ast::SourceLocation& loc, array_view<Ast::Expr*>, Ast::Expr** out ) -> Result {
        *out = number( "0" );
        RETURN_ERROR( Error::CompileTimeArithmeticError, loc, "Test callback failure" );
    } );
    CHECK( Eval::evaluate( nullptr, fail, &result ).code == Error::CompileTimeArithmeticError );
    CHECK( !result );

    auto missing = call( &Ast::s_typeInt, {}, Ast::FunctionDecl::NotImplemented );
    CHECK( Eval::evaluate( nullptr, missing, &result ).code == Error::CompileTimeUnsupported && !result );
    auto ordinary = call( &Ast::s_typeInt, {}, nullptr );
    CHECK( Eval::evaluate( nullptr, ordinary, &result ).code == Error::CompileTimeUnsupported && !result );

    auto invalid =
        call( &Ast::s_typeInt, {}, []( const Ast::SourceLocation&, array_view<Ast::Expr*>, Ast::Expr** ) -> Result { return Result::OK; } );
    CHECK( Eval::evaluate( nullptr, invalid, &result ).code == Error::InternalIntrinsicResult && !result );
    auto wrongType = call( &Ast::s_typeString, {}, []( const Ast::SourceLocation&, array_view<Ast::Expr*>, Ast::Expr** out ) -> Result {
        *out = number( "1" );
        return Result::OK;
    } );
    CHECK( Eval::evaluate( nullptr, wrongType, &result ).code == Error::InternalIntrinsicResult && !result );
    auto nonConstant = call( &Ast::s_typeInt, {}, []( const Ast::SourceLocation&, array_view<Ast::Expr*>, Ast::Expr** out ) -> Result {
        auto ref = new Ast::Reference( number( "1" ) );
        ref->m_type = &Ast::s_typeInt;
        *out = ref;
        return Result::OK;
    } );
    CHECK( Eval::evaluate( nullptr, nonConstant, &result ).code == Error::InternalIntrinsicResult && !result );
    return 0;
}
