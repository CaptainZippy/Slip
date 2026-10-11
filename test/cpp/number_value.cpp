#include "slip/Ast.h"
#include "slip/Errors.h"
#include "slip/Intrinsic.h"

#define CHECK( condition )                                  \
    do {                                                    \
        if( !( condition ) ) {                              \
            Slip::diagnostic( "Failed: %s\n", #condition ); \
            return 1;                                       \
        }                                                   \
    } while( false )

using namespace Slip;

namespace {
    std::string dump( Ast::Number& n ) {
        std::vector<char> buffer;
        Io::TextOutput out{ &buffer };
        Ast::print( &n, out );
        return { buffer.begin(), buffer.end() };
    }
}  // namespace

int main() {
    Io::SourceNameAndContents source{ "number_value.slip", "+0042" };
    Ast::Number literal{ [&]( auto& n ) { n.m_loc = { &source, 0, 5 }; } };
    CHECK( !literal.m_type && !literal.hasValue() );
    CHECK( dump( literal ).find( "+0042" ) != std::string::npos );

    int64_t integer = 99;
    bool boolean = false;
    double real = 9.5;
    std::string text = "prefix:";
    CHECK( literal.getValue( integer ).code == Error::InternalUncheckedEvaluation && integer == 99 );
    CHECK( literal.format( text ).code == Error::InternalUncheckedEvaluation && text == "prefix:" );
    CHECK( literal.setValue( int64_t{ 42 } ).code == Error::InternalTypeConstraintFailed && !literal.hasValue() );

    literal.m_type = &Ast::s_typeInt;
    Ast::Expr* result = nullptr;
    CHECK( Eval::evaluate( &literal, &result ).code == Error::InternalUncheckedEvaluation && !result );
    CHECK( literal.setValue( true ).code == Error::InternalTypeConstraintFailed && !literal.hasValue() );
    CHECK( literal.setValue( 42.0 ).code == Error::InternalTypeConstraintFailed && !literal.hasValue() );
    CHECK( Sema::type_check( &literal ).isOk() && literal.hasValue() );
    CHECK( literal.getValue( integer ).isOk() && integer == 42 );
    CHECK( literal.getValue( boolean ).code == Error::InternalTypeConstraintFailed && !boolean );
    CHECK( literal.getValue( real ).code == Error::InternalTypeConstraintFailed && real == 9.5 );
    CHECK( literal.setValue( int64_t{ 43 } ).code == Error::InternalTypeConstraintFailed );
    CHECK( literal.getValue( integer ).isOk() && integer == 42 );
    CHECK( Sema::type_check( &literal ).isOk() );
    CHECK( literal.format( text ).isOk() && text == "prefix:42" );
    CHECK( dump( literal ).find( "Number  42" ) != std::string::npos );
    CHECK( Eval::evaluate( &literal, &result ).isOk() );
    auto copy = dynamic_cast<Ast::Number*>( result );
    CHECK( copy && copy != &literal && copy->m_type == literal.m_type && copy->m_serial != literal.m_serial );
    CHECK( copy->getValue( integer ).isOk() && integer == 42 );

    for( bool value : { false, true } ) {
        Ast::Number n{ []( auto& n ) { n.m_type = &Ast::s_typeBool; } };
        CHECK( n.setValue( int64_t{ 1 } ).code == Error::InternalTypeConstraintFailed && !n.hasValue() );
        CHECK( n.setValue( value ).isOk() );
        CHECK( n.getValue( boolean ).isOk() && boolean == value );
        CHECK( n.getValue( integer ).code == Error::InternalTypeConstraintFailed );
        text = "prefix:";
        CHECK( n.format( text ).isOk() && text == ( value ? "prefix:true" : "prefix:false" ) );
        CHECK( Eval::evaluate( &n, &result ).isOk() );
        copy = dynamic_cast<Ast::Number*>( result );
        CHECK( copy && copy->m_type == &Ast::s_typeBool && copy->getValue( boolean ).isOk() && boolean == value );
    }

    Ast::Number failed{ []( auto& n ) { n.m_type = &Ast::s_typeError; } };
    CHECK( failed.setValue( int64_t{ 1 } ).isOk() );
    CHECK( failed.getValue( integer ).isOk() && integer == 1 );
    CHECK( Intrinsic::integer( &failed, integer ).code == Error::InternalTypeConstraintFailed );
    text.clear();
    CHECK( failed.format( text ).isOk() && text == "1" );
    CHECK( Eval::evaluate( &failed, &result ).code == Error::CompileTimeUnsupported && !result );

    for( auto type : { &Ast::s_typeFloat, &Ast::s_typeDouble } ) {
        Io::SourceNameAndContents floating{ "number_value.slip", "1e3" };
        Ast::Number n{ [&]( auto& n ) {
            n.m_type = type;
            n.m_loc = { &floating, 0, 3 };
        } };
        CHECK( n.setValue( int64_t{ 1000 } ).code == Error::InternalTypeConstraintFailed && !n.hasValue() );
        CHECK( Sema::type_check( &n ).isOk() && n.hasValue() );
        CHECK( n.getValue( real ).isOk() && real == 1000.0 );
        CHECK( n.getValue( integer ).code == Error::InternalTypeConstraintFailed );
        text.clear();
        CHECK( n.format( text ).isOk() && text == "1000.0" );
        CHECK( dump( n ).find( "Number  1000.0" ) != std::string::npos );
        CHECK( Eval::evaluate( &n, &result ).code == Error::CompileTimeUnsupported && !result );

        Ast::Number negativeZero{ [&]( auto& n ) { n.m_type = type; } };
        CHECK( negativeZero.setValue( -0.0 ).isOk() );
        text.clear();
        CHECK( negativeZero.format( text ).isOk() && text == "-0.0" );
    }

    for( auto spelling : { "2147483648", "1.5", "42junk" } ) {
        Io::SourceNameAndContents invalid{ "number_value.slip", spelling };
        Ast::Number n{ [&]( auto& n ) {
            n.m_type = &Ast::s_typeInt;
            n.m_loc = { &invalid, 0, long( invalid.m_contents.size() ) };
        } };
        CHECK( Sema::type_check( &n ).code == Error::InvalidNumberLiteral && !n.hasValue() );
        CHECK( n.getValue( integer ).code == Error::InternalUncheckedEvaluation );
    }

    Ast::Number unsupported{ []( auto& n ) { n.m_type = &Ast::s_typeString; } };
    CHECK( unsupported.setValue( int64_t{ 1 } ).code == Error::InternalTypeConstraintFailed );
    CHECK( unsupported.setValue( true ).code == Error::InternalTypeConstraintFailed );
    CHECK( unsupported.setValue( 1.0 ).code == Error::InternalTypeConstraintFailed );
    CHECK( !unsupported.hasValue() && dump( unsupported ).find( "<unevaluated number>" ) != std::string::npos );
    CHECK( unsupported.getValue( boolean ).code == Error::InternalUncheckedEvaluation );
    CHECK( unsupported.getValue( real ).code == Error::InternalUncheckedEvaluation );

    Io::SourceNameAndContents program{ "number_value.slip", "(func probe ():double 1000.00)" };
    Io::TextInput input{ program.m_contents.data(), program.m_contents.data() + program.m_contents.size(), &program };
    unique_ptr_del<Ast::LexList> lex{ nullptr, nullptr };
    unique_ptr_del<Ast::Module> module{ nullptr, nullptr };
    CHECK( Ast::lex_input( input, lex ).isOk() );
    CHECK( Parse::module( "number_value", *lex, module ).isOk() );
    auto function = dynamic_cast<Ast::FunctionDecl*>( module->exports()[0].expr );
    CHECK( function );
    auto body = dynamic_cast<Ast::Number*>( function->m_body );
    CHECK( body && !body->hasValue() );
    CHECK( Sema::type_check( module.get() ).isOk() );
    Ast::Number uninitialized{ [&]( auto& n ) {
        n.m_type = body->m_type;
        n.m_loc = body->m_loc;
    } };
    function->m_body = &uninitialized;
    std::vector<char> generated{ 'k', 'e', 'e', 'p' };
    Io::TextOutput output{ &generated };
    auto generatedResult = Backend::generate( *module, output );
    function->m_body = body;
    CHECK( generatedResult.code == Error::InternalUncheckedEvaluation );
    CHECK( std::string( generated.begin(), generated.end() ) == "keep" );
    CHECK( Backend::generate( *module, output ).isOk() );
    CHECK( std::string( generated.begin(), generated.end() ).find( "return (double)1000.0;" ) != std::string::npos );
    return 0;
}
