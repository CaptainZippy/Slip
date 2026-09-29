#include "slip/Ast.h"
#include "slip/Errors.h"

#define CHECK( condition )                                  \
    do {                                                    \
        if( !( condition ) ) {                              \
            Slip::diagnostic( "Failed: %s\n", #condition ); \
            return 1;                                       \
        }                                                   \
    } while( false )

int main() {
    using namespace Slip;
    Io::SourceNameAndContents source{ "eval_boundary.slip", "(func probe () #(add #(sub 9 12) (mul 2 4)))" };
    Io::TextInput input{ source.m_contents.data(), source.m_contents.data() + source.m_contents.size(), &source };
    unique_ptr_del<Ast::LexList> lex{ nullptr, nullptr };
    unique_ptr_del<Ast::Module> module{ nullptr, nullptr };
    CHECK( Ast::lex_input( input, lex ).isOk() );
    CHECK( Parse::module( "eval_boundary", *lex, module ).isOk() );
    auto function = dynamic_cast<Ast::FunctionDecl*>( module->exports()[0].expr );
    CHECK( function );
    auto now = dynamic_cast<Ast::NowExpr*>( function->m_body );
    CHECK( now && !now->m_type && !now->m_resolved && !now->m_expr->m_type );

    Ast::Expr* result = nullptr;
    CHECK( Eval::evaluate( now->m_expr, &result ).code == Error::InternalUncheckedEvaluation );
    CHECK( !result && !now->m_expr->m_type );
    CHECK( Sema::type_check( module.get() ).isOk() );
    auto number = dynamic_cast<Ast::Number*>( now->m_resolved );
    CHECK( number && number->m_num == "5" && number->m_type == &Ast::s_typeInt );
    CHECK( now->m_type == number->m_type );
    CHECK( number->m_loc.m_file == &source && number->m_loc.text() == now->m_loc.text() );
    CHECK( number->m_loc.text() == "#(add #(sub 9 12) (mul 2 4))" );

    std::vector<char> generated;
    Io::TextOutput output{ &generated };
    CHECK( Backend::generate( *module, output ).isOk() );
    CHECK( std::string( generated.begin(), generated.end() ).find( "return (int)5;" ) != std::string::npos );

    now->m_resolved = nullptr;
    generated.clear();
    CHECK( Backend::generate( *module, output ).code == Error::InternalUnresolvedNowExpr );
    CHECK( generated.empty() );
    now->m_resolved = number;

    Ast::String text{ std::string( "a\0b", 3 ), []( auto& n ) { n.m_type = &Ast::s_typeString; } };
    CHECK( Eval::evaluate( &text, &result ).isOk() );
    auto string = dynamic_cast<Ast::String*>( result );
    CHECK( string && string->m_str == text.m_str && string->m_type == &Ast::s_typeString );
    return 0;
}
