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
    Io::SourceNameAndContents source{ "backend_result.slip",
                                      "(func main ():int (var x 3) (set! x (add x 1)) (if (lt? x 5) (add x 1) (sub x 1)))" };
    Io::TextInput input{ source.m_contents.data(), source.m_contents.data() + source.m_contents.size(), &source };
    unique_ptr_del<Ast::LexList> lex{ nullptr, nullptr };
    unique_ptr_del<Ast::Module> module{ nullptr, nullptr };
    CHECK( Ast::lex_input( input, lex ).isOk() );
    CHECK( Parse::module( "backend_result", *lex, module ).isOk() );
    CHECK( Sema::type_check( module.get() ).isOk() );

    std::vector<char> golden;
    Io::TextOutput output{ &golden };
    CHECK( Backend::generate( *module, output ).isOk() );
    CHECK( !golden.empty() );
    auto function = dynamic_cast<Ast::FunctionDecl*>( module->exports()[0].expr );
    CHECK( function );
    auto sequence = dynamic_cast<Ast::Sequence*>( function->m_body );
    CHECK( sequence && sequence->m_items.size() == 3 );
    auto variable = dynamic_cast<Ast::VariableDecl*>( sequence->m_items[0] );
    auto assignment = dynamic_cast<Ast::Assignment*>( sequence->m_items[1] );
    auto conditional = dynamic_cast<Ast::If*>( sequence->m_items[2] );
    CHECK( variable && assignment && conditional );
    auto named = dynamic_cast<Ast::NamedFunctionCall*>( conditional->m_true );
    CHECK( named );
    auto call = dynamic_cast<Ast::FunctionCall*>( named->m_resolved );
    CHECK( call && call->m_args.size() == 2 );
    auto reference = dynamic_cast<Ast::Reference*>( call->m_args[0] );
    CHECK( reference );

    Ast::Expr** slots[] = {
        &module->exports()[0].expr,  &function->m_body,  &sequence->m_items[0], &sequence->m_items[1], &sequence->m_items[2],
        &variable->m_initializer[0], &assignment->m_lhs, &assignment->m_rhs,    &conditional->m_cond,  &conditional->m_true,
        &conditional->m_false,       &named->m_resolved, &call->m_func,         &call->m_args[0],      &call->m_args[1],
        &reference->m_target,
    };
    Ast::LexNumber unsupported{ function->m_loc };
    const std::vector<char> prefix{ 'k', 'e', 'e', 'p', '\n' };
    for( auto slot : slots ) {
        auto saved = *slot;
        *slot = &unsupported;
        auto actual = prefix;
        Io::TextOutput sink{ &actual };
        auto result = Backend::generate( *module, sink );
        *slot = saved;
        CHECK( result.code == Error::NotImplemented );
        CHECK( actual == prefix );
    }

    named->m_resolved = nullptr;
    auto actual = prefix;
    Io::TextOutput sink{ &actual };
    auto result = Backend::generate( *module, sink );
    named->m_resolved = call;
    CHECK( result.code == Error::UnresolvedCall );
    CHECK( actual == prefix );
    CHECK( Backend::generate( *module, sink ).isOk() );
    auto expected = prefix;
    expected.insert( expected.end(), golden.begin(), golden.end() );
    CHECK( actual == expected );
    return 0;
}
