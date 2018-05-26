
// 1. lex + tree
// 2. tree shape check
// 3. Lex basics
// 4. Lex 

#include "pch/Pch.h"
#include "Slip.h"

static void compile(const char* fname) {
    using namespace Slip;

    auto smanager = Io::makeSourceManager();
    auto lex = Lex::parse_file(*smanager, fname );
    
    auto ast = Parse::module(*lex);
    //Ast::print(ast.get());
    Sema::type_check(*ast);
    //Ast::print(ast.get());
    Backend::generate(*ast);
}

int main( int argc, const char* argv[] ) {
    if( argc < 2 ) {
        Error.fmt( "Need a script to run" );
        return 1;
    }
    try {
        compile(argv[1]);
        return 0;
    }
    catch (const Slip::Exception& se) {
        printf("%s\n", se.what());
        return 1;
    }
    catch (const std::exception& ) {
        return 2;
    }
}
