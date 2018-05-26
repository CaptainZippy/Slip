
// 1. lex + tree
// 2. tree shape check
// 3. Lex basics
// 4. Lex 

#include "pch/Pch.h"
#include "Source.h"
#include "Lex.h"
#include "Reflect.h"
#include "Ast.h"
#include "Parse.h"
#include "Sema.h"
#include "Backend.h"

Result compile(const char* fname) {
    auto smanager = Lex::SourceManager::make();
    auto lex = Lex::parse_file(*smanager, fname);
    
    auto ast = Parse::module(*lex);
    Ast::print(ast.get());
    RETURN_IF_FAILED(Sema::type_check(ast.get()));
    Ast::print(ast.get());
    RETURN_IF_FAILED(Backend::generate(ast.get()));
    return Result::OK;
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
