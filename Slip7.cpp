
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
    Lex::SourceManager smanager;
    Lex::List* lex;
    RETURN_IF_FAILED(Lex::parse_file(smanager, fname, &lex));
    Ast::Module* ast;
    RETURN_IF_FAILED(Parse::module(lex, &ast));
    Ast::print(ast);
    RETURN_IF_FAILED(Sema::type_check(ast));
    Ast::print(ast);
    RETURN_IF_FAILED(Backend::generate(ast));
    return Result::OK;
}

int main( int argc, const char* argv[] ) {
    if( argc < 2 ) {
        Error.fmt( "Need a script to run" );
        return 1;
    }
    return compile(argv[1]).isOk() ? 0 : 1;
}
