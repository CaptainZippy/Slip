#pragma once

namespace Slip {

    namespace Io {
        struct TextOutput;
        struct TextInput;
        struct SourceLocation;
        struct SourceManager;

        unique_ptr_del<SourceManager> makeSourceManager();
    }  // namespace Io

    namespace Ast {
#define AST_NODE( X ) struct X;
#include "Ast.inc"
#undef AST_NODE

        /// Lex an input
        Result lex_input( Io::TextInput& input, unique_ptr_del<LexList>& lex );
        Result lex_file( Io::SourceManager& sm, const char* fname, unique_ptr_del<LexList>& lex );

        void print( Node* node );
        void print( Node* node, Io::TextOutput& output );
    }  // namespace Ast

    namespace Parse {
        struct Evaluator;

        /// Parse lexed input
        Result module( Ast::LexList& Lex, unique_ptr_del<Ast::Module>& mod );
    }  // namespace Parse

    namespace Sema {
        /// Check types
        Result type_check( Ast::Node* node );
    }  // namespace Sema

    namespace Eval {
        Result evaluate( Ast::Environment* env, Ast::Node* node, Ast::Node** out );
    }

    namespace Backend {
        /// Generate output
        Result generate( Ast::Module& mod, Io::TextOutput& out );
    }  // namespace Backend

}  // namespace Slip
