#pragma once

namespace Slip {

    namespace Io {
        struct TextOutput;
        struct TextInput;
        struct SourceLocation;
        struct SourceManager;

        unique_ptr_del<SourceManager> makeSourceManager();
    }  // namespace Io

    namespace Lex {
        struct Atom;
        struct List;
        struct Number;
        struct String;
        struct Symbol;

        /// Lex an input
        Result parse_input( Io::TextInput& input, unique_ptr_del<List>& lex );
        Result parse_file( Io::SourceManager& sm, const char* fname, unique_ptr_del<List>& lex );
    }  // namespace Lex

    namespace Ast {
#define AST_NODE( X ) struct X;
#include "Ast.inc"
#undef AST_NODE
        void print( Node* node );
        void print( Module* node );
    }  // namespace Ast

    namespace Parse {
        struct Evaluator;

        /// Parse lexed input
        Result module( Lex::List& Lex, unique_ptr_del<Ast::Module>& mod );
    }  // namespace Parse

    namespace Sema {
        /// Check types
        void type_check( Ast::Module& mod );
    }  // namespace Sema

    namespace Backend {
        /// Generate output
        void generate( Ast::Module& mod, Io::TextOutput& out );
    }  // namespace Backend
}  // namespace Slip
