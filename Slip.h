#pragma once

namespace Slip {
    using namespace std;

    template<typename T>
    using unique_ptr_del = std::unique_ptr<T, void(*)(T*)>;

    namespace Io {
        struct TextOutput;
        struct Input;
        struct SourceLocation;

        struct SourceManager {
            virtual Input load(const char* fname) = 0;
        };
        unique_ptr<SourceManager> makeSourceManager();
    }

    namespace Lex {
        struct Atom;
        struct List;
        struct Number;
        struct String;
        struct Symbol;

        /// Lex an input
        unique_ptr_del<List> parse_input(Io::Input& input);
        unique_ptr_del<List> parse_file(Io::SourceManager& sm, const char* fname);
    }

    namespace Ast {
        #define AST_NODE(X) struct X;
        #include "Ast.inc"
        #undef AST_NODE
    }

    namespace Parse {
        struct Evaluator;

        /// Parse lexed input
        unique_ptr_del<Ast::Module> module(Lex::List& Lex);
    }

    namespace Sema {
        struct TypeInfo;
        void type_check(Ast::Module& mod);
    }

    namespace Backend {
        void generate(Ast::Module& mod);
    }
}
