#pragma once
namespace Lex {
    struct List;
}
namespace Ast {
    struct Module;
}

namespace Parse {
    Result module(Lex::List* Lex, Ast::Module** out);
}
