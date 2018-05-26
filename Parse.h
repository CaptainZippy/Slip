#pragma once
namespace Lex {
    struct List;
}
namespace Ast {
    struct Module;
}

namespace Parse {
    using namespace std;
    unique_ptr<Ast::Module> module(Lex::List& Lex);
}
