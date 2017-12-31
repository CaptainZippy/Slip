#pragma once

namespace Ast{
    struct Module;
}

namespace Backend {
    Result generate(Ast::Module* mod);
}
