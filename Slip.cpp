
// 1. lex + tree
// 2. tree shape check
// 3. Lex basics
// 4. Lex 

#include "pch/Pch.h"
#include "Slip.h"

namespace Slip::Args {
    bool dumpParse{ false };
    bool dumpInfer{ false };
    vector<string> inputs;

    struct Parser {
        using Action = function<void(string_view)>;
        struct Opt {
            string_view m_meta;
            string_view m_help;
            Action m_action;
        };
        std::vector<Opt> m_opts;
        std::vector<Opt> m_positional;

        void add(string_view meta, string_view help, Action&& action) {
            auto& o = (meta[0] == '-') ? m_opts.emplace_back() : m_positional.emplace_back();
            o.m_meta = meta;
            o.m_help = help;
            o.m_action = move(action);
        }

        void parse(array_view<const char*> args) {
            auto positionals = make_array_view(m_positional);
            for (string_view key_val : args) {
                if (key_val[0] == '-') {
                    auto equals = key_val.find("=");
                    string_view key = key_val;
                    string_view val;
                    if (equals != string_view::npos) {
                        key = key_val.substr(0, equals);
                        val = key_val.substr(equals + 1);
                    }
                    bool found = false;
                    for (auto opt : m_opts) {
                        if (opt.m_meta == key) {
                            opt.m_action(val);
                            found = true;
                            break;
                        }
                    }
                    if (!found) {
                        THROW(string_format("Unknown argument %s", key_val.data()));
                    }
                }
                else { // positional
                    if (positionals.empty()) {
                        THROW(string_format("Extra positional argument"));
                    }
                    positionals.front().m_action(key_val);
                    positionals = positionals.ltrim(1);
                }
            }
        }
    };
}

static void compile(const char* fname) {
    using namespace Slip;

    auto smanager = Io::makeSourceManager();
    auto lex = Lex::parse_file(*smanager, fname );
    auto ast = Parse::module(*lex);
    if( Args::dumpParse )
        Ast::print(ast.get());
    Sema::type_check(*ast);
    if (Args::dumpInfer)
        Ast::print(ast.get());
    Backend::generate(*ast);
}

int main( int argc, const char* argv[] ) {
    using namespace Slip;
    auto parser = Args::Parser();
    parser.add("--dump-parse", {}, [](string_view v) { Args::dumpParse = true; });
    parser.add("--dump-infer", {}, [](string_view v) { Args::dumpInfer = true; });
    parser.add("input", "Input file", [](string_view v) { Args::inputs.emplace_back(v); });
    try {
        parser.parse(make_array_view(argv + 1, argc - 1));
        for (auto input : Args::inputs) {
            compile(input.c_str());
        }
        return 0;
    }
    catch (const Exception& se) {
        printf("%s\n", se.what());
        return 1;
    }
    catch (const std::exception& ) {
        return 2;
    }
}
