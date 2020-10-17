
// 1. lex + tree
// 2. tree shape check
// 3. Lex basics
// 4. Lex

#include "pch/Pch.h"

#include "Ast.h"
#include "Io.h"
#include "Slip.h"

namespace Slip::Args {
    bool dumpParse{false};
    bool dumpInfer{false};
    bool tapTest{false};
    vector<string> inputs;
    string outputDir{"."};

    struct Parser {
        using Action = function<void( string_view )>;
        struct Opt {
            string_view m_meta;  // e.g. --arg1=FOO, --arg2, --arg3 FOO BAR
            string_view m_key;   // The part to match, m_meta up to the first one of " ,.[=" e.g. "--arg"
            string_view m_help;  // Text for humans
            Action m_action;
        };
        std::vector<Opt> m_opts;
        std::vector<Opt> m_positional;

        void add( string_view meta, string_view help, Action&& action ) {
            auto& o = ( meta[0] == '-' ) ? m_opts.emplace_back() : m_positional.emplace_back();
            auto sl = meta.find_first_of( ". [=" );
            o.m_meta = meta;
            o.m_key = sl == string::npos ? meta : meta.substr( 0, sl );
            o.m_help = help;
            o.m_action = move( action );
        }

        Result parse( array_view<const char*> args ) {
            auto positionals = make_array_view( m_positional );
            for( string_view key_val : args ) {
                if( key_val[0] == '-' ) {
                    auto equals = key_val.find( "=" );
                    string_view key = key_val;
                    string_view val;
                    if( equals != string_view::npos ) {
                        key = key_val.substr( 0, equals );
                        val = key_val.substr( equals + 1 );
                    }
                    bool found = false;
                    for( auto&& opt : m_opts ) {
                        if( opt.m_key == key ) {
                            opt.m_action( val );
                            found = true;
                            break;
                        }
                    }
                    RETURN_ERR_IF( !found, "Unknown argument %s", key_val.data() );
                } else {  // positional
                    RETURN_ERR_IF( positionals.empty(), "Extra positional argument" );
                    positionals.front().m_action( key_val );
                    positionals = positionals.ltrim( 1 );
                }
            }
            return Result::OK;
        }

        void help() {
            diagnostic( "slip " );
            for( auto&& opt : m_opts ) {
                diagnostic( "%s", string_concat( "[", opt.m_meta, "] " ).c_str() );
            }
            for( auto&& opt : m_positional ) {
                diagnostic( "%s", string_concat( opt.m_meta ).c_str() );
            }
            diagnostic( "\n" );
            for( auto&& opt : m_opts ) {
                diagnostic( "%s\n", string_concat( "\t", opt.m_meta, "\t", opt.m_help ).c_str() );
            }
            for( auto&& opt : m_positional ) {
                diagnostic( "%s\n", string_concat( "\t", opt.m_meta, "\t", opt.m_help ).c_str() );
            }
        }
    };
}  // namespace Slip::Args

static Slip::Result compile( const char* fname ) {
    using namespace Slip;

    auto smanager = Io::makeSourceManager();
    unique_ptr_del<Ast::LexList> lex{nullptr, nullptr};
    RETURN_IF_FAILED( Ast::lex_file( *smanager, fname, lex ) );

    Slip::unique_ptr_del<Ast::Module> ast{nullptr, nullptr};
    RETURN_IF_FAILED( Parse::module( *lex, ast ) );
    if( Args::dumpParse )
        Ast::print( ast.get() );
    RETURN_IF_FAILED( Sema::type_check( ast.get() ) );
    if( Args::dumpInfer )
        Ast::print( ast.get() );

    string_view path{fname};
    auto slash = path.find_last_of( "\\/"sv );
    if( slash != string::npos ) {
        path.remove_prefix( slash + 1 );
    }
    auto suff = path.find( ".slip"sv );
    if( suff != string::npos ) {
        path.remove_suffix( path.size() - suff );
    }
    Io::TextOutput out{string_concat( Args::outputDir, "/", path, ".cpp" ).c_str()};
    RETURN_IF_FAILED( Backend::generate( *ast, out ) );
    return Result::OK;
}

namespace Tap {
    enum State { at_start, in_header, in_diag };
    static State state{at_start};

    static int header( const char* fmt, ... ) {
        if( Slip::Args::tapTest ) {
            va_list ap;
            va_start( ap, fmt );
            auto s = Slip::string_formatv( fmt, ap );
            va_end( ap );
            if( !s.empty() ) {
                if( state == in_diag ) {
                    fwrite( "\n", 1, 3, stderr );
                }
                fwrite( s.c_str(), 1, s.size(), stderr );
                state = s.back() == '\n' ? at_start : in_header;
            }
        }
        return 0;
    }

    static int diagnostic( const char* fmt, va_list args ) {
        auto s = Slip::string_formatv( fmt, args );
        if( s.empty() ) {
            return 0;
        }
        switch( state ) {
            case at_start:
                fwrite( "# ", 1, 2, stderr );
                break;
            case in_header:
                fwrite( "\n# ", 1, 3, stderr );
                break;
            case in_diag:
                break;
        }
        state = in_diag;
        for( size_t cur = 0; cur != s.size(); ) {
            auto nl = s.find( '\n', cur );
            if( nl == std::string::npos ) {
                fwrite( s.c_str() + cur, 1, s.length() - cur, stderr );
                return 0;
            }
            fwrite( s.c_str() + cur, 1, 1 + nl - cur, stderr );
            cur = nl + 1;
        }
        if( s.back() == '\n' ) {
            state = at_start;
        }
        return 0;
    }

};  // namespace Tap

Slip::Result slip_main( int argc, const char* argv[] ) {
    using namespace Slip;
    auto parser = Args::Parser();
    parser.add( "--dump-parse"sv, "Debug print each module after parsing"sv, []( string_view v ) { Args::dumpParse = true; } );
    parser.add( "--dump-infer", "Debug print each module after type inference"sv, []( string_view v ) { Args::dumpInfer = true; } );
    parser.add( "-h", "Show help"sv, [&parser]( string_view v ) { parser.help(); } );
    parser.add( "--help", "Show help"sv, [&parser]( string_view v ) { parser.help(); } );
    parser.add( "--nop[=ignored]", "Ignore this argument", []( string_view v ) { /*ignore arg*/ } );
    parser.add( "--output-dir=dir", "Output to specified directory", []( string_view v ) { Args::outputDir = v; } );
    parser.add( "--tap", "Use TAP test mode", []( string_view v ) {
        Args::tapTest = true;
        set_diagnostic_fn( &Tap::diagnostic );
    } );
    parser.add( "input...", "Input files to compile", []( string_view v ) { Args::inputs.emplace_back( v ); } );
    RETURN_IF_FAILED( parser.parse( make_array_view( argv + 1, argc - 1 ) ) );

    if( Args::tapTest == false ) {
        for( auto input : Args::inputs ) {
            RETURN_IF_FAILED( compile( input.c_str() ) );
        }
    } else {
        Tap::header( "1..%i\n", Args::inputs.size() );
        int i = 0;
        for( auto input : Args::inputs ) {
            const char* status = compile( input.c_str() ).isOk() ? "ok" : "not ok";
            Tap::header( "%s %i - %s\n", status, ++i, input.c_str() );
        }
    }

    return Result::OK;
}

int main( int argc, const char* argv[] ) {
    if( slip_main( argc, argv ).isOk() ) {
        return 0;
    }
    return 1;
}
