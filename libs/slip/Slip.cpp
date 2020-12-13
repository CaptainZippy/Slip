
// 1. lex + tree
// 2. tree shape check
// 3. Lex basics
// 4. Lex

#include "slip/pch/Pch.h"

#include "Ast.h"
#include "Errors.h"
#include "Io.h"
#include "Slip.h"

namespace Slip::Args {

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
                if( key_val[0] == '-' ) {  // switch?
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
                    RETURN_ERROR_IF( !found, Error::UnknownCmdlineArgument, Io::SourceLocation(), "%s", key_val.data() );
                } else {  // positional
                    RETURN_ERROR_IF( positionals.empty(), Error::ExtraPositionalCmdlineArgument, Io::SourceLocation(), "'%s'",
                                     key_val.data() );
                    positionals.front().m_action( key_val );
                    // don't consume variadic positionals
                    if( positionals.back().m_meta.find( "..." ) == string_view::npos ) {
                        positionals = positionals.ltrim( 1 );
                    }
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

    struct Args {
        bool dumpParse{false};
        bool dumpInfer{false};
        bool tapTest{false};
        vector<string> inputs;
        string outputDir{"."};
    };
}  // namespace Slip::Args

static std::string modNameFromPath( const char* path ) {
    const char* s = path;
    if( auto p = strrchr( path, '/' ) ) {
        s = p + 1;
    }
    if( auto p = strrchr( path, '\\' ); p > s ) {
        s = p + 1;
    }

    if( auto e = strrchr( path, '.' ) ) {
        return {s, size_t( e - s )};
    }
    return s;
}

static Slip::Result compile( const Slip::Args::Args& args, Slip::Io::SourceManager& smanager, const char* fname ) {
    using namespace Slip;

    unique_ptr_del<Ast::LexList> lex{nullptr, nullptr};
    RETURN_IF_FAILED( Ast::lex_file( smanager, fname, lex ) );

    Slip::unique_ptr_del<Ast::Module> ast{nullptr, nullptr};

    RETURN_IF_FAILED( Parse::module( modNameFromPath( fname ).c_str(), *lex, ast ) );
    if( args.dumpParse )
        Ast::print( ast.get() );
    RETURN_IF_FAILED( Sema::type_check( ast.get() ) );
    if( args.dumpInfer )
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
    Io::TextOutput out{string_concat( args.outputDir, "/", path, ".cpp" ).c_str()};
    RETURN_IF_FAILED( Backend::generate( *ast, out ) );
    return Result::OK;
}

namespace Tap {
    enum State { at_start, in_header, in_diag };
    static State state{at_start};

    static int header( const char* fmt, ... ) {
        va_list ap;
        va_start( ap, fmt );
        auto s = Slip::string_formatv( fmt, ap );
        va_end( ap );
        if( !s.empty() ) {
            if( state == in_diag ) {
                fwrite( "\n", 1, 1, stdout );
            }
            fwrite( s.c_str(), 1, s.size(), stdout );
            state = s.back() == '\n' ? at_start : in_header;
        }
        return 0;
    }

    static int diagnosticv( const char* fmt, va_list args ) {
        return 0;
        auto s = Slip::string_formatv( fmt, args );
        if( s.empty() ) {
            return 0;
        }
        switch( state ) {
            case at_start:
                fwrite( "# ", 1, 2, stdout );
                break;
            case in_header:
                fwrite( "\n# ", 1, 3, stdout );
                break;
            case in_diag:
                break;
        }
        state = in_diag;
        for( size_t cur = 0; cur != s.size(); ) {
            auto nl = s.find( '\n', cur );
            if( nl == std::string::npos ) {
                fwrite( s.c_str() + cur, 1, s.length() - cur, stdout );
                return 0;
            }
            fwrite( s.c_str() + cur, 1, 1 + nl - cur, stdout );
            cur = nl + 1;
        }
        if( s.back() == '\n' ) {
            state = at_start;
        }
        return 0;
    }

    Slip::Result getExpectedError( Slip::Io::SourceManager& smanager, const char* filename, std::string_view& expected ) {
        // First line of file contains a comment with the expected error. e.g. "; LexInvalidCharacter"
        using namespace Slip;
        Io::TextInput text;
        RETURN_IF_FAILED( smanager.load( filename, text ) );
        RETURN_ERROR_IF( text.next() != ';', Error::MalformedTestResult, text.location() );
        text.eatwhite();
        const char* start = text.cur;
        while( isalnum( text.next() ) ) {
        }
        expected = string_view{start, size_t( text.cur - start - 1 )};
        return Result::OK;
    }

};  // namespace Tap

Slip::Result Slip::Main::main( int argc, const char* argv[] ) {
    using namespace Slip;
    auto parser = Args::Parser();
    auto args = Args::Args();
    parser.add( "--dump-parse"sv, "Debug print each module after parsing"sv, [&args]( string_view v ) { args.dumpParse = true; } );
    parser.add( "--dump-infer", "Debug print each module after type inference"sv, [&args]( string_view v ) { args.dumpInfer = true; } );
    parser.add( "-h", "Show help"sv, [&parser]( string_view v ) { parser.help(); } );
    parser.add( "--help", "Show help"sv, [&parser]( string_view v ) { parser.help(); } );
    parser.add( "--nop[=ignored]", "Ignore this argument", []( string_view v ) { /*ignore arg*/ } );
    parser.add( "--output-dir=dir", "Output to specified directory", [&args]( string_view v ) { args.outputDir = v; } );
    parser.add( "--tap", "Use TAP test mode", [&args]( string_view v ) {
        args.tapTest = true;
        set_diagnostic_fn( &Tap::diagnosticv );
    } );
    parser.add( "input...", "Input files to compile", [&args]( string_view v ) { args.inputs.emplace_back( v ); } );
    RETURN_IF_FAILED( parser.parse( make_array_view( argv + 1, argc - 1 ) ) );

    auto smanager = Io::makeSourceManager();

    if( args.tapTest == false ) {
        for( auto input : args.inputs ) {
            RETURN_IF_FAILED( compile( args, *smanager, input.c_str() ) );
        }
    } else {
        Tap::header( "1..%i\n", args.inputs.size() );
        int i = 0;
        for( auto input : args.inputs ) {
            auto result = compile( args, *smanager, input.c_str() );
            string_view expected;
            const char* actual = Error::toString( result.code );
            if( Tap::getExpectedError( *smanager, input.c_str(), expected ).isOk() ) {
                if( result.isOk() ) {
                    Tap::header( "not ok %i - succeeded but expected %.*s in %s\n", ++i, expected.size(), expected.data(), input.c_str() );
                } else if( actual != expected ) {
                    Tap::header( "not ok %i - expected %.*s but got E%04i(%s) in %s\n", ++i, expected.size(), expected.data(), result.code,
                                 actual, input.c_str() );
                } else {
                    Tap::header( "ok %i - %s\n", ++i, input.c_str() );
                }
            } else {
                Tap::header( "not ok %i - Error code missing from first line in %s %s\n", ++i, input.c_str(), actual );
            }
        }
    }

    return Result::OK;
}

