#include "pch/Pch.h"

#include "Ast.h"
#include "Backend.h"
#include "Io.h"

namespace {
    using namespace std;
    using namespace Slip;
    struct Generator {
        Io::TextOutput& out;
        std::unordered_map<Ast::Node*, istring> dispatched;
        int m_counter = 1;

        Generator( Io::TextOutput& o ) : out( o ) {
            // populate "dispatched" with intrinsics
        }

        string newVarId() { return string_format( "_%i", m_counter++ ); }

        void addName( Ast::Node* n, istring name ) {
            auto it = dispatched.emplace( n, name );
            assert( it.second );  // assert we inserted a new
        }

        string dispatch( Ast::Node* n ) { return Ast::dispatch<string>( n, *this ); }

        string operator()( Ast::Node* n ) {
            assert( 0 );
            return "";
        }

        string operator()( Ast::MacroDecl* n ) { return ""; }

        string operator()( Ast::MacroExpansion* n ) { return dispatch( n->m_expansion ); }

        string operator()( Ast::Reference* n ) {
            auto it = dispatched.find( n->m_target );
            if( it == dispatched.end() ) {
                if( auto t = dynamic_cast<Ast::Named*>( n->m_target ) ) {  // TODO: hack
                    return sanitize( t->name() );
                }
                assert( false );
            }
            return it->second.std_str();
#if 0
            if( auto t = dynamic_cast<Ast::Named*>( n->m_target ) ) {
                return sanitize(t->name());
            }
            return "??";
#endif
        }
        string operator()( Ast::Parameter* n ) {
            addName( n, n->name() );
            return n->m_name.std_str();
        }
        string operator()( Ast::Number* n ) { return string_format( "(%s)%s", n->m_type->name().c_str(), n->m_num.c_str() ); }
        string operator()( Ast::String* n ) { return string_concat( "\"", n->m_str, "\"_str" ); }
        string operator()( Ast::Definition* n ) {
            auto val = dispatch( n->m_value );
            out.write( string_format( "%s %s = %s", n->m_type->name().c_str(), n->m_name, val.c_str() ) );
            return n->m_name.std_str();
        }
        string operator()( Ast::Sequence* n ) {
            for( auto a : n->items().rtrim( 1 ) ) {
                dispatch( a );
                out.nl();
            }
            return dispatch( n->m_items.back() );
        }
#if 0
        string operator()(Ast::Scope* n) {
            auto id = newVarId();
            out.write(string_format("%s %s", n->m_type->m_name.c_str(), n->m_name));
            out.begin(" {\n");
            auto r = dispatch(n->m_child);
            out.end("}\n");
            out.write(string_format("%s = %s", id.c_str(), n->m_name));
            return id;
        }
#endif
        string operator()( Ast::If* n ) {
            auto ret = newVarId();
            out.write( string_format( "%s %s;", n->m_type->name().c_str(), ret.c_str() ) );
            out.begin( " {\n" );
            auto cond = dispatch( n->m_cond );
            out.begin( string_format( "if(%s) {\n", cond.c_str() ) );
            string t = dispatch( n->m_true );
            out.write( string_concat( ret, " = ", t, ";\n" ) );
            out.end( "}\n" );
            out.begin( "else {\n" );
            string f = dispatch( n->m_false );
            out.write( string_concat( ret, " = ", f, ";\n" ) );
            out.end( "}" );
            out.end( "}\n" );
            return ret;
        }

        string operator()( Ast::While* n ) {
            out.begin( "while(true) {" );
            auto cond = dispatch( n->m_cond );
            out.write( string_concat( "if(!", cond, ") { break; }\n" ) );
            string b = dispatch( n->m_body );
            out.end( "}\n" );
            return "";
        }

        string operator()( Ast::Cond* n ) {
            auto ret = newVarId();
            out.write( string_format( "%s %s;", n->m_type->name().c_str(), ret.c_str() ) );
            out.begin( " {\n" );
            for( auto c : n->m_cases ) {
                auto cond = dispatch( c.first );
                out.begin( string_format( "if(%s) {\n", cond.c_str() ) );
                string t = dispatch( c.second );
                out.write( string_concat( ret, " = ", t, ";\n" ) );
                out.end( "}\n" );
                out.begin( "else {\n" );
            }
            for( auto c : n->m_cases ) {
                (void)c;
                out.end( "}" );
            }
            out.end( "\n}\n" );
            return ret;
        }

        static string sanitize( string_view inp ) {
            string s;
            size_t cur = 0;
            while( true ) {
                auto p = inp.find_first_of( "?!"sv, cur );
                if( p == string::npos ) {
                    s.append( inp.begin() + cur, inp.end() );
                    return s;
                }
                s.append( inp.begin() + cur, inp.begin() + cur + p );
                s.append( "_"sv );
                cur = p + 1;
            }
        }

        string operator()( Ast::FunctionDecl* n ) {
            std::string symbol = n->m_name.std_str();
            if( symbol != "main" ) {
                for( auto p : n->m_params ) {
                    assert( p->m_type );
                    assert( p->m_name.c_str() );
                    symbol.append( "__" );
                    symbol.append( p->m_type->name() );  // todo valid symbol, spaces etc
                }
            }
            addName( n, istring::make( symbol ) );
            out.begin( string_concat( "\n", n->m_type->m_callable[0]->name(), " ", symbol, "(" ) );
            const char* sep = "";
            for( auto p : n->m_params ) {
                assert( p->m_type );
                assert( p->m_name.c_str() );
                out.write( string_concat( sep, p->m_type->name(), " ", p->m_name ) );
                sep = ", ";
            }
            out.write( ") {\n" );
            string ret = dispatch( n->m_body );
            out.write( string_concat( "return ", ret, ";\n" ) );
            out.end( "}\n" );
            return symbol;
        }

        string operator()( Ast::VariableDecl* n ) {
            string init{"{"};
            const char* sep{""};
            for( auto&& i : n->m_initializer ) {
                init.append( sep );
                init.append( dispatch( i ) );
                sep = ", ";
            }
            std::string name = string_format( "%s_%lu", n->name().c_str(), n->m_serial );
            addName( n, istring::make( name ) );
            out.begin( string_concat( n->m_type->name(), " "sv, name ) );
            if( n->m_initializer.empty() == false ) {
                out.write( init );
            }
            out.end( "};"sv );
            return name;
        }

        string operator()( Ast::Assignment* n ) {
            auto it = dispatched.find( n->m_lhs );
            assert( it != dispatched.end() );
            out.write( string_concat( it->second, " = "sv, dispatch( n->m_rhs ), ";\n"sv ) );
            return it->second.std_str();
        }

        string operator()( Ast::FunctionCall* n ) {
            auto func = dispatch( n->m_func );
            vector<string> args;
            for( auto a : n->m_args ) {
                args.push_back( dispatch( a ) );
            }
            string retId;
            if( n->m_type != &Ast::s_typeVoid ) {
                retId = newVarId();
                out.write( string_concat( "auto ", retId, " = ", func, "(" ) );
            } else {
                retId = "/*void*/";
                out.write( string_concat( func, "(" ) );
            }
            auto sep = "";
            for( auto a : args ) {
                out.write( string_concat( sep, a ) );
                sep = ", ";
            }
            out.write( ");\n" );
            return retId;
        }

        string operator()( Ast::NamedFunctionCall* n ) {
            assert( n->m_resolved );
            return dispatch( n->m_resolved );
        }

        string operator()( Ast::Module* n ) {
            out.begin( "#include<stdio.h>\n" );
            out.write( "#include<string>\n" );
            out.write( "#include<vector>\n" );
            out.write( "#include<cstring>\n" );
            out.begin( "namespace XX {" );
            out.write(
                "struct string { std::string m_s; "
                "string() = default; "
                "string(const char* s) : m_s(s,s?std::strlen(s):0) {} "
                "string(const char* s, size_t l) : m_s(s,l) {} "
                "};\n" );
            out.write(
                "template<typename T> struct array_view { "
                "T* m_data; size_t m_count; "
                "T operator[](int i) { return m_data[i]; } };\n" );
            out.write( "inline bool eq_(int a, int b) { return a==b; }\n" );
            out.write( "inline bool lt_(int a, int b) { return a<b; }\n" );
            out.write( "inline int add(int a, int b) { return a+b; }\n" );
            out.write( "inline int sub(int a, int b) { return a-b; }\n" );
            out.write( "inline double dfromi(int a) { return (double)a; }\n" );
            out.write( "inline int atoi(const string& s) { return ::atoi(s.m_s.data()); }\n" );
            out.write( "inline double divd(double a, double b) { return a/b; }\n" );
            out.write( "inline double addd(double a, double b) { return a+b; }\n" );
            out.write( "inline int puts(const string& a) { return printf(\"%s\\n\", a.m_s.c_str()); }\n" );
            out.write( "inline int puti(int a) { return printf(\"%i\\n\", a); }\n" );
            out.write( "inline int putd(double a) { return printf(\"%f\\n\", a); }\n" );
            out.write( "typedef array_view<int> array_view__int__;\n" );
            out.write( "typedef array_view<string> array_view__string__;\n" );
            out.write( "typedef int array_const__int__[];\n" );
            out.write( "template<typename T, int N> inline int size(const T(&)[N]) { return N; }\n" );
            out.write( "template<typename T, int N> inline T at(const T(&a)[N], int i) { return a[i]; }\n" );
            out.write( "template<typename T> inline int size(array_view<T> a) { return (int)a.m_count; }\n" );
            out.write( "template<typename T> inline T at(array_view<T> a, int i) { return a[i]; }\n" );

            out.write( "void strcat_(string& a, const string& b) { a.m_s += b.m_s; }\n" );
            out.write( "string operator \"\" _str( const char* str, size_t len ) noexcept { return string{str,len}; }\n" );

            istring mainStr = istring::make( "main" );
            int mainKind = -1;
            for( auto n : n->m_items ) {
                dispatch( n );
                if( auto fd = dynamic_cast<Ast::FunctionDecl*>( n ) ) {
                    if( fd->name() == mainStr ) {
                        mainKind = fd->m_params.size() ? 1 : 0;
                    }
                }
            }
            out.end( "}\n" );

            switch( mainKind ) {
                case 0:
                    out.write(
                        "int main(int argc, const char** argv) {\n"
                        "   return XX::main();\n"
                        "}" );
                    break;
                case 1:
                    out.write(
                        "int main(int argc, const char** argv) {\n"
                        "   std::vector<XX::string> args;\n"
                        "   for( int i = 0; i < argc; ++i ) {\n"
                        "       args.emplace_back(argv[i]);\n"
                        "   }\n"
                        "   XX::array_view<XX::string> view{args.data(), args.size()};\n"
                        "   return XX::main(view);\n"
                        "}" );
                    break;
                default:
                    break;
            }

            return "";
        }
    };
}  // namespace

void Slip::Backend::generate( Ast::Module& module, Io::TextOutput& out ) {
    Generator g{out};
    Ast::dispatch<string>( &module, g );
}
