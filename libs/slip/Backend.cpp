#include "slip/pch/Pch.h"

#include "Ast.h"
#include "Io.h"

namespace {
    using namespace std;
    using namespace Slip;
    // true false or undefined only allowing transitions "true <-> undefined <-> false"
    struct TriBool {
        bool get() const {
            assert( m_val != -1 );
            return !!m_val;
        }
        void define( bool b ) {
            assert( m_val == -1 );
            m_val = b ? 1 : 0;
        }
        void undefine() {
            assert( m_val != -1 );
            m_val = -1;
        }

       private:
        int m_val{-1};
    };
    struct Generator {
        Io::TextOutput& out;
        std::unordered_map<Ast::Expr*, istring> dispatched;
        int m_counter = 1;
        TriBool m_outerFuncCanFail;

        Generator( Io::TextOutput& o ) : out( o ) {
            // populate "dispatched" with intrinsics
        }

        string newVarId() { return string_format( "_%i", m_counter++ ); }

        void addName( Ast::Expr* n, istring name ) {
            auto it = dispatched.emplace( n, name );
            assert( it.second );  // assert we inserted a new
        }

        string dispatch( Ast::Expr* n ) {
            auto it = dispatched.find( n );
            if( it != dispatched.end() ) {
                return it->second.std_str();
            }
            auto s = Ast::dispatch<string>( n, *this );
            if( s.size() ) {
                dispatched.emplace( n, istring::make( s ) );
            }
            return s;
        }

        string operator()( Ast::Expr* n ) {
            assert( 0 );
            return "";
        }

        string operator()( Ast::Type* n ) { return n->name().std_str(); }

        string operator()( Ast::CatchExpr* n ) {
            auto rhs = dispatch( n->m_expr );
            out.write( string_concat( "if(", rhs, ".fail) {\n" ) );
            auto fail = dispatch( n->m_fail );
            out.write( string_concat( rhs, ".ok = ", fail, ";\n" ) );
            out.write( "}\n" );
            return string_concat( rhs, ".ok" );
        }

        string operator()( Ast::CoroutineYield* n ) {
            auto r = dispatch( n->m_expr );
            out.write( string_concat( "state_ = 1;\n" ) );
            out.write( string_concat( "return ", r, ";\n" ) );
            out.write( string_concat( "case_1: /*yield*/\n" ) );
            return "/*yield*/";  // FIXME.coro
        }

        string operator()( Ast::CoroutineDecl* n ) {
            out.begin( string_concat( "struct ", n->name(), "{" ) );
            out.write( "int state_{0};\n" );
            for( auto p : n->m_params ) {
                out.write( string_concat( p->m_type->name(), " ", p->name(), ";\n" ) );
            }
            out.begin( string_concat( n->name(), "(" ) );
            const char* sep = "";
            for( auto p : n->m_params ) {
                out.write( string_concat( sep, p->m_type->name(), " _", p->name() ) );
                sep = ", ";
            }
            out.end( ")\n" );
            const char* sep2 = ":";
            for( auto p : n->m_params ) {
                out.write( string_concat( sep2, p->name(), "(_", p->name(), ")\n" ) );
                sep2 = ",";
            }
            out.write( "{}\n" );
            string name = dispatch( n->m_declReturnTypeExpr );
            out.begin( string_concat( name, " operator()(int& status) {\n" ) );
            out.begin( "switch( state_ ) {\n" );
            out.write( "case 0: goto case_0;\n" );
            out.write( "case 1: goto case_1;\n" );
            out.end( "}\n" );
            out.write( "case_err:\n" );
            out.write( "status = -1;\n" );
            out.write( "return {};\n" );

            out.write( "case_0:\n" );
            m_outerFuncCanFail.define( true );
            dispatch( n->m_body );
            m_outerFuncCanFail.undefine();
            out.write( "goto case_err;\n" );
            out.begin( "}\n" );
            out.end( "};" );
            return n->name().std_str();
        }
        string operator()( Ast::MacroDecl* n ) { return ""; }

        string operator()( Ast::MacroExpansion* n ) { return dispatch( n->m_expansion ); }

        string operator()( Ast::Reference* n ) {
            return dispatch( n->m_target );
        }
        string operator()( Ast::Parameter* n ) {
            addName( n, n->name() );
            return n->m_name.std_str();
        }

        string operator()( Ast::Nop* n ) { return ""; }

        string operator()( Ast::Number* n ) { return string_format( "(%s)%s", n->m_type->name().c_str(), n->m_num.c_str() ); }
        string operator()( Ast::String* n ) {
            string s = n->m_str;
            std::replace( s.begin(), s.end(), '\n', ' ' );
            return string_concat( "\"", s, "\"_str" );
        }
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

        string operator()( Ast::Scope* n ) { return dispatch( n->m_child ); }

        string operator()( Ast::Block* n ) {
            std::string var;
            if( n->m_type != &Ast::s_typeVoid ) {
                var = string_format( "v_%s", n->name().c_str() );
                out.write( string_format( "%s %s;\n", n->m_type->name().c_str(), var.c_str() ) );
            }
            out.begin( "{\n" );
            string s = dispatch( n->m_contents );
            if( var.size() ) {
                out.end( string_format( "\n%s = %s;\n}\n", var.c_str(), s.c_str() ) );
            }
            out.write( string_format( "\nL%s:\n", n->name().c_str() ) );
            return var;
        }

        string operator()( Ast::Break* n ) {
            string s = dispatch( n->m_value );
            out.write( string_format( "v_%s = %s;\n", n->m_target->m_name.c_str(), s.c_str() ) );
            out.write( string_format( "goto L%s;\n", n->m_target->m_name.c_str() ) );
            return s;
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
            bool hasVar = n->m_type != &Ast::s_typeVoid;
            auto ret = hasVar ? newVarId() : "";
            if( hasVar ) {
                out.write( string_format( "%s %s;\n", n->m_type->name().c_str(), ret.c_str() ) );
            }
            out.begin( " {\n" );
            auto cond = dispatch( n->m_cond );
            out.begin( string_format( "if(%s) {\n", cond.c_str() ) );
            string t = dispatch( n->m_true );
            if( hasVar ) {
                out.write( string_concat( ret, " = ", t, ";\n" ) );
            }
            out.end( "}\n" );
            out.begin( "else {\n" );
            string f = dispatch( n->m_false );
            if( hasVar ) {
                out.write( string_concat( ret, " = ", f, ";\n" ) );
            }
            out.end( "}" );
            out.end( "}\n" );
            return ret;
        }

        string operator()( Ast::While* n ) {
            bool hasVar = n->m_type != &Ast::s_typeVoid;
            auto ret = hasVar ? newVarId() : "";
            if( hasVar ) {
                out.write( string_format( "%s %s;\n", n->m_type->name().c_str(), ret.c_str() ) );
            }
            out.begin( "while(true) {\nif(" );
            auto cond = dispatch( n->m_cond );
            out.write( string_concat( " !", cond, ") { break; }\n" ) );
            string b = dispatch( n->m_body );
            if( hasVar ) {
                out.write( string_concat( ret, " = ", b, ";\n" ) );
            }
            out.end( "}\n" );
            return ret;
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
            for( auto p = n->environment_; p; p = p->parent() ) {
                if( auto m = dynamic_cast<Ast::Module*>( p ) ) {
                    symbol.insert( 0, "_" );
                    symbol.insert( 0, m->name().view() );
                }
            }
            if( n->m_intrinsic ) {
                return sanitize( symbol );
            }
            if( n->name() != "main"sv ) {
                for( auto p : n->m_params ) {
                    assert( p->m_type );
                    assert( p->m_name.c_str() );
                    symbol.append( "__" );
                    symbol.append( p->m_type->name() );  // todo valid symbol, spaces etc
                    if( p->m_ref ) {
                        symbol.append( "_ref" );
                    }
                }
            } else {
                symbol = "main"sv;
            }

            addName( n, istring::make( symbol ) );
            out.begin( string_concat( "\n", n->m_type->m_callable[0]->name(), " ", symbol, "(" ) );
            const char* sep = "";
            if( n->m_type->m_callCanFail ) {
                out.write( "int& status" );
                sep = ", ";
                m_outerFuncCanFail.define( true );
            } else {
                m_outerFuncCanFail.define( false );
            }
            for( auto p : n->m_params ) {
                assert( p->m_type );
                assert( p->m_name.c_str() );
                out.write( string_concat( sep, p->m_type->name(), p->m_ref ? "& " : " ", p->m_name ) );
                sep = ", ";
            }
            out.write( ") {\n" );
            string ret = dispatch( n->m_body );
            m_outerFuncCanFail.undefine();
            out.write( string_concat( "return ", ret, ";\n" ) );
            out.end( "}\n" );
            return symbol;
        }

        string operator()( Ast::VariableDecl* n ) {
            string init{""};
            const char* sep{""};
            for( auto&& i : n->m_initializer ) {
                init.append( sep );
                init.append( dispatch( i ) );
                sep = ", ";
            }
            std::string name = string_format( "%s_%lu", n->name().c_str(), n->m_serial );
            addName( n, istring::make( name ) );
            const char* qual = "";
            switch( n->m_kind ) {
                case Ast::VariableDecl::Kind::Immutable:
                    qual = "const ";
                    break;
                case Ast::VariableDecl::Kind::Mutable:
                    qual = "";
                    break;
                case Ast::VariableDecl::Kind::Constant:
                    qual = "static const ";
                    break;
            }
            out.begin( string_concat( qual, n->m_type->name(), " "sv, name, "{" ) );
            if( n->m_initializer.empty() == false ) {
                out.write( init );
            }
            out.end( "};"sv );
            return name;
        }

        string operator()( Ast::Assignment* n ) {
            auto lhs = dispatch( n->m_lhs );
            out.write( string_concat( lhs, " = "sv, dispatch( n->m_rhs ), ";\n"sv ) );
            return lhs;
        }

        string operator()( Ast::FunctionCall* n ) {
            auto func = dispatch( n->m_func );
            if( n->m_func->m_type->m_callCanFail && m_outerFuncCanFail.get() == false ) {
                out.write( "int status{0};\n" );
            }
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
            const char* sep = "";
            if( n->m_func->m_type->m_callCanFail ) {
                out.write( "status" );
                sep = ", ";
            }
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

        string operator()( Ast::PipelineExpr* n ) {
            string ret = newVarId();
            out.write( string_concat( n->m_type->m_name, " ", ret, ";\n" ) );
            string cur;
            string closing = "";
            for( auto&& stage : n->m_stages ) {
                cur = dispatch( stage.expr );
                if( stage.canFail ) {
                    out.write( string_concat( "if(", cur, ".fail) { ", ret, ".setFail(", cur, ".fail); } else {\n" ) );
                    closing += "}";
                }
            }
            out.write( string_concat( ret, " = ", cur, ".ok;\n", closing, "\n" ) );

            return ret;
        }

        string operator()( Ast::UnwrapResult* n ) {
            auto c = dispatch( n->m_src );
            return string_concat( c, ".ok" );
        }

        string operator()( Ast::Selector* n ) {
            auto lhs = dispatch( n->m_lhs );
            return string_concat( lhs.c_str(), ".", n->m_rhs->text() );
        }

        string operator()( Ast::StructDecl* n ) {
            out.begin( string_concat( "struct ", n->name(), " {\n" ) );
            for( auto f : n->m_fields ) {
                out.write( string_concat( f->m_type->name(), " ", f->name(), ";\n" ) );
            }
            out.end( "};" );
            return n->name().std_str();
        }

        string operator()( Ast::TryExpr* n ) {
            auto rhs = dispatch( n->m_expr );
            out.write( string_concat( "if(", rhs, ".fail) {\n" ) );
            out.write( string_concat( "return ", rhs, ".fail;\n" ) );
            out.write( "}\n" );
            return string_concat( rhs, ".ok" );
        }

        string operator()( Ast::Module* n ) {
            out.begin( "#include<stdio.h>\n" );
            out.write( "#include<string>\n" );
            out.write( "#include<vector>\n" );
            out.write( "#include<cstring>\n" );
            out.write( "struct MainReg {\n" );
            out.write( "    typedef int(*mainfunc)(int argc, const char**);\n" );
            out.write( "    MainReg(const char* n, mainfunc m); const char* name; mainfunc main; const MainReg* next; };\n" );
            out.write( "enum builtin_Error { failed=1 };\n" );
            out.write( "template<typename T> struct builtin_Result { T ok; int fail;\n" );
            out.write( "    builtin_Result( ) : fail( failed ) {}\n" );
            out.write( "    builtin_Result( const T& t ) : ok( t ), fail(0) {}\n" );
            out.write( "    builtin_Result( builtin_Error e ) : fail( e ) {}\n" );
            out.write( "    void setFail( int f ) { fail = f; }\n" );
            out.write( "    void operator=( const T& t ) { ok = t; fail = 0; }\n" );
            out.write( "    void operator=( builtin_Error e ) { fail = e; }\n};\n" );
            out.write(
                "struct string { std::string m_s; "
                "inline string() = default; "
                "inline string(const char* s) : m_s(s,s?std::strlen(s):0) {} "
                "inline string(const char* s, size_t l) : m_s(s,l) {} "
                "};\n" );
            out.write(
                "template<typename T> struct builtin_array_view { "
                "T* m_data; size_t m_count; "
                "inline T operator[](int i) { return m_data[i]; } };\n" );
            out.write( "inline bool builtin_eq_(int a, int b) { return a==b; }\n" );
            out.write( "inline bool builtin_lt_(int a, int b) { return a<b; }\n" );
            out.write( "inline bool builtin_ge_(int a, int b) { return a>=b; }\n" );
            out.write( "inline int builtin_add(int a, int b) { return a+b; }\n" );
            out.write( "inline int builtin_sub(int a, int b) { return a-b; }\n" );
            out.write( "inline int builtin_mul(int a, int b) { return a*b; }\n" );
            out.write( "inline int builtin_div( int a, int b ) { return a / b; }\n" );
            out.write( "inline int builtin_mod(int a, int b) { return a%b; }\n" );
            out.write( "inline double builtin_dfromi(int a) { return (double)a; }\n" );
            out.write(
                "inline builtin_Result<int> builtin_parsei(const string& s) { int r = ::strtol(s.m_s.data(), nullptr, 0); if(r) return r; return failed; "
                "}\n" );
            out.write( "inline double builtin_muld(double a, double b) { return a*b; }\n" );
            out.write( "inline double builtin_divd(double a, double b) { return a/b; }\n" );
            out.write( "inline double builtin_addd(double a, double b) { return a+b; }\n" );
            out.write( "inline int builtin_puts(const string& a) { return printf(\"%s\\n\", a.m_s.c_str()); }\n" );
            out.write( "inline int builtin_puti(int a) { return printf(\"%i\\n\", a); }\n" );
            out.write( "inline int builtin_putd(double a) { return printf(\"%f\\n\", a); }\n" );
            out.write( "typedef builtin_array_view<int> array_view__int__;\n" );
            out.write( "typedef builtin_array_view<string> array_view__string__;\n" );
            out.write( "typedef std::vector<int> array_heap__int__;\n" );
            out.write( "typedef int array_const__int__[];\n" );
            out.write( "template<typename T, int N> inline int size(const T(&)[N]) { return N; }\n" );
            out.write( "template<typename T, int N> inline T at(const T(&a)[N], int i) { return a[i]; }\n" );
            out.write(
                "template<typename T, int N> inline builtin_Result<T> get(const T(&a)[N], int i) { if( unsigned(i) < unsigned(N) ) return a[i]; "
                "return "
                "failed; }\n" );
            out.write( "template<typename T> inline int size(builtin_array_view<T> a) { return (int)a.m_count; }\n" );
            out.write( "template<typename T> inline T at(builtin_array_view<T> a, int i) { return a[i]; }\n" );
            out.write(
                "template<typename T> inline builtin_Result<T> get(builtin_array_view<T> a, int i) { if( unsigned(i) < unsigned(size(a))) return a[i]; "
                "return "
                "failed; }\n" );
            out.write( "template<typename T> inline void resize(std::vector<T>& a, int n) { a.resize(n); }\n" );
            out.write( "template<typename T> inline void put_(std::vector<T>& a, int i, const T& t) { a[i] = t; }\n" );
            out.write( "template<typename T> inline T at(std::vector<T>& a, int i) { return a[i]; }\n" );
            out.write(
                "template<typename T> inline builtin_Result<T> get(std::vector<T>& a, int i) { if(unsigned(i) < a.size()) return a[i]; return "
                "failed; }\n" );

            out.write( "inline void builtin_strcat_(string& a, const string& b) { a.m_s += b.m_s; }\n" );
            out.write( "inline string operator \"\" _str( const char* str, size_t len ) noexcept { return string{str,len}; }\n" );

            out.write( "inline int bitops_asl(int a, int b) { return a<<b; } \n" );
            out.write( "inline int bitops_lsl(int a, int b) { return a<<b; } \n" );
            out.write( "inline int bitops_asr(int a, int b) { return a>>b; } \n" );
            out.write( "inline int bitops_lsr(int a, int b) { return int(unsigned(a)>>b); } \n" );

            out.begin( string_concat( "namespace ", n->m_name, " {" ) );

            istring mainStr = istring::make( "main" );
            int mainKind = -1;
            for( auto n : n->items() ) {
                dispatch( n );
                if( auto fd = dynamic_cast<Ast::FunctionDecl*>( n ) ) {
                    if( fd->name() == mainStr ) {
                        mainKind = fd->m_params.size() ? 1 : 0;
                    }
                }
            }

            switch( mainKind ) {
                case 0:
                    out.write(
                        "int main_entry(int argc, const char** argv) {\n"
                        "   return main();\n"
                        "}" );
                    break;
                case 1:
                    out.write(
                        "int main_entry(int argc, const char** argv) {\n"
                        "   std::vector<string> args;\n"
                        "   for( int i = 0; i < argc; ++i ) {\n"
                        "       args.emplace_back(argv[i]);\n"
                        "   }\n"
                        "   builtin_array_view<string> view{args.data(), args.size()};\n"
                        "   return main(view);\n"
                        "}" );
                    break;
                default:
                    break;
            }
            if( mainKind != -1 ) {
                out.write( string_concat( "\nstatic MainReg mainreg(\"", n->m_name, "\", &main_entry );\n" ) );
            }

            out.end( "}\n" );

            return "";
        }
    };
}  // namespace

Slip::Result Slip::Backend::generate( Ast::Module& module, Io::TextOutput& out ) {
    Generator g{out};
    Ast::dispatch<string>( &module, g );
    return Result::OK;
}
