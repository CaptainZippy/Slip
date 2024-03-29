#include "slip/pch/Pch.h"

#include "Ast.h"
#include "Io.h"

namespace {
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
        int m_val{ -1 };
    };
    struct Generator {
        std::vector<char> body_;
        Io::TextOutput out;
        std::unordered_map<Ast::Expr*, istring> dispatched_;
        std::vector<std::string> decls_;
        int m_counter = 1;
        TriBool m_outerFuncCanFail;

        Generator() {
            // populate "dispatched" with intrinsics
        }

        std::string newVarId() { return string_format( "_%i", m_counter++ ); }

        void addName( Ast::Expr* n, istring name ) {
            auto it = dispatched_.emplace( n, name );
            assert( it.second );  // assert we inserted a new
        }

        std::string dispatch( Ast::Expr* n ) {
            auto it = dispatched_.find( n );
            if( it != dispatched_.end() ) {
                return it->second.std_str();
            }
            auto s = Ast::dispatch<std::string>( n, *this );
            if( s.size() ) {
                dispatched_.emplace( n, istring::make( s ) );
            }
            return s;
        }

        std::string operator()( Ast::Expr* n ) {
            assert( 0 );
            return "";
        }

        std::string operator()( Ast::Type* n ) { return n->name().std_str(); }

        std::string operator()( Ast::CatchExpr* n ) {
            auto rhs = dispatch( n->m_expr );
            out.write( string_concat( "if(", rhs, ".fail) {\n" ) );
            auto fail = dispatch( n->m_fail );
            out.write( string_concat( rhs, ".ok = ", fail, ";\n" ) );
            out.write( "}\n" );
            return string_concat( rhs, ".ok" );
        }

        std::string operator()( Ast::CoroutineYield* n ) {
            auto r = dispatch( n->m_expr );
            out.write( string_concat( "state_ = 1;\n" ) );
            out.write( string_concat( "return ", r, ";\n" ) );
            out.write( string_concat( "case_1: /*yield*/\n" ) );
            return "/*yield*/";  // FIXME.coro
        }

        std::string operator()( Ast::CoroutineDecl* n ) {
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
            std::string name = dispatch( n->m_declReturnTypeExpr );
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
        std::string operator()( Ast::MacroDecl* n ) { return ""; }

        std::string operator()( Ast::MacroExpansion* n ) { return dispatch( n->m_expansion ); }

        std::string operator()( Ast::Reference* n ) { return dispatch( n->m_target ); }
        std::string operator()( Ast::Parameter* n ) {
            addName( n, n->name() );
            return n->m_name.std_str();
        }

        std::string operator()( Ast::Nop* n ) { return ""; }

        std::string operator()( Ast::Number* n ) {
            return string_format( "(%s)%s", sanitize( n->m_type->name() ).c_str(), n->m_num.c_str() );
        }
        std::string operator()( Ast::String* n ) {
            std::string s = n->m_str;
            std::replace( s.begin(), s.end(), '\n', ' ' );
            return string_concat( "\"", s, "\"_builtin_str" );
        }

        std::string operator()( Ast::DataList* n ) {
            std::string s = string_concat( "(", dispatch( n->m_type ), ") {" );
            const char* sep = "";
            for( auto a : n->items() ) {
                out.nl();
                s += sep;
                s += dispatch( a );
                sep = ",";
            }
            s += "}";
            return s;
        }

        std::string operator()( Ast::Definition* n ) {
            auto val = dispatch( n->m_value );
            out.write( string_format( "%s %s = %s", sanitize( n->m_type->name() ).c_str(), n->m_name, val.c_str() ) );
            return n->m_name.std_str();
        }

        std::string operator()( Ast::Sequence* n ) {
            for( auto a : n->items().rtrim( 1 ) ) {
                dispatch( a );
                out.nl();
            }
            return dispatch( n->m_items.back() );
        }

        std::string operator()( Ast::Scope* n ) { return dispatch( n->m_child ); }

        std::string operator()( Ast::Block* n ) {
            std::string var;
            if( n->m_type != &Ast::s_typeVoid ) {
                var = string_format( "v_%s", n->name().c_str() );
                out.write( string_format( "%s %s;\n", sanitize( n->m_type->name() ).c_str(), var.c_str() ) );
            }
            out.begin( "{\n" );
            std::string s = dispatch( n->m_contents );
            if( var.size() ) {
                out.end( string_format( "\n%s = %s;\n}\n", var.c_str(), s.c_str() ) );
            }
            out.write( string_format( "\nL%s:\n", n->name().c_str() ) );
            return var;
        }

        std::string operator()( Ast::Break* n ) {
            std::string s = dispatch( n->m_value );
            out.write( string_format( "v_%s = %s;\n", n->m_target->m_name.c_str(), s.c_str() ) );
            out.write( string_format( "goto L%s;\n", n->m_target->m_name.c_str() ) );
            return s;
        }
#if 0
        std::string operator()(Ast::Scope* n) {
            auto id = newVarId();
            out.write(string_format("%s %s", n->m_type->m_name.c_str(), n->m_name));
            out.begin(" {\n");
            auto r = dispatch(n->m_child);
            out.end("}\n");
            out.write(string_format("%s = %s", id.c_str(), n->m_name));
            return id;
        }
#endif
        std::string operator()( Ast::If* n ) {
            bool hasVar = n->m_type != &Ast::s_typeVoid;
            auto ret = hasVar ? newVarId() : "";
            if( hasVar ) {
                out.write( string_format( "%s %s;\n", sanitize( n->m_type->name() ).c_str(), ret.c_str() ) );
            }
            out.begin( " {\n" );
            auto cond = dispatch( n->m_cond );
            out.begin( string_format( "if(%s) {\n", cond.c_str() ) );
            std::string t = dispatch( n->m_true );
            if( hasVar ) {
                out.write( string_concat( ret, " = ", t, ";\n" ) );
            }
            out.end( "}\n" );
            out.begin( "else {\n" );
            std::string f = dispatch( n->m_false );
            if( hasVar ) {
                out.write( string_concat( ret, " = ", f, ";\n" ) );
            }
            out.end( "}" );
            out.end( "}\n" );
            return ret;
        }

        std::string operator()( Ast::While* n ) {
            bool hasVar = n->m_type != &Ast::s_typeVoid;
            auto ret = hasVar ? newVarId() : "";
            if( hasVar ) {
                out.write( string_format( "%s %s;\n", sanitize( n->m_type->name() ).c_str(), ret.c_str() ) );
            }
            out.begin( "while(true) {\nif(" );
            auto cond = dispatch( n->m_cond );
            out.write( string_concat( " !", cond, ") { break; }\n" ) );
            std::string b = dispatch( n->m_body );
            if( hasVar ) {
                out.write( string_concat( ret, " = ", b, ";\n" ) );
            }
            out.end( "}\n" );
            return ret;
        }

        std::string operator()( Ast::Cond* n ) {
            auto ret = newVarId();
            out.write( string_format( "%s %s;", sanitize( n->m_type->name() ).c_str(), ret.c_str() ) );
            out.begin( " {\n" );
            for( auto c : n->m_cases ) {
                auto cond = dispatch( c.first );
                out.begin( string_format( "if(%s) {\n", cond.c_str() ) );
                std::string t = dispatch( c.second );
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

        static std::string sanitize( string_view inp, string_view badchars ) {
            // Symbols
            // Hex   ASC  Example
            // _7b = '{'  {template params...}
            // _20 = ' '  {list int}
            // _40 = '@'  module@symbol
            //
            std::string s;
            for( auto c : inp ) {
                if( isalnum( c ) ) {
                    s.push_back( c );
                } else if( c == '_' ) {
                    s.append( "__" );
                } else {
                    s.append( string_format( "_%02x", c ) );
                }
            }
            return s;
        }

        static std::string sanitize( string_view inp ) { return sanitize( inp, "<>{}.!?"_sv ); }

        std::string operator()( Ast::FunctionDecl* n ) {
            std::string name = n->m_name.std_str();
            if( n->environment_ ) {
                name.insert( 0, "@" );
                name.insert( 0, n->environment_->module()->name().view() );
            }
            if( n->m_intrinsic ) {
                return sanitize( name );
            }
            if( true || n->name() != "main"_sv ) {
                for( auto p : n->m_params ) {
                    assert( p->m_type );
                    assert( p->m_name.c_str() );
                    name.append( ":" );
                    name.append( p->m_type->name() );  // todo valid symbol, spaces etc
                    if( p->m_ref ) {
                        name.append( "&" );
                    }
                }
            } else {
                name = "main"_sv;
            }

            std::string symbol = sanitize( name );
            addName( n, istring::make( symbol ) );
            out.begin( string_concat( "\n", sanitize( n->m_type->m_callable[0]->name() ), " ", symbol, "(" ) );
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
                out.write( string_concat( sep, sanitize( p->m_type->name() ), p->m_ref ? "& " : " ", p->m_name ) );
                sep = ", ";
            }
            out.write( ") {\n" );
            std::string ret = dispatch( n->m_body );
            m_outerFuncCanFail.undefine();
            out.write( string_concat( "return ", ret, ";\n" ) );
            out.end( "}\n" );
            return symbol;
        }

        std::string operator()( Ast::VariableDecl* n ) {
            std::string init{ "" };
            const char* sep{ "" };
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
            out.begin( string_concat( qual, sanitize( n->m_type->name() ), " "_sv, name, "{" ) );
            if( n->m_initializer.empty() == false ) {
                out.write( init );
            }
            out.end( "};"_sv );
            return name;
        }

        std::string operator()( Ast::Assignment* n ) {
            auto lhs = dispatch( n->m_lhs );
            out.write( string_concat( lhs, " = "_sv, dispatch( n->m_rhs ), ";\n"_sv ) );
            return lhs;
        }

        std::string operator()( Ast::FunctionCall* n ) {
            auto func = dispatch( n->m_func );
            if( n->m_func->m_type->m_callCanFail && m_outerFuncCanFail.get() == false ) {
                out.write( "int status{0};\n" );
            }
            std::vector<std::string> args;
            for( auto a : n->m_args ) {
                args.push_back( dispatch( a ) );
            }
            std::string retId;
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

        std::string operator()( Ast::NamedFunctionCall* n ) {
            assert( n->m_resolved );
            return dispatch( n->m_resolved );
        }

        std::string operator()( Ast::PipelineExpr* n ) {
            std::string ret = newVarId();
            out.write( string_concat( sanitize( n->m_type->m_name ), " ", ret, ";\n" ) );
            std::string cur;
            std::string closing = "";
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

        std::string operator()( Ast::UnwrapResult* n ) {
            auto c = dispatch( n->m_src );
            return string_concat( c, ".ok" );
        }

        std::string operator()( Ast::Selector* n ) {
            auto lhs = dispatch( n->m_lhs );
            return string_concat( lhs.c_str(), ".", n->m_rhs->text() );
        }

        std::string operator()( Ast::StructDecl* n ) {
            out.begin( string_concat( "struct ", n->name(), " {\n" ) );
            decls_.push_back( string_concat( "struct ", n->name(), ";\n" ) );
            for( auto f : n->m_fields ) {
                out.write( string_concat( sanitize( f->m_type->name() ), " ", f->name(), ";\n" ) );
            }
            out.end( "};" );
            return n->name().std_str();
        }

        std::string operator()( Ast::TryExpr* n ) {
            auto rhs = dispatch( n->m_expr );
            out.write( string_concat( "if(", rhs, ".fail) {\n" ) );
            out.write( string_concat( "return ", rhs, ".fail;\n" ) );
            out.write( "}\n" );
            return string_concat( rhs, ".ok" );
        }

        static void _boilerplate( Io::TextOutput& out ) {
            out.begin( "#include<stdio.h>\n" );
            out.write( "#include<string>\n" );
            out.write( "#include<array>\n" );
            out.write( "#include<vector>\n" );
            out.write( "#include<cstring>\n" );
            out.write( "struct MainReg {\n" );
            out.write( "    typedef int(*mainfunc)(int argc, const char**);\n" );
            out.write( "    MainReg(const char* n, mainfunc m);\n" );
            out.write( "    const char* name; mainfunc main; const MainReg* next; };\n" );
            out.write( "enum builtin__Error { failed=1 };\n" );
            out.write( "template<typename T> struct builtin_Result { T ok; int fail;\n" );
            out.write( "    builtin_Result( ) : fail( failed ) {}\n" );
            out.write( "    builtin_Result( const T& t ) : ok( t ), fail(0) {}\n" );
            out.write( "    builtin_Result( builtin__Error e ) : fail( e ) {}\n" );
            out.write( "    void setFail( int f ) { fail = f; }\n" );
            out.write( "    void operator=( const T& t ) { ok = t; fail = 0; }\n" );
            out.write( "    void operator=( builtin__Error e ) { fail = e; }\n};\n" );
            out.write(
                "struct builtin__string { std::string m_s; "
                "inline builtin__string() = default; "
                "inline explicit builtin__string(std::string s) : m_s(std::move(s)) {} "
                "inline builtin__string(const char* s, size_t l) : m_s(s,l) {} "
                "};\n" );

            // builtin math
            out.write( "inline bool builtin_40eq_3f(int a, int b) { return a==b; }\n" );
            out.write( "inline bool builtin_40lt_3f(int a, int b) { return a<b; }\n" );
            out.write( "inline bool builtin_40ge_3f(int a, int b) { return a>=b; }\n" );
            out.write( "inline int builtin_40add(int a, int b) { return a+b; }\n" );
            out.write( "inline int builtin_40sub(int a, int b) { return a-b; }\n" );
            out.write( "inline int builtin_40mul(int a, int b) { return a*b; }\n" );
            out.write( "inline int builtin_40div( int a, int b ) { return a / b; }\n" );
            out.write( "inline int builtin_40mod(int a, int b) { return a%b; }\n" );
            out.write( "inline double builtin_40dfromi(int a) { return (double)a; }\n" );
            out.write(
                "inline builtin_Result<int> builtin_40parsei(const builtin__string& s) { int r = ::strtol(s.m_s.data(), nullptr, 0); if(r) "
                "return r; return failed; "
                "}\n" );
            out.write( "inline double builtin_40muld(double a, double b) { return a*b; }\n" );
            out.write( "inline double builtin_40divd(double a, double b) { return a/b; }\n" );
            out.write( "inline double builtin_40addd(double a, double b) { return a+b; }\n" );
            // builtin io
            out.write( "inline int builtin_40puts(const builtin__string& a) { return printf(\"%s\\n\", a.m_s.c_str()); }\n" );
            out.write( "inline int builtin_40puti(int a) { return printf(\"%i\\n\", a); }\n" );
            out.write( "inline int builtin_40putd(double a) { return printf(\"%f\\n\", a); }\n" );
            // array_fixed
            out.write( "template<typename T, size_t N> using builtin_array_fixed = std::array<T,N>;\n" );
            out.write( "template<typename T, size_t N> inline int size(const builtin_array_fixed<T,N>& a) { return N; }\n" );
            out.write( "template<typename T, size_t N> inline T at(const builtin_array_fixed<T,N>& a, int i) { return a[i]; }\n" );
            out.write(
                "template<typename T, size_t N> inline builtin_Result<T> get(const builtin_array_fixed<T,N>& a, int i) { if( unsigned(i) < "
                "unsigned(N) ) return a[i]; "
                "return "
                "failed; }\n" );
            // array_view
            out.write(
                "template<typename T> struct builtin_array_view { "
                "T* m_data; size_t m_count; };\n" );
            out.write( "template<typename T> inline int size(builtin_array_view<T> a) { return (int)a.m_count; }\n" );
            out.write( "template<typename T> inline T at(builtin_array_view<T> a, int i) { return a.m_data[i]; }\n" );
            out.write(
                "template<typename T> inline builtin_Result<T> get(builtin_array_view<T> a, int i) { if( unsigned(i) < unsigned(size(a))) "
                "return a.m_data[i]; "
                "return "
                "failed; }\n" );
            // array_heap
            out.write( "template<typename T> using builtin_array_Heap = std::vector<T>;\n" );
            out.write( "template<typename T> inline void resize(std::vector<T>& a, int n) { a.resize(n); }\n" );
            out.write( "template<typename T> inline void put_(std::vector<T>& a, int i, const T& t) { a[i] = t; }\n" );
            out.write( "template<typename T> inline T at(std::vector<T>& a, int i) { return a[i]; }\n" );
            out.write(
                "template<typename T> inline builtin_Result<T> get(std::vector<T>& a, int i) { if(unsigned(i) < a.size()) return a[i]; "
                "return "
                "failed; }\n" );
            // builtin string
            out.write( "inline void builtin_40strcat_21(builtin__string& a, const builtin__string& b) { a.m_s += b.m_s; }\n" );
            out.write(
                "inline builtin__string operator \"\" _builtin_str( const char* str, size_t len ) noexcept { return "
                "builtin__string{str,len}; }\n" );
            out.write( "inline builtin__string builtin_40tostring( const builtin__string& str) noexcept { return str; }\n" );
            out.write( "inline builtin__string builtin_40tostring( int i ) noexcept { return builtin__string{std::to_string(i)}; }\n" );
            out.write( "inline builtin__string builtin_40tostring( double d ) noexcept { return builtin__string{std::to_string(d)}; }\n" );
            out.write(
                "inline builtin__string builtin_40strjoin( const builtin__string& a, const builtin__string& b, const builtin__string& c ) "
                "noexcept {"
                "builtin__string r = a; r.m_s += b.m_s; r.m_s += c.m_s; return r; }\n" );
            out.write(
                "inline builtin__string builtin_40strjoin("
                "const builtin__string& a, const builtin__string& b, const builtin__string& c, const "
                "builtin__string& d ) noexcept {"
                "builtin__string r = a; r.m_s += b.m_s; r.m_s += c.m_s; r.m_s += d.m_s; return r; }\n" );
            out.write(
                "inline builtin__string builtin_40strjoin("
                "const builtin__string& a, const builtin__string& b, const builtin__string& c, const builtin__string& d, const "
                "builtin__string& e ) noexcept {"
                "builtin__string r = a; r.m_s += b.m_s; r.m_s += c.m_s; r.m_s += d.m_s; r.m_s += e.m_s; return r; }\n" );
            // bitops
            out.write( "inline int bitops_40asl(int a, int b) { return a<<b; } \n" );
            out.write( "inline int bitops_40lsl(int a, int b) { return a<<b; } \n" );
            out.write( "inline int bitops_40asr(int a, int b) { return a>>b; } \n" );
            out.write( "inline int bitops_40lsr(int a, int b) { return int(unsigned(a)>>b); } \n" );
        }

        std::string operator()( Ast::Module* n ) {
            std::vector<char> pass1;
            out.open( &pass1 );
            for( auto n : n->exports() ) {
                dispatch( n.expr );
            }

            // forward declare decls
            out.open( &body_ );
            _boilerplate( out );
            for( auto decl : decls_ ) {
                out.write( decl );
                out.write( "//OK\n" );
            }
            out.write( "//OK2\n" );

            for( auto inst : n->instantiations() ) {
                if( inst.second->generic_ == nullptr ) {
                    out.write( string_format( "//instantiate %s AKA %s\n", inst.first.c_str(), sanitize( inst.first ).c_str() ) );
                    continue;
                }
                std::string cname;
                const char* sep = "";
                for( auto i : inst.second->generic_->args_ ) {
                    cname.append( sep );
                    sep = ",";
                    if( auto n = dynamic_cast<Ast::NamedDecl*>( i ) ) {
                        cname.append( sanitize( n->name() ) );
                    } else if( auto c = dynamic_cast<Ast::LexNumber*>( i ) ) {
                        cname.append( c->text() );
                    } else {
                        assert( false );
                    }
                }
                out.write( string_format( "typedef %s< %s > %s;\n", inst.second->generic_->decl_->m_name.c_str(), cname.c_str(),
                                          sanitize( inst.first ).c_str() ) );
            }
            out.write( { pass1.data(), pass1.size() } );

            istring mainStr = istring::make( "main" );
            istring mainSym;
            int mainKind = -1;
            for( auto n : n->exports() ) {
                if( n.name == mainStr ) {
                    if( auto fd = dynamic_cast<Ast::FunctionDecl*>( n.expr ) ) {
                        mainSym = dispatched_[fd];
                        mainKind = fd->m_params.size() ? 1 : 0;
                    }
                }
            }

            switch( mainKind ) {
                case 0:
                    out.write(
                        string_format( "int %s_main_entry(int argc, const char** argv) {\n"
                                       "   return %s();\n"
                                       "}",
                                       n->name().c_str(), mainSym.c_str() ) );
                    break;
                case 1:
                    out.write(
                        string_format( "int %s_main_entry(int argc, const char** argv) {\n"
                                       "   std::vector<builtin__string> args;\n"
                                       "   for( int i = 0; i < argc; ++i ) {\n"
                                       "       args.emplace_back(argv[i]);\n"
                                       "   }\n"
                                       "   builtin_array_view<builtin__string> view{args.data(), args.size()};\n"
                                       "   return %s(view);\n"
                                       "}",
                                       n->name().c_str(), mainSym.c_str() ) );
                    break;
                default:
                    break;
            }
            if( mainKind != -1 ) {
                out.write(
                    string_concat( "\nstatic MainReg ", n->m_name, "_mainreg(\"", n->m_name, "\", &", n->m_name, "_main_entry );\n" ) );
            }
            return "";
        }

        void write( Io::TextOutput& txt ) { txt.write( { body_.data(), body_.size() } ); }
    };
}  // namespace

Slip::Result Slip::Backend::generate( Ast::Module& module, Io::TextOutput& out ) {
    Generator gen;
    Ast::dispatch<std::string>( &module, gen );
    gen.write( out );

    return Result::OK;
}
