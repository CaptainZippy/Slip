#include "slip/pch/Pch.h"

#include "Ast.h"
#include "Errors.h"
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

        Result dispatch( Ast::Expr* n, std::string& value ) {
            value.clear();
            auto it = dispatched_.find( n );
            if( it != dispatched_.end() ) {
                value = it->second.std_str();
                return Result::OK;
            }
            std::string s;
            RETURN_IF_FAILED( Ast::dispatch<Result>( n, *this, s ) );
            if( s.size() ) {
                dispatched_.emplace( n, istring::make( s ) );
            }
            value = std::move( s );
            return Result::OK;
        }

        Result operator()( Ast::Expr* n, std::string& value ) {
            RETURN_ERROR( Error::NotImplemented, n->m_loc, "Cannot generate C++ for '%s'", n->dynamicType()->name );
        }

        Result operator()( Ast::Type* n, std::string& value ) {
            value = n->name().std_str();
            return Result::OK;
        }

        Result operator()( Ast::CatchExpr* n, std::string& value ) {
            std::string rhs;
            RETURN_IF_FAILED( dispatch( n->m_expr, rhs ) );
            out.write( string_concat( "if(", rhs, ".fail) {\n" ) );
            std::string fail;
            RETURN_IF_FAILED( dispatch( n->m_fail, fail ) );
            out.write( string_concat( rhs, ".ok = ", fail, ";\n" ) );
            out.write( "}\n" );
            value = string_concat( rhs, ".ok" );
            return Result::OK;
        }

        Result operator()( Ast::CoroutineYield* n, std::string& value ) {
            std::string r;
            RETURN_IF_FAILED( dispatch( n->m_expr, r ) );
            out.write( string_concat( "state_ = 1;\n" ) );
            out.write( string_concat( "return ", r, ";\n" ) );
            out.write( string_concat( "case_1: /*yield*/\n" ) );
            value = "/*yield*/";  // FIXME.coro
            return Result::OK;
        }

        Result operator()( Ast::CoroutineDecl* n, std::string& value ) {
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
            std::string name;
            RETURN_IF_FAILED( dispatch( n->m_declReturnTypeExpr, name ) );
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
            std::string ignored;
            RETURN_IF_FAILED( dispatch( n->m_body, ignored ) );
            m_outerFuncCanFail.undefine();
            out.write( "goto case_err;\n" );
            out.begin( "}\n" );
            out.end( "};" );
            value = n->name().std_str();
            return Result::OK;
        }
        Result operator()( Ast::MacroDecl* n, std::string& value ) {
            value.clear();
            return Result::OK;
        }

        Result operator()( Ast::MacroExpansion* n, std::string& value ) { return dispatch( n->m_expansion, value ); }

        Result operator()( Ast::Reference* n, std::string& value ) { return dispatch( n->m_target, value ); }
        Result operator()( Ast::Parameter* n, std::string& value ) {
            addName( n, n->name() );
            value = n->m_name.std_str();
            return Result::OK;
        }

        Result operator()( Ast::Nop* n, std::string& value ) {
            value.clear();
            return Result::OK;
        }

        Result operator()( Ast::Number* n, std::string& value ) {
            value = string_format( "(%s)%s", sanitize( n->m_type->name() ).c_str(), n->m_num.c_str() );
            return Result::OK;
        }
        Result operator()( Ast::String* n, std::string& value ) {
            std::string s;
            for( unsigned char c : n->m_str ) {
                switch( c ) {
                    case '\\': s += "\\\\"; break;
                    case '"': s += "\\\""; break;
                    case '\n': s += "\\n"; break;
                    case '\r': s += "\\r"; break;
                    case '\t': s += "\\t"; break;
                    default:
                        if( c < 32 || c == 127 ) {
                            s += string_format( "\\%03o", static_cast<unsigned>( c ) );
                        } else {
                            s += c;
                        }
                        break;
                }
            }
            value = string_concat( "\"", s, "\"_builtin_str" );
            return Result::OK;
        }

        Result operator()( Ast::DataList* n, std::string& value ) {
            // Use standard C++ brace-init ("Type{...}") rather than a C99-style
            // compound literal ("(Type) {...}"), which MSVC rejects under /permissive-.
            std::string type;
            RETURN_IF_FAILED( dispatch( n->m_type, type ) );
            std::string s = string_concat( type, "{" );
            const char* sep = "";
            for( auto a : n->items() ) {
                out.nl();
                s += sep;
                std::string item;
                RETURN_IF_FAILED( dispatch( a, item ) );
                s += item;
                sep = ",";
            }
            s += "}";
            value = std::move( s );
            return Result::OK;
        }

        Result operator()( Ast::Definition* n, std::string& value ) {
            std::string val;
            RETURN_IF_FAILED( dispatch( n->m_value, val ) );
            out.write( string_format( "%s %s = %s", sanitize( n->m_type->name() ).c_str(), n->m_name, val.c_str() ) );
            value = n->m_name.std_str();
            return Result::OK;
        }

        Result operator()( Ast::Sequence* n, std::string& value ) {
            for( auto a : n->items().rtrim( 1 ) ) {
                std::string ignored;
                RETURN_IF_FAILED( dispatch( a, ignored ) );
                out.nl();
            }
            return dispatch( n->m_items.back(), value );
        }

        Result operator()( Ast::Scope* n, std::string& value ) { return dispatch( n->m_child, value ); }

        Result operator()( Ast::Block* n, std::string& value ) {
            std::string var;
            if( n->m_type != &Ast::s_typeVoid ) {
                var = string_format( "v_%s", n->name().c_str() );
                out.write( string_format( "%s %s;\n", sanitize( n->m_type->name() ).c_str(), var.c_str() ) );
            }
            out.begin( "{\n" );
            std::string s;
            RETURN_IF_FAILED( dispatch( n->m_contents, s ) );
            if( var.size() ) {
                out.end( string_format( "\n%s = %s;\n}\n", var.c_str(), s.c_str() ) );
            }
            out.write( string_format( "\nL%s:\n", n->name().c_str() ) );
            value = std::move( var );
            return Result::OK;
        }

        Result operator()( Ast::Break* n, std::string& value ) {
            std::string s;
            RETURN_IF_FAILED( dispatch( n->m_value, s ) );
            out.write( string_format( "v_%s = %s;\n", n->m_target->m_name.c_str(), s.c_str() ) );
            out.write( string_format( "goto L%s;\n", n->m_target->m_name.c_str() ) );
            value = std::move( s );
            return Result::OK;
        }
        Result operator()( Ast::If* n, std::string& value ) {
            bool hasVar = n->m_type != &Ast::s_typeVoid;
            auto ret = hasVar ? newVarId() : "";
            if( hasVar ) {
                out.write( string_format( "%s %s;\n", sanitize( n->m_type->name() ).c_str(), ret.c_str() ) );
            }
            out.begin( " {\n" );
            std::string cond;
            RETURN_IF_FAILED( dispatch( n->m_cond, cond ) );
            out.begin( string_format( "if(%s) {\n", cond.c_str() ) );
            std::string t;
            RETURN_IF_FAILED( dispatch( n->m_true, t ) );
            if( hasVar ) {
                out.write( string_concat( ret, " = ", t, ";\n" ) );
            }
            out.end( "}\n" );
            out.begin( "else {\n" );
            std::string f;
            RETURN_IF_FAILED( dispatch( n->m_false, f ) );
            if( hasVar ) {
                out.write( string_concat( ret, " = ", f, ";\n" ) );
            }
            out.end( "}" );
            out.end( "}\n" );
            value = std::move( ret );
            return Result::OK;
        }

        Result operator()( Ast::While* n, std::string& value ) {
            bool hasVar = n->m_type != &Ast::s_typeVoid;
            auto ret = hasVar ? newVarId() : "";
            if( hasVar ) {
                out.write( string_format( "%s %s;\n", sanitize( n->m_type->name() ).c_str(), ret.c_str() ) );
            }
            out.begin( "while(true) {\n");
            // Evaluate the condition (and any temporaries it needs) in its own
            // nested scope so they go out of scope before the loop body, which
            // may contain a coroutine resume label ("case_N:") that a goto can
            // jump directly into; otherwise MSVC (rightly) rejects jumping past
            // the temporaries' initialization with C2362.
            out.begin( "{\n" );
            std::string cond;
            RETURN_IF_FAILED( dispatch( n->m_cond, cond ) );
            out.write( string_concat( "if(!", cond, ") { break; }\n" ) );
            out.end( "}\n" );
            std::string b;
            RETURN_IF_FAILED( dispatch( n->m_body, b ) );
            if( hasVar ) {
                out.write( string_concat( ret, " = ", b, ";\n" ) );
            }
            out.end( "}\n" );
            value = std::move( ret );
            return Result::OK;
        }

        Result operator()( Ast::Cond* n, std::string& value ) {
            auto ret = newVarId();
            out.write( string_format( "%s %s;", sanitize( n->m_type->name() ).c_str(), ret.c_str() ) );
            out.begin( " {\n" );
            for( auto c : n->m_cases ) {
                std::string cond;
                RETURN_IF_FAILED( dispatch( c.first, cond ) );
                out.begin( string_format( "if(%s) {\n", cond.c_str() ) );
                std::string t;
                RETURN_IF_FAILED( dispatch( c.second, t ) );
                out.write( string_concat( ret, " = ", t, ";\n" ) );
                out.end( "}\n" );
                out.begin( "else {\n" );
            }
            for( auto c : n->m_cases ) {
                (void)c;
                out.end( "}" );
            }
            out.end( "\n}\n" );
            value = std::move( ret );
            return Result::OK;
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

        Result operator()( Ast::FunctionDecl* n, std::string& value ) {
            std::string name = n->m_name.std_str();
            if( n->environment_ ) {
                name.insert( 0, "@" );
                name.insert( 0, n->environment_->module()->name().view() );
            }
            if( n->m_intrinsic ) {
                value = sanitize( name );
                return Result::OK;
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
            std::string ret;
            RETURN_IF_FAILED( dispatch( n->m_body, ret ) );
            m_outerFuncCanFail.undefine();
            if( n->m_type->m_callable[0] == &Ast::s_typeVoid ) {
                out.write( "return;\n" );
            } else {
                out.write( string_concat( "return ", ret, ";\n" ) );
            }
            out.end( "}\n" );
            value = std::move( symbol );
            return Result::OK;
        }

        Result operator()( Ast::VariableDecl* n, std::string& value ) {
            std::string init{ "" };
            const char* sep{ "" };
            for( auto&& i : n->m_initializer ) {
                init.append( sep );
                std::string item;
                RETURN_IF_FAILED( dispatch( i, item ) );
                init.append( item );
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
            value = std::move( name );
            return Result::OK;
        }

        Result operator()( Ast::Assignment* n, std::string& value ) {
            std::string lhs, rhs;
            RETURN_IF_FAILED( dispatch( n->m_lhs, lhs ) );
            RETURN_IF_FAILED( dispatch( n->m_rhs, rhs ) );
            out.write( string_concat( lhs, " = "_sv, rhs, ";\n"_sv ) );
            value = std::move( lhs );
            return Result::OK;
        }

        Result operator()( Ast::FunctionCall* n, std::string& value ) {
            std::string func;
            RETURN_IF_FAILED( dispatch( n->m_func, func ) );
            if( n->m_func->m_type->m_callCanFail && m_outerFuncCanFail.get() == false ) {
                out.write( "int status{0};\n" );
            }
            std::vector<std::string> args;
            for( auto a : n->m_args ) {
                std::string arg;
                RETURN_IF_FAILED( dispatch( a, arg ) );
                args.push_back( std::move( arg ) );
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
            value = std::move( retId );
            return Result::OK;
        }

        Result operator()( Ast::NamedFunctionCall* n, std::string& value ) {
            RETURN_ERROR_IF( !n->m_resolved, Error::UnresolvedCall, n->m_loc );
            return dispatch( n->m_resolved, value );
        }

        Result operator()( Ast::PipelineExpr* n, std::string& value ) {
            std::string ret = newVarId();
            out.write( string_concat( sanitize( n->m_type->m_name ), " ", ret, ";\n" ) );
            std::string cur;
            std::string closing = "";
            for( auto&& stage : n->m_stages ) {
                RETURN_IF_FAILED( dispatch( stage.expr, cur ) );
                if( stage.canFail ) {
                    out.write( string_concat( "if(", cur, ".fail) { ", ret, ".setFail(", cur, ".fail); } else {\n" ) );
                    closing += "}";
                }
            }
            out.write( string_concat( ret, " = ", cur, ".ok;\n", closing, "\n" ) );

            value = std::move( ret );
            return Result::OK;
        }

        Result operator()( Ast::UnwrapResult* n, std::string& value ) {
            std::string c;
            RETURN_IF_FAILED( dispatch( n->m_src, c ) );
            value = string_concat( c, ".ok" );
            return Result::OK;
        }

        Result operator()( Ast::Selector* n, std::string& value ) {
            std::string lhs;
            RETURN_IF_FAILED( dispatch( n->m_lhs, lhs ) );
            value = string_concat( lhs.c_str(), ".", n->m_rhs->text() );
            return Result::OK;
        }

        Result operator()( Ast::StructDecl* n, std::string& value ) {
            out.begin( string_concat( "struct ", n->name(), " {\n" ) );
            decls_.push_back( string_concat( "struct ", n->name(), ";\n" ) );
            for( auto f : n->m_fields ) {
                out.write( string_concat( sanitize( f->m_type->name() ), " ", f->name(), ";\n" ) );
            }
            out.end( "};" );
            value = n->name().std_str();
            return Result::OK;
        }

        Result operator()( Ast::TryExpr* n, std::string& value ) {
            std::string rhs;
            RETURN_IF_FAILED( dispatch( n->m_expr, rhs ) );
            out.write( string_concat( "if(", rhs, ".fail) {\n" ) );
            out.write( string_concat( "return ", rhs, ".fail;\n" ) );
            out.write( "}\n" );
            value = string_concat( rhs, ".ok" );
            return Result::OK;
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

        Result operator()( Ast::Module* n, std::string& value ) {
            std::vector<char> pass1;
            RETURN_IF_FAILED( out.open( &pass1 ) );
            for( auto n : n->exports() ) {
                std::string ignored;
                RETURN_IF_FAILED( dispatch( n.expr, ignored ) );
            }

            // forward declare decls
            RETURN_IF_FAILED( out.open( &body_ ) );
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
                        RETURN_ERROR( Error::NotImplemented, i->m_loc, "Cannot generate C++ for generic argument '%s'",
                                      i->dynamicType()->name );
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
            value.clear();
            return Result::OK;
        }

        void write( Io::TextOutput& txt ) { txt.write( { body_.data(), body_.size() } ); }
    };
}  // namespace

Slip::Result Slip::Backend::generate( Ast::Module& module, Io::TextOutput& out ) {
    Generator gen;
    std::string ignored;
    RETURN_IF_FAILED( gen.dispatch( &module, ignored ) );
    gen.write( out );

    return Result::OK;
}
