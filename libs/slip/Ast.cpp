#include "slip/pch/Pch.h"

#include "Ast.h"
#include "Errors.h"
#include "Io.h"

namespace Slip::Ast {
#define AST_NODE( X ) \
    int X::tag() const { return Detail::TagOf<X>::Tag; }
#include "Ast.inc"
#undef AST_NODE

    REFLECT_BEGIN( Expr )
    // REFLECT_FIELD2(m_declTypeExpr, Flags::Abbrev)
    REFLECT_END()

    REFLECT_BEGIN( Type )
    // REFLECT_FIELD(m_type)
    REFLECT_FIELD2( m_name, Flags::Abbrev )
    REFLECT_END()

    REFLECT_BEGIN( Decl )
    REFLECT_PARENT( Expr )
    REFLECT_END()

    REFLECT_BEGIN( Block )
    REFLECT_PARENT( Named )
    REFLECT_FIELD2( m_contents, Flags::Child )
    REFLECT_END()

    REFLECT_BEGIN( Break )
    REFLECT_PARENT( Expr )
    REFLECT_FIELD2( m_target, Flags::Abbrev )
    REFLECT_FIELD2( m_value, Flags::Child )
    REFLECT_END()

    REFLECT_BEGIN( CatchExpr )
    REFLECT_PARENT( Expr )
    REFLECT_FIELD2( m_expr, Flags::Child )
    REFLECT_FIELD2( m_fail, Flags::Child )
    REFLECT_END()

    REFLECT_BEGIN( CoroutineDecl )
    REFLECT_PARENT( Named )
    REFLECT_FIELD2( m_params, Flags::Child )
    // REFLECT_FIELD2(m_declReturnTypeExpr, Flags::Child)
    REFLECT_FIELD2( m_body, Flags::Child )
    REFLECT_END()

    REFLECT_BEGIN( CoroutineYield )
    REFLECT_PARENT( Expr )
    REFLECT_FIELD2( m_expr, Flags::Child )
    REFLECT_END()

    REFLECT_BEGIN( Nop )
    REFLECT_PARENT( Expr )
    REFLECT_END()

    REFLECT_BEGIN( Number )
    REFLECT_PARENT( Expr )
    REFLECT_FIELD2( m_num, Flags::Abbrev )
    REFLECT_END()

    REFLECT_BEGIN( String )
    REFLECT_PARENT( Expr )
    REFLECT_FIELD2( m_str, Flags::Abbrev )
    REFLECT_END()

    REFLECT_BEGIN( Module )
    REFLECT_PARENT( Expr )
    REFLECT_FIELD2( m_items, Flags::Child )
    REFLECT_END()

    REFLECT_BEGIN( FunctionCall )
    REFLECT_PARENT( Expr )
    REFLECT_FIELD2( m_func, Flags::Child )
    REFLECT_FIELD2( m_args, Flags::Child )
    REFLECT_END()

    REFLECT_BEGIN( Named )
    REFLECT_PARENT( Expr )
    REFLECT_FIELD2( m_name, Flags::Abbrev )
    REFLECT_END()

    REFLECT_BEGIN( If )
    REFLECT_PARENT( Expr )
    REFLECT_FIELD2( m_cond, Flags::Child )
    REFLECT_FIELD2( m_true, Flags::Child )
    REFLECT_FIELD2( m_false, Flags::Child )
    REFLECT_END()

    REFLECT_BEGIN( While )
    REFLECT_PARENT( Expr )
    REFLECT_FIELD2( m_cond, Flags::Child )
    REFLECT_FIELD2( m_body, Flags::Child )
    REFLECT_END()

    REFLECT_BEGIN( Cond )
    REFLECT_PARENT( Expr )
    REFLECT_FIELD2( m_cases, Flags::Child )
    REFLECT_END()

    REFLECT_BEGIN( FunctionDecl )
    REFLECT_PARENT( Named )
    REFLECT_FIELD2( m_params, Flags::Child )
    // REFLECT_FIELD2(m_declReturnTypeExpr, Flags::Child)
    REFLECT_FIELD2( m_body, Flags::Child )
    REFLECT_END()

    REFLECT_BEGIN( Builtin )
    REFLECT_PARENT( Expr )
    REFLECT_END()

    REFLECT_BEGIN( Environment )
    REFLECT_PARENT( Expr )
    REFLECT_END()

    REFLECT_BEGIN( MacroDecl )
    REFLECT_PARENT( Named )
    REFLECT_FIELD2( m_params, Flags::Child )
    // REFLECT_FIELD2( m_body, Flags::Child )
    REFLECT_END()

    REFLECT_BEGIN( MacroExpansion )
    REFLECT_PARENT( Expr )
    REFLECT_FIELD2( m_expansion, Flags::Child )
    REFLECT_FIELD2( m_macro, Flags::Child )
    REFLECT_FIELD2( m_args, Flags::Child )
    REFLECT_END()

    REFLECT_BEGIN( NamedFunctionCall )
    REFLECT_PARENT( Named )
    REFLECT_FIELD2( m_resolved, Flags::Hidden )
    REFLECT_FIELD2( m_candidates, Flags::Hidden )
    REFLECT_FIELD2( m_args, Flags::Child )
    REFLECT_END()

    REFLECT_BEGIN( VariableDecl )
    REFLECT_PARENT( Named )
    REFLECT_FIELD2( m_initializer, Flags::Child )
    REFLECT_END()

    REFLECT_BEGIN( Assignment )
    REFLECT_PARENT( Expr )
    REFLECT_FIELD2( m_lhs, Flags::Child )
    REFLECT_FIELD2( m_rhs, Flags::Child )
    REFLECT_END()

    REFLECT_BEGIN( Sequence )
    REFLECT_PARENT( Expr )
    REFLECT_FIELD2( m_items, Flags::Child )
    REFLECT_END()

    REFLECT_BEGIN( Parameter )
    REFLECT_PARENT( Named )
    REFLECT_END()

    REFLECT_BEGIN( PipelineExpr )
    REFLECT_PARENT( Expr )
    REFLECT_FIELD2( m_stages, Flags::Child )
    REFLECT_END()

    REFLECT_BEGIN( PipelineExpr::Stage )
    REFLECT_FIELD2( expr, Flags::Child )
    REFLECT_END()

    REFLECT_BEGIN( Reference )
    REFLECT_PARENT( Expr )
    REFLECT_FIELD2( m_target, Flags::Abbrev )
    REFLECT_END()

    REFLECT_BEGIN( Scope )
    REFLECT_PARENT( Expr )
    REFLECT_FIELD2( m_child, Flags::Child )
    REFLECT_END()

    REFLECT_BEGIN( Selector )
    REFLECT_PARENT( Expr )
    REFLECT_FIELD2( m_lhs, Flags::Child )
    REFLECT_FIELD2( m_rhs, Flags::Child )
    REFLECT_END()

    REFLECT_BEGIN( StructDecl )
    REFLECT_PARENT( Named )
    REFLECT_FIELD2( m_fields, Flags::Child )
    REFLECT_END()

    REFLECT_BEGIN( StructField )
    REFLECT_PARENT( Named )
    REFLECT_END()

    REFLECT_BEGIN( Definition )
    REFLECT_PARENT( Named )
    REFLECT_FIELD2( m_value, Flags::Child )
    REFLECT_END()

    REFLECT_BEGIN( TryExpr )
    REFLECT_PARENT( Expr )
    REFLECT_FIELD2( m_expr, Flags::Child )
    REFLECT_END()

    REFLECT_BEGIN( UnwrapResult )
    REFLECT_PARENT( Expr )
    REFLECT_FIELD2( m_src, Flags::Child )
    REFLECT_END()
}  // namespace Slip::Ast

using namespace Slip;

size_t Ast::Expr::s_serial;

Ast::Expr* Ast::Expr::resolve() { return this; }

Result Ast::Environment::bind( istring sym, Expr* value ) {
    auto it = syms_.lower_bound( sym );
    if( it == syms_.end() || it->first != sym ) {  // new element
        syms_.emplace( sym, value );
        return Result::OK;
    }
    // Duplicate. Only function overloads are allowed.
    auto fdVal = dynamic_cast<Ast::FunctionDecl*>( value );
    auto fdCur = dynamic_cast<Ast::FunctionDecl*>( it->second );
    auto& loc = it->second->m_loc;
    RETURN_ERROR_IF( fdVal == nullptr || fdCur == nullptr, Error::CannotOverload, value->m_loc,
                     "Only functions can be overloaded. '%s' is already defined\n"
                     "%s:%i:%i: Previously defined here",
                     sym.c_str(), loc.filename(), loc.line(), loc.col() );
    syms_.emplace_hint( it, sym, value );
    return Result::OK;
}

Result Ast::FunctionDecl::NotImplemented( array_view<Ast::Expr*> args, Ast::Expr** out ) { return Error::NotImplemented; }

Ast::FunctionDecl* Ast::FunctionDecl::makeBinaryOp( string_view name, Parameter* a, Parameter* b, Expr* ret ) {
    auto f = new FunctionDecl( name );
    f->m_declReturnTypeExpr = ret;
    f->m_body = new Expr();
    f->m_body->m_declTypeExpr = ret;
    f->m_params.push_back( a );
    f->m_params.push_back( b );
    return f;
}

Ast::FunctionDecl* Ast::FunctionDecl::makeIntrinsic( string_view name, Func<IntrinsicProto>&& intrin, Expr* ret,
                                                     std::initializer_list<Parameter*> params ) {
    auto f = new FunctionDecl( name );
    f->m_intrinsic = std::move( intrin );
    f->m_declReturnTypeExpr = ret;
    f->m_body = new Expr();
    f->m_body->m_declTypeExpr = ret;
    f->m_params = params;
    return f;
}

Ast::Expr* Ast::Reference::resolve() { return m_target; }

Ast::Type Ast::s_typeType( "Type"sv );
Ast::Type Ast::s_typeInt( "int"sv );
Ast::Type Ast::s_typeBool( "bool"sv );
Ast::Type Ast::s_typeError( "Error"sv );
Ast::Type Ast::s_typeDouble( "double"sv );
Ast::Type Ast::s_typeFloat( "float"sv );
Ast::Type Ast::s_typeVoid( "void"sv );
Ast::Type Ast::s_typeString( "string"sv );

Ast::Type::Type( string_view sym ) : Named( istring::make( sym ) ) {}

Ast::Type::Type( istring sym ) : Named( sym ) {}

static void print( Reflect::Var top, Io::TextOutput& out, bool abbrev ) {
    if( auto f = top.type->toString ) {
        string_view s = ( *f )( top.addr );
        out.write( s );
        return;
    }
    switch( top.type->kind ) {
        case Reflect::Kind::Array: {
            auto et = top.type->sub;
            char* s = ( (char**)top.addr )[0];
            char* e = ( (char**)top.addr )[1];
            unsigned count = unsigned( ( e - s ) / et->size );
            for( unsigned i = 0; i < count; ++i ) {
                Reflect::Var e{s + i * et->size, et};
                print( e, out, false );
            }
            break;
        }
        case Reflect::Kind::Record: {
            if( top.type->fields.empty() && top.type->parent ) {
                print( {top.addr, top.type->parent}, out, abbrev );
                return;
            }
            std::vector<const Reflect::Type*> chain;
            for( auto c = top.type; c; c = c->parent ) {
                chain.push_back( c );
            }
            if( abbrev ) {
                for( auto c : reversed( chain ) ) {
                    for( auto f : c->fields ) {
                        if( ( f.flags & ( Ast::Flags::Abbrev | Ast::Flags::Child ) ) == Ast::Flags::Abbrev ) {
                            out.sep();
                            print( top[f], out, true );
                        }
                    }
                }
            } else {
                out.write( string_format( "\n%s " /*0x%p*/, top.type->name, top.addr ) );
                for( auto c : reversed( chain ) ) {
                    for( auto f : c->fields ) {
                        if( f.flags == Ast::Flags::Hidden ) {
                        } else if( ( f.flags & Ast::Flags::Child ) == 0 ) {
                            out.write( string_concat( " ", f.name, "={" ) );
                            print( top[f], out, true );
                            out.write( "}" );
                        }
                    }
                }

                out.begin( {} );
                for( auto c : reversed( chain ) ) {
                    for( auto f : c->fields ) {
                        if( f.flags & Ast::Flags::Child ) {
                            print( top[f], out, false );
                        }
                    }
                }
                out.end();
            }
            break;
        }
        case Reflect::Kind::Pointer: {
            if( void* obj = *(void**)top.addr ) {
                auto sub = top.type->sub;
                Reflect::Var e{obj, sub->dynamicType( obj )};
                print( e, out, abbrev );
            } else {
                out.write( "null" );
            }
            break;
        }
        case Reflect::Kind::String: {
            string_view s = top.type->toString( top.addr );
            out.write( string_concat( "\"", s, "\"" ) );
            break;
        }
        default: { assert( false ); }
    }
}

void Ast::print( Expr* node ) {
    Io::TextOutput out;
    Reflect::Var top{node};
    ::print( top, out, false );
    out.nl();
}

void Ast::print( Expr* node, Io::TextOutput& out ) {
    if( !node )
        return;
    Reflect::Var top{node};
    ::print( top, out, false );
    out.nl();
}
