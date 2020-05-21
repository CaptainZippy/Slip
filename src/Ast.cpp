#include "pch/Pch.h"

#include "Ast.h"
#include "Io.h"

namespace Slip::Ast {
#define AST_NODE( X ) \
    int X::tag() const { return Detail::TagOf<X>::Tag; }
#include "Ast.inc"
#undef AST_NODE

    REFLECT_BEGIN( Node )
    // REFLECT_FIELD2(m_declTypeExpr, Flags::Abbrev)
    REFLECT_END()

    REFLECT_BEGIN( Type )
    // REFLECT_FIELD(m_type)
    REFLECT_FIELD2( m_name, Flags::Abbrev )
    REFLECT_END()

    REFLECT_BEGIN( Decl )
    REFLECT_PARENT( Node )
    REFLECT_END()

    REFLECT_BEGIN( Number )
    REFLECT_PARENT( Node )
    REFLECT_FIELD2( m_num, Flags::Abbrev )
    REFLECT_END()

    REFLECT_BEGIN( String )
    REFLECT_PARENT( Node )
    REFLECT_FIELD2( m_str, Flags::Abbrev )
    REFLECT_END()

    REFLECT_BEGIN( Module )
    REFLECT_PARENT( Node )
    REFLECT_FIELD2( m_items, Flags::Child )
    REFLECT_END()

    REFLECT_BEGIN( FunctionCall )
    REFLECT_PARENT( Node )
    REFLECT_FIELD2( m_func, Flags::Child )
    REFLECT_FIELD2( m_args, Flags::Child )
    REFLECT_END()

    REFLECT_BEGIN( Named )
    REFLECT_PARENT( Node )
    REFLECT_FIELD2( m_name, Flags::Abbrev )
    REFLECT_END()

    REFLECT_BEGIN( If )
    REFLECT_PARENT( Node )
    REFLECT_FIELD2( m_cond, Flags::Child )
    REFLECT_FIELD2( m_true, Flags::Child )
    REFLECT_FIELD2( m_false, Flags::Child )
    REFLECT_END()

    REFLECT_BEGIN( While )
    REFLECT_PARENT( Node )
    REFLECT_FIELD2( m_cond, Flags::Child )
    REFLECT_FIELD2( m_body, Flags::Child )
    REFLECT_END()

    REFLECT_BEGIN( Cond )
    REFLECT_PARENT( Node )
    REFLECT_FIELD2( m_cases, Flags::Child )
    REFLECT_END()

    REFLECT_BEGIN( FunctionDecl )
    REFLECT_PARENT( Named )
    REFLECT_FIELD2( m_args, Flags::Child )
    // REFLECT_FIELD2(m_declReturnTypeExpr, Flags::Child)
    REFLECT_FIELD2( m_body, Flags::Child )
    REFLECT_END()

    REFLECT_BEGIN( Builtin )
    REFLECT_PARENT( Node )
    REFLECT_END()

    REFLECT_BEGIN( Environment )
    REFLECT_PARENT( Node )
    REFLECT_END()

    REFLECT_BEGIN( MacroDecl )
    REFLECT_PARENT( Named )
    REFLECT_FIELD2( m_args, Flags::Child )
    //REFLECT_FIELD2( m_body, Flags::Child )
    REFLECT_END()

    REFLECT_BEGIN( MacroExpansion )
    REFLECT_PARENT( Node )
    REFLECT_FIELD2( m_expansion, Flags::Child )
    REFLECT_FIELD2( m_macro, Flags::Child )
    REFLECT_FIELD2( m_args, Flags::Child )
    REFLECT_END()

    REFLECT_BEGIN( NamedFunctionCall )
    REFLECT_PARENT( Named )
    REFLECT_FIELD2( m_candidates, Flags::Child )
    REFLECT_FIELD2( m_args, Flags::Child )
    REFLECT_END()

    REFLECT_BEGIN( VariableDecl )
    REFLECT_PARENT( Named )
    REFLECT_FIELD2( m_initializer, Flags::Child )
    REFLECT_END()

    REFLECT_BEGIN( Assignment )
    REFLECT_PARENT( Node )
    REFLECT_FIELD2( m_lhs, Flags::Child )
    REFLECT_FIELD2( m_rhs, Flags::Child )
    REFLECT_END()

    REFLECT_BEGIN( Sequence )
    REFLECT_PARENT( Node )
    REFLECT_FIELD2( m_items, Flags::Child )
    REFLECT_END()

    REFLECT_BEGIN( Argument )
    REFLECT_PARENT( Named )
    REFLECT_END()

    REFLECT_BEGIN( Reference )
    REFLECT_PARENT( Node )
    REFLECT_FIELD2( m_target, Flags::Abbrev )
    REFLECT_END()

    REFLECT_BEGIN( Scope )
    REFLECT_PARENT( Node )
    REFLECT_FIELD2( m_child, Flags::Child )
    REFLECT_END()

    REFLECT_BEGIN( Definition )
    REFLECT_PARENT( Named )
    REFLECT_FIELD2( m_value, Flags::Child )
    REFLECT_END()
}  // namespace Slip::Ast

using namespace Slip;

size_t Ast::Node::s_serial;

Ast::Node* Ast::Node::resolve() { return this; }

Ast::Node* Ast::Reference::resolve() { return m_target; }

Ast::Type Ast::s_typeType( "Type"sv );
Ast::Type Ast::s_typeInt( "int"sv );
Ast::Type Ast::s_typeBool( "bool"sv );
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
                        if( ( f.flags & Ast::Flags::Child ) == 0 ) {
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

void Ast::print( Node* node ) {
    Io::TextOutput out;
    Reflect::Var top{node};
    ::print( top, out, false );
    out.nl();
}

void Ast::print( Module* node ) { print( static_cast<Node*>( node ) ); }

void Ast::print( Node* node, Io::TextOutput& out ) {
    if( !node ) return;
    Reflect::Var top{node};
    ::print( top, out, true );
    out.nl();
}

Ast::FunctionDecl* Ast::FunctionDecl::makeBinaryOp( string_view name, Argument* a, Argument* b, Node* ret ) {
    auto f = new FunctionDecl( name );
    f->m_declReturnTypeExpr = ret;
    f->m_body = new Node();
    f->m_body->m_declTypeExpr = ret;
    f->m_args.push_back( a );
    f->m_args.push_back( b );
    return f;
}

Ast::FunctionDecl* Ast::FunctionDecl::makeIntrinsic( string_view name, Intrinsic intrin, Node* ret,
                                                     std::initializer_list<Argument*> args ) {
    auto f = new FunctionDecl( name );
    f->m_intrinsic = intrin;
    f->m_declReturnTypeExpr = ret;
    f->m_body = new Node();
    f->m_body->m_declTypeExpr = ret;
    f->m_args = args;
    return f;
}
