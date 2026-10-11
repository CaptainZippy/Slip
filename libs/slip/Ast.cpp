#include "slip/pch/Pch.h"

#include "Ast.h"
#include "Errors.h"
#include "Io.h"

Slip::Ast::Number::Representation Slip::Ast::Number::representation() const {
    if( m_type == &s_typeBool )
        return Representation::Boolean;
    if( m_type == &s_typeInt || m_type == &s_typeError )
        return Representation::Integer;
    if( m_type == &s_typeFloat || m_type == &s_typeDouble )
        return Representation::Real;
    return Representation::None;
}

Slip::Result Slip::Ast::Number::checkRepresentation( Representation expected ) const {
    RETURN_ERROR_IF( representation() != expected, Error::InternalTypeConstraintFailed, m_loc,
                     "Number type '%s' does not match the requested value representation",
                     m_type ? m_type->name().c_str() : "<unresolved>" );
    return Result::OK;
}

Slip::Result Slip::Ast::Number::setValue( bool value ) {
    RETURN_ERROR_IF( m_hasValue, Error::InternalTypeConstraintFailed, m_loc, "Number value is already initialized" );
    RETURN_IF_FAILED( checkRepresentation( Representation::Boolean ) );
    m_value.boolean = value;
    m_hasValue = true;
    return Result::OK;
}

Slip::Result Slip::Ast::Number::setValue( int64_t value ) {
    RETURN_ERROR_IF( m_hasValue, Error::InternalTypeConstraintFailed, m_loc, "Number value is already initialized" );
    RETURN_IF_FAILED( checkRepresentation( Representation::Integer ) );
    m_value.integer = value;
    m_hasValue = true;
    return Result::OK;
}

Slip::Result Slip::Ast::Number::setValue( double value ) {
    RETURN_ERROR_IF( m_hasValue, Error::InternalTypeConstraintFailed, m_loc, "Number value is already initialized" );
    RETURN_IF_FAILED( checkRepresentation( Representation::Real ) );
    m_value.real = value;
    m_hasValue = true;
    return Result::OK;
}

Slip::Result Slip::Ast::Number::getValue( bool& value ) const {
    RETURN_ERROR_IF( !m_hasValue, Error::InternalUncheckedEvaluation, m_loc, "Number literal has not been evaluated" );
    RETURN_IF_FAILED( checkRepresentation( Representation::Boolean ) );
    value = m_value.boolean;
    return Result::OK;
}

Slip::Result Slip::Ast::Number::getValue( int64_t& value ) const {
    RETURN_ERROR_IF( !m_hasValue, Error::InternalUncheckedEvaluation, m_loc, "Number literal has not been evaluated" );
    RETURN_IF_FAILED( checkRepresentation( Representation::Integer ) );
    value = m_value.integer;
    return Result::OK;
}

Slip::Result Slip::Ast::Number::getValue( double& value ) const {
    RETURN_ERROR_IF( !m_hasValue, Error::InternalUncheckedEvaluation, m_loc, "Number literal has not been evaluated" );
    RETURN_IF_FAILED( checkRepresentation( Representation::Real ) );
    value = m_value.real;
    return Result::OK;
}

Slip::Result Slip::Ast::Number::format( std::string& out ) const {
    RETURN_ERROR_IF( !m_hasValue, Error::InternalUncheckedEvaluation, m_loc, "Number literal has not been evaluated" );
    switch( representation() ) {
        case Representation::Boolean:
            out.append( m_value.boolean ? "true" : "false" );
            break;
        case Representation::Integer:
            out.append( std::to_string( m_value.integer ) );
            break;
        case Representation::Real: {
            // Shortest spelling that reads back as the same double.
            char buf[32];
            auto r = std::to_chars( buf, buf + sizeof( buf ), m_value.real );
            RETURN_ERROR_IF( r.ec != std::errc{}, Error::InternalTypeConstraintFailed, m_loc, "Cannot format floating point number" );
            std::string s( buf, r.ptr );
            if( s.find_first_of( ".eEn" ) == std::string::npos ) {
                s += ".0";  // keep it a floating point literal in generated code
            }
            out.append( s );
            break;
        }
        default:
            RETURN_ERROR( Error::InternalTypeConstraintFailed, m_loc, "Number does not have a numeric type" );
    }
    return Result::OK;
}

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
    REFLECT_PARENT( NamedDecl )
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
    REFLECT_PARENT( NamedDecl )
    REFLECT_FIELD2( m_params, Flags::Child )
    // REFLECT_FIELD2(m_declReturnTypeExpr, Flags::Child)
    REFLECT_FIELD2( m_body, Flags::Child )
    REFLECT_END()

    REFLECT_BEGIN( CoroutineYield )
    REFLECT_PARENT( Expr )
    REFLECT_FIELD2( m_expr, Flags::Child )
    REFLECT_END()

    REFLECT_BEGIN( DataList )
    REFLECT_PARENT( Expr )
    REFLECT_FIELD2( m_items, Flags::Child )
    REFLECT_END()

    REFLECT_BEGIN( Nop )
    REFLECT_PARENT( Expr )
    REFLECT_END()

    REFLECT_BEGIN( NowExpr )
    REFLECT_PARENT( Expr )
    REFLECT_FIELD2( m_expr, Flags::Child )
    REFLECT_FIELD2( m_resolved, Flags::Child )
    REFLECT_END()

    REFLECT_BEGIN( Number )
    REFLECT_PARENT( Expr )
    REFLECT_TO_STRING( []( const void* addr, std::string& out ) {
        auto n = static_cast<const Number*>( addr );
        if( n->hasValue() ) {
            if( !n->format( out ).isOk() )
                out.append( "<invalid number>" );
        } else if( n->m_loc.m_file ) {
            out.append( n->m_loc.text() );
        } else {
            out.append( "<unevaluated number>" );
        }
    } )
    REFLECT_END()

    REFLECT_BEGIN( String )
    REFLECT_PARENT( Expr )
    REFLECT_FIELD2( m_str, Flags::Abbrev )
    REFLECT_END()

    REFLECT_BEGIN( Module )
    REFLECT_PARENT( NamedDecl )
    REFLECT_FIELD2( exports_, Flags::Child )
    REFLECT_END()

    REFLECT_BEGIN( Module::Export )
    REFLECT_FIELD( name )
    REFLECT_FIELD2( expr, Flags::Abbrev )
    REFLECT_END()

    REFLECT_BEGIN( FunctionCall )
    REFLECT_PARENT( Expr )
    REFLECT_FIELD2( m_func, Flags::Child )
    REFLECT_FIELD2( m_args, Flags::Child )
    REFLECT_END()

    REFLECT_BEGIN( NamedDecl )
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
    REFLECT_PARENT( NamedDecl )
    REFLECT_FIELD2( m_params, Flags::Child )
    // REFLECT_FIELD2(m_declReturnTypeExpr, Flags::Child)
    REFLECT_FIELD2( m_body, Flags::Child )
    REFLECT_END()

    REFLECT_BEGIN( GenericDecl )
    REFLECT_PARENT( Expr )
    REFLECT_FIELD2( params_, Flags::Child )
    REFLECT_FIELD2( body_, Flags::Child )
    REFLECT_END()

    REFLECT_BEGIN( GenericInstantiation )
    REFLECT_PARENT( NamedDecl )
    REFLECT_FIELD2( args_, Flags::Child )
    REFLECT_FIELD2( decl_, Flags::Child )
    REFLECT_END()

    REFLECT_BEGIN( Builtin )
    REFLECT_PARENT( Expr )
    REFLECT_END()

    REFLECT_BEGIN( Environment )
    REFLECT_PARENT( Expr )
    REFLECT_END()

    REFLECT_BEGIN( MacroDecl )
    REFLECT_PARENT( NamedDecl )
    REFLECT_FIELD2( m_params, Flags::Child )
    // REFLECT_FIELD2( m_body, Flags::Child )
    REFLECT_END()

    REFLECT_BEGIN( MacroExpansion )
    REFLECT_PARENT( Expr )
    REFLECT_FIELD2( m_expansion, Flags::Child )
    REFLECT_FIELD2( m_macro, Flags::Child )
    REFLECT_FIELD2( m_args, Flags::Child )
    REFLECT_END()

    REFLECT_BEGIN( MetaBinding )
    REFLECT_PARENT( Expr )
    REFLECT_FIELD2( m_value, Flags::Abbrev )
    REFLECT_END()

    REFLECT_BEGIN( NamedFunctionCall )
    REFLECT_PARENT( NamedDecl )
    REFLECT_FIELD2( m_resolved, Flags::Hidden )
    REFLECT_FIELD2( m_candidates, Flags::Hidden )
    REFLECT_FIELD2( m_args, Flags::Child )
    REFLECT_END()

    REFLECT_BEGIN( VariableDecl )
    REFLECT_PARENT( NamedDecl )
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
    REFLECT_PARENT( NamedDecl )
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
    REFLECT_PARENT( NamedDecl )
    REFLECT_FIELD2( m_fields, Flags::Child )
    REFLECT_END()

    REFLECT_BEGIN( StructField )
    REFLECT_PARENT( NamedDecl )
    REFLECT_END()

    REFLECT_BEGIN( Definition )
    REFLECT_PARENT( NamedDecl )
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

void Ast::Environment::lookupAll( istring sym, std::vector<Ast::Expr*>& out ) const {
    std::unordered_set<const Environment*> queued;
    for( auto scope = this; scope; scope = scope->parent_ ) {
        auto start = out.size();
        std::vector<const Environment*> todo{ scope };
        queued.insert( scope );
        while( !todo.empty() ) {
            auto env = todo.back();
            todo.pop_back();
            auto localStart = out.size();
            for( auto it = env->syms_.lower_bound( sym ); it != env->syms_.end() && it->first == sym; ++it ) {
                out.push_back( it->second );
            }
            if( out.size() != localStart ) {
                continue; // matches in scope override usings
            }
            for( auto it : env->usings_ ) {
                if( it.second == sym ) {
                    out.push_back( it.first );
                } else if( it.second.empty() && queued.insert( it.first->env() ).second ) {
                    todo.push_back( it.first->env() );
                }
            }
        }
        if( out.size() != start ) {
            return; // found matches in this scope, no need to check parent scopes
        }
    }
}

Result Ast::Environment::lookup( string_view sym, Expr** out ) const {
    *out = nullptr;
    std::vector<Ast::Expr*> all;
    lookupAll( istring::make( sym ), all );
    if( all.size() != 1 ) {
        // This api expects only 1 match
        // If overloads are OK, the caller should use lookup_iter
        return Result::ERR;
    }
    *out = all[0];
    return Result::OK;
}

Result Ast::Environment::bind( istring sym, Expr* value, bool allowOverload ) {
    auto it = syms_.lower_bound( sym );
    if( it == syms_.end() || it->first != sym ) {  // new element
        syms_.emplace( sym, value );
        return Result::OK;
    }
    // Duplicate. Only function overloads are allowed.
    auto fdVal = dynamic_cast<Ast::FunctionDecl*>( value );
    auto fdCur = dynamic_cast<Ast::FunctionDecl*>( it->second );
    auto& loc = it->second->m_loc;
    RETURN_ERROR_IF( !allowOverload || fdVal == nullptr || fdCur == nullptr, Error::CannotOverload, value->m_loc,
                     "%s'%s' is already defined\n%s:%i:%i: Previously defined here",
                     allowOverload ? "Only functions can be overloaded. " : "", sym.c_str(), loc.filename(), loc.line(), loc.col() );
    syms_.emplace_hint( it, sym, value );
    return Result::OK;
}

Result Ast::Environment::importAll( Ast::Environment* other ) {
    for( auto& it : other->syms() ) {
        RETURN_IF_FAILED( bind( it.first, it.second ) );
    }
    return Result::OK;
}

Result Ast::Module::instantiate( istring name, const Func<Result( Ast::Type** )>& create, Ast::Type** out ) {
    *out = nullptr;
    if( auto it = instantiations_.find( name ); it != instantiations_.end() ) {
        *out = it->second;
        return Result::OK;
    }

    Ast::Type* t;
    RETURN_IF_FAILED( create( &t ) );
    assert( t && t->name() == name );
    instantiations_.emplace( name, t );
    for( auto m : t->m_methods ) {
        environment_->bind( m->m_name, m );
    }
    *out = t;
    return Result::OK;
}

Result Ast::FunctionDecl::NotImplemented( const SourceLocation& loc, array_view<Ast::Expr*> args, Ast::Expr** out ) {
    *out = nullptr;
    RETURN_ERROR( Error::CompileTimeUnsupported, loc, "This intrinsic has no compile-time implementation" );
}

Ast::Expr* Ast::Reference::resolve() { return m_target; }

Ast::Type Ast::s_typeType( "Type"_istr, fixMeDeclContext );
Ast::Type Ast::s_typeInt( "int"_istr, fixMeDeclContext );
Ast::Type Ast::s_typeBool( "bool"_istr, fixMeDeclContext );
Ast::Type Ast::s_typeError( "builtin_Error"_istr, fixMeDeclContext );
Ast::Type Ast::s_typeDouble( "double"_istr, fixMeDeclContext );
Ast::Type Ast::s_typeFloat( "float"_istr, fixMeDeclContext );
Ast::Type Ast::s_typeVoid( "void"_istr, fixMeDeclContext );
Ast::Type Ast::s_typeString( "builtin_string"_istr, fixMeDeclContext );

Ast::Type::Type( istring sym, Expr* declContext ) : NamedDecl( sym, declContext ) {}

static void print( Reflect::Var top, Io::TextOutput& out, bool abbrev ) {
    if( top.type->kind != Reflect::Kind::Record ) {
        if( auto f = top.type->toString ) {
            std::string s;
            ( *f )( top.addr, s );
            out.write( s );
            return;
        }
    }
    switch( top.type->kind ) {
        case Reflect::Kind::Array: {
            auto et = top.type->sub;
            void* data;
            size_t count = top.type->arrayItems( top.addr, &data );
            char* s = static_cast<char*>( data );
            for( size_t i = 0; i < count; ++i ) {
                Reflect::Var e{ s + i * et->size, et };
                print( e, out, false );
            }
            break;
        }
        case Reflect::Kind::Record: {
            // A record's toString replaces only the fields declared at that level of the parent chain.
            std::vector<const Reflect::Type*> chain;
            for( auto c = top.type; c; c = c->parent ) {
                chain.push_back( c );
            }
            if( abbrev ) {
                for( auto c : reversed( chain ) ) {
                    if( c->toString ) {
                        std::string s;
                        c->toString( top.addr, s );
                        out.sep();
                        out.write( s );
                        continue;
                    }
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
                    if( c->toString ) {
                        std::string s = " ";
                        c->toString( top.addr, s );
                        out.write( s );
                        continue;
                    }
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
                    if( c->toString ) {
                        continue;
                    }
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
                Reflect::Var e{ obj, sub->dynamicType( obj ) };
                print( e, out, abbrev );
            } else {
                out.write( "null" );
            }
            break;
        }
        case Reflect::Kind::String: {
            assert( top.type->toString );
            std::string s = "\"";
            top.type->toString( top.addr, s );
            s += '"';
            out.write( s );
            break;
        }
        default: {
            assert( false );
        }
    }
}

void Ast::print( Expr* node ) {
    Io::TextOutput out{ Io::TextOutput::Stdout };
    Reflect::Var top{ node };
    ::print( top, out, false );
    out.nl();
}

void Ast::print( Expr* node, Io::TextOutput& out ) {
    if( !node )
        return;
    Reflect::Var top{ node };
    ::print( top, out, false );
    out.nl();
}
