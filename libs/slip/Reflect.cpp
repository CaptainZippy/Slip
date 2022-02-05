#include "slip/pch/Pch.h"

#include "Io.h"
#include "Reflect.h"
#include "Slip.h"

namespace Slip::Reflect {
    namespace Detail {

        void printVar( Io::TextOutput& out, Var top ) {
            if( top.addr == nullptr ) {
                out.write( "null" );
                return;
            } else if( auto f = top.type->toString ) {
                out.write( ( *f )( top.addr ) );
                return;
            }
            switch( top.type->kind ) {
                case Kind::Pointer: {
                    auto obj = *(void**)top.addr;
                    if( obj ) {
                        auto sub = top.type->sub;
                        Var e{obj, sub->dynamicType( obj )};
                        printVar( out, e );
                    } else {
                        out.write( "null" );
                    }
                    break;
                }
                case Kind::Record: {
                    out.begin( top.type->name );
                    out.write( "{" );
                    std::vector<const Reflect::Type*> chain;
                    for( auto c = top.type; c; c = c->parent ) {
                        chain.push_back( c );
                    }
                    for( auto c : reversed( chain ) ) {
                        if( c->toString ) {
                            out.write( c->toString( top.addr ) );
                        } else {
                            for( auto f : c->fields ) {
                                // if (f.name == "m_type") continue;
                                out.nl();
                                Var v = top[f];
                                printVar( out, v );
                            }
                        }
                    }
                    out.end( "}" );
                    break;
                }
                case Kind::Array: {
                    auto et = top.type->sub;
                    char* s = ( (char**)top.addr )[0];
                    char* e = ( (char**)top.addr )[1];
                    unsigned count = unsigned( ( e - s ) / et->size );
                    out.begin( "[" );
                    for( unsigned i = 0; i < count; ++i ) {
                        if( i != 0 )
                            out.nl();
                        Var e{s + i * et->size, et};
                        printVar( out, e );
                    }
                    out.end( "]" );
                    break;
                }
                case Kind::String: {
                    string_view s = top.type->toString( top.addr );
                    out.write( string_concat( "\"", s, "\"" ) );
                    break;
                }
                default:
                    out.write( "???" );
                    break;
            }
        }
    }  // namespace Detail
}  // namespace Slip::Reflect

void Slip::Reflect::printVar( Var top ) {
    Io::TextOutput output{Io::TextOutput::Stdout};
    Detail::printVar( output, top );
    output.nl();
}
