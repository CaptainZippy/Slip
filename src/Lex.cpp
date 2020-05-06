#include "pch/Pch.h"

#include "Lex.h"

namespace Slip::Lex {
    REFLECT_BEGIN( Lex::Value )
    REFLECT_PARENT( Atom )
    // REFLECT_FIELD(m_text)
    REFLECT_TO_STRING( []( const void* arg ) { return static_cast<const Lex::Value*>( arg )->text(); } )
    REFLECT_END()

    REFLECT_BEGIN( Lex::Atom )
    REFLECT_FIELD( m_attrs )
    REFLECT_END()

    /// Parse one atom including an optional type
    Result parse_atom( TextInput& in, Atom** atom );
    /// Parse one atom, no
    Result parse_term( TextInput& in, Atom** atom );
}  // namespace Slip::Lex

using namespace Slip;

static std::string lex_error( const Lex::SourceLocation& loc, const char* fmt, ... ) {
    va_list ap;
    va_start( ap, fmt );
    auto l = string_format( "%s:%i:%i:", loc.filename(), loc.line(), loc.col() );
    auto m = string_formatv( fmt, ap );
    va_end( ap );
    return string_concat( l, m );
}

Slip::Result Lex::parse_term( Io::TextInput& in, Lex::Atom** atom ) {
#define LEX_ERROR( LOC, ... ) RETURN_RES_IF_REACHED( Result::ERR, "%s", lex_error( LOC, __VA_ARGS__ ).c_str() );

    *atom = nullptr;
    while( in.available() ) {
        switch( in.peek() ) {
            case '\0':
                LEX_ERROR( in.location(), "null in input" );
            case ' ':
            case '\r':
            case '\n':
            case '\t':
                in.eatwhite();
                break;
            case ';':
                while( int c = in.next() ) {
                    if( c == '\n' || c == -1 ) {
                        break;
                    }
                }
                break;
            case '(': {
                vector<Atom*> c;
                auto start = in.tell();
                in.next();
                while( 1 ) {
                    Atom* a;
                    RETURN_IF_FAILED( parse_atom( in, &a ) );
                    if( a ) {
                        c.push_back( a );
                    } else {
                        break;
                    }
                }
                if( in.next() != ')' ) {
                    LEX_ERROR( in.location( start ), "Missing ')' for list begun here" );
                }

                auto l = new List( in.location( start, in.tell() ) );
                l->m_items.swap( c );
                *atom = l;
                return Result::OK;
            }
            case ')':
                return Result::OK;
            case '0':
            case '1':
            case '2':
            case '3':
            case '4':
            case '5':
            case '6':
            case '7':
            case '8':
            case '9': {  // number
                auto start = in.tell();
                in.next();
                bool isFloat = false;
                while( in.available() ) {
                    int c = in.peek();
                    // if(isdigit(c) || isalpha(c) || c == '_'){
                    if( isdigit( c ) ) {
                        in.next();
                    } else if( c == '.' && isFloat == false ) {
                        isFloat = true;
                        in.next();
                    } else
                        break;
                }
                *atom = new Number( in.location( start, in.tell() ) );
                return Result::OK;
            }
            case '"': {  // string
                auto start = in.tell();
                in.next();
                while( 1 ) {
                    switch( in.next() ) {
                        case -1:
                            LEX_ERROR( in.location( start ), "End of file reached while parsing string" );
                        case 0:
                            LEX_ERROR( in.location( start ), "Null in string" );
                        case '"': {
                            *atom = new String( in.location( start + 1, in.tell() - 1 ) );
                            return Result::OK;
                        }
                        default:
                            break;
                    }
                }
                break;
            }
            default: {  // symbol
                int c = in.peek();
                if( isalpha( c ) || c == '_' || c == '@' || c == '#' ) {
                    auto start = in.tell();
                    in.next();
                    while( in.available() ) {
                        int c = in.peek();
                        if( isdigit( c ) || isalpha( c ) || c == '@' || c == '_' || c == '?' || c == '!' ) {
                            in.next();
                        } else {
                            break;
                        }
                    }
                    *atom = new Symbol( in.location( start, in.tell() ) );
                    return Result::OK;
                }
                LEX_ERROR( in.location(), "unexpected character '%c'", in.peek() );
            }
        }
    }
    return Result::OK;
}

Slip::Result Lex::parse_atom( Io::TextInput& in, Lex::Atom** atom ) {
    *atom = nullptr;
    Atom* a;
    RETURN_IF_FAILED( parse_term( in, &a ) );
    if( a ) {
        in.eatwhite();
        if( in.available() && in.peek() == ':' ) {
            in.next();
            RETURN_IF_FAILED( parse_term( in, &a->m_decltype ) );
        }
    }
    *atom = a;
    return Result::OK;
}

Slip::Result Lex::parse_input( Lex::TextInput& input, Slip::unique_ptr_del<Lex::List>& lex ) {
    auto l = make_unique_del<List>( input.location( input.tell(), input.tellEnd() ) );
    while( 1 ) {
        Atom* a;
        RETURN_IF_FAILED( parse_atom( input, &a ) );
        if( a ) {
            l->append( a );
        } else {
            break;
        }
    }
    lex = std::move( l );
    return Result::OK;
}

Slip::Result Slip::Lex::parse_file( Slip::Io::SourceManager& sm, const char* fname, Slip::unique_ptr_del<Lex::List>& lex ) {
    Io::TextInput text;
    RETURN_IF_FAILED( sm.load( fname, text ) );
    return parse_input( text, lex );
}
