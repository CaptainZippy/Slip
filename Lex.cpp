#include "pch/Pch.h"
#include "Lex.h"

namespace Lex {
    REFLECT_BEGIN(Lex::Value)
        REFLECT_PARENT(Atom)
        //REFLECT_FIELD(m_text)
        REFLECT_TO_STRING([](const void*arg) {
            return static_cast<const Lex::Value*>(arg)->text(); })
    REFLECT_END()

    REFLECT_BEGIN(Lex::Atom)
        REFLECT_FIELD(m_attrs)
    REFLECT_END()
}

const char* Lex::Atom::indent( int n ) {
    static char buf[128];
    for( int i = 0; i < n; ++i )
        buf[i] = ' ';
    buf[n] = 0;
    return buf;
}


Lex::Atom* Lex::parse_one( Input& in ) {
    while( 1 ) {
        switch( in.peek() ) {
            case '\0':
                return nullptr;
            case ' ':
            case '\r':
            case '\n':
            case '\t':
                in.eatwhite();
                break;
            case ';':
                while( int c = in.next() ) {
                    if( c == '\n' ) { break; }
                }
                break;
            case '(': {
                std::vector<Atom*> c;
                auto start = in.tell();
                in.next();
                while( Atom* a = parse_string( in ) ) {
                    c.push_back( a );
                }
                if( in.next() != ')' ) {
                    Error.at( in.location( start, in.tell() ) ).fmt( "Missing ')' for list begun here" );
                    throw 0;
                }
                auto l = new List( in.location( start, in.tell() ) );
                l->m_items.swap( c );
                return l;
            }
            case ')':
                return nullptr;
            case '0': case '1': case '2': case '3': case '4':
            case '5': case '6': case '7': case '8': case '9': {
                auto start = in.tell();
                bool isFloat = false;
                while( int c = in.peek() ) {
                    //if(isdigit(c) || isalpha(c) || c == '_'){
                    if( isdigit( c ) ) {
                        in.next();
                    }
                    else if( c == '.' && isFloat == false ) {
                        isFloat = true;
                        in.next();
                    }
                    else break;
                }
                return new Number( in.location( start, in.tell() ) );
            }
            case '"': {
                in.next();
                auto start = in.tell();
                while( 1 ) {
                    switch( in.next() ) {
                        case -1:
                        case 0:
                            Error.at( in.location() ).fmt( "End of input while parsing quoted string" );
                            return nullptr;
                        case '"':
                        {
                            return new String( in.location( start, in.tell() - 1 ) );
                        }
                        default:
                            break;
                    }
                }
                break;
            }
            default: {
                if( isalpha( in.peek() ) || in.peek() == '_' || in.peek() == '@' ) {
                    auto start = in.tell();
                    in.next();
                    while( int c = in.peek() ) {
                        if( isdigit( c ) || isalpha( c ) || c == '@' || c == '_' || c == '?' || c == '!' ) {
                            in.next();
                        }
                        else break;
                    }
                    return new Symbol( in.location( start, in.tell() ) );
                }
                else throw 0;
            }
        }
    }
}

Lex::Atom* Lex::parse_string( Input& in ) {
    if( Atom* a = parse_one( in ) ) {
        in.eatwhite();
        if( in.peek() == ':' ) {
            in.next();
            a->m_decltype = parse_one( in );
        }
        return a;
    }
    return nullptr;
}

Result Lex::parse_file( SourceManager& sm, const char* fname, Lex::List** out ) {
    Input input;
    RETURN_IF_FAILED(sm.load(fname, &input), "Failed to open '{}'", fname);
    List* l = new List( input.location( input.tell(), input.tellEnd() ) );
    while( Atom* a = parse_string( input ) ) {
        l->append( a );
    }
    *out = l;
    return Result::OK;
}
