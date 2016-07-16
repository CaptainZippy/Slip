
#include "Pch.h"

#define Error( fmt, ... ) Slip::Detail::_Error("%s(%i,%i): error " fmt, __FILE__, __LINE__, 0, __VA_ARGS__)
#define Error_At( loc, fmt, ... ) Slip::Detail::_Error("%s(%i,%i): error " fmt, loc.filename(), loc.line(), loc.col(), __VA_ARGS__)

#include "SourceManager.h"
#include "Syntax.h"

namespace Slip {
    namespace Detail {
        void _Error( const char* fmt, ... ) {
            va_list va;
            va_start( va, fmt );
            char buf[2048];
            vsnprintf( buf, sizeof( buf ), fmt, va );
            va_end( va );
            printf( "%s", buf );
            OutputDebugStringA( buf );
            throw 0;
        }
    }
}



namespace Detail {
    void _Error_At( SourceManager::Location loc, char* fmt, ... ) {
        va_list va;
        va_start( va, fmt );
        vprintf( fmt, va );
        va_end( va );
    }
}


Atom* parse_one( SourceManager::Input& in ) {
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
                while( Atom* a = parse_one( in ) ) {
                    c.push_back( a );
                }
                if( in.next() != ')' ) {
                    Error_At( in.location(start, in.tell()), "Missing ')' for list begun here" );
                    throw 0;
                }
                List* l = new List(in.location(start, in.tell()));
                l->items.swap( c );
                return l;
            }
            case ')':
                return nullptr;
            case '0': case '1': case '2': case '3': case '4':
            case '5': case '6': case '7': case '8': case '9': {
                auto start = in.tell();
                const char* s = in.peekbuf();
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
                return new Number( in.location(start, in.tell() ) );
            }
            case '"': {
                in.next();
                auto start = in.tell();
                const char* s = in.peekbuf();
                while( 1 ) {
                    switch( int c = in.next() ) {
                        case -1:
                        case 0:
                            Error_At( in.location(), "End of input while parsing quoted string" );
                            return nullptr;
                        case '"':
                        {
                            return new String( in.location(start, in.tell()-1) );
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
                    const char* s = in.peekbuf();
                    in.next();
                    while( int c = in.peek() ) {
                        if( isdigit( c ) || isalpha( c ) || c == '@' || c == '_' || c == '?' || c == '!' ) {
                            in.next();
                        }
                        else break;
                    }
                    return new Symbol( in.location(start, in.tell() ) );
                }
                else throw 0;
            }
        }
    }
}

Atom* parse_string( SourceManager::Input& in ) {
    if( Atom* a = parse_one( in ) ) {
        in.eatwhite();
        if( 0 ) if( in.peek() == ':' ) {
            in.next();
            //Box rhs = parse_one( state, in );
            //rhs->m_type = ret;
            //ret = rhs;
        }
        return a;
    }
    return nullptr;
}

List* parse_file( SourceManager& sm, const char* fname ) {
    if( SourceManager::Input input = sm.load( fname ) ) {
        List* l = new List(input.location(input.tell(), input.tellEnd()));
        while( Atom* a = parse_string( input ) ) {
            l->append( a );
        }
        return l;
    }
    else {
        Error( "Unable to open '%s'", fname );
        return nullptr;
    }
}

int main( int argc, const char* argv[] ) {
    if( argc < 2 ) {
        Error( "Need a script to run" );
        return 1;
    }
    try {
        SourceManager smanager;
        if( Atom* syntax = parse_file( smanager, argv[1] ) ) {
            syntax->print();
            return 0;
        }
    }
    catch( float ) {
        return 1;
    }
}
