
#include "Pch.h"
#include "SourceManager.h"
#include "Syntax.h"

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
                    Error.at( in.location(start, in.tell()) ).fmt("Missing ')' for list begun here");
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
                            Error.at( in.location() ).fmt("End of input while parsing quoted string" );
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
        Error.fmt( "Unable to open '%s'", fname );
        return nullptr;
    }
}

int main( int argc, const char* argv[] ) {
    if( argc < 2 ) {
        Error.fmt( "Need a script to run" );
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
