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


Result Lex::parse_one( Input& in, Lex::Atom** out) {
    *out = nullptr;
    while( in.available() ) {
        switch( in.peek() ) {
            case '\0':
                RETURN_RES_IF_REACHED(Result::ERR, "null in input");
            case ' ':
            case '\r':
            case '\n':
            case '\t':
                in.eatwhite();
                break;
            case ';':
                while( int c = in.next() ) {
                    if( c == '\n' || c == -1 ) { break; }
                }
                break;
            case '(': {
                std::vector<Atom*> c;
                auto start = in.tell();
                in.next();
                while( 1 ) {
                    Atom* a;
                    RETURN_IF_FAILED(parse_input(in, &a));
                    if (a) {
                        c.push_back(a);
                    }
                    else {
                        break;
                    }
                }
                RETURN_RES_IF(Result::ERR, in.next() != ')', "Missing ')' for list begun here");
                //Error.at( in.location( start, in.tell() ) ).fmt(  );

                auto l = new List( in.location( start, in.tell() ) );
                l->m_items.swap( c );
                *out = l;
                return Result::OK;
            }
            case ')':
                return Result::OK;
            case '0': case '1': case '2': case '3': case '4':
            case '5': case '6': case '7': case '8': case '9': { // number
                auto start = in.tell();
                in.next();
                bool isFloat = false;
                while( in.available() ) {
                    int c = in.peek();
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
                *out = new Number( in.location( start, in.tell() ) );
                return Result::OK;
            }
            case '"': { // string
                in.next();
                auto start = in.tell();
                while( 1 ) {
                    switch(in.next()) {
                        case -1: RETURN_RES_IF_REACHED(Result::ERR, "EOF in string");
                        case 0: RETURN_RES_IF_REACHED(Result::ERR, "Null in input");
                        case '"':
                        {
                            *out = new String( in.location( start, in.tell() - 1 ) );
                            return Result::OK;
                        }
                        default:
                            break;
                    }
                }
                break;
            }
            default: { // symbol
                if( isalpha( in.peek() ) || in.peek() == '_' || in.peek() == '@' ) {
                    auto start = in.tell();
                    in.next();
                    while( in.available() ) {
                        int c = in.peek();
                        if( isdigit( c ) || isalpha( c ) || c == '@' || c == '_' || c == '?' || c == '!' ) {
                            in.next();
                        }
                        else {
                            break;
                        }
                    }
                    *out = new Symbol( in.location( start, in.tell() ) );
                    return Result::OK;
                }
                RETURN_RES_IF_REACHED(Result::ERR, "unexpected");
            }
        }
    }
    return Result::OK;
}

Result Lex::parse_input( Input& in, Lex::Atom** out) {
    Atom* a;
    RETURN_IF_FAILED(parse_one(in, &a));
    if( a ) {
        in.eatwhite();
        if( in.available() && in.peek() == ':' ) {
            in.next();
            RETURN_IF_FAILED(parse_one(in, &a->m_decltype));
        }
    }
    *out = a;
    return Result::OK;
}

Result Lex::parse_file( SourceManager& sm, const char* fname, Lex::List** out ) {
    Input input;
    RETURN_IF_FAILED(sm.load(fname, &input), "Failed to open '{}'", fname);
    List* l = new List( input.location( input.tell(), input.tellEnd() ) );
    while(1) {
        Atom* a;
        RETURN_IF_FAILED(parse_input(input, &a));
        if (a) {
            l->append( a );
        }
        else {
            break;
        }
    }
    *out = l;
    return Result::OK;
}
