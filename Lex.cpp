#include "pch/Pch.h"
#include "Lex.h"

namespace Slip::Lex {
    REFLECT_BEGIN(Lex::Value)
        REFLECT_PARENT(Atom)
        //REFLECT_FIELD(m_text)
        REFLECT_TO_STRING([](const void*arg) {
            return static_cast<const Lex::Value*>(arg)->text(); })
    REFLECT_END()

    REFLECT_BEGIN(Lex::Atom)
        REFLECT_FIELD(m_attrs)
    REFLECT_END()

        /// Parse one atom including an optional type
    Atom* parse_atom(Input& in);
        /// Parse one atom, no
    Atom* parse_term(Input& in);
}

using namespace Slip;

static std::string lex_error(const Lex::SourceLocation& loc, const char* fmt, ...) {
    va_list ap;
    va_start(ap, fmt);
    auto l = string_format("%s:%i:%i:", loc.filename(), loc.line(), loc.col());
    auto m = string_formatv(fmt, ap);
    va_end(ap);
    return string_concat(l, m);
}

const char* Lex::Atom::indent( int n ) {
    static char buf[128];
    for( int i = 0; i < n; ++i )
        buf[i] = ' ';
    buf[n] = 0;
    return buf;
}


Lex::Atom* Lex::parse_term( Io::Input& in ) {
    while( in.available() ) {
        switch( in.peek() ) {
            case '\0':
                THROW( lex_error(in.location(), "null in input") );
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
                vector<Atom*> c;
                auto start = in.tell();
                in.next();
                while( 1 ) {
                    Atom* a = parse_atom(in);
                    if (a) {
                        c.push_back(a);
                    }
                    else {
                        break;
                    }
                }
                if(in.next() != ')') {
                    THROW( lex_error(in.location(start), "Missing ')' for list begun here") );
                }

                auto l = new List( in.location( start, in.tell() ) );
                l->m_items.swap( c );
                return l;
            }
            case ')':
                return nullptr;
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
                return new Number( in.location( start, in.tell() ) );
            }
            case '"': { // string
                auto start = in.tell();
                in.next();
                while( 1 ) {
                    switch(in.next()) {
                        case -1: THROW( lex_error(in.location(start), "End of file reached while parsing string") );
                        case 0: THROW( lex_error(in.location(start), "Null in string") );
                        case '"':
                        {
                            return new String( in.location( start + 1, in.tell() - 1 ) );
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
                    return new Symbol( in.location( start, in.tell() ) );
                }
                THROW( lex_error(in.location(), "unexpected character '%c'", in.peek()) );
            }
        }
    }
    return nullptr;
}

Lex::Atom* Lex::parse_atom(Io::Input& in ) {
    Atom* a = parse_term(in);
    if( a ) {
        in.eatwhite();
        if( in.available() && in.peek() == ':' ) {
            in.next();
            a->m_decltype = parse_term(in);
        }
    }
    return a;
}

Slip::unique_ptr_del<Lex::List> Lex::parse_input( Lex::Input& input ) {
    auto l = make_unique_del<List>( input.location( input.tell(), input.tellEnd() ) );
    while(1) {
        Atom* a = parse_atom(input);
        if (a) {
            l->append( a );
        }
        else {
            break;
        }
    }
    return l;
}

Slip::unique_ptr_del<Lex::List> Slip::Lex::parse_file(Slip::Io::SourceManager& sm, const char* fname) {
    return parse_input(sm.load(fname));
}
