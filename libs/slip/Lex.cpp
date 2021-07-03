#include "slip/pch/Pch.h"

#include "Errors.h"
#include "Ast.h"

namespace Slip::Ast {
    using namespace Io;

    REFLECT_BEGIN( LexNode )
    REFLECT_FIELD( m_attrs )
    REFLECT_END()

    REFLECT_BEGIN( LexValue )
    REFLECT_PARENT( LexNode )
    // REFLECT_FIELD(m_text)
    REFLECT_TO_STRING( []( const void* arg ) { return static_cast<const LexValue*>( arg )->text(); } )
    REFLECT_END()

    REFLECT_BEGIN( LexDot )
    REFLECT_PARENT( LexNode )
    REFLECT_FIELD2( m_lhs, Flags::Child )
    REFLECT_FIELD2( m_rhs, Flags::Child )
    REFLECT_END()

    REFLECT_BEGIN( LexString )
    REFLECT_PARENT( LexValue )
    REFLECT_END()

    REFLECT_BEGIN( LexNowExpr )
    REFLECT_PARENT( LexValue )
    REFLECT_END()

    REFLECT_BEGIN( LexNumber )
    REFLECT_PARENT( LexValue )
    REFLECT_END()

    REFLECT_BEGIN( LexIdent )
    REFLECT_PARENT( LexValue )
    REFLECT_END()

    REFLECT_BEGIN( LexList )
    REFLECT_PARENT( LexNode )
    REFLECT_TO_STRING( []( const void* arg ) { return static_cast<const LexList*>( arg )->m_loc.text(); } )
    REFLECT_FIELD2( m_items, Flags::Child )
    REFLECT_END()

    /// Parse one atom including an optional type
    Result lex_atom( TextInput& in, LexNode** atom );
    /// Parse one atom, no
    Result lex_term( TextInput& in, LexNode** atom );
}  // namespace Slip::Ast

using namespace Slip;

Slip::Result Ast::lex_term( Io::TextInput& in, LexNode** atom ) {
    *atom = nullptr;
    while( in.available() ) {
        switch( in.peek() ) {
            case '\0':
                RETURN_ERROR( Error::LexInvalidCharacter, in.location(), "null in input" );
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
                std::vector<LexNode*> c;
                auto start = in.tell();
                in.next();
                while( 1 ) {
                    LexNode* a;
                    RETURN_IF_FAILED( lex_atom( in, &a ) );
                    if( a ) {
                        c.push_back( a );
                    } else {
                        break;
                    }
                }
                if( in.next() != ')' ) {
                    RETURN_ERROR( Error::MissingClosingParen, in.location( start ), "Missing ')' for list begun here" );
                }

                auto l = new LexList( in.location( start, in.tell() ) );
                l->m_items.swap( c );
                *atom = l;
                return Result::OK;
            }
            case ')':
                return Result::OK;
            case '-':
            case '+':
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
                *atom = new LexNumber( in.location( start, in.tell() ) );
                return Result::OK;
            }
            case '"': {  // string
                auto start = in.tell();
                in.next();
                while( 1 ) {
                    switch( in.next() ) {
                        case -1:
                            RETURN_ERROR( Error::LexPrematureEndOfFile, in.location( start ), "End of file reached while parsing string" );
                        case 0:
                            RETURN_ERROR( Error::LexInvalidCharacter, in.location( start ), "Null in string" );
                        case '"': {
                            *atom = new LexString( in.location( start + 1, in.tell() - 1 ) );
                            return Result::OK;
                        }
                        default:
                            break;
                    }
                }
                break;
            }
            case '#': {
                auto start = in.tell();
                in.next();
                LexNode* expr;
                RETURN_IF_FAILED( lex_atom( in, &expr ) );
                *atom = new LexNowExpr( in.location( start, in.tell() ), expr );
                return Result::OK;
            }
            case '@': {
                std::vector<Ast::LexNode*> attrs;
                while( in.peek() == '@' ) {
                    in.next();
                    LexNode* attr;
                    RETURN_IF_FAILED( lex_atom( in, &attr ) );
                    attrs.push_back( attr );
                    in.eatwhite();
                }
                LexNode* expr;
                RETURN_IF_FAILED( lex_atom( in, &expr ) );
                if( expr == nullptr ) {
                    RETURN_ERROR( Error::LexMalformedAttribute, in.location(), "Attribute missing expression" );
                }
                expr->m_attrs.swap( attrs );
                *atom = expr;
                return Result::OK;
            }
            case '.': {
                return Result::OK;
            }
            default: {  // symbol
                int c = in.peek();
                if( isalpha( c ) || c == '_' ) {
                    auto start = in.tell();
                    in.next();
                    while( in.available() ) {
                        int c = in.peek();
                        if( isdigit( c ) || isalpha( c ) || c == '_' || c == '?' || c == '!' ) {
                            in.next();
                        } else {
                            break;
                        }
                    }
                    *atom = new LexIdent( in.location( start, in.tell() ) );
                    return Result::OK;
                }
                RETURN_ERROR( Error::LexInvalidCharacter, in.location(), "unexpected character '%c'", in.peek() );
            }
        }
    }
    return Result::OK;
}

Slip::Result Ast::lex_atom( Io::TextInput& in, LexNode** atom ) {
    *atom = nullptr;
    LexNode* ret;
    RETURN_IF_FAILED( lex_term( in, &ret ) );
    while( ret ) {
        in.eatwhite();
        if( in.available() ) {
            if( in.peek() == ':' ) {
                in.next();
                RETURN_IF_FAILED( lex_term( in, &ret->m_decltype ) );
            } else if( in.peek() == '.' ) {
                in.next();
                LexNode* b;
                RETURN_IF_FAILED( lex_term( in, &b ) );
                ret = new Ast::LexDot( ret, b );
            } else {
                break;
            }
        } else {
            break;
        }
    }
    *atom = ret;
    return Result::OK;
}

Slip::Result Ast::lex_input( Io::TextInput& input, Slip::unique_ptr_del<LexList>& lex ) {
    auto l = make_unique_del<LexList>( input.location( input.tell(), input.tellEnd() ) );
    while( 1 ) {
        LexNode* a;
        RETURN_IF_FAILED( lex_atom( input, &a ) );
        if( a ) {
            l->append( a );
        } else {
            break;
        }
    }
    input.eatwhite();
    if( input.available() ) {
        RETURN_ERROR( Error::LexSpuriousChars, input.location( input.tell() ), "Extra characters found after file body" );
    }

    lex = std::move( l );
    return Result::OK;
}

Slip::Result Slip::Ast::lex_file( Slip::Io::SourceManager& sm, const char* fname, Slip::unique_ptr_del<LexList>& lex ) {
    Io::TextInput text;
    RETURN_IF_FAILED( sm.load( fname, text ) );
    return lex_input( text, lex );
}
