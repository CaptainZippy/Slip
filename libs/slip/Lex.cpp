#include "slip/pch/Pch.h"

#include "Ast.h"
#include "Errors.h"

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

static Slip::Result read_past_delimiter( Io::TextInput& in, char delim ) {
    int start = in.tell();
    while( 1 ) {
        switch( auto cur = in.next() ) {
            case -1:
                RETURN_ERROR( Error::LexPrematureEndOfFile, in.location( start ), "End of file reached while parsing string" );
            case 0:
                RETURN_ERROR( Error::LexInvalidCharacter, in.location( start ), "Null in string" );
            default:
                if( cur == delim ) {
                    return Result::OK;
                }
                break;
        }
    }
}

namespace {
    struct Token {
        enum Kind { Invalid, EndOfFile, Number, Identifier, String, ListBegin, ListEnd, LexColon, LexDot, LexHash, LexAt };
        Token() : kind( Invalid ) {}
        Token( Kind k, Io::SourceLocation l ) : kind( k ), loc( l ) {}
        Kind kind;
        Io::SourceLocation loc;
    };
}  // namespace

static Slip::Result next_token( Io::TextInput& in, Token& tokenOut ) {
    while( in.available() ) {
        int start = in.tell();
        switch( int c = in.next() ) {
            case '\0':
                RETURN_ERROR( Error::LexInvalidCharacter, in.location(), "null in input" );
            case ' ':
            case '\r':
            case '\n':
            case '\t':
                in.eatwhite();
                break;
            case ';':  // comment to EOL
                while( int n = in.next() ) {
                    if( n == '\n' || n == -1 ) {
                        break;
                    }
                }
                break;
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
                tokenOut = Token( Token::Number, in.location( start, in.tell() ) );
                return Result::OK;
            }
            case '(':
                tokenOut = Token( Token::ListBegin, { in.info, start, in.tell() } );
                return Result::OK;
            case ')':
                tokenOut = Token( Token::ListEnd, { in.info, start, in.tell() } );
                return Result::OK;
            case '\'': {  // delimted string - 'abc[string_contents]abc'
                auto headDelim = in.tell();
                RETURN_IF_FAILED( read_past_delimiter( in, '[' ) );
                auto stringStart = in.tell();
                int countDelim = stringStart - headDelim - 1;
                while( true ) {
                    RETURN_IF_FAILED( read_past_delimiter( in, ']' ) );
                    RETURN_ERROR_IF( in.available( countDelim + 1 /*'*/ ) == false, Error::LexPrematureEndOfFile, in.location() );
                    auto tailDelim = in.tell();
                    if( in.start[tailDelim + countDelim] == '\'' &&
                        memcmp( in.start + headDelim, in.start + tailDelim, countDelim ) == 0 ) {
                        in.skip( countDelim + 1 );
                        tokenOut = Token( Token::String, in.location( stringStart, tailDelim - 1 ) );
                        break;
                    }
                    in.next();
                }
                return Result::OK;
            }
            case '"': {  // 'plain' string
                auto start = in.tell();
                RETURN_IF_FAILED( read_past_delimiter( in, '"' ) );
                tokenOut = Token( Token::String, in.location( start, in.tell() - 1 ) );
                return Result::OK;
            }
            case '.':
                tokenOut = Token( Token::LexDot, { in.info, start, in.tell() } );
                return Result::OK;
            case ':':
                tokenOut = Token( Token::LexColon, { in.info, start, in.tell() } );
                return Result::OK;
            case '@':
                tokenOut = Token( Token::LexAt, { in.info, start, in.tell() } );
                return Result::OK;
            case '#':
                tokenOut = Token( Token::LexHash, { in.info, start, in.tell() } );
                return Result::OK;
            default: {  // symbol?
                if( isalpha( c ) || c == '_' ) {
                    while( in.available() ) {
                        int n = in.next();
                        if( isdigit( n ) || isalpha( n ) || n == '_' || n == '?' || n == '!' ) {
                        } else {
                            in.bump( -1 );
                            break;
                        }
                    }
                    tokenOut = Token( Token::Identifier, { in.info, start, in.tell() } );
                    return Result::OK;
                } else {
                    RETURN_ERROR( Error::LexInvalidCharacter, in.location( start ), "unexpected character '%c'", c );
                }
                break;
            }
        }
    }
    tokenOut = Token( Token::EndOfFile, in.location() );
    return Result::OK;
}

// In any case, fills in tokOut.
// If the next token kind matches, advances the input, returns true.
// Otherwise don't advance the input and return false.
static bool expect_token( Token::Kind kind, Io::TextInput& in, Token& tokOut ) {
    int origPos = in.tell();
    // TODO caching - we often reparse at the same location
    // A single cache entry should be enough.
    if( next_token( in, tokOut ).isOk() && tokOut.kind == kind ) {
        return true;
    } else {
        in.seek( origPos );
        return false;
    }
}

static Slip::Result lex_expression( Io::TextInput& in, Ast::LexNode** atomOut );

Slip::Result Ast::lex_atom( Io::TextInput& in, Ast::LexNode** atomOut ) {
    *atomOut = nullptr;
    Token tok;
    RETURN_IF_FAILED( next_token( in, tok ) );
    switch( tok.kind ) {
        case Token::Number:
            *atomOut = new Ast::LexNumber( tok.loc );
            break;
        case Token::Identifier:
            *atomOut = new Ast::LexIdent( tok.loc );
            return Result::OK;
        case Token::String:
            *atomOut = new Ast::LexString( tok.loc );
            break;
        case Token::ListBegin: {
            auto list = new Ast::LexList( in.location(), '(' );
            int listStart = in.tell();
            while( true ) {
                if( expect_token( Token::ListEnd, in, tok ) ) {
                    break;
                }
                RETURN_ERROR_IF( tok.kind == Token::EndOfFile, Error::LexPrematureEndOfFile, in.location( listStart ),
                                 "End of file while parsing list" );
                Ast::LexNode* n;
                RETURN_IF_FAILED( lex_expression( in, &n ) );
                list->m_items.push_back( n );
            }
            *atomOut = list;
            return Result::OK;
        }
        case Token::EndOfFile:
            break;
        case Token::ListEnd:
        case Token::LexColon:
        case Token::LexHash:
        case Token::LexAt:
            RETURN_ERROR( Error::LexUnexpected, tok.loc, "Unexpected character '%.*s' in input", STRING_VIEW_VARG( tok.loc.text() ) );
        default:
            assert( false );
            break;
    }
    return Result::OK;
}

// expr : attr* now? atom type?
// now  : #
// attr : '@' atom
// atom : number list string ident
// type : ':' attr* atom

static Slip::Result lex_expression( Io::TextInput& in, Ast::LexNode** atomOut ) {
    *atomOut = nullptr;
    std::vector<Ast::LexNode*> attrs;
    Token tok;
    if( expect_token( Token::EndOfFile, in, tok ) ) {
        return Result::OK;
    }

    while( expect_token( Token::LexAt, in, tok ) ) {
        Ast::LexNode* a;
        RETURN_IF_FAILED( lex_atom( in, &a ) );
        attrs.push_back( a );
    }

    int lexNowStart = -1;
    if( expect_token( Token::LexHash, in, tok ) ) {
        lexNowStart = tok.loc.m_start;
    }

    Ast::LexNode* atom;
    RETURN_IF_FAILED( lex_atom( in, &atom ) );

    if( lexNowStart >= 0 ) {
        atom = new Ast::LexNowExpr( in.location(lexNowStart), atom );
    }
    atom->m_attrs.swap( attrs );

    while( expect_token( Token::LexDot, in, tok ) ) {
        Ast::LexNode* a;
        RETURN_IF_FAILED( lex_atom( in, &a ) );
        atom = new Ast::LexDot( atom, a );
        atom->m_loc = a->m_loc;
    }

    if( expect_token( Token::LexColon, in, tok ) ) {
        std::vector<Ast::LexNode*> typeAttrs;
        while( expect_token( Token::LexAt, in, tok ) ) {
            Ast::LexNode* a;
            RETURN_IF_FAILED( lex_atom( in, &a ) );
            typeAttrs.push_back( a );
        }
        Ast::LexNode* a;
        RETURN_IF_FAILED( lex_atom( in, &a ) );
        a->m_attrs.swap( typeAttrs );
        atom->m_declTypeLex = a;
    }

    *atomOut = atom;
    return Result::OK;
}

Slip::Result Ast::lex_input( Io::TextInput& input, Slip::unique_ptr_del<LexList>& lex ) {
    auto l = make_unique_del<LexList>( input.location( input.tell(), input.tellEnd() ), '[' );
    while( 1 ) {
        LexNode* a;
        RETURN_IF_FAILED( lex_expression( input, &a ) );
        if( a ) {
            l->append( a );
        } else {
            break;
        }
    }
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
