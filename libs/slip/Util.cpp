#include "slip/pch/Pch.h"

#include "Io.h"

namespace Slip {

    int default_diagnostic_fn( const char* fmt, va_list args ) { return vfprintf( stderr, fmt, args ); }

    int ( *diagnostic_fn )( const char* fmt, va_list args ) = &default_diagnostic_fn;

    void set_diagnostic_fn( int ( *fn )( const char* fmt, va_list args ) ) { diagnostic_fn = fn; }

    int diagnostic( const char* fmt, ... ) {
        va_list ap;
        va_start( ap, fmt );
        int r = diagnosticv( fmt, ap );
        va_end( ap );
        return r;
    }
    int diagnosticv( const char* fmt, va_list args ) { return ( *diagnostic_fn )( fmt, args ); }

    void Result::failed( int code, const Io::SourceLocation& loc, const char* fmt, ... ) {
        if( code >= 0 ) {
            diagnostic( "%s:%i:%i: error: E%04i(%s)", loc.filename(), loc.line(), loc.col(), code, Error::toString( code ) );
        } else {
            diagnostic( "%s:%i:%i: ", loc.filename(), loc.line(), loc.col() );
        }
        if( fmt[0] ) {
            diagnostic( ": " );
            va_list ap;
            va_start( ap, fmt );
            diagnosticv( fmt, ap );
            va_end( ap );
        }
        diagnostic( "\n" );
        return;
    }

    void Result::debugContext( const char* expr, const char* file, int line, const char* fmt, ... ) {
        diagnostic( "%s:%i:1: debug: %s", file, line, expr );
        if( fmt && fmt[0] ) {
            va_list ap;
            va_start( ap, fmt );
            diagnosticv( fmt, ap );
            va_end( ap );
        }
        diagnostic( "\n" );
    }

    const char* Error::toString( int code ) {
        const char* s[] = {
            "Success",
#define ERROR_CASE( A ) #A,
#include "Errors.inc"
#undef ERROR_CASE
        };
        if( code >= 0 && code < int( sizeof( s ) / sizeof( char* ) ) ) {
            return s[code];
        } else {
            return "Unknown error";
        }
    }

    std::string string_format( const char* fmt, ... ) {
        va_list ap;
        va_start( ap, fmt );
        std::string str = string_formatv( fmt, ap );
        va_end( ap );
        return str;
    }

    std::string string_formatv( const char* fmt, va_list arglist ) {
        std::string str;
        str.resize( str.capacity() );

        while( 1 ) {
            va_list al;
            va_copy( al, arglist );
            int n = vsnprintf( &str[0], str.size(), fmt, al ) + 1;  // incl nul

            va_end( al );

            if( n < 0 ) {
                str.resize( str.size() * 2 );
            } else if( unsigned( n ) > str.capacity() ) {
                str.resize( n );
            } else {
                str.resize( n - 1 );
                return str;
            }
        }
    }

    std::string string_concat( array_view<string_view> strs ) {
        size_t size = rng::accumulate( strs, size_t{ 0 }, []( size_t a, string_view b ) { return a + b.size(); } );
        std::string ret;
        ret.reserve( size );
        for( auto s : strs ) {
            ret.append( s.begin(), s.end() );
        }
        return ret;
    }

    const char istring::s_empty[2 * sizeof( void* )] = {};

    istring istring::make( const char* s ) { return make( string_view( s ) ); }

    istring istring::make( const char* s, size_t l ) { return make( string_view( s, l ) ); }

    istring istring::make( string_view s ) {
        struct Item {
            ssize_t size{ -1 };
            char data[1];  // actually size+1 bytes
            static Item* make( string_view v ) {
                auto i = (Item*)malloc( sizeof( Item ) + v.size() + 1 );
                i->size = v.size();
                memcpy( i->data, v.data(), v.size() );
                i->data[v.size()] = 0;
                return i;
            }
        };
        static std::unordered_map<string_view, Item*> s_items;
        auto it = s_items.find( s );
        if( it != s_items.end() ) {
            return istring( it->second->data );
        }
        auto item = Item::make( s );
        string_view key{ item->data, s.size() };
        s_items.emplace( key, item );

        return istring( item->data );
    }

}  // namespace Slip

#if defined( _MSC_VER )
namespace {
    template <unsigned bytes>
    struct FnvParams;
    template <>
    struct FnvParams<4> {
        static constexpr auto offset_basis = 0x811c9dc5;
        static constexpr auto prime = 0x1000193;
    };
    template <>
    struct FnvParams<8> {
        static constexpr auto offset_basis = 0xcbf29ce484222325;
        static constexpr auto prime = 0x100000001b3;
    };
}  // namespace

std::size_t std::hash<std::string_view>::operator()( std::string_view const& s ) const {
    typedef FnvParams<sizeof( size_t )> Fnv;
    size_t hash = Fnv::offset_basis;
    for( auto c : s ) {
        hash ^= static_cast<unsigned char>( c );
        hash *= Fnv::prime;
    }
    return hash;
}
#endif
