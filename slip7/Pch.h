#define _CRT_SECURE_NO_WARNINGS
#define _ITERATOR_DEBUG_LEVEL 0
#pragma warning(disable:4996)
#include <vector>
#include <cctype>
#include <string>
#include <map>
#include <cstdarg>
#include <cstdint>
#include <windows.h>
#include <unordered_map>
#include <unordered_set>
#include <memory>

#define Error Slip::Detail::_Error{__FILE__, __LINE__}
typedef unsigned long long uptr;

namespace Slip {
    namespace Detail {
        struct _Error {
            const char* file;
            int line;
            int col;
            char buf[512];
            _Error& fmt( const char* fmt, ... ) {
                va_list va;
                va_start( va, fmt );
                int cnt = snprintf( buf, sizeof( buf ), "%s(%i,%i):error:", file, line, col );
                vsnprintf( &buf[cnt], sizeof( buf ) - cnt, fmt, va );
                va_end( va );
                return *this;
            }
            _Error& at( const char* f, int l, int c = 0 ) {
                file = f;
                line = l;
                col = c;
                return *this;
            }
            template<typename LOC>
            _Error& at( const LOC& loc ) {
                return at( loc.filename(), loc.line(), loc.col() );
            }
            ~_Error() {
                print( buf );
            }
        protected:
            void print( const char* s ) {
                printf( "%s", s );
                OutputDebugStringA( s );
            }
        };
    }
}


inline void error( const char* msg ) {
    __debugbreak();
}
template<typename T>
void assert2( T t, const char* msg ) {
    if( !t ) Error.fmt( msg );
}
#define assert(A) assert2(A, #A)
#define cast(T,a) dynamic_cast<T*>(a)
#define verify(A) assert2(A, #A)

struct Result {
    enum Code { OK = 0, ERR = 1 };
    Result( Code c ) : code( c ) {}
    bool isOk() const { return code == OK; }
    Code code;
};
#define R_OK Result::OK
template<typename T>
struct view_ptr {
    view_ptr() : m_ptr( nullptr ) {}
    view_ptr( T* p ) : m_ptr( p ) {}
    operator T*( ) const { return m_ptr; }
    void operator=( T* t ) { m_ptr = t; }
    T* operator->() const { return m_ptr; }
    T* m_ptr;
};

template<typename T>
struct array_view {
    typedef T* iterator;
    array_view() : m_begin( nullptr ), m_end( nullptr ) {}
    template<int N>
    array_view( const T (&t)[N] ) : m_begin( t ), m_end( m_begin+N ) {}
    array_view( const T* s, const T* e ) : m_begin( s ), m_end( e ) {}
    array_view( const T* s, uptr n ) : m_begin( s ), m_end( s+n ) {}
    array_view( const std::vector<T>& a ) : m_begin( a.data() ), m_end( m_begin + a.size() ) {}
    void operator=( const array_view<T>& a ) {
        m_begin = a.m_begin;
        m_end = a.m_end;
    }

    size_t size() const { return m_end - m_begin; }
    const T& operator[]( unsigned i ) const { assert( i < size() ); return m_begin[i]; }
    const T* begin() const { return m_begin; }
    const T* end() const { return m_end; }
    array_view<T> ltrim( unsigned n ) const { assert( n <= size() ); return array_view<T>( m_begin + n, m_end ); }
    const T* m_begin;
    const T* m_end;
};

namespace array_view_t {
    template<typename T>
    array_view<T> from_single( const T& t ) { return array_view<T>( &t, &t + 1 ); }
    template<typename T, int N>
    array_view<T> make( T( &arr )[N] ) { return array_view<T>( t, t + N ); }
}

template<typename S>
struct safe_cast_t {
    safe_cast_t( const S& s ) : m_s( s ) {}
    template<typename D>
    operator D() { assert( S( D( m_s ) ) == m_s ); return D( m_s ); }
    const S& m_s;
};

template<typename T>
safe_cast_t<T> safe_cast( const T& t ) {
    return safe_cast_t<T>( t );
};

template<typename T>
struct Optional {
    Optional() : m_ptr( nullptr ) {}
    explicit Optional( const T& t ) {
        m_ptr = new( m_buf ) T( t );
    }
    ~Optional() {
        reset();
    }
    void reset() {
        if( m_ptr ) m_ptr->~T();
    }
    explicit operator bool() const {
        return m_ptr != nullptr;
    }
    T operator*() {
        assert( m_ptr );
        return *m_ptr;
    }
    T& operator->() {
        assert( m_ptr );
        return *m_ptr;
    }
    void set( const T& t ) {
        reset();
        m_ptr = new( m_buf ) T( t );
    }
    T get() {
        return *m_ptr;
    }
private:
    Optional( const Optional& o );
    void operator=( const Optional& o );
    void* m_buf[( sizeof( T ) + sizeof( void* ) - 1 ) / sizeof( void* )];
    T* m_ptr;
};

template<typename T>
struct Iter {
    Iter() = default;

    template<typename T>
    Iter( std::vector<T>& v ) {
        T* t = v.size() ? &v[0] : nullptr;
        m_begin = t;
        m_end = t + v.size();
    }

    T cur() const {
        assert( size() );
        return *m_begin;
    }

    bool advance() {
        assert( m_begin < m_end );
        m_begin += 1;
        return m_begin < m_end;
    }

    bool used() const {
        return m_begin == m_end;
    }

    uptr size() const {
        return m_end - m_begin;
    }

    bool match() {
        return used();
    }
    template<typename F, typename...R>
    bool match(F f, R...r) {
        f = cur();
        if( advance() ) {
            return match( r... );
        }
        return false;
    }

    T* m_begin{ nullptr };
    T* m_end{ nullptr };
};
