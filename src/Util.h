#pragma once

namespace Slip {
    using namespace std;

    typedef unsigned char byte;

    inline void error( const char* msg ) { __debugbreak(); }

#define assert( A ) \
    if( !( A ) )    \
    __debugbreak()
#define cast( T, a ) dynamic_cast<T*>( a )

    struct Result {
        enum OkCode { OK = 0 };
        enum ErrCode { ERR = 1 };
        Result( OkCode c ) : code( c ) {}
        Result( int c ) : code( c ) {}
        bool isOk() const { return code == OK; }
        int code;
        static void failed( const char* what, const char* file, int line, const char* fmt, ... );
    };
#define R_OK Result::OK
#define RETURN_IF_FAILED( COND, ... )                                    \
    do {                                                                 \
        Result res = ( COND );                                           \
        if( !res.isOk() ) {                                              \
            Result::failed( #COND, __FILE__, __LINE__, "" __VA_ARGS__ ); \
            return res;                                                  \
        }                                                                \
    } while( 0 )

#define RETURN_ERR_IF( COND, ... )                                       \
    do {                                                                 \
        if( ( COND ) ) {                                                 \
            Result::failed( #COND, __FILE__, __LINE__, "" __VA_ARGS__ ); \
            return Result::ERR;                                          \
        }                                                                \
    } while( 0 )

#define RETURN_RES_IF_REACHED( RES, ... )                               \
    do {                                                                \
        Result::failed( "Failed", __FILE__, __LINE__, "" __VA_ARGS__ ); \
        return RES;                                                     \
    } while( 0 )

    template <typename T>
    struct array_view {
        typedef T* iterator;
        array_view() : m_begin( nullptr ), m_end( nullptr ) {}
        template <typename U, size_t N>
        array_view( U ( &t )[N] ) : m_begin( t ), m_end( m_begin + N ) {
            static_assert( sizeof( T ) == sizeof( U ), "" );
        }
        template <typename U>
        array_view( U* s, U* e ) : m_begin( s ), m_end( e ) {
            static_assert( sizeof( T ) == sizeof( U ), "" );
        }
        template <typename U>
        array_view( U* s, size_t n ) : m_begin( s ), m_end( s + n ) {
            static_assert( sizeof( T ) == sizeof( U ), "" );
        }
        template <typename U>
        array_view( array_view<U> u ) : m_begin( u.m_begin ), m_end( u.m_end ) {
            static_assert( sizeof( T ) == sizeof( U ), "" );
        }
        array_view( std::vector<T>& a ) : m_begin( a.data() ), m_end( m_begin + a.size() ) {}
        void operator=( array_view<T> a ) {
            m_begin = a.m_begin;
            m_end = a.m_end;
        }

        size_t size() const { return m_end - m_begin; }
        bool empty() const { return m_end == m_begin; }
        T& operator[]( unsigned i ) const {
            assert( i < size() );
            return m_begin[i];
        }
        T* begin() const { return m_begin; }
        T* end() const { return m_end; }
        T& front() const { return *( m_begin ); }
        T& back() const { return *( m_end - 1 ); }
        array_view<T> ltrim( unsigned n ) const {
            assert( n <= size() );
            return array_view<T>( m_begin + n, m_end );
        }
        array_view<T> rtrim( unsigned n ) const {
            assert( n <= size() );
            return array_view<T>( m_begin, m_end - n );
        }
        std::vector<T> std_vec() const { return std::vector<T>( m_begin, m_end ); }
        T* m_begin;
        T* m_end;
    };

    template <typename T>
    array_view<T> make_array_single( T& t ) {
        return array_view<T>( &t, &t + 1 );
    }
    template <typename T, int N>
    array_view<T> make_array_view( T ( &t )[N] ) {
        return array_view<T>( t, t + N );
    }
    template <typename T>
    array_view<const T> make_array_view( const std::vector<T>& a ) {
        return array_view<const T>( a.data(), a.size() );
    }
    template <typename T>
    array_view<T> make_array_view( T* t, size_t n ) {
        return array_view<T>( t, n );
    }

    template <typename S>
    struct safe_cast_t {
        safe_cast_t( const S& s ) : m_s( s ) {}
        template <typename D>
        operator D() {
            assert( S( D( m_s ) ) == m_s );
            return D( m_s );
        }
        const S& m_s;
    };

    template <typename T>
    safe_cast_t<T> safe_cast( const T& t ) {
        return safe_cast_t<T>( t );
    };

    template <typename T>
    struct Optional {
        Optional() : m_ptr( nullptr ) {}
        explicit Optional( const T& t ) { m_ptr = new( m_buf ) T( t ); }
        ~Optional() { reset(); }
        Optional( const Optional& o ) = delete;
        void operator=( const Optional& o ) = delete;
        void reset() {
            if( m_ptr )
                m_ptr->~T();
        }
        explicit operator bool() const { return m_ptr != nullptr; }
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
        T get() { return *m_ptr; }

       private:
        void* m_buf[( sizeof( T ) + sizeof( void* ) - 1 ) / sizeof( void* )];
        T* m_ptr;
    };

    std::string string_format( const char* fmt, ... );
    std::string string_formatv( const char* fmt, va_list va );

    namespace Detail {
        template <typename T>
        struct reverse_wrapper {
            T& iterable;
        };

        template <typename T>
        auto begin( reverse_wrapper<T> w ) {
            return rbegin( w.iterable );
        }

        template <typename T>
        auto end( reverse_wrapper<T> w ) {
            return rend( w.iterable );
        }
    }  // namespace Detail

    template <typename T>
    Detail::reverse_wrapper<T> reversed( T&& iterable ) {
        return {iterable};
    }

    template <typename Iter>
    struct range {
        range() : begin{}, end{} {}
        range( Iter b, Iter e ) : begin( b ), end( e ) {}
        bool empty() const { return begin == end; }
        Iter begin;
        Iter end;
    };

    template <typename T, typename V>
    auto find( T&& iterable, const V& v ) -> range<typename std::decay_t<T>::iterator> {
        return {std::find( iterable.begin(), iterable.end(), v ), iterable.end()};
    }

    template <typename T, typename P>
    auto find_if( T&& iterable, P&& p ) -> range<typename std::decay_t<T>::iterator> {
        return {std::find_if( iterable.begin(), iterable.end(), p ), iterable.end()};
    }

    template <typename Cont, typename Lambda>
    bool all_of( const Cont& c, Lambda&& lambda ) {
        return std::all_of( c.begin(), c.end(), lambda );
    }
    template <typename Cont, typename Lambda>
    bool any_of( const Cont& c, Lambda&& lambda ) {
        return std::any_of( c.begin(), c.end(), lambda );
    }

    template <typename Cont, typename Lambda>
    auto erase_if( Cont& c, Lambda&& lambda ) {
        return c.erase( std::remove_if( c.begin(), c.end(), lambda ), c.end() );
    }

    template <typename Cont, typename Lambda>
    auto for_each( Cont& c, Lambda&& lambda ) {
        return std::for_each( c.begin(), c.end(), lambda );
    }

    // Interned string
    struct istring {
        static istring make( const char* s );
        static istring make( const char* s, size_t l );
        static istring make( std::string_view s );

        inline istring() : m_str( &s_empty[sizeof( size_t )] ) {}

        inline operator std::string_view() const { return {m_str, size()}; }
        inline std::string_view view() const { return {m_str, size()}; }
        inline const char* c_str() const { return m_str; }
        inline std::string std_str() const { return m_str; }
        inline size_t size() const { return reinterpret_cast<const size_t*>( m_str )[-1]; }

       private:
        static const char s_empty[];
        istring( const char* s ) : m_str( s ) {}
        const char* m_str;
    };

    inline bool operator<( istring a, istring b ) { return a.view() < b.view(); }

    inline bool operator==( istring a, istring b ) { return a.c_str() == b.c_str(); }
    inline bool operator!=( istring a, istring b ) { return a.c_str() != b.c_str(); }

    std::string string_concat( array_view<std::string_view> strs );

    template <typename... ARGS>
    std::string string_concat( const ARGS&... args ) {
        string_view strs[] = {string_view( args )...};
        return string_concat( make_array_view( strs ) );
    }

    template <typename T>
    using unique_ptr_del = std::unique_ptr<T, void ( * )( T* )>;

    template <class _Ty, class... _Types, enable_if_t<!is_array<_Ty>::value, int> = 0>
    inline unique_ptr_del<_Ty> make_unique_del( _Types&&... _Args ) {
        return unique_ptr_del<_Ty>( new _Ty( std::forward<_Types>( _Args )... ), []( _Ty* t ) { delete t; } );
    }

    template <typename DST, typename SRC>
    Result dynCast( SRC* src, DST** dst ) {
        if( auto p = dynamic_cast<DST*>( src ) ) {
            *dst = p;
            return Result::OK;
        }
        return Result::ERR;
    }
}  // namespace Slip

namespace std {

#if defined( _MSC_VER )
    template <>
    struct hash<string_view> {
        typedef string_view argument_type;
        typedef std::size_t result_type;
        std::size_t operator()( string_view const& s ) const;
    };
#endif
    template <>
    struct hash<Slip::istring> {
        typedef Slip::istring argument_type;
        typedef std::size_t result_type;
        std::size_t operator()( Slip::istring const& s ) const { return reinterpret_cast<size_t>( s.c_str() ); }
    };
}  // namespace std
