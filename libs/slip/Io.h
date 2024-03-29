#pragma once
#include "slip/Slip.h"

namespace Slip::Io {
    struct TextOutput {
        struct Impl {
            virtual ~Impl() = 0;
            virtual void put( string_view s ) = 0;
        };
        std::string m_indent;
        char m_impl[32] = {};
        enum class State {
            Normal,
            Start,
            Mid,
        };
        enum StdStream {
            Stdout,
        };

        State m_state{ State::Normal };

        TextOutput();
        TextOutput( StdStream s );
        TextOutput( std::vector<char>* txt );
        ~TextOutput();

        // close current stream and open another
        Result open( StdStream s );
        Result open( const char* fname );
        Result open( std::vector<char>* txt );
        Result close();

        void begin( string_view s ) {
            write( s );
            m_indent.push_back( ' ' );
        }

        /// Write, indenting on newlines
        void write( string_view s );

        void sep() {}
        void end( string_view s = {} ) {
            m_indent.erase( m_indent.size() - 1 );
            write( s );
        }
        void nl() {
            _writeImpl( "\n"_sv );
            m_state = State::Start;
        }

       private:
        /// Raw Write
        void _write( string_view s );
        void _writeImpl( string_view s );
    };

    struct SourceManager {
        virtual ~SourceManager() = default;
        virtual Result load( const char* fname, Io::TextInput& text ) = 0;
    };

    struct SourceNameAndContents {
        std::string m_name;
        std::string m_contents;
    };

    struct SourceLocation {
        const SourceNameAndContents* m_file{ nullptr };
        size_t m_start{ 0 };
        size_t m_end{ 0 };

        SourceLocation() = default;

        SourceLocation( const SourceNameAndContents* f, long s, long e = -1 ) : m_file( f ), m_start( s ), m_end( e >= 0 ? e : s ) {}

        SourceLocation( SourceLocation s, SourceLocation e ) : m_file( s.m_file ), m_start( s.m_start ), m_end( e.m_start ) {
            assert( s.m_file == e.m_file );
        }
        const char* filename() const { return m_file ? m_file->m_name.data() : "<unnamed>"; }
        int line() const;
        int col() const;

        string_view text() const {
            auto s = m_file->m_contents.c_str();
            assert( m_end >= 0 );
            return { s + m_start, m_end - m_start };
        }
    };

    struct TextInput {
        TextInput() = default;
        TextInput( const char* s, const char* c, const char* e, const SourceNameAndContents* n )
            : cur( c ), start( s ), end( e ), info( n ) {}
        TextInput( const char* s, const char* e, const SourceNameAndContents* n ) : cur( s ), start( s ), end( e ), info( n ) {}
        void reset( const char* s, const char* e, const SourceNameAndContents* n ) {
            cur = s;
            start = s;
            end = e;
            info = n;
        }

        explicit operator bool() const { return cur != end; }
        void eatwhite() {
            while( cur != end && isspace( *cur ) ) {
                ++cur;
            }
        }
        bool available( int count = 1 ) const { return ( cur + count ) <= end; }
        bool accept( int c ) {
            if( cur < end ) {
                if( *cur == c ) {
                    cur += 1;
                    return true;
                }
            }
            return ( c == -1 ) && ( cur == end );
        }
        template<typename Pred>
        bool acceptP( Pred&& pred ) {
            if( cur < end ) {
                if( pred( *cur ) ) {
                    cur += 1;
                    return true;
                }
            }
            return ( cur == end ) ? pred( -1 ) : false;
        }
        int peek() const {
            assert( cur < end );
            return *cur;
        }
        int next() {
            if( cur == end )
                return -1;
            return *cur++;
        }
        Result skip( int n ) {
            if( cur + n > end )
                return -1;
            cur += n;
            return Result::OK;
        }
        long tell() const { return safe_cast( cur - start ); }
        // bump the read position by a delta
        void bump( long delta ) {
            assert( cur + delta >= start && cur + delta <= end );
            cur += delta;
        }
        // seek to absolute position
        void seek( long pos ) {
            assert( pos <= end - start );
            cur = start + pos;
        }
        long tellEnd() const { return safe_cast( end - start ); }
        SourceLocation location() const { return SourceLocation( info, tell() ); }
        SourceLocation location( long s, long e = -1 ) const { return SourceLocation( info, s, e >= 0 ? e : s + 1 ); }
        const char* cur{ nullptr };
        const char* start{ nullptr };
        const char* end{ nullptr };
        const SourceNameAndContents* info{ nullptr };
    };
}  // namespace Slip::Io
