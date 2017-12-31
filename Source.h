#pragma once

namespace Lex {

    struct SourceNameAndContents {
        std::string m_name;
        std::string m_contents;
    };

    struct SourceLocation {

        view_ptr<const SourceNameAndContents> m_file;
        ssize_t m_start;
        ssize_t m_end;

        SourceLocation() : m_start( 0 ), m_end( 0 ) {
        }
        SourceLocation( const SourceNameAndContents* f, long s, long e=-1 ) : m_file( f ), m_start( s ), m_end( e ) {
        }

        SourceLocation( SourceLocation s, SourceLocation e ) : m_file(s.m_file), m_start(s.m_start), m_end(e.m_start) {
            assert( s.m_file == e.m_file );
            assert( s.m_end == -1 );
            assert( e.m_end == -1);
        }
        const char* filename() const {
            return m_file ? m_file->m_name.data() : "<unnamed>";
        }
        int line() const;
        int col() const;
    };

    struct Input {
        Input() = default;
        Input( const char* s, const char* e, SourceNameAndContents* n )
            : cur( s ), start( s ), end( e ), info( n ) {}

        explicit operator bool() const {
            return cur != end;
        }
        void eatwhite() {
            while( cur != end && isspace( *cur ) ) {
                ++cur;
            }
        }
        int peek() const {
            return *cur;
        }
        int next() {
            if( cur == end ) return -1;
            return *cur++;
        }
        const char* peekbuf() const {
            return cur;
        }
        long tell() const {
            return safe_cast(cur - start);
        }
        long tellEnd() const {
            return safe_cast( end - start );
        }
        SourceLocation location() const {
            return SourceLocation( info, tell() );
        }
        SourceLocation location(long s, long e) const {
            return SourceLocation( info, s, e );
        }
        const char* cur{nullptr};
        const char* start{nullptr};
        const char* end{nullptr};
        const SourceNameAndContents* info{nullptr};
    };

    struct SourceManager {
        std::map< std::string, SourceNameAndContents* > m_files;

        Result load(const char* fname, Input* out);
    };
}
