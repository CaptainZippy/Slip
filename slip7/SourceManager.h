#pragma once

struct SourceManager {
    struct FileInfo {
        std::string m_name;
        std::string m_contents;
    };

    struct Location {

        view_ptr<const FileInfo> m_file;
        long m_start;
        long m_end;

        Location() : m_start( 0 ), m_end( 0 ) {
        }
        Location( const FileInfo* f, long s, long e=-1 ) : m_file( f ), m_start( s ), m_end( e ) {
        }

        Location( Location s, Location e ) : m_file(s.m_file), m_start(s.m_start), m_end(e.m_start) {
            assert( s.m_file == e.m_file );
            assert( s.m_end == -1 );
            assert( e.m_end == -1);
        }
        const char* filename() const {
            return m_file ? m_file->m_name.data() : "<unnamed>";
        }
        int line() const {
            if( m_file ) {
                auto& txt = m_file->m_contents;
                return safe_cast( std::count( txt.begin(), txt.begin() + m_start, '\n' ) + 1 );
            }
            return 0;
        }
        int col() const {
            if( m_file ) {
                auto& txt = m_file->m_contents;
                auto nl = txt.rfind( '\n', m_start );
                return safe_cast( m_start - ( ( nl == std::string::npos ) ? 0 : nl ) );
            }
            return 0;
        }
    };

    struct Input {
        Input( const char* s, const char* e, FileInfo* n ) : start( s ), cur( s ), end( e ), info( n ) {}
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
        Location location() const {
            return Location( info, tell() );
        }
        Location location(long s, long e) const {
            return Location( info, s, e );
        }
        const char* cur;
        const char* start;
        const char* end;
        const FileInfo* info;
    };

    Input load( const char* fname ) {
        while( 1 ) {
            auto it = m_files.find( fname );
            if( it != m_files.end() ) {
                auto& txt = it->second->m_contents;
                return Input( &txt[0], &txt[0] + txt.size(), it->second );
            }
            else if( FILE* fin = fopen( fname, "r" ) ) {
                std::string txt;
                while( 1 ) {
                    char buf[4096];
                    size_t n = fread( buf, 1, sizeof( buf ), fin );
                    if( n == -1 ) error( "Read error" );
                    else if( n == 0 ) break;
                    else txt.append( buf, buf + n );
                }
                m_files[fname] = new FileInfo{ fname, txt };
            }
            else {
                return Input( nullptr, nullptr, nullptr );
            }
        }
    }

    std::map< std::string, FileInfo* > m_files;
};

