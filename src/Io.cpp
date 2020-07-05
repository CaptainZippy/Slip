#include "pch/Pch.h"

#include "Io.h"
#include "Util.h"

namespace Slip::Io {
    struct SourceManagerImpl : SourceManager {
        map<string, SourceNameAndContents*> m_files;

        Result load( const char* fname, Io::TextInput& text ) override;
    };
}  // namespace Slip::Io

using namespace Slip;

Slip::unique_ptr_del<Slip::Io::SourceManager> Slip::Io::makeSourceManager() {
    return {new SourceManagerImpl, []( SourceManager* s ) { delete s; }};
}

int Io::SourceLocation::line() const {
    if( m_file ) {
        auto& txt = m_file->m_contents;
        return safe_cast( count( txt.begin(), txt.begin() + m_start, '\n' ) + 1 );
    }
    return 0;
}

int Io::SourceLocation::col() const {
    if( m_file ) {
        auto& txt = m_file->m_contents;
        auto nl = txt.rfind( '\n', m_start );
        return safe_cast( m_start - ( ( nl == string::npos ) ? 0 : nl ) );
    }
    return 0;
}

Slip::Result Io::SourceManagerImpl::load( const char* fname, TextInput& text ) {
    while( 1 ) {
        auto it = m_files.find( fname );
        if( it != m_files.end() ) {
            auto& txt = it->second->m_contents;
            text.reset( &txt[0], &txt[0] + txt.size(), it->second );
            return Result::OK;
        }
        FILE* fin = fopen( fname, "r" );
        RETURN_ERR_IF( !fin, "Unable to open %s for read", fname );
        string txt;
        while( 1 ) {
            char buf[4096];
            size_t n = fread( buf, 1, sizeof( buf ), fin );
            if( n == 0 )
                break;
            else
                txt.append( buf, buf + n );
        }
        m_files[fname] = new SourceNameAndContents{fname, txt};
    }
}

namespace {
    struct FileImpl : Slip::Io::TextOutput::Impl {
        FILE* m_file;
        bool m_close;
        FileImpl( FILE* f, bool c ) : m_file( f ), m_close( c ) {}
        virtual ~FileImpl() {
            if( m_close ) {
                fclose( m_file );
            }
        }
        virtual void put( string_view s ) override { fwrite( s.data(), 1, s.size(), m_file ); }
    };

    struct VecImpl : Slip::Io::TextOutput::Impl {
        std::vector<char>* m_vec;
        VecImpl( std::vector<char>* v ) : m_vec( v ) {}
        virtual ~VecImpl() {}
        virtual void put( string_view s ) override { m_vec->insert( m_vec->end(), s.begin(), s.end() ); }
    };
}  // namespace

Slip::Io::TextOutput::Impl::~Impl() {}
Slip::Io::TextOutput::~TextOutput() { reinterpret_cast<Impl*>( m_impl )->~Impl(); }

Slip::Io::TextOutput::TextOutput() { new( m_impl ) FileImpl( stdout, false ); }

Slip::Io::TextOutput::TextOutput( const char* fname ) { new( m_impl ) FileImpl( fopen( fname, "w" ), true ); }

Slip::Io::TextOutput::TextOutput( std::vector<char>* vec ) { new( m_impl ) VecImpl( vec ); }

void Slip::Io::TextOutput::_writeImpl( std::string_view s ) { reinterpret_cast<Impl*>( m_impl )->put( s ); }

void Slip::Io::TextOutput::_write( std::string_view s ) {
    switch( m_state ) {
        case State::Start:
            _writeImpl( string_view{m_indent.c_str(), m_indent.size()} );
            break;
        default:
            break;
    }
    _writeImpl( s );
    m_state = State::Normal;
}

void Slip::Io::TextOutput::write( std::string_view s ) {
    size_t off = 0;
    do {
        auto nl = s.find( '\n', off );
        if( nl == string_view::npos ) {
            _write( s.substr( off ) );
            // auto c = s.end()[-1];
            // m_sep = isalnum(c);
            break;
        }
        _write( s.substr( off, nl - off + 1 ) );
        m_state = State::Start;
        off = nl + 1;
    } while( 1 );
}
