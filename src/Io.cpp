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
        RETURN_RES_IF( Result::ERR, !fin, "Unable to open %s for read", fname );
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

void Slip::Io::TextOutput::_write( std::string_view s ) {
    switch( m_state ) {
        case State::Start:
            fwrite( m_indent.c_str(), 1, m_indent.size(), m_file );
            break;
        default:
            break;
    }
    fwrite( s.data(), 1, s.size(), m_file );
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
        _write( s.substr( off, nl + 1 ) );
        m_state = State::Start;
        off = nl + 1;
    } while( 1 );
}
