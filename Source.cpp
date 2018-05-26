#include "pch/Pch.h"
#include "Source.h"

namespace Slip::Lex {
    struct SourceManagerImpl : SourceManager {
        map< string, SourceNameAndContents* > m_files;

        Input load(const char* fname) override;
    };
}
using namespace Slip;

std::unique_ptr<Lex::SourceManager> Lex::SourceManager::make() {
    return make_unique<SourceManagerImpl>();
}

int Lex::SourceLocation::line() const {
    if (m_file) {
        auto& txt = m_file->m_contents;
        return safe_cast(count(txt.begin(), txt.begin() + m_start, '\n') + 1);
    }
    return 0;
}

int Lex::SourceLocation::col() const {
    if (m_file) {
        auto& txt = m_file->m_contents;
        auto nl = txt.rfind('\n', m_start);
        return safe_cast(m_start - ((nl == string::npos) ? 0 : nl));
    }
    return 0;
}

Lex::Input Lex::SourceManagerImpl::load( const char* fname) {
    while( 1 ) {
        auto it = m_files.find( fname );
        if( it != m_files.end() ) {
            auto& txt = it->second->m_contents;
            return Input( &txt[0], &txt[0] + txt.size(), it->second );
        }
        else if( FILE* fin = fopen( fname, "r" ) ) {
            string txt;
            while( 1 ) {
                char buf[4096];
                size_t n = fread( buf, 1, sizeof( buf ), fin );
                if( n == 0 ) break;
                else txt.append( buf, buf + n );
            }
            m_files[fname] = new SourceNameAndContents{ fname, txt };
        }
        else {
            THROW(string_format("Unable to open %s", fname));
        }
    }
}

