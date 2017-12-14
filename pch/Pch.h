#pragma once
#if defined(_MSC_VER)
    #define _CRT_SECURE_NO_WARNINGS
    #define _ITERATOR_DEBUG_LEVEL 0
    #pragma warning(disable:4996)
    #include <windows.h>
    #ifdef _WIN64
        typedef __int64 ssize_t;
    #else
        typedef __int32 ssize_t;
    #endif
#else
    #define __debugbreak() asm("int $3")
#endif
#include <vector>
#include <cctype>
#include <string>
#include <cstring>
#include <numeric>
#include <map>
#include <list>
#include <cstddef>
#include <cstdarg>
#include <cstdint>
#include <unordered_map>
#include <unordered_set>
#include <memory>
#include <algorithm>
#include <functional>

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
                //OutputDebugStringA( s );
            }
        };
    }
}


inline void error( const char* msg ) {
    __debugbreak();
}
template<typename T>
void assert2( T t, const char* msg ) {
    if (!t) {
        Error.fmt(msg);
        __debugbreak();
    }
}
#define assert(A) if(!(A)) __debugbreak()
#define cast(T,a) dynamic_cast<T*>(a)

#include "Util.h"
