#pragma once
#if defined(_MSC_VER)
    #define _CRT_SECURE_NO_WARNINGS
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
#include <algorithm>
#include <cctype>
#include <cstdarg>
#include <cstddef>
#include <cstdint>
#include <cstring>
#include <deque>
#include <functional>
#include <list>
#include <map>
#include <memory>
#include <numeric>
#include <set>
#include <string>
#include <string_view>
#include <type_traits>
#include <unordered_map>
#include <unordered_set>
#include <variant>
#include <vector>

constexpr std::string_view operator"" _sv( const char* str, size_t len ) noexcept { return std::string_view( str, len ); }

// Local utilities
#include "slip/Func.h"
#include "slip/Util.h"
