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
#include <set>
#include <list>
#include <cstddef>
#include <cstdarg>
#include <cstdint>
#include <unordered_map>
#include <unordered_set>
#include <memory>
#include <algorithm>
#include <functional>

#if defined(_MSC_VER)
    #pragma warning(suppress: 4455)
    constexpr std::string_view operator "" sv(const char* str, size_t len) noexcept {
        return std::string_view(str, len);
    }
#endif

// Local utilities
#include "Util.h"
