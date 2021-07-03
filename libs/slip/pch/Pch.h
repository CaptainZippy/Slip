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
#include <type_traits>
#include <string_view>
#include <vector>
#include <deque>
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
#include <variant>

constexpr std::string_view operator "" _sv(const char* str, size_t len) noexcept {
    return std::string_view(str, len);
}

// Local utilities
#include "slip/Util.h"
#include "slip/Func.h"
