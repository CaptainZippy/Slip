#include "pch/Pch.h"

void Result::failed(const char* what, const char* fmt, ...) {
    printf("Failed '%s'", what);
    if (fmt && fmt[0]) {
        printf(" - ");
        va_list ap;
        va_start(ap, fmt);
        vprintf(fmt, ap);;
        va_end(ap);
    }
    printf("\n");
}


std::string string_format(const char* fmt, ...) {
    std::string str;
    str.resize(str.capacity());
    va_list ap;

    while (1) {
        va_start(ap, fmt);
        int n = vsnprintf(&str[0], str.size(), fmt, ap) + 1;
        va_end(ap);

        if (n < 0) {
            str.resize(str.size() * 2);
        }
        else if (unsigned(n) > str.capacity()) {
            str.resize(n);
        }
        else {
            str.resize(n);
            return str;
        }
    }
}

std::string string_concat(array_view<string_view> strs) {
    size_t size = std::accumulate(strs.begin(), strs.end(), size_t{0}, [](size_t a, string_view b) {
        return a + b.size(); });
    std::string ret; ret.reserve(size);
    for (auto s : strs) {
        ret.append(s.begin(),s.size());
    }
    return ret;
}
