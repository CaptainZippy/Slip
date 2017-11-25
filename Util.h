#pragma once


struct string_view {
    template<int N>
    string_view(const char (&c)[N])
        : m_begin(c), m_end(c+N) {
    }
    string_view(const char* s)
        : m_begin(s), m_end(s+std::strlen(s)) {
    }
    string_view(const std::string& s)
        : m_begin(s.c_str()), m_end(s.c_str()+s.size()) {
    }
    size_t size() const {
        return m_end - m_begin;
    }
    const char* begin() const {
        return m_begin;
    }
    const char* end() const {
        return m_end;
    }
private:
    const char* m_begin;
    const char* m_end;
};

std::string string_concat(array_view<string_view> strs);

template<typename...ARGS>
std::string string_concat(const ARGS&...args) {
    string_view strs[] = { string_view(args)... };
    return string_concat( array_view_t::make(strs) );
}

