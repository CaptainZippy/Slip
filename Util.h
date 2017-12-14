#pragma once

struct Result {
    enum Code { OK = 0, ERR = 1 };
    Result(Code c) : code(c) {}
    bool isOk() const { return code == OK; }
    Code code;
    static void failed(const char* what, const char* file, int line, const char* fmt, ...);
};
#define R_OK Result::OK
#define RETURN_IF_FAILED(COND, ...) do { \
    Result res = (COND); \
    if(!res.isOk()) { \
        Result::failed(#COND, __FILE__, __LINE__, "" __VA_ARGS__); \
        return res; } } while(0)

#define RETURN_RES_IF(RES, COND, ...) do { \
    if((COND)) { \
        Result::failed(#COND, __FILE__, __LINE__, "" __VA_ARGS__); \
        return RES; } } while(0)

template<typename T>
struct view_ptr {
    view_ptr() : m_ptr(nullptr) {}
    view_ptr(T* p) : m_ptr(p) {}
    operator T*() const { return m_ptr; }
    void operator=(T* t) { m_ptr = t; }
    T* operator->() const { return m_ptr; }
    T* m_ptr;
};

template<typename T>
struct array_view {
    typedef T* iterator;
    array_view() : m_begin(nullptr), m_end(nullptr) {}
    template<int N>
    array_view(const T(&t)[N]) : m_begin(t), m_end(m_begin + N) {}
    array_view(const T* s, const T* e) : m_begin(s), m_end(e) {}
    array_view(const T* s, uptr n) : m_begin(s), m_end(s + n) {}
    array_view(const std::vector<T>& a) : m_begin(a.data()), m_end(m_begin + a.size()) {}
    void operator=(const array_view<T>& a) {
        m_begin = a.m_begin;
        m_end = a.m_end;
    }

    size_t size() const { return m_end - m_begin; }
    const T& operator[](unsigned i) const { assert(i < size()); return m_begin[i]; }
    const T* begin() const { return m_begin; }
    const T* end() const { return m_end; }
    const T& front() const { return *(m_begin); }
    const T& back() const { return *(m_end-1); }
    array_view<T> ltrim(unsigned n) const { assert(n <= size()); return array_view<T>(m_begin + n, m_end); }
    array_view<T> rtrim(unsigned n) const { assert(n <= size()); return array_view<T>(m_begin, m_end - n); }
    const T* m_begin;
    const T* m_end;
};


namespace array_view_t {
    template<typename T>
    array_view<T> from_single(const T& t) { return array_view<T>(&t, &t + 1); }
    template<typename T, int N>
    array_view<T> make(T(&t)[N]) { return array_view<T>(t, t + N); }
    template<typename T>
    array_view<T> make(const std::vector<T>& a) { return array_view<T>(a.data(), a.size()); }
}

template<typename S>
struct safe_cast_t {
    safe_cast_t(const S& s) : m_s(s) {}
    template<typename D>
    operator D() { assert(S(D(m_s)) == m_s); return D(m_s); }
    const S& m_s;
};

template<typename T>
safe_cast_t<T> safe_cast(const T& t) {
    return safe_cast_t<T>(t);
};

template<typename T>
struct Optional {
    Optional() : m_ptr(nullptr) {}
    explicit Optional(const T& t) {
        m_ptr = new(m_buf) T(t);
    }
    ~Optional() {
        reset();
    }
    void reset() {
        if (m_ptr) m_ptr->~T();
    }
    explicit operator bool() const {
        return m_ptr != nullptr;
    }
    T operator*() {
        assert(m_ptr);
        return *m_ptr;
    }
    T& operator->() {
        assert(m_ptr);
        return *m_ptr;
    }
    void set(const T& t) {
        reset();
        m_ptr = new(m_buf) T(t);
    }
    T get() {
        return *m_ptr;
    }
private:
    Optional(const Optional& o);
    void operator=(const Optional& o);
    void* m_buf[(sizeof(T) + sizeof(void*) - 1) / sizeof(void*)];
    T* m_ptr;
};

template<typename T>
struct Iter {
    Iter() = default;

    Iter(std::vector<T>& v) {
        T* t = v.size() ? &v[0] : nullptr;
        m_begin = t;
        m_end = t + v.size();
    }

    T cur() const {
        assert(size());
        return *m_begin;
    }

    bool advance() {
        assert(m_begin < m_end);
        m_begin += 1;
        return m_begin < m_end;
    }

    bool used() const {
        return m_begin == m_end;
    }

    uptr size() const {
        return m_end - m_begin;
    }

    bool match() {
        return used();
    }
    template<typename F, typename...R>
    bool match(F f, R...r) {
        f = cur();
        if (advance()) {
            return match(r...);
        }
        return false;
    }
    T* begin() { return m_begin; }
    T* end() { return m_end; }

    T* m_begin{ nullptr };
    T* m_end{ nullptr };
};

std::string string_format(const char* fmt, ...);

namespace Detail {
    template <typename T>
    struct reverse_wrapper { T& iterable; };

    template <typename T>
    auto begin(reverse_wrapper<T> w) { return rbegin(w.iterable); }

    template <typename T>
    auto end(reverse_wrapper<T> w) { return rend(w.iterable); }
}


template <typename T>
Detail::reverse_wrapper<T> reversed(T&& iterable) { return { iterable }; }

template <typename Cont, typename Lambda>
bool all_of(const Cont& c, Lambda&& lambda) {
    return std::all_of(c.begin(), c.end(), lambda);
}
template <typename Cont, typename Lambda>
bool any_of(const Cont& c, Lambda&& lambda) {
    return std::any_of(c.begin(), c.end(), lambda);
}

template <typename Cont, typename Lambda>
auto erase_if(Cont& c, Lambda&& lambda) {
    return c.erase(std::remove_if(c.begin(), c.end(), lambda), c.end());
}



struct string_view;

// Interned string
struct istring {
    static istring make(const char* s);
    static istring make(string_view s);

    inline istring() : m_str(&s_empty[sizeof(size_t)]) {}

    inline operator const char*() const {
        return m_str;
    }
    inline const char* c_str() const {
        return m_str;
    }
    inline size_t size() const {
        return reinterpret_cast<const size_t*>(m_str)[-1];
    }

private:
    static const char s_empty[];
    istring(const char* s) : m_str(s) {}
    const char* m_str;
};


// Readonly view of a possibly unterminated string
struct string_view {
    string_view()
        : m_begin(nullptr), m_end(nullptr) {
    }
    template<int N>
    string_view(const char (&c)[N])
        : m_begin(c), m_end(c+N) {
    }
    string_view(const char* s)
        : m_begin(s), m_end(s+std::strlen(s)) {
    }
    string_view(const char* s, size_t len)
        : m_begin(s), m_end(s+len) {
    }
    string_view(const char* s, const char* e)
        : m_begin(s), m_end(e) {
    }
    string_view(const std::string& s)
        : m_begin(s.c_str()), m_end(s.c_str()+s.size()) {
    }
    string_view(istring s)
        : m_begin(s.c_str()), m_end(s.c_str() + s.size()) {
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
    explicit operator bool() const {
        return m_begin != m_end;
    }
    explicit operator std::string() const {
        return { m_begin, m_end };
    }
private:
    const char* m_begin;
    const char* m_end;
};


namespace std {
    template<> struct hash<string_view> {
        typedef string_view argument_type;
        typedef std::size_t result_type;
        std::size_t operator()(string_view const& s) const;
    };
    template<> struct hash<istring> {
        typedef istring argument_type;
        typedef std::size_t result_type;
        std::size_t operator()(istring const& s) const { return reinterpret_cast<size_t>(s.c_str()); }
    };
}

inline bool operator==(string_view a, string_view b) {
    if( a.size() != b.size() ) return false;
    return memcmp(a.begin(), b.begin(), a.size())==0;
}

inline bool operator==(istring a, istring b) {
    return (const char*)a == (const char*)b;
}

std::string string_concat(array_view<string_view> strs);

template<typename...ARGS>
std::string string_concat(const ARGS&...args) {
    string_view strs[] = { string_view(args)... };
    return string_concat( array_view_t::make(strs) );
}



namespace Io {
    struct TextOutput {
        FILE* m_file;
        bool m_close;
        TextOutput()
            : m_file(stdout)
            , m_close(false) {
        }
        TextOutput(const char* fname)
            : m_file(fopen(fname, "w"))
            , m_close(true) {
        }
        ~TextOutput() {
            if (m_close) {
                fclose(m_file);
            }
        }
        void begin(string_view s) {
            write(s);
            m_indent.push_back(' ');
            m_sep = false;
        }
        void write(string_view s) {
            if (s.size()) {
                fwrite(s.begin(), 1, s.size(), m_file);
                auto c = s.end()[-1];
                m_sep = isalnum(c);
            }
        }
        //void write(const void* s) {
        //    fprintf(m_file, "%p", s);
        //    m_sep = true;
        //}
        void sep() {
            if (m_sep) {
                fprintf(m_file, " ");
                m_sep = false;
            }
        }
        void end(string_view s = {}) {
            m_indent.erase(m_indent.size() - 1);
            write(s);
        }
        void nl() {
            fprintf(m_file, "\n%s", m_indent.c_str());
            m_sep = false;
        }
        std::string m_indent;
        bool m_sep{ false };
    };
}
