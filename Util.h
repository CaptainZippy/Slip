#pragma once

#include "Slip.h"

#define Error Slip::Detail::_Error{__FILE__, __LINE__}
typedef unsigned long long uptr;

namespace Slip {
    namespace Detail {
        struct _Error {
            const char* file;
            int line;
            int col;
            char buf[512];
            _Error& fmt(const char* fmt, ...) {
                va_list va;
                va_start(va, fmt);
                int cnt = snprintf(buf, sizeof(buf), "%s(%i,%i):error:", file, line, col);
                vsnprintf(&buf[cnt], sizeof(buf) - cnt, fmt, va);
                va_end(va);
                return *this;
            }
            _Error& at(const char* f, int l, int c = 0) {
                file = f;
                line = l;
                col = c;
                return *this;
            }
            template<typename LOC>
            _Error& at(const LOC& loc) {
                return at(loc.filename(), loc.line(), loc.col());
            }
            ~_Error() {
                print(buf);
            }
        protected:
            void print(const char* s) {
                printf("%s", s);
                //OutputDebugStringA( s );
            }
        };
    }
    class Exception
    {
    public:
        Exception() = default;
        Exception(std::string w) : m_what(std::move(w)) {}
        virtual ~Exception();
        const char* what() const { return m_what.c_str(); }
        std::string m_what;
    };

inline void error(const char* msg) {
    __debugbreak();
}
template<typename T>
void assert2(T t, const char* msg) {
    if (!t) {
        Error.fmt(msg);
        __debugbreak();
    }
}
#define assert(A) if(!(A)) __debugbreak()
#define cast(T,a) dynamic_cast<T*>(a)

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

#define THROW_IF_FAILED(COND, ...) do { \
    Result res = (COND); \
    if(!res.isOk()) { \
        throw Slip::Exception{"" __VA_ARGS__}; \
        } } while(0)

#define RETURN_RES_IF(RES, COND, ...) do { \
    if((COND)) { \
        Result::failed(#COND, __FILE__, __LINE__, "" __VA_ARGS__); \
        return RES; } } while(0)

#define RETURN_RES_IF_REACHED(RES, ...) do { \
    Result::failed("Failed", __FILE__, __LINE__, "" __VA_ARGS__); \
    return RES; } while(0)

#define THROW(__VA_ARGS__) throw Slip::Exception(__VA_ARGS__)

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
    template<typename U, size_t N> array_view(U(&t)[N]) : m_begin(t), m_end(m_begin + N) { static_assert(sizeof(T) == sizeof(U),""); }
    template<typename U> array_view(U* s, U* e) : m_begin(s), m_end(e) { static_assert(sizeof(T) == sizeof(U), ""); }
    template<typename U> array_view(U* s, size_t n) : m_begin(s), m_end(s + n) { static_assert(sizeof(T) == sizeof(U), ""); }
    template<typename U> array_view(array_view<U> u) : m_begin(u.m_begin), m_end(u.m_end) { static_assert(sizeof(T) == sizeof(U), ""); }
    array_view(std::vector<T>& a) : m_begin(a.data()), m_end(m_begin + a.size()) {}
    void operator=(array_view<T> a) {
        m_begin = a.m_begin;
        m_end = a.m_end;
    }

    size_t size() const { return m_end - m_begin; }
    T& operator[](unsigned i) const { assert(i < size()); return m_begin[i]; }
    T* begin() const { return m_begin; }
    T* end() const { return m_end; }
    T& front() const { return *(m_begin); }
    T& back() const { return *(m_end-1); }
    array_view<T> ltrim(unsigned n) const { assert(n <= size()); return array_view<T>(m_begin + n, m_end); }
    array_view<T> rtrim(unsigned n) const { assert(n <= size()); return array_view<T>(m_begin, m_end - n); }
    std::vector<T> std_vec() const { return std::vector<T>(m_begin, m_end); }
    T* m_begin;
    T* m_end;
};


namespace array_view_t {
    template<typename T>
    array_view<T> from_single(T& t) { return array_view<T>(&t, &t + 1); }
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

    Iter(array_view<T> v) {
        m_begin = v.begin();
        m_end = v.end();
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
std::string string_formatv(const char* fmt, va_list va);

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

template <typename T, typename V>
auto find(T&& iterable, const V& v) { return std::find(iterable.begin(), iterable.end(), v); }

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

// Interned string
struct istring {
    static istring make(const char* s);
    static istring make(std::string_view s);


    inline istring() : m_str(&s_empty[sizeof(size_t)]) {}

    inline operator std::string_view() const {
        return { m_str, size() };
    }
    inline operator const char*() const {
        return m_str;
    }
    inline const char* c_str() const {
        return m_str;
    }
    inline std::string std_str() const {
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



inline bool operator==(istring a, istring b) {
    return (const char*)a == (const char*)b;
}

std::string string_concat(array_view<std::string_view> strs);

template<typename...ARGS>
std::string string_concat(const ARGS&...args) {
    string_view strs[] = { string_view(args)... };
    return string_concat( array_view_t::make(strs) );
}


}

namespace std {
    template<> struct hash<string_view> {
        typedef string_view argument_type;
        typedef std::size_t result_type;
        std::size_t operator()(string_view const& s) const;
    };
    template<> struct hash<Slip::istring> {
        typedef Slip::istring argument_type;
        typedef std::size_t result_type;
        std::size_t operator()(Slip::istring const& s) const { return reinterpret_cast<size_t>(s.c_str()); }
    };
}


namespace Slip::Io {
    struct TextOutput {
        FILE* m_file{ nullptr };
        bool m_close{ false };
        std::string m_indent;
        enum class State {
            Normal,
            Start,
            Mid,
        };
        State m_state{ State::Normal };

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
            //m_sep = false;
        }

            /// Write, indenting on newlines
        void write(string_view s);
            /// Raw Write
        void _write(string_view s);

        //void write(const void* s) {
        //    fprintf(m_file, "%p", s);
        //    m_sep = true;
        //}
        void sep() {
            //if (m_sep) {
            //    fprintf(m_file, " ");
            //    m_sep = false;
            //}
        }
        void end(string_view s = {}) {
            m_indent.erase(m_indent.size() - 1);
            write(s);
        }
        void nl() {
            fprintf(m_file, "\n");
            m_state = State::Start;
        }
    };
}
