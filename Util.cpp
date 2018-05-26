#include "pch/Pch.h"

Slip::Exception::~Exception() = default;

namespace Slip {

void Result::failed(const char* what, const char* file, int line, const char* fmt, ...) {
    printf("%s:%i:", file, line);
    if (fmt && fmt[0]) {
        va_list ap;
        va_start(ap, fmt);
        vprintf(fmt, ap);;
        va_end(ap);
    }
    else {
        printf("Failed");
    }
    printf(" - '%s'\n", what);
}


std::string string_format(const char* fmt, ...) {
    va_list ap;
    va_start(ap, fmt);
    std::string str = string_formatv(fmt, ap);
    va_end(ap);
    return str;
}

std::string string_formatv(const char* fmt, va_list ap) {
    std::string str;
    str.resize(str.capacity());

    while (1) {
        int n = vsnprintf(&str[0], str.size(), fmt, ap) + 1; // incl nul

        if (n < 0) {
            str.resize(str.size() * 2);
        }
        else if (unsigned(n) > str.capacity()) {
            str.resize(n);
        }
        else {
            str.resize(n - 1);
            return str;
        }
    }
}

std::string string_concat(array_view<string_view> strs) {
    size_t size = std::accumulate(strs.begin(), strs.end(), size_t{0}, [](size_t a, string_view b) {
        return a + b.size(); });
    std::string ret; ret.reserve(size);
    for (auto s : strs) {
        ret.append(s.begin(),s.end());
    }
    return ret;
}

const char istring::s_empty[2 * sizeof(void*)] = {};

istring istring::make(const char* s) {
    return make(string_view(s));
}

istring istring::make(string_view s) {
    struct Item {
        ssize_t size{-1};
        char data[1]; //actually size+1 bytes
        static Item* make(string_view v) {
            auto i = (Item*)malloc(sizeof(Item) + v.size() + 1);
            i->size = v.size();
            memcpy(i->data, v.data(), v.size());
            i->data[v.size()] = 0;
            return i;
        }
    };
    static std::unordered_map<string_view, Item*> s_items;
    auto it = s_items.find(s);
    if( it != s_items.end() ) {
        return istring(it->second->data);
    }
    auto item = Item::make(s);
    s_items.emplace(s, item);

    return istring(item->data);
}

void Io::TextOutput::_write(string_view s) {
    switch (m_state) {
        case State::Start:
            fwrite(m_indent.c_str(), 1, m_indent.size(), m_file);
            break;
        default:
            break;
    }
    fwrite(s.data(), 1, s.size(), m_file);
    m_state = State::Normal;
}


void Io::TextOutput::write(string_view s) {
    size_t off = 0;
    do {
        auto nl = s.find('\n', off);
        if (nl == string_view::npos) {
            _write(s.substr(off));
            //auto c = s.end()[-1];
            //m_sep = isalnum(c);
            break;
        }
        _write(s.substr(off, nl+1));
        m_state = State::Start;
        off = nl + 1;
    } while (1);
}
}

namespace {
    template<unsigned bytes> struct FnvParams;
    template<> struct FnvParams<4> {
        static constexpr auto offset_basis = 0x811c9dc5;
        static constexpr auto prime = 0x1000193;
    };
    template<> struct FnvParams<8> {
        static constexpr auto offset_basis = 0xcbf29ce484222325;
        static constexpr auto prime = 0x100000001b3;
    };
}

std::size_t std::hash<std::string_view>::operator()(std::string_view const& s) const {
    typedef FnvParams<sizeof(size_t)> Fnv;
    size_t hash = Fnv::offset_basis;
    for (auto c : s) {
        hash ^= static_cast<unsigned char>(c);
        hash *= Fnv::prime;
    }
    return hash;
}
