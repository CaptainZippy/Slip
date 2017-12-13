#include "pch/Pch.h"

void Result::failed(const char* what, const char* file, int line, const char* fmt, ...) {
    printf("%s:%i:Failed '%s'", file, line, what);
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

std::size_t std::hash<string_view>::operator()(string_view const& s) const {
    typedef FnvParams<sizeof(size_t)> Fnv;
    size_t hash = Fnv::offset_basis;
    for(auto c : s) {
        hash ^= static_cast<unsigned char>(c);
        hash *= Fnv::prime;
    }
    return hash;
}

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
            memcpy(i->data, v.begin(), v.size()+1);
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
