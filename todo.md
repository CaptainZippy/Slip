
# Backend

* check for unused values (mostly so backend doesn't have unused variable warnings)
* neatly handle "void" in backend

# Sema

* error checking for immutable, mutable, const
    * must initialize const with constant expr

# Features 

* modules
* generics
* ranges/iterators
* debug logging
* tail recursion

# Syntax 

* method syntax (obj.method a b) == (method obj a b)

* string syntax - raw strings, quoted strings, here documents

# Snippet

struct array_view__builtin_string__ {
    builtin_string* m_data;
    size_t m_count;
 };
 inline int array_view__builtin_string__size(const array_view__builtin_string__& a) { return a.m_count; }
 inline builtin_string array_view__builtin_string__at(const array_view__builtin_string__& a, int i) { return a.m_data[i]; }
 inline builtin_Result<builtin_string> array_view__builtin_string__get(const array_view__builtin_string__& a, int i) { if( unsigned(i) < unsigned(a.m_count) ) return a.m_data[i]; return failed; }
 