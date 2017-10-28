#pragma once

namespace Reflect {
    struct Type;
    struct Var;
    struct Field;

    void printVar(Var top);

    struct AbstractReflected {
        virtual const Reflect::Type* dynamicType() const = 0;
        virtual ~AbstractReflected() {}
    };

    enum class Kind : unsigned {
        Void,
        Pointer,
        Record,
        Array,
        String,
    };

    struct Field {
        std::string name;
        const Type* type;
        int offset;
        unsigned flags;
    };

    template<typename T>
    const Type* getType() {
        return T::staticType();
    }

    struct Var {
        template<typename T>
        Var(T* t) : addr(t), type(t->dynamicType()) {}
        Var(void* a, const Type* t) : addr(a), type(t) {}

        void* addr;
        const Type* type;

        Var operator[]( const Field& f ) {
            return Var{ (char*) addr + f.offset, f.type };
        }
    };

    struct Type {
        typedef void* (*MakeFunc)();
        typedef const Type* (*DynamicTypeFunc)(const void*);
        typedef std::string (*ToStringFunc)(const void*);
        Kind kind;// { Kind::Void };
        const Type* parent;//{ nullptr };
        const Type* sub;//{ nullptr };
        const char* name;//{ nullptr };
        unsigned size;
        MakeFunc make;//{ nullptr };
        DynamicTypeFunc dynamicType;//{ nullptr };
        ToStringFunc toString;//{ nullptr };
        std::vector<Field> fields;

        template<typename BASE>
        bool extends() const {
            auto b = BASE::staticType();
            for (auto c = this; c; c = c->parent) {
                if (c == b) {
                    return true;
                }
            }
            return false;
        }
        bool isPointer() const { return kind == Kind::Pointer; }
        bool isArray() const { return kind == Kind::Array; }

        Var newInstance() const {
            assert( make );
            return Var{ ( *make )( ), this };
        }
    };
    template<typename T>
    struct TypeOf {
        static const Type* value( ) {
            return T::staticType();
        }
    };
    template<typename T>
    struct TypeOf<T*> {
        static const Type* value() {
            static const Type t{ Kind::Pointer, nullptr, TypeOf<T>::value(), "T*", sizeof(T*), nullptr, {}, };
            return &t;
        }
    };
    template<typename T>
    struct TypeOf< std::vector<T> > {
        static const Type* value() {
            static const Type t{ Kind::Array, nullptr, TypeOf<T>::value(), "T[]", sizeof(std::vector<T>), nullptr,{}, };
            return &t;
        }
    };
    template<>
    struct TypeOf< std::string > {
        static const Type* value() {
            static const Type t{ Kind::String, nullptr, nullptr, "char[]",
                sizeof(std::string), nullptr, nullptr, [](const void* addr) { return *(std::string*)addr; } };
            return &t;
        }
    };

    namespace Detail {
        template<typename T>
        void* maker() { return new T(); }
        template<typename T>
        const Type* dynamicType(const void* addr) { return static_cast<const AbstractReflected*>(addr)->dynamicType(); }
    }
}

#define PP_NARG(...)    PP_NARG_(__VA_ARGS__,PP_RSEQ_N())
#define PP_NARG_(...)   PP_ARG_N(__VA_ARGS__)
#define PP_ARG_N(_1,_2,_3,_4,_5,_6,_7,_8,_9,_10,N,...) N
#define PP_RSEQ_N() 10,9,8,7,6,5,4,3,2,1,0
#define PP_PASTE(a,b) PP_PASTE_(a,b)
#define PP_PASTE_(a,b) a ## b

#if 1
    #define PP_APPLY(MACRO, ...)  MACRO(__VA_ARGS__)
    #define PP_FOR_1(MACRO,X)     PP_APPLY(MACRO,X)
    #define PP_FOR_2(MACRO,X,...) PP_APPLY(MACRO,X) PP_APPLY(PP_FOR_1,MACRO,__VA_ARGS__)
    #define PP_FOREACH(MACRO,...) PP_APPLY(PP_PASTE(PP_FOR_, PP_NARG(__VA_ARGS__)),MACRO, __VA_ARGS__)
#else //TODO non msvc
    #define PP_FOR_1(MACRO,X)     MACRO(X)
    #define PP_FOR_2(MACRO,X,...) MACRO(X) PP_FOR_1(MACRO,__VA_ARGS__)
    #define PP_FOREACH(MACRO,...) PP_PASTE(PP_FOR_, PP_NARG(__VA_ARGS__))(MACRO, __VA_ARGS__)
#endif

#define REFLECT_DECL() \
    struct _Auto; friend struct _Auto; \
    static Reflect::Type s_reflectType; \
    static const Reflect::Type* staticType() { return &s_reflectType; } \
    const Reflect::Type* dynamicType() const { return &s_reflectType; } \
    struct Terminator

#define REFLECT_BEGIN(NAME) \
    struct NAME::_Auto { static Reflect::Type make(); }; \
    Reflect::Type NAME::s_reflectType = NAME::_Auto::make(); \
    Reflect::Type NAME::_Auto::make() { Reflect::Type self{}; \
    typedef NAME DECL; \
    self.name = #NAME; self.kind = Reflect::Kind::Record; self.size = sizeof(NAME); \
    self.dynamicType = &Reflect::Detail::dynamicType<NAME>;
#define REFLECT_END() return self; }

#define REFLECT_PARENT(NAME) self.parent = &NAME::s_reflectType;
#define REFLECT_FIELD2(NAME, FLAGS) self.fields.push_back( {#NAME, Reflect::TypeOf<decltype(DECL::NAME)>::value(), offsetof(DECL,NAME), FLAGS} );
#define REFLECT_FIELD(NAME) REFLECT_FIELD2(NAME,0)
#define REFLECT_TO_STRING(FUNC) self.toString = FUNC;


