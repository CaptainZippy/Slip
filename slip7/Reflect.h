#pragma once

namespace Reflect {
    struct Type;
    struct Var;
    struct Field;

    struct AbstractReflected {
        virtual const Reflect::Type* dynamicType() const = 0;
        virtual ~AbstractReflected() {}
    };

    enum class Kind {
        Void,
        Pointer,
        Record,
        Array,
    };

    struct Field {
        const char* name;
        const Type* type;
        int offset;
    };

    template<typename T>
    const Type* getType() {
        return T::staticType();
    }

    struct Var {
        void* addr;
        const Type* type;

        Var operator[]( const Field& f ) {
            return Var{ (char*) addr + f.offset, f.type };
        }
    };

    struct Type {
        typedef void* (*MakeFunc)( );
        Kind kind;// { Kind::Void };
        const Type* parent;//{ nullptr };
        const Type* sub;//{ nullptr };
        const char* name;//{ nullptr };
        unsigned size;
        MakeFunc make;//{ nullptr };
        array_view<Field> fields;

        static bool extends( const Type* test, const Type* base ) {
            for( auto c = test; c; c = c->parent ) {
                if( c == base ) {
                    return true;
                }
            }
            return false;
        }
        bool isPointer() const { return kind == Kind::Pointer; }

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

    namespace Detail {
        template<typename T>
        void* maker() { return new T(); }
    }
}

#define PP_NARG(...)    PP_NARG_(__VA_ARGS__,PP_RSEQ_N())
#define PP_NARG_(...)   PP_ARG_N(__VA_ARGS__)
#define PP_ARG_N(_1,_2,_3,_4,_5,_6,_7,_8,_9,_10,N,...) N
#define PP_RSEQ_N() 10,9,8,7,6,5,4,3,2,1,0
#define PP_PASTE(a,b) PP_PASTE_(a,b)
#define PP_PASTE_(a,b) a ## b

#if 1 //TODO
    #define PP_APPLY(MACRO, ...)  MACRO(__VA_ARGS__)
    #define PP_FOR_1(MACRO,X)     PP_APPLY(MACRO,X)
    #define PP_FOR_2(MACRO,X,...) PP_APPLY(MACRO,X) PP_APPLY(PP_FOR_1,MACRO,__VA_ARGS__)
    #define PP_FOREACH(MACRO,...) PP_APPLY(PP_PASTE(PP_FOR_, PP_NARG(__VA_ARGS__)),MACRO, __VA_ARGS__)
#else
    #define PP_FOR_1(MACRO,X)     MACRO(X)
    #define PP_FOR_2(MACRO,X,...) MACRO(X) PP_FOR_1(MACRO,__VA_ARGS__)
    #define PP_FOREACH(MACRO,...) PP_PASTE(PP_FOR_, PP_NARG(__VA_ARGS__))(MACRO, __VA_ARGS__)
#endif
    //static const Reflect::Field s_reflectFields[];

#define REFLECT_DECL() \
    struct _Auto; friend struct _Auto; \
    static const Reflect::Type s_reflectType; \
    static const Reflect::Type* staticType() { return &s_reflectType; } \
    const Reflect::Type* dynamicType() const { return &s_reflectType; } \
    struct Terminator

#define REFLECT_FIELD(DECL,NAME) { #NAME, Reflect::TypeOf<decltype(DECL::NAME)>::value(), offsetof(DECL,NAME) }

#define REFLECT_DEFN(NAME, PARENT, ...) \
    const Reflect::Type NAME::s_reflectType = { \
        Reflect::Kind::Record, \
        &PARENT::s_reflectType, #NAME, &Reflect::Detail::maker<NAME>, \
        { __VA_ARGS__ } }

//&s_reflectFields[0], &NAME::s_reflectFields[sizeof(NAME::s_reflectFields)/sizeof(Reflect::Field)]
