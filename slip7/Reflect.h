#pragma once

namespace Reflect {
    struct Type;
    struct Field {
        const char* name;
        const Type* type;
        int offset;
    };
    struct Type {
        const Type* parent;
        const char* name;
        array_view<Field> fields;
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
            return nullptr;
        }
    };
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

#define REFLECT_DECL() \
    static const Reflect::Type s_reflectType; \
    static const Reflect::Field s_reflectFields[]; \
    static const Reflect::Type* staticType() { return &s_reflectType; } \
    const Reflect::Type* dynamicType() const override { return &s_reflectType; }

#define REFLECT_FIELD(DECL,NAME) { #NAME, Reflect::TypeOf<decltype(NAME)>::value(), offsetof(DECL,NAME) }

#define REFLECT_DEFN(NAME, PARENT, ...) \
    const Reflect::Type NAME::s_reflectType = { \
        &PARENT::s_reflectType, #NAME, \
        { &NAME::s_reflectFields[0], &NAME::s_reflectFields[sizeof(NAME::s_reflectFields)/sizeof(Reflect::Field)] } }
