
project('slip', 'cpp',
    default_options:['cpp_std=c++17'])
executable('slipc',
    'Ast.cpp',
    'Backend.cpp',
    'Io.cpp',
    'Lex.cpp',
    'Parse.cpp',
    'Reflect.cpp',
    'Sema.cpp',
    'Slip.cpp',
    'Util.cpp',
    cpp_args:[
        '-Wno-invalid-offsetof',
        '-Wno-unused-local-typedefs',
        '-Wno-pragma-once-outside-header'],
    cpp_pch:['pch/Pch.h', 'pch/Pch.cpp'],
    override_options:['cpp_std=c++17'])
