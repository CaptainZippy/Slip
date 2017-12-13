
project('slip', 'cpp',
   default_options:['cpp_std=c++14'])
executable('slipc',
    'Ast.cpp', 'Lex.cpp', 'Parse.cpp', 'Reflect.cpp', 'Slip7.cpp', 'Type.cpp', 'Util.cpp',
    cpp_args:['-Wno-invalid-offsetof', '-Wno-unused-local-typedefs'],
    cpp_pch:['pch/Pch.h', 'pch/Pch.cpp'])
