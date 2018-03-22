
project('slip', 'cpp',
   default_options:['cpp_std=c++14'])
executable('slipc',
    'Ast.cpp', 'Backend.cpp', 'Lex.cpp', 'Parse.cpp', 'Reflect.cpp', 'Sema.cpp',
    'Slip7.cpp', 'Source.cpp', 'Util.cpp',
    cpp_args:['-Wno-invalid-offsetof', '-Wno-unused-local-typedefs'],
    cpp_pch:['pch/Pch.h', 'pch/Pch.cpp'])
