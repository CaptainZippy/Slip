
project('slip', 'cpp',
    default_options:['cpp_std=c++17'])

slip_srcs = [
    'src/Ast.cpp',
    'src/Backend.cpp',
    'src/Io.cpp',
    'src/Lex.cpp',
    'src/Parse.cpp',
    'src/Reflect.cpp',
    'src/Sema.cpp',
    'src/Slip.cpp',
    'src/Util.cpp',
]

slip_incdir = include_directories('src')

# ====

slipcc = executable('slipcc',
    slip_srcs,
    cpp_args:[
        '-Wno-invalid-offsetof',
        '-Wno-unused-local-typedefs',
        '-Wno-pragma-once-outside-header'],
    cpp_pch:['src/pch/Pch.h'],
    include_directories: slip_incdir,
    override_options:['cpp_std=c++17'])

slip_compile = generator(slipcc,
    output : ['@BASENAME@.cpp'],
    arguments: ['--output-dir=@BUILD_DIR@', '@INPUT@'])

subdir('test')
subdir('bench')
