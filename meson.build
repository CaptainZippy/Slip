
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

slip_tests = [
    'array_type_01',
#   'array_type_02',
    'cond_expr_01',
#   'function_overload_01',
    'intrinsic_01',
    'let_expr_01',
    'macro_expr_01',
    'recursive_01',
    'string_type_01',
    'type_infer_00',
    'type_infer_01',
    'type_infer_02',
    'type_infer_03',
    'var_expr_01',
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

foreach t : slip_tests
    cpp = slip_compile.process('test/@0@.slip'.format(t))
    exe = executable(t, cpp, 'test/util/main.cpp')
    test(t, exe)
endforeach
