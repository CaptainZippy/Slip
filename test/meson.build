

slip_tests = [
    'array_type_01',
    'array_type_02',
    'cond_expr_01',
#   'function_overload_01',
    'intrinsic_01',
    'let_expr_01',
#    'macro_expr_01',
    'recursive_01',
    'string_type_01',
    'type_infer_00',
    'type_infer_01',
    'type_infer_02',
    'type_infer_03',
    'var_expr_01',
]

# ====

testmain = static_library('testmain', 'util/main.cpp')

foreach t : slip_tests
    cpp = slip_compile.process('@0@.slip'.format(t))
    exe = executable(t, cpp, link_with:testmain )
    test(t, exe)
endforeach
