

slip_tests = [
    'array_type_01',
    'array_type_02',
    'attr_expr_01',
    'coro_expr_01',
    'cond_expr_01',
    'function_overload_01',
    'intrinsic_01',
   # 'iterable_01',
    'let_expr_01',
    'loop_expr_01',
    'macro_expr_01',
    'macro_expr_02',
    #'macro_expr_03',
    'now_expr_01',
    'now_expr_02',
    'recursive_01',
    'string_type_01',
    'struct_type_01',
    'struct_type_02',
    'struct_type_03',
    'struct_type_04',
    'try_expr_01',
    'try_expr_02',
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
