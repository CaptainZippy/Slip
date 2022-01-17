

slip_tests = [
    'array_type_01',
    'array_type_02',
    'attr_expr_01',
    'coro_expr_01',
    'cond_expr_01',
    'function_overload_01',
    'fmt_expr_01',
    'intrinsic_01',
   # 'iterable_01',
    'let_expr_01',
    'loop_expr_01',
    'macro_expr_01',
    'macro_expr_02',
    #'macro_expr_03',
    'module_01',
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

srcs = []
foreach t : slip_tests
    srcs += '@0@.slip'.format(t)
endforeach
cpps = slip_compile.process(srcs)

exe = executable('feature', cpps, '../util/main.cpp',
        cpp_args: get_variable('warning_opts'))

foreach t : slip_tests
    test(t, exe,
        args: t,
        suite: 'feature',
        protocol: 'exitcode')
endforeach
