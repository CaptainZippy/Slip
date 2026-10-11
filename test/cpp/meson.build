cpp_tests = [
    'backend_result',
    'eval_boundary',
    'intrinsic_callback',
    'number_value',
]

foreach t : cpp_tests
    exe = executable(t, '@0@.cpp'.format(t),
        include_directories: '../../libs',
        link_with: libslip,
        cpp_args: get_variable('warning_opts'))
    test(t, exe, suite: 'cpp')
endforeach
