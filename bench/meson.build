
slip_bench = [
    'ackermann',
    #'ary',
    'fibo',
    'harmonic',
    'hash',
    'heapsort',
    'hello',
    #'moments',
    'nbody',
    'nestedloop',
    #'nsieve',
]

# ====

srcs = []
foreach t : slip_bench
    srcs += '@0@.slip'.format(t)
endforeach
cpps = slip_compile.process(srcs)

exe = executable('bench', cpps, 'util/main.cpp',
        cpp_args: get_variable('warning_opts'))

foreach t : slip_bench
    test(t, exe,
        args: t,
        suite: 'bench',
        protocol: 'exitcode')
endforeach
