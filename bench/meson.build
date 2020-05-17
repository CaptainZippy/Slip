
slip_bench = [
    'ack',
    #'ary',
    'fibo',
    'harmonic',
    #'hello',
    #'moments',
    #'nestedloop',
    #'nsieve',
]

# ====

benchmain = static_library('benchmain', 'util/main.cpp')

foreach t : slip_bench
    cpp = slip_compile.process('@0@.slip'.format(t))
    exe = executable(t, cpp, link_with:benchmain )
    test(t, exe)
endforeach
