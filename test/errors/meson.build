

slip_errs = [
    'parse_cond_01',
    'syntax_01',
    'syntax_02',
    'try_err_01',
    'overload_01',
    'parse_err_01',
]

srcs = []
foreach err : slip_errs
    s = meson.current_source_dir() + '/' + err + '.slip'
    test(err, slipcc,
        args: ['--tap', s],
        suite: 'errs',
        protocol: 'tap')
endforeach
