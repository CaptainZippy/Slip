

slip_errs = [
    'syntax_01',
    'syntax_02',
    'overload_01',
]

srcs = []
foreach err : slip_errs
    s = meson.current_source_dir() + '/' + err + '.slip'
    test(err, slipcc,
        args: ['--tap', s],
        protocol: 'tap')
endforeach
