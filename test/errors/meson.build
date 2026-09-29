

slip_errs = [
    'parse_cond_01',
    'syntax',
    'try_err_01',
    'overload',
    'parse_err_01',
    'meta_operation',
    'meta_payload',
    'meta_prefix',
    'meta_eof',
    'meta_syntax',
    'prefix_space',
    'intrinsic_eval_errors',
]

# These tests don't produce an executable.
# They verify that compiling the input files give an appropriate error.
# A single '.slip' file may bundle several independent cases separated by
# lines containing only ';===' -- slipcc reports each as its own TAP result.

srcs = []
foreach err : slip_errs
    s = meson.current_source_dir() + '/' + err + '.slip'
    test(err, slipcc,
        args: ['--tap', s],
        suite: 'errs',
        protocol: 'tap')
endforeach
