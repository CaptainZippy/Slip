

slip_errs = [
    'parse_cond_01',
    'syntax_01',
    'syntax_02',
    'try_err_01',
    'overload_01',
    'parse_err_01',
    'meta_payload_01',
    'meta_payload_02',
    'meta_payload_03',
    'meta_payload_04',
    'meta_payload_05',
    'meta_payload_06',
    'meta_prefix_01',
    'meta_prefix_02',
    'meta_prefix_03',
    'meta_prefix_04',
    'meta_prefix_05',
    'meta_prefix_06',
    'meta_eof_01',
    'meta_eof_02',
    'meta_eof_03',
    'meta_eof_04',
    'meta_eof_05',
    'meta_eof_06',
    'meta_eof_07',
    'meta_eof_08',
    'meta_eof_09',
    'meta_eof_10',
    'meta_syntax_01',
    'meta_syntax_02',
    'meta_syntax_03',
    'meta_syntax_04',
    'meta_syntax_05',
    'meta_syntax_06',
    'prefix_space_01',
    'prefix_space_02',
    'prefix_space_03',
    'prefix_space_04',
    'prefix_space_05',
    'prefix_space_06',
    'prefix_space_07',
    'prefix_space_08',
    'prefix_space_09',
    'prefix_space_10',
    'prefix_space_11',
    'prefix_space_12',
    'prefix_space_13',
]

# These tests don't produce an executable.
# They verify that compiling the input files give an appropriate error.

srcs = []
foreach err : slip_errs
    s = meson.current_source_dir() + '/' + err + '.slip'
    test(err, slipcc,
        args: ['--tap', s],
        suite: 'errs',
        protocol: 'tap')
endforeach
