
slipcc = executable('slipcc',
    'slipcc.cpp',
    link_with: libslip,
    include_directories: '../../libs'
)

slip_compile = generator(slipcc,
    output : ['@BASENAME@.cpp'],
    arguments: ['--output-dir=@BUILD_DIR@', '@INPUT@'])