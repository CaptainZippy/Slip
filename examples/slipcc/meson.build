
slipcc = executable('slipcc',
    'slipcc.cpp',
    link_with: libslip,
    include_directories: '../../libs'
)

slip_compile = generator(slipcc,
    output : ['@BASENAME@.cpp'],
    depends : [slipcc, libslip],
    arguments: ['--output-dir=@BUILD_DIR@', '@INPUT@'])