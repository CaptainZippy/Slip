
project('slip', 'cpp',
    default_options:['cpp_std=c++17']
)

if meson.get_compiler('cpp').get_id() == 'g++'
    warning_opts = ['-Wno-unused-but-set-variable']
else
    warning_opts = []
endif

subdir('libs/slip')
subdir('examples/slipcc')
subdir('test/feature')
subdir('test/errors')
subdir('bench')
