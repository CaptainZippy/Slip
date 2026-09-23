
if meson.get_compiler('cpp').get_id() == 'msvc'
    libslip_cpp_args = []
else
    libslip_cpp_args = [
        '-Wno-invalid-offsetof',
        '-Wno-unused-local-typedefs',
        '-Wno-pragma-once-outside-header']
endif

# Built as a static library: none of the sources carry dllexport/dllimport
# annotations, so a shared library would produce no exported symbols (and,
# on MSVC, no import .lib) for slipcc/tests to link against.
libslip = static_library('slip',
    'Ast.cpp',
    'Backend.cpp',
    'Eval.cpp',
    'Io.cpp',
    'Lex.cpp',
    'Parse.cpp',
    'Reflect.cpp',
    'Sema.cpp',
    'Slip.cpp',
    'Util.cpp',
    include_directories: '..',
    cpp_args: libslip_cpp_args,
)

