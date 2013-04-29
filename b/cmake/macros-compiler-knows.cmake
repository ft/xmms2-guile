include(CheckCCompilerFlag)
include(CheckCSourceCompiles)

macro(__compiler_knows_option _opt)
    set(_var COMPILER_KNOWS_OPTION_${_opt})
    check_c_compiler_flag(-${_opt} ${_var})
    if (${_var})
        add_definitions(-${_opt})
    endif (${_var})
endmacro(__compiler_knows_option)

macro(__compiler_knows_feature _feat)
    set(_var COMPILER_KNOWS_${_feat})
    set(_guard 1)
    string(TOUPPER "${_var}" _var)

    if ((${_feat} STREQUAL "cold") OR
         (${_feat} STREQUAL "hot") OR
         (${_feat} STREQUAL "warn_unused_result"))
        set(_code
            "__attribute__((${_feat})) int foo(void);
             int foo(void) { int i = 0; return i; }
             int main(void) { return 0; }")
    elseif (${_feat} STREQUAL "noreturn")
        set(_code
            "#include <stdlib.h>
             __attribute__((${_feat})) void foo(void);
             void foo(void) { exit(0); }
             int main(void) { return 0; }")
    elseif (${_feat} STREQUAL "deprecated")
        set(_code
            "void foo(void) __attribute__((${_feat}));
             void foo(void) { return; }
             int main(void) { return 0; }")
    elseif (${_feat} STREQUAL "expect")
        set(_code
            "int a = 3;
             int b = 4;
             int
             main(void)
             {
                 if (__builtin_expect(!!(a < b), 1))
                 return 0;
             }")
    elseif (${_feat} STREQUAL "printf_fmt")
        set(_code
            "#include <stdarg.h>
             __attribute__((format(__printf__, 1, 2))) void
             foo(const char *fmt, ...) { return; }
             int main(void) { return 0; }")
    elseif (${_feat} STREQUAL "unused")
        set(_code
            "__attribute__((unused)) void foo(void) { return; }
             int bar(__attribute__((${_feat})) int i) { return 0; }
             int main(void) { return 0; }")
    elseif (${_feat} STREQUAL "fumble")
        set(_code "__attribute__((${_feat})) int main(void) { return; }")
    else()
        set(_guard 0)
    endif()

    if (${_guard} EQUAL 1)
        set(CMAKE_REQUIRED_FLAGS "-Werror")
        check_c_source_compiles("${_code}" "${_var}")
    else()
        message("-- Unknown compiler feature: " ${_feat})
        set(${_var} 0)
    endif()

    if (${_var})
        set(${_var} 1)
    endif (${_var})
endmacro()
