/*
 * Copyright (c) 2013 xmms2-guile workers, All rights reserved.
 *
 * Terms for redistribution and use can be found in LICENCE.
 */

#ifndef INC_COMPILER_H
#define INC_COMPILER_H

/**
 * @file  compiler.h
 * @brief Short-hand macros for compiler features
 *
 * Some of these macros allow program annotation with support within the
 * compiler. Another class of macros allows the developer to help the compiler
 * with optimisations. Other macros provide additional information about
 * functions, to help compilers to provide better warnings etc.
 *
 * These features are compiler-specific, so they are guarded by
 * COMPILER_KNOWS_* macros, which are tested for by `cmake' during environment
 * tests and defined in `PROJECT_BASE/config.h'.
 *
 * If for example the system's compiler knows `__attribute__((unused))', then
 * UNUSED expands to it; otherwise it expands to nothing.
 */

/* The COMPILER_KNOWS_* macros are defined here: */
#include "config.h"

/*
 * Annotation macros
 */

#ifdef COMPILER_KNOWS_UNUSED

/**
 * Tell the compiler, that it is known that a variable is currently unused.
 *
 * @code
 * void
 * some_callback(int index, UNUSED char *stuff)
 * {
 *     // ...code...
 * }
 * @endcode
 */
#define UNUSED __attribute__((unused))

#else
#define UNUSED
#endif /* COMPILER_KNOWS_UNUSED */

#ifdef COMPILER_KNOWS_NORETURN

/**
 * Tell the compiler that function will never ever return.
 *
 * @code
 * NORETURN void die(const char *fmt, ...);
 * @endcode
 */
#define NORETURN __attribute__((noreturn))

#else
#define NORETURN
#endif /* COMPILER_KNOWS_NORETURN */

/*
 * Optimisation macros
 */

#ifdef COMPILER_KNOWS_COLD

/**
 * Tell the compiler if it is very unlikely, that a function is called.
 *
 * @code
 * COLD void rare_problem_solver(struct problem *p);
 * @endcode
 */
#define COLD __attribute__((cold))

#else
#endif /* COMPILER_KNOWS_COLD */

#ifdef COMPILER_KNOWS_HOT

/**
 * Tell the compiler, that a function is a "hot-spot".
 *
 * This may result in more aggressive optimisations by the compiler.
 *
 * @code
 * HOT int important_computation(int start);
 * @endcode
 */
#define HOT __attribute__((hot))

#else
#endif /* COMPILER_KNOWS_HOT */

#ifdef COMPILER_KNOWS_EXPECT

/**
 * Tell the compiler, the expression 'EXP' is likely to be true.
 *
 * @code
 *     if (LIKELY(a > b)) {
 *         // ...code...
 *     }
 * @endcode
 */
#define LIKELY(EXP) __builtin_expect(!!(EXP), 1)

/**
 * Tell the compiler, the expression 'EXP' is unlikely to be true.
 *
 * @code
 *     if (UNLIKELY(a < b)) {
 *         // ...code...
 *     }
 * @endcode
 */
#define UNLIKELY(EXP) __builtin_expect(!!(EXP), 0)

#else
#define LIKELY(EXP) (EXP)
#define UNLIKELY(EXP) (EXP)
#endif /* COMPILER_KNOWS_EXPECT */

/*
 * Semantic helper macros
 */

#ifdef COMPILER_KNOWS_PRINTF_FMT

/**
 * Tell the compiler that a function takes printf-like arguments.
 *
 * This allows the compiler to apply the same set of tests and warnings as with
 * printf.
 *
 * @code
 * PRINTF_FMT(1, 2) void debug_printf(const char *fmt, ...);
 * @endcode
 */
#define PRINTF_FMT(FORMAT_INDEX, ARG_INDEX)                             \
    __attribute__((format(__printf__, FORMAT_INDEX, ARG_INDEX)))

#else
#define PRINTF_FMT(FORMAT_INDEX, ARG_INDEX)
#endif /* COMPILER_KNOWS_PRINTF_FMT */

#ifdef COMPILER_KNOWS_WARN_UNUSED_RESULT

/**
 * Tell the compiler to warn if the return value of a function is ignored.
 *
 * That would make sense for `malloc(3)' etc.
 *
 * @code
 * WARN_UNUSED_RESULT struct keytab *lookup_key(char *description);
 * @endcode
 */
#define WARN_UNUSED_RESULT __attribute__((warn_unused_result))

#else
#define WARN_UNUSED_RESULT
#endif /* COMPILER_KNOWS_WARN_UNUSED_RESULT */

#ifdef COMPILER_KNOWS_DEPRECATED

/*
 * Other macros
 */

/**
 * Emit a warning if a function marked like this is used in the code.
 *
 * @code
 * void old_thing(int horrible, char *api) DEPRECATED;
 * @endcode
 */
#define DEPRECATED __attribute__((deprecated))

#else
#endif /* COMPILER_KNOWS_DEPRECATED */

#endif /* INC_COMPILER_H */
