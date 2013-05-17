/*
 * Copyright (c) 2013 xmms2-guile workers, All rights reserved.
 *
 * Terms for redistribution and use can be found in LICENCE.
 */

/**
 * @file primitive-value.c
 * @brief xmmsv_t glue code
 *
 * `xmmsv_t' is the central, opaque value type in XMMS2's client library.
 *
 * This files implements the required glue to map the different flavours of
 * xmmsv_ts to scheme data types. Here is how the flavours actually are mapped:
 *
 *   - XMMSV_TYPE_INT64      integer
 *   - XMMSV_TYPE_STRING     string
 *   - XMMSV_TYPE_DICT       alist
 *
 * The API follows the following conventions (all primitives are prefixed by
 * "xmms2:primitive/" like in the rest of the C code, too):
 *
 * The scheme procedures are named like this:
 *
 *   value->TYPE-OF-XMMSV-T
 *
 * ...for example `value->dictionary'. They are implemented by C functions,
 * that are named like this:
 *
 *   x2_value_to_TYPE-OF-XMMSV-T()
 *
 * ...for example `x2_value_to_dictionary()'. Each of the "x2_*()" functions
 * actually only unbox the `xmmsv_t' out of the `SCM' value from scheme and
 * call their corresponding worker function, that is named like this:
 *
 *   value_to_TYPE-OF-XMMSV-T()
 *
 * ...i.e. the same name as the high-level function, just without the "x2_"
 * prefix.
 *
 * For complex xmmsv_ts like dicts and lists, the function `value_t_to_scm()'
 * provides a dispatcher to the worker functions for the specific types. It is
 * the counterpart to the `xmms2-value->scheme-data' from the (xmms2 core
 * value) module. If an unsupported type is encountered, both functions return
 * a data pair, of which the first always is the symbol:
 *
 *    XMMS2-UNSUPPORTED-DATA-TYPE
 *
 * The difference between the functions is, that the scheme function will have
 * the magic type value from C translated to a symbol such as `integer'. The C
 * function will insert the magic type value from C in its place. You can
 * retrieve the correct symbol for the magic value from the pair, by doing this:
 *
 *   (integer->value-type (cdr returned-pair-variable-or-value))
 *
 *
 * The aforementioned magic values are values from C enums, and thus represent
 * symbolic names for integers to identify certain types of values, statuses
 * etc.
 *
 * This code provides many of these magic values as constant variables, that
 * are imported into the (xmms2 core primitives) module. Code that uses
 * xmms2-guile should not use these values. The (xmms2 core values) provides an
 * API to turn the magic values into proper scheme symbols.
 */

#include "compiler.h"
#include "xmms2-guile.h"

#include <libguile.h>
#include <xmmsclient/xmmsclient.h>

#define RETURN_ERROR_VALUE_IF_ERROR(value)                         \
    do {                                                           \
        const char *error;                                         \
        if (xmmsv_get_error(value, &error))                        \
            return scm_string_to_symbol(                           \
                scm_from_locale_string("erroneous-value"));        \
    } while (0)

static SCM x2co_S_PAUSED;
static SCM x2co_S_PLAYING;
static SCM x2co_S_STOPPED;
static SCM x2co_V_BIN;
static SCM x2co_V_BITBUFFER;
static SCM x2co_V_COLL;
static SCM x2co_V_DICT;
static SCM x2co_V_ERROR;
static SCM x2co_V_FLOAT;
static SCM x2co_V_INTEGER;
static SCM x2co_V_LIST;
static SCM x2co_V_NONE;
static SCM x2co_V_STRING;

static SCM value_to_dict(xmmsv_t *);
static SCM x2_value_to_dict(SCM);
static SCM value_to_integer(xmmsv_t *);
static SCM x2_value_to_integer(SCM);
static SCM value_to_list(xmmsv_t *);
static SCM x2_value_to_list(SCM);
static SCM value_to_string(xmmsv_t *);
static SCM x2_value_to_string(SCM);

static SCM x2_result_to_scheme(SCM);
static SCM x2_value_to_scheme(SCM);

static SCM x2_type_of_value(SCM);

static SCM value_to_container(xmmsv_t *);
static SCM value_t_to_scm(xmmsv_t *);
static SCM unbox_and_call(SCM, SCM (*)(xmmsv_t *));

static SCM
unbox_and_call(SCM value, SCM (*cb)(xmmsv_t *))
{
    xmmsv_t *v;

    v = (xmmsv_t *) SCM_SMOB_DATA(value);
    return cb(v);
}

static SCM
value_to_container(xmmsv_t *v)
{
    SCM value;

    RETURN_ERROR_VALUE_IF_ERROR(v);
    value = make_x2_value();
    SCM_SET_SMOB_DATA(value, v);
    return value;
}

static SCM
x2_result_to_scheme(SCM result)
{
    xmmsc_result_t *r;
    xmmsv_t *v;

    r = (xmmsc_result_t *) SCM_SMOB_DATA(result);
    v = xmmsc_result_get_value(r);
    return value_to_container(v);
}

static SCM
x2_value_to_scheme(SCM value)
{
    xmmsv_t *v;

    v = (xmmsv_t *) SCM_SMOB_DATA(value);
    RETURN_ERROR_VALUE_IF_ERROR(v);
    return value;
}

static SCM
x2_type_of_value(SCM value)
{
    xmmsv_t *v;

    v = (xmmsv_t *) SCM_SMOB_DATA(value);
    return scm_from_int(xmmsv_get_type(v));
}

/**
 * Value type to worker function dispatcher.
 *
 * @param  v    Pointer to the xmmsv_t that is to be converted.
 *
 * @return Actual scheme value in an SCM
 * @sideeffects none
 */
static SCM
value_t_to_scm(xmmsv_t *v)
{
    int type;

    type = xmmsv_get_type(v);
    switch (type) {
    case XMMSV_TYPE_INT64:
        return value_to_integer(v);
    case XMMSV_TYPE_STRING:
        return value_to_string(v);
    case XMMSV_TYPE_DICT:
        return value_to_dict(v);
    case XMMSV_TYPE_LIST:
        return value_to_list(v);
    default:
        return scm_cons(
            scm_string_to_symbol(
                scm_from_locale_string("XMMS2-UNSUPPORTED-DATA-TYPE")),
                scm_from_int(type));
    }
}

/* Conversions of xmmsv_t and SCMs that wrap one to actual scheme values */

static SCM
value_to_integer(xmmsv_t *v)
{
    int i;

    i = -1;
    xmmsv_get_int(v, &i);
    return scm_from_int(i);
}

static SCM
x2_value_to_integer(SCM value)
{
    return unbox_and_call(value, value_to_integer);
}

static SCM
value_to_string(xmmsv_t *v)
{
    const char *c;

    xmmsv_get_string(v, &c);
    return scm_from_locale_string(c);
}

static SCM
x2_value_to_string(SCM value)
{
    return unbox_and_call(value, value_to_string);
}

static SCM
value_to_dict(xmmsv_t *v)
{
    const char *key;
    xmmsv_dict_iter_t *it;
    xmmsv_t *iv;
    SCM result;

    xmmsv_get_dict_iter(v, &it);
    result = SCM_EOL;
    while (xmmsv_dict_iter_valid(it)) {
        xmmsv_dict_iter_pair(it, &key, &iv);
        result = scm_cons(scm_cons(
                              scm_string_to_symbol(
                                  scm_from_locale_string(key)),
                              value_t_to_scm(iv)),
                          result);
        xmmsv_dict_iter_next(it);
    }
    xmmsv_dict_iter_explicit_destroy(it);
    return result;
}

static SCM
x2_value_to_dict(SCM value)
{
    return unbox_and_call(value, value_to_dict);
}

static SCM
value_to_list(xmmsv_t *v)
{
    xmmsv_list_iter_t *it;
    xmmsv_t *iv;
    SCM result;

    xmmsv_get_list_iter(v, &it);
    result = SCM_EOL;
    while (xmmsv_list_iter_entry(it, &iv)) {
        result = scm_cons(value_t_to_scm(iv), result);
        xmmsv_list_iter_next(it);
    }
    xmmsv_list_iter_explicit_destroy(it);
    return result;
}

static SCM
x2_value_to_list(SCM value)
{
    return unbox_and_call(value, value_to_list);
}

void
init_x2_primitive_value(void)
{
    /* Constants */
    X2_SCM_EXPORT_CONSTANT(x2co_S_PAUSED, "XMMS2-STATUS-PAUSED",
                           scm_from_int(XMMS_PLAYBACK_STATUS_PAUSE));
    X2_SCM_EXPORT_CONSTANT(x2co_S_PLAYING, "XMMS2-STATUS-PLAYING",
                           scm_from_int(XMMS_PLAYBACK_STATUS_PLAY));
    X2_SCM_EXPORT_CONSTANT(x2co_S_STOPPED, "XMMS2-STATUS-STOPPED",
                           scm_from_int(XMMS_PLAYBACK_STATUS_STOP));

    X2_SCM_EXPORT_CONSTANT(x2co_V_BIN, "XMMS2-VALUE-BINARY",
                           scm_from_int(XMMSV_TYPE_BIN));
    X2_SCM_EXPORT_CONSTANT(x2co_V_BITBUFFER, "XMMS2-VALUE-BITBUFFER",
                           scm_from_int(XMMSV_TYPE_BITBUFFER));
    X2_SCM_EXPORT_CONSTANT(x2co_V_COLL, "XMMS2-VALUE-COLLECTION",
                           scm_from_int(XMMSV_TYPE_COLL));
    X2_SCM_EXPORT_CONSTANT(x2co_V_DICT, "XMMS2-VALUE-DICTIONARY",
                           scm_from_int(XMMSV_TYPE_DICT));
    X2_SCM_EXPORT_CONSTANT(x2co_V_ERROR, "XMMS2-VALUE-ERROR",
                           scm_from_int(XMMSV_TYPE_ERROR));
    X2_SCM_EXPORT_CONSTANT(x2co_V_FLOAT, "XMMS2-VALUE-FLOAT",
                           scm_from_int(XMMSV_TYPE_FLOAT));
    X2_SCM_EXPORT_CONSTANT(x2co_V_INTEGER, "XMMS2-VALUE-INTEGER",
                           scm_from_int(XMMSV_TYPE_INT64));
    X2_SCM_EXPORT_CONSTANT(x2co_V_LIST, "XMMS2-VALUE-LIST",
                           scm_from_int(XMMSV_TYPE_LIST));
    X2_SCM_EXPORT_CONSTANT(x2co_V_NONE, "XMMS2-VALUE-NONE",
                           scm_from_int(XMMSV_TYPE_NONE));
    X2_SCM_EXPORT_CONSTANT(x2co_V_STRING, "XMMS2-VALUE-STRING",
                           scm_from_int(XMMSV_TYPE_STRING));

    /* Primitives */
    xg_scm_define_and_export("xmms2:primitive/result->scheme",
                             1, 0, 0, x2_result_to_scheme);
    xg_scm_define_and_export("xmms2:primitive/type-of-value",
                             1, 0, 0, x2_type_of_value);
    xg_scm_define_and_export("xmms2:primitive/value->scheme",
                             1, 0, 0, x2_value_to_scheme);

    xg_scm_define_and_export("xmms2:primitive/value->integer",
                             1, 0, 0, x2_value_to_integer);
    xg_scm_define_and_export("xmms2:primitive/value->list",
                             1, 0, 0, x2_value_to_list);
    xg_scm_define_and_export("xmms2:primitive/value->string",
                             1, 0, 0, x2_value_to_string);
    xg_scm_define_and_export("xmms2:primitive/value->dictionary",
                             1, 0, 0, x2_value_to_dict);
}
