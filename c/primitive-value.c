/*
 * Copyright (c) 2013 xmms2-guile workers, All rights reserved.
 *
 * Terms for redistribution and use can be found in LICENCE.
 */

#include "compiler.h"
#include "xmms2-guile.h"

#include <libguile.h>
#include <xmmsclient/xmmsclient.h>

static SCM x2co_S_PAUSED;
static SCM x2co_S_PLAYING;
static SCM x2co_S_STOPPED;
static SCM x2co_V_INTEGER;
static SCM x2co_V_NONE;
static SCM x2co_V_ERROR;

static SCM x2_result_to_scheme(SCM);
static SCM x2_type_of_value(SCM);
static SCM x2_value_to_scheme(SCM);

static SCM value_to_scm(xmmsv_t *);
static void value_guard(xmmsv_t *);

static void
value_guard(xmmsv_t *v)
{
    const char *err;

    if (xmmsv_get_error (v, &err))
        scm_throw(
            scm_string_to_symbol(
                scm_from_locale_string("xmms2:primitive/value-error")),
            scm_from_locale_string(err));
}

static SCM
value_to_scm(xmmsv_t *v)
{
    SCM value;

    value_guard(v);
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
    return value_to_scm(v);
}

static SCM
x2_value_to_scheme(SCM value)
{
    xmmsv_t *v;

    v = (xmmsv_t *) SCM_SMOB_DATA(value);
    value_guard(v);
    return value;
}

static SCM
x2_type_of_value(SCM value)
{
    xmmsv_t *v;

    v = (xmmsv_t *) SCM_SMOB_DATA(value);
    return scm_from_int(xmmsv_get_type(v));
}

static SCM
x2_value_to_integer(SCM value)
{
    xmmsv_t *v;
    int i;

    v = (xmmsv_t *) SCM_SMOB_DATA(value);
    i = -1;
    xmmsv_get_int(v, &i);
    return scm_from_int(i);
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
    X2_SCM_EXPORT_CONSTANT(x2co_V_INTEGER, "XMMS2-VALUE-INTEGER",
                           scm_from_int(XMMSV_TYPE_INT64));
    X2_SCM_EXPORT_CONSTANT(x2co_V_NONE, "XMMS2-VALUE-NONE",
                           scm_from_int(XMMSV_TYPE_NONE));
    X2_SCM_EXPORT_CONSTANT(x2co_V_ERROR, "XMMS2-VALUE-ERROR",
                           scm_from_int(XMMSV_TYPE_ERROR));
    /* Primitives */
    xg_scm_define_and_export("xmms2:primitive/result->scheme",
                             1, 0, 0, x2_result_to_scheme);
    xg_scm_define_and_export("xmms2:primitive/type-of-value",
                             1, 0, 0, x2_type_of_value);
    xg_scm_define_and_export("xmms2:primitive/value->integer",
                             1, 0, 0, x2_value_to_integer);
    xg_scm_define_and_export("xmms2:primitive/value->scheme",
                             1, 0, 0, x2_value_to_scheme);
}
