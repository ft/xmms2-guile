/*
 * Copyright (c) 2013 xmms2-guile workers, All rights reserved.
 *
 * Terms for redistribution and use can be found in LICENCE.
 */

#include "compiler.h"
#include "xmms2-guile.h"

#include <libguile.h>
#include <xmmsclient/xmmsclient.h>

static SCM x2_play(SCM);

static SCM
x2_play(SCM connection)
{
    struct x2_connection *c;
    xmmsc_result_t *result;
    SCM retval;

    c = (struct x2_connection *) SCM_SMOB_DATA(connection);
    result = xmmsc_playback_start(c->c);
    retval = make_x2_result();
    SCM_SET_SMOB_DATA(retval, result);
    return retval;
}

static SCM
x2_pause(SCM connection)
{
    struct x2_connection *c;
    xmmsc_result_t *result;
    SCM retval;

    c = (struct x2_connection *) SCM_SMOB_DATA(connection);
    result = xmmsc_playback_pause(c->c);
    retval = make_x2_result();
    SCM_SET_SMOB_DATA(retval, result);
    return retval;
}

void
init_x2_primitive_playback(void)
{
    scm_c_define_gsubr("xmms2:primitive/play", 1, 0, 0, x2_play);
    scm_c_export("xmms2:primitive/play");
    scm_c_define_gsubr("xmms2:primitive/pause", 1, 0, 0, x2_pause);
    scm_c_export("xmms2:primitive/pause");
}
