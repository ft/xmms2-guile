/*
 * Copyright (c) 2013 xmms2-guile workers, All rights reserved.
 *
 * Terms for redistribution and use can be found in LICENCE.
 */

/**
 * @file primitive-medialib.c
 * @brief Implementation for primitives that wrap xmmsc_medialib_*() functions
 */

#include "compiler.h"
#include "xmms2-guile.h"

#include <libguile.h>
#include <xmmsclient/xmmsclient.h>

static SCM x2_medialib_get_info(SCM, SCM);

static SCM
x2_medialib_get_info(SCM connection, SCM id)
{
    X2_SERVER_CMD_HEADER(c, connection, result, retval);
    result = xmmsc_medialib_get_info(c->c,
                                     scm_to_int64(id));
    X2_SERVER_CMD_FOOTER(result, retval);
}

/** Sub-Initialisation routine */
void
init_x2_primitive_medialib(void)
{
    xg_scm_define_and_export("xmms2:primitive/medialib-get-info",
                             2, 0, 0, x2_medialib_get_info);
}
