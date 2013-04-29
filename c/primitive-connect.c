/*
 * Copyright (c) 2013 xmms2-guile workers, All rights reserved.
 *
 * Terms for redistribution and use can be found in LICENCE.
 */

#include "compiler.h"
#include "xmms2-guile.h"

#include <libguile.h>
#include <xmmsclient/xmmsclient.h>

static SCM x2_connect(SCM, SCM);

static SCM
x2_connect(SCM connection, SCM uri)
{
    int rc;
    char *u;
    struct x2_connection *c;

    c = (struct x2_connection *) SCM_SMOB_DATA(connection);
    u = scm_to_locale_string(uri);
    rc = xmmsc_connect(c->c, u);
    if (rc == false)
        scm_throw(
            scm_string_to_symbol(
                scm_from_locale_string("xmms2:sync/connection-failure")),
            scm_from_locale_string(
                xmmsc_get_last_error(c->c)));
    return SCM_BOOL_T;
}

void
init_x2_primitive_connect(void)
{
    scm_c_define_gsubr("xmms2:primitive/connect", 2, 0, 0, x2_connect);
    scm_c_export("xmms2:primitive/connect");
}
