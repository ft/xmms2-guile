/*
 * Copyright (c) 2013 xmms2-guile workers, All rights reserved.
 *
 * Terms for redistribution and use can be found in LICENCE.
 */

#include "xmms2-guile.h"

#include <libguile.h>

void
xg_scm_define_and_export(const char *name,
                         int req,
                         int opt,
                         int rst,
                         SCM (*fnc)())
{
    scm_c_define_gsubr(name, req, opt, rst, fnc);
    scm_c_export(name);
}

SCM
x2_trivial_server_action(xmmsc_result_t *(*fnc)(xmmsc_connection_t *),
                         SCM connection)
{
    X2_SERVER_CMD_HEADER(c, connection, result, retval);
    result = fnc(c->c);
    X2_SERVER_CMD_FOOTER(result, retval);
}
