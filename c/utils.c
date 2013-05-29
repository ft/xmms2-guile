/*
 * Copyright (c) 2013 xmms2-guile workers, All rights reserved.
 *
 * Terms for redistribution and use can be found in LICENCE.
 */

/**
 * @file utils.c
 * @brief xmms2-guile wide C-level utility functions
 */

#include "xmms2-guile.h"

#include <libguile.h>
#include <xmmsclient/xmmsclient.h>

/**
 * Define and export a primitive scheme procedure.
 *
 * The function takes the same arguments as guile's `scm_c_define_gsubr()'
 * function. Its functionality is purely a sideeffect: Define a primitive
 * scheme subroutine and automatically mark it for export with the active
 * module.
 *
 * @return void
 * @sideeffects See description.
 */
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

/**
 * Perform a trivial server action.
 *
 * A trivial action only takes the `xmmsc_connection_t' pointer, that
 * references an active XMMS2 server connection, and is thus done by functions
 * that match the function pointer prototype of the `fnc' parameter.
 *
 * @param  fnc         Pointer to the function from the XMMS2 client library
 *                     that executes the required action.
 * @param  connection  Smob that contains the `xmmsc_connection_t' pointer.
 *
 * @return A smob that wraps the `xmmsv_result_t' from the server-reply.
 * @sideeffects none
 */
SCM
x2_trivial_server_action(xmmsc_result_t *(*fnc)(xmmsc_connection_t *),
                         SCM connection)
{
    X2_SERVER_CMD_HEADER(c, connection, result, retval);
    result = fnc(c->c);
    X2_SERVER_CMD_FOOTER(result, retval);
}
