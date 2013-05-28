/*
 * Copyright (c) 2013 xmms2-guile workers, All rights reserved.
 *
 * Terms for redistribution and use can be found in LICENCE.
 */

/**
 * @file type-connection.c
 * @brief Implementation of a guile type for xmms2 connections
 *
 * To talk to an xmms2 server, first a connection needs to be established. The
 * new data container returned by this function encapsulates the C level
 * connection value, so it can be passed around by scheme code.
 *
 * It also keeps track of the client's name: While connecting, the client
 * submits a tag to be identified by to the server. This value is passed
 * into the container upon creation.
 */

#include "compiler.h"
#include "xmms2-guile.h"

#include <libguile.h>
#include <xmmsclient/xmmsclient.h>

/* Type utility prototypes */

static SCM make_x2_connection(SCM);

/* SMOB interface function prototypes */

static size_t free_x2_connection(SCM);
static SCM mark_x2_connection(SCM);
static int print_x2_connection(SCM, SCM, scm_print_state *);

/** Bit-field to identify the new data type by */
static scm_t_bits x2_connection_tag;

/**
 * Create a new xmms2 connection container SMOB.
 *
 * The client name is required in the SMOB, because it is needed in the
 * initialisation phase as done by `xmmsc_init()'.
 *
 * @param  clientname   The client name to insert
 *
 * @return The new SMOB, ready to use
 * @sideeffects none
 */
static SCM
make_x2_connection(SCM clientname)
{
    struct x2_connection *c;
    SCM smob;
    char *cn;

    c = (struct x2_connection *)scm_gc_malloc(sizeof (struct x2_connection),
                                              "xmms2:type/connection");
    c->clientname = clientname;
    c->c = NULL;
    SCM_NEWSMOB(smob, x2_connection_tag, c);
    cn = scm_to_locale_string(c->clientname);
    c->c = xmmsc_init(cn);
    free(cn);

    return smob;
}

static size_t
free_x2_connection(SCM smob)
{
    struct x2_connection *c;

    c = (struct x2_connection *) SCM_SMOB_DATA(smob);
    xmmsc_unref(c->c);
    scm_gc_free(c, sizeof (struct x2_connection), "xmms2:type/connection");
    return 0;
}

static SCM
mark_x2_connection(SCM smob)
{
    struct x2_connection *c;

    c = (struct x2_connection *) SCM_SMOB_DATA(smob);
    return c->clientname;
}

static int
print_x2_connection(SCM smob, SCM port, UNUSED scm_print_state *pstate)
{
    struct x2_connection *c;

    c = (struct x2_connection *) SCM_SMOB_DATA(smob);
    scm_puts("#<xmms2:type/connection ", port);
    scm_display(c->clientname, port);
    scm_puts(">", port);
    return 1;
}

void
init_x2_type_connection(void)
{
    x2_connection_tag = scm_make_smob_type("xmms2:type/connection",
                                           sizeof(struct x2_connection));
    scm_set_smob_mark(x2_connection_tag, mark_x2_connection);
    scm_set_smob_print(x2_connection_tag, print_x2_connection);
    scm_set_smob_free(x2_connection_tag, free_x2_connection);
    xg_scm_define_and_export("xmms2:type/make-connection",
                             1, 0, 0, make_x2_connection);
}
