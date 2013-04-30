/*
 * Copyright (c) 2013 xmms2-guile workers, All rights reserved.
 *
 * Terms for redistribution and use can be found in LICENCE.
 */

/**
 * @file type-result.c
 * @brief Implement a guile representation for xmmsc_result_t.
 */

#include "compiler.h"
#include "xmms2-guile.h"

#include <libguile.h>
#include <xmmsclient/xmmsclient.h>

/* SMOB interface function prototypes */

static size_t free_x2_result(SCM);
static int print_x2_result(SCM, SCM, scm_print_state *);

/** Bit-field to identify the new data type by */
static scm_t_bits x2_result_tag;

SCM
make_x2_result(void)
{
    xmmsc_result_t *r;
    SCM smob;

    r = (xmmsc_result_t *)NULL;
    SCM_NEWSMOB(smob, x2_result_tag, r);
    return smob;
}

static size_t
free_x2_result(SCM smob)
{
    xmmsc_result_t *r;

    r = (xmmsc_result_t *) SCM_SMOB_DATA(smob);
    xmmsc_result_unref(r);
    return 0;
}

static int
print_x2_result(UNUSED SCM smob, SCM port, UNUSED scm_print_state *pstate)
{
    scm_puts("#<xmms2:type/result>", port);
    return 1;
}

void
init_x2_type_result(void)
{
    x2_result_tag = scm_make_smob_type("xmms2:type/result", 0);
    scm_set_smob_print(x2_result_tag, print_x2_result);
    scm_set_smob_free(x2_result_tag, free_x2_result);
    xg_scm_define_and_export("xmms2:type/make-result", 0, 0, 0, make_x2_result);
}
