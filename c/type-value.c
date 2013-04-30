/*
 * Copyright (c) 2013 xmms2-guile workers, All rights reserved.
 *
 * Terms for redistribution and use can be found in LICENCE.
 */

/**
 * @file type-value.c
 * @brief Guile representation for xmmsv_t
 *
 * We don't need to free this data container. With synchronous connections,
 * it'll get wiped away with the corresponding `xmmsc_result_t'. With
 * asynchronous connections it gets freed after a callback is done with it.
 */

#include "compiler.h"
#include "xmms2-guile.h"

#include <libguile.h>
#include <xmmsclient/xmmsclient.h>

static SCM make_x2_value(void);
static int print_x2_value(SCM, SCM, scm_print_state *);
static scm_t_bits x2_value_tag;

static SCM
make_x2_value(void)
{
    xmmsv_t *r;
    SCM smob;

    r = (xmmsv_t *)NULL;
    SCM_NEWSMOB(smob, x2_value_tag, r);
    return smob;
}

static int
print_x2_value(UNUSED SCM smob, SCM port, UNUSED scm_print_state *pstate)
{
    scm_puts("#<xmms2:type/value>", port);
    return 1;
}

void
init_x2_type_value(void)
{
    x2_value_tag = scm_make_smob_type("xmms2:type/value", 0);
    scm_set_smob_print(x2_value_tag, print_x2_value);
    scm_set_smob_free(x2_value_tag, NULL);
    xg_scm_define_and_export("xmms2:type/make-value", 0, 0, 0, make_x2_value);
}
