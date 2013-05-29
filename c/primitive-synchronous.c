/*
 * Copyright (c) 2013 xmms2-guile workers, All rights reserved.
 *
 * Terms for redistribution and use can be found in LICENCE.
 */

/**
 * @file primitive-playback.c
 * @brief Implementation for synchronous-connection primitives
 */

#include "compiler.h"
#include "xmms2-guile.h"

#include <libguile.h>
#include <xmmsclient/xmmsclient.h>

static SCM x2_sync_wait(SCM);

static SCM
x2_sync_wait(SCM result)
{
    xmmsc_result_t *r;

    r = (xmmsc_result_t *) SCM_SMOB_DATA(result);
    xmmsc_result_wait(r);
    return result;
}

void
init_x2_primitive_synchronous(void)
{
    xg_scm_define_and_export("xmms2:primitive/sync-wait",
                             1, 0, 0, x2_sync_wait);
}
