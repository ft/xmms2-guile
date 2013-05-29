/*
 * Copyright (c) 2013 xmms2-guile workers, All rights reserved.
 *
 * Terms for redistribution and use can be found in LICENCE.
 */

/**
 * @file primitive-config.c
 * @brief Implementation for primitives that wrap xmmsc_config_*() functions
 */

#include "compiler.h"
#include "xmms2-guile.h"

#include <libguile.h>
#include <xmmsclient/xmmsclient.h>

X2_TRIVIAL_SERVER_ACTION(x2_config_list, xmmsc_config_list_values)

/** Sub-Initialisation routine */
void
init_x2_primitive_config(void)
{
    xg_scm_define_and_export("xmms2:primitive/config-list",
                             1, 0, 0, x2_config_list);
}
