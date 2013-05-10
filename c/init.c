/*
 * Copyright (c) 2013 xmms2-guile workers, All rights reserved.
 *
 * Terms for redistribution and use can be found in LICENCE.
 */

/**
 * @file init.c
 * @brief XMMS2-Guile extension library initialisation
 */

#include "xmms2-guile.h"

void
xmms2_guile_ext_init(void)
{
    /* Initialise types */
    init_x2_type_connection();
    init_x2_type_result();
    init_x2_type_value();
    /* Add new primitives */
    init_x2_primitive_connect();
    init_x2_primitive_playback();
    init_x2_primitive_synchronous();
    init_x2_primitive_value();
}
