/*
 * Copyright (c) 2013 xmms2-guile workers, All rights reserved.
 *
 * Terms for redistribution and use can be found in LICENCE.
 */

#include "compiler.h"
#include "xmms2-guile.h"

#include <libguile.h>
#include <xmmsclient/xmmsclient.h>

static SCM x2_play(SCM);

static SCM
x2_play(SCM connection)
{
    return x2_trivial_server_action(xmmsc_playback_start, connection);
}

static SCM
x2_pause(SCM connection)
{
    return x2_trivial_server_action(xmmsc_playback_pause, connection);
}

void
init_x2_primitive_playback(void)
{
    xg_scm_define_and_export("xmms2:primitive/play", 1, 0, 0, x2_play);
    xg_scm_define_and_export("xmms2:primitive/pause", 1, 0, 0, x2_pause);
}
