/*
 * Copyright (c) 2013 xmms2-guile workers, All rights reserved.
 *
 * Terms for redistribution and use can be found in LICENCE.
 */

#include "compiler.h"
#include "xmms2-guile.h"

#include <libguile.h>
#include <xmmsclient/xmmsclient.h>

X2_TRIVIAL_SERVER_ACTION(x2_pause, xmmsc_playback_pause)
X2_TRIVIAL_SERVER_ACTION(x2_play, xmmsc_playback_start)

void
init_x2_primitive_playback(void)
{
    xg_scm_define_and_export("xmms2:primitive/pause", 1, 0, 0, x2_pause);
    xg_scm_define_and_export("xmms2:primitive/play", 1, 0, 0, x2_play);
}
