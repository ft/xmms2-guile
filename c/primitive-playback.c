/*
 * Copyright (c) 2013 xmms2-guile workers, All rights reserved.
 *
 * Terms for redistribution and use can be found in LICENCE.
 */

/**
 * @file primitive-playback.c
 * @brief Implementation for primitives that wrap xmmsc_playback_*() functions
 */

#include "compiler.h"
#include "xmms2-guile.h"

#include <libguile.h>
#include <xmmsclient/xmmsclient.h>

X2_TRIVIAL_SERVER_ACTION(x2_current_id, xmmsc_playback_current_id)
X2_TRIVIAL_SERVER_ACTION(x2_pause, xmmsc_playback_pause)
X2_TRIVIAL_SERVER_ACTION(x2_play, xmmsc_playback_start)
X2_TRIVIAL_SERVER_ACTION(x2_playtime, xmmsc_playback_playtime)
X2_TRIVIAL_SERVER_ACTION(x2_status, xmmsc_playback_status)
X2_TRIVIAL_SERVER_ACTION(x2_stop, xmmsc_playback_stop)
X2_TRIVIAL_SERVER_ACTION(x2_tickle, xmmsc_playback_tickle)
X2_TRIVIAL_SERVER_ACTION(x2_volume_get, xmmsc_playback_volume_get)

/** Sub-Initialisation routine */
void
init_x2_primitive_playback(void)
{
    xg_scm_define_and_export("xmms2:primitive/current-id",
                             1, 0, 0, x2_current_id);
    xg_scm_define_and_export("xmms2:primitive/pause", 1, 0, 0, x2_pause);
    xg_scm_define_and_export("xmms2:primitive/play", 1, 0, 0, x2_play);
    xg_scm_define_and_export("xmms2:primitive/playtime", 1, 0, 0, x2_playtime);
    xg_scm_define_and_export("xmms2:primitive/status", 1, 0, 0, x2_status);
    xg_scm_define_and_export("xmms2:primitive/stop", 1, 0, 0, x2_stop);
    xg_scm_define_and_export("xmms2:primitive/tickle", 1, 0, 0, x2_tickle);
    xg_scm_define_and_export("xmms2:primitive/volume-get",
                             1, 0, 0, x2_volume_get);
}
