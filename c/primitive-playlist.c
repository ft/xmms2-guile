/*
 * Copyright (c) 2013 xmms2-guile workers, All rights reserved.
 *
 * Terms for redistribution and use can be found in LICENCE.
 */

#include "compiler.h"
#include "xmms2-guile.h"

#include <libguile.h>
#include <xmmsclient/xmmsclient.h>

X2_TRIVIAL_SERVER_ACTION(x2_active_playlist, xmmsc_playlist_current_active)
X2_TRIVIAL_SERVER_ACTION(x2_get_playlists, xmmsc_playlist_list)

void
init_x2_primitive_playlist(void)
{
    xg_scm_define_and_export("xmms2:primitive/active-playlist",
                             1, 0, 0, x2_active_playlist);
    xg_scm_define_and_export("xmms2:primitive/get-playlists",
                             1, 0, 0, x2_get_playlists);
}
