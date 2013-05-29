/*
 * Copyright (c) 2013 xmms2-guile workers, All rights reserved.
 *
 * Terms for redistribution and use can be found in LICENCE.
 */

/**
 * @file primitive-playlist.c
 * @brief Implementation for primitives that wrap xmmsc_playlist_*() functions
 */

#include "compiler.h"
#include "xmms2-guile.h"

#include <libguile.h>
#include <xmmsclient/xmmsclient.h>

static SCM x2_playlist_entries(SCM, SCM);

X2_TRIVIAL_SERVER_ACTION(x2_active_playlist, xmmsc_playlist_current_active)
X2_TRIVIAL_SERVER_ACTION(x2_get_playlists, xmmsc_playlist_list)

static SCM
x2_playlist_entries(SCM connection, SCM playlist)
{
    X2_SERVER_CMD_HEADER(c, connection, result, retval);
    result = xmmsc_playlist_list_entries(c->c,
                                         scm_to_locale_string(playlist));
    X2_SERVER_CMD_FOOTER(result, retval);
}

void
init_x2_primitive_playlist(void)
{
    xg_scm_define_and_export("xmms2:primitive/active-playlist",
                             1, 0, 0, x2_active_playlist);
    xg_scm_define_and_export("xmms2:primitive/get-playlists",
                             1, 0, 0, x2_get_playlists);
    xg_scm_define_and_export("xmms2:primitive/playlist-entries",
                             2, 0, 0, x2_playlist_entries);
}
