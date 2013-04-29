#ifndef INC_XMMS2_GUILE_H
#define INC_XMMS2_GUILE_H

#include <libguile.h>
#include <xmmsclient/xmmsclient.h>

struct x2_connection {
    /** The client name to announce to the server upon connect */
    SCM clientname;
    /** The actual connection data. */
    xmmsc_connection_t *c;
};

void xg_scm_define_and_export(const char *, int, int, int, SCM (*)());

SCM make_x2_result(void);

void init_x2_primitive_connect(void);
void init_x2_primitive_playback(void);
void init_x2_primitive_synchronous(void);
void init_x2_type_connection(void);
void init_x2_type_result(void);
void init_x2_type_value(void);
void xmms2_guile_ext_init(void);

#endif /* INC_XMMS2_GUILE_H */
