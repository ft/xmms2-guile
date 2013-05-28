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

struct x2_value {
    /** The actual value container */
    xmmsv_t *value;
    /**
     * This is a backreference to the parent `xmmsc_result_t' container (well,
     * its SCM version). This is needed to be able to keep the parent container
     * referenced while the value container is used in the C glue code. When
     * this is not done, it is possible to access parts of memory the garbage
     * collection process already freed again. This would (and did) cause
     * segfaults.
     */
    SCM parent_result;
};

void xg_scm_define_and_export(const char *, int, int, int, SCM (*)());

#define X2_SERVER_CMD_HEADER(c, connection, result, retval)     \
    struct x2_connection *c;                                    \
    xmmsc_result_t *result;                                     \
    SCM retval;                                                 \
    c = (struct x2_connection *) SCM_SMOB_DATA(connection)

#define X2_SERVER_CMD_FOOTER(result, retval)    \
    retval = make_x2_result();                  \
    SCM_SET_SMOB_DATA(retval, result);          \
    return retval

#define X2_SCM_EXPORT_CONSTANT(c_level, scm_level, value)               \
    do {                                                                \
        c_level = scm_permanent_object(scm_c_define(scm_level, value)); \
        scm_c_export(scm_level);                                        \
    } while (0)

#define X2_TRIVIAL_SERVER_ACTION(API_FNC, XMMS2_FNC)                    \
    static SCM API_FNC(SCM);                                            \
    static SCM API_FNC(SCM connection)                                  \
    {                                                                   \
        return x2_trivial_server_action(XMMS2_FNC, connection);         \
    }

SCM x2_trivial_server_action(xmmsc_result_t *(*)(xmmsc_connection_t *), SCM);

SCM make_x2_result(void);
SCM make_x2_value(void);

void init_x2_primitive_config(void);
void init_x2_primitive_connect(void);
void init_x2_primitive_medialib(void);
void init_x2_primitive_playback(void);
void init_x2_primitive_playlist(void);
void init_x2_primitive_synchronous(void);
void init_x2_primitive_value(void);
void init_x2_type_connection(void);
void init_x2_type_result(void);
void init_x2_type_value(void);
void xmms2_guile_ext_init(void);

#endif /* INC_XMMS2_GUILE_H */
