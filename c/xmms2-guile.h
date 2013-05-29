/*
 * Copyright (c) 2013 xmms2-guile workers, All rights reserved.
 *
 * Terms for redistribution and use can be found in LICENCE.
 */

/**
 * @file xmms2-guile.h
 * @brief C-level API for xmms2-guile
 *
 * This file should probably be included by all C source code of the project.
 */


#ifndef INC_XMMS2_GUILE_H
#define INC_XMMS2_GUILE_H

#include <libguile.h>
#include <xmmsclient/xmmsclient.h>

/**
 * Structure for smobs, that describe XMMS2 connections. The client name is
 * kept with it, because it is required with `xmmsc_init()', which is required
 * to initialise an `xmmsc_connection_t'.
 */
struct x2_connection {
    /** The client name to announce to the server upon connect */
    SCM clientname;
    /** The actual connection data. */
    xmmsc_connection_t *c;
};

/**
 * Structure for `xmmsv_t' smobs, plus data required to properly do garbage
 * collection.
 */
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
    /**
     * Indicates if a value was created manually be the glue code. If so, it
     * requires to be unreferenced in the garbage collection's free() routine.
     */
    int needs_unref;
};

/**
 * Macro to expand to code commonly required for writing server commands. Note,
 * that for writing trivial server commands, you should use the
 * `#X2_TRIVIAL_SERVER_ACTION()' macro below.
 *
 * @code
 *  static SCM
 *  x2_medialib_get_info(SCM connection, SCM id)
 *  {
 *      X2_SERVER_CMD_HEADER(c, connection, result, retval);
 *      result = xmmsc_medialib_get_info(c->c, scm_to_int64(id));
 *      X2_SERVER_CMD_FOOTER(result, retval);
 *  }
 * @endcode
 *
 * @param   c            Name for the `x2_connection structure.
 * @param   connection   Name for the connection smob `c' is unpacked from.
 * @param   result       Name for the `xmmsc_result_t' container used in the
 *                       body.
 * @param   retval       Name for the SCM value that contains the function's
 *                       return value.
 */
#define X2_SERVER_CMD_HEADER(c, connection, result, retval)     \
    struct x2_connection *c;                                    \
    xmmsc_result_t *result;                                     \
    SCM retval;                                                 \
    c = (struct x2_connection *) SCM_SMOB_DATA(connection)

/**
 * Macro that expands to code commonly required at the very end of functions,
 * that implement server commands. See the `#X2_SERVER_CMD_HEADER()' for
 * example usage.
 *
 * The parameters have to be the same as with the header definition.
 *
 * @param   result       Name for the `xmmsc_result_t' container.
 * @param   retval       Name for the SCM return value.
 */
#define X2_SERVER_CMD_FOOTER(result, retval)    \
    retval = make_x2_result();                  \
    SCM_SET_SMOB_DATA(retval, result);          \
    return retval

/**
 * Macro that expands to code for defining and exporting constants accessible
 * from scheme code.
 *
 * The following defines a scheme variable named `XMMS2-STATUS-PAUSED', that
 * is tied to the C-level symbol `x2co_S_PAUSED' and is set to an integer
 * value, that is defined by the `XMMS_PLAYBACK_STATUS_PAUSE'.
 *
 * @code
 *  X2_SCM_EXPORT_CONSTANT(x2co_S_PAUSED, "XMMS2-STATUS-PAUSED",
 *                         scm_from_int(XMMS_PLAYBACK_STATUS_PAUSE));
 * @endcode
 *
 * @param   c_level      Name of toplevel SCM container.
 * @param   scm_level    Name for the scheme-level variable.
 * @param   value        Expression that returns the desired value.
 */
#define X2_SCM_EXPORT_CONSTANT(c_level, scm_level, value)               \
    do {                                                                \
        c_level = scm_permanent_object(scm_c_define(scm_level, value)); \
        scm_c_export(scm_level);                                        \
    } while (0)

/**
 * Macro that expands to a static function definition, that uses
 * `#x2_trivial_server_action()' to do submit a trivial server command.
 *
 * A trivial server command is a command that only requires a server connection
 * container to work. For example:
 *
 * @code
 *  X2_TRIVIAL_SERVER_ACTION(x2_pause, xmmsc_playback_pause)
 * @endcode
 *
 * Note that there is *NO* trailing semicolon at the end of the macro call.
 *
 * @param   API_FNC      xmms2-guile api function name.
 * @param   XMMS2_FNC    Function from XMMS2's client library that actually
 *                       does the deed.
 */
#define X2_TRIVIAL_SERVER_ACTION(API_FNC, XMMS2_FNC)                    \
    static SCM API_FNC(SCM);                                            \
    static SCM API_FNC(SCM connection)                                  \
    {                                                                   \
        return x2_trivial_server_action(XMMS2_FNC, connection);         \
    }

/*
 * Prototypes for code from `utils.c'.
 */

void xg_scm_define_and_export(const char *, int, int, int, SCM (*)());
SCM x2_trivial_server_action(xmmsc_result_t *(*)(xmmsc_connection_t *), SCM);

/*
 * Constructors for smobs, that the C glue code uses internally as well.
 */

SCM make_x2_result(void);
SCM make_x2_value(void);

/*
 * Prototypes for initialisation functions.
 *
 * `xmms2_guile_ext_init()' calls all of these and is therefore the function,
 * the scheme code should reference with its `load-extension' call.
 */
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
