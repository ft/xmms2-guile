;; Copyright (c) 2015 xmms2-guile workers, All rights reserved.
;;
;; Terms for redistribution and use can be found in LICENCE.

(define-module (xmms2 constants)
  #:use-module (rnrs bytevectors)
  #:use-module (xmms2 data-conversion)
  #:use-module (xmms2 enumeration))

;; Some utilities:
(define (int32-bv value)
  (let ((rv (make-bytevector 4 0)))
    (uint32-set! rv 0 value)
    rv))

;; ‘define-type-constant’ defines two variables given a name FOO and
;; a value 1234:
;;
;; (define-public TYPE-FOO 1234)
;; (define-public TAG-FOO #vu(0 0 4 210))
(define-syntax define-type-constant
  (lambda (x)
    (define (prefixed-name kw prefix name)
      (datum->syntax kw (symbol-append prefix
                                   (syntax->datum name))))
    (syntax-case x ()
      ((kw name value)
       (with-syntax ((type-id-name (prefixed-name #'kw 'TYPE- #'name))
                     (tag-name (prefixed-name #'kw 'TAG- #'name)))
         #'(begin (define-public type-id-name value)
                  (define-public tag-name (int32-bv type-id-name))))))))

(define-syntax value-name-pair
  (syntax-rules ()
    ((_ value) (cons value (quote value)))))

;; Constant definitions below:

(define-public PROTOCOL-VERSION 23)
(define-public XMMS2-HEADER-SIZE 16)
(define-public XMMS2-DEFAULT-PORT 9667)
(define-public COLLECTION_NAMESPACE_ALL "*")
(define-public COLLECTION_NS_COLLECTIONS "Collections")
(define-public COLLECTION_NS_PLAYLISTS "Playlists")
(define-public NAME_OF_ACTIVE_PLAYLIST "_active")

(define-enum (=> define-type-constant)
  NONE
  ERROR
  INT64
  STRING
  COLLECTION
  BINARY
  LIST
  DICTIONARY
  BITBUFFER
  FLOAT)

(define-public xref-types
  (list (value-name-pair TYPE-NONE)
        (value-name-pair TYPE-ERROR)
        (value-name-pair TYPE-INT64)
        (value-name-pair TYPE-STRING)
        (value-name-pair TYPE-COLLECTION)
        (value-name-pair TYPE-BINARY)
        (value-name-pair TYPE-LIST)
        (value-name-pair TYPE-DICTIONARY)
        (value-name-pair TYPE-BITBUFFER)
        (value-name-pair TYPE-FLOAT)))

(define-enum (<> xref-objects)
  OBJECT-SIGNAL
  OBJECT-MAIN
  OBJECT-PLAYLIST
  OBJECT-CONFIGURATION
  OBJECT-PLAYBACK
  OBJECT-MEDIALIB
  OBJECT-COLLECTION
  OBJECT-VISUALIZATION
  OBJECT-MEDIAINFO-READER
  OBJECT-XFORM
  OBJECT-BINDATA
  OBJECT-COLLECTION-SYNC
  OBJECT-COURIER
  OBJECT-IPC-MANAGER)

(define-enum (<> xref-signals)
  SIGNAL-PLAYLIST-CHANGED
  SIGNAL-CONFIGVALUE-CHANGED
  SIGNAL-PLAYBACK-STATUS
  SIGNAL-PLAYBACK-VOLUME-CHANGED
  SIGNAL-PLAYBACK-PLAYTIME
  SIGNAL-PLAYBACK-CURRENTID
  SIGNAL-PLAYLIST-CURRENT-POS
  SIGNAL-PLAYLIST-LOADED
  SIGNAL-MEDIALIB-ENTRY-ADDED
  SIGNAL-MEDIALIB-ENTRY-UPDATE
  SIGNAL-MEDIALIB-ENTRY-REMOVED
  SIGNAL-COLLECTION-CHANGED
  SIGNAL-QUIT
  SIGNAL-MEDIAINFO-READER-STATUS
  SIGNAL-MEDIAINFO-READER-UNINDEXED
  SIGNAL-COURIER-MESSAGE
  SIGNAL-IPC-MANAGER-CLIENT-CONNECTED
  SIGNAL-IPC-MANAGER-CLIENT-DISCONNECTED)

;; There are FIRST-CMD-ID command identifiers, that are reserved for special
;; use. For now only two are used.
(define-public FIRST-CMD-ID #x20)

(define-enum (<> xref-special-cmds)
  CMD-REPLY
  CMD-ERROR)

(define-enum (<> xref-server-messages)
  ;; These are messages initiated by the server, rather than being replies to a
  ;; command.
  (CMD-SIGNAL FIRST-CMD-ID)
  CMD-BROADCAST)

(define-enum (<> xref-main-cmds)
  (CMD-HELLO FIRST-CMD-ID)
  CMD-QUIT
  CMD-PLUGIN-LIST
  CMD-STATS)

(define-enum (<> xref-playlist-cmds)
  (CMD-REPLACE FIRST-CMD-ID)
  CMD-SET-POSITION
  CMD-SET-POSITION-RELATIVE
  CMD-ADD-URL
  CMD-ADD-COLLECTION
  CMD-REMOVE-ENTRY
  CMD-MOVE-ENTRY
  CMD-LIST
  CMD-CURRENT-POSITION
  CMD-CURRENT-ACTIVE
  CMD-INSERT-URL
  CMD-INSERT-COLLECTION
  CMD-LOAD
  CMD-RECURSIVE-ADD
  CMD-RECURSIVE-INSERT)

(define-enum (<> xref-config-cmds)
  (CMD-GET-VALUE FIRST-CMD-ID)
  CMD-SET-VALUE
  CMD-REGISTER-VALUE
  CMD-LIST-VALUES)

(define-enum (<> xref-playback-cmds)
  (CMD-START FIRST-CMD-ID)
  CMD-STOP
  CMD-PAUSE
  CMD-KILL-DECODER
  CMD-CURRENT-PLAYTIME
  CMD-SEEK-MS
  CMD-SEEK-SAMPLES
  CMD-PLAYBACK-STATUS
  CMD-CURRENT-ID
  CMD-VOLUME-SET
  CMD-VOLUME-GET)

(define-enum (<> xref-medialib-cmds)
  (CMD-PATH-IMPORT FIRST-CMD-ID)
  CMD-REHASH
  CMD-GET-ID
  CMD-REMOVE-ID
  CMD-PROPERTY-SET-STRING
  CMD-PROPERTY-SET-INTEGER
  CMD-PROPERTY-REMOVE
  CMD-MOVE-URL
  CMD-MEDIALIB-ADD-URL)

(define-enum (<> xref-collection-sync-cmds)
  (CMD-COLLECTION-SYNC FIRST-CMD-ID))

(define-enum (<> xref-collection-cmds)
  (CMD-COLLECTION-GET FIRST-CMD-ID)
  CMD-COLLECTION-LIST
  CMD-COLLECTION-SAVE
  CMD-COLLECTION-REMOVE
  CMD-COLLECTION-FIND
  CMD-COLLECTION-RENAME
  CMD-QUERY
  CMD-QUERY-INFOS
  CMD-IDLIST-FROM-PLS)

(define-enum (<> xref-binary-data-cmds)
  (CMD-GET-DATA FIRST-CMD-ID)
  CMD-ADD-DATA
  CMD-REMOVE-DATA
  CMD-LIST-DATA)

(define-enum (<> xref-visualisation-cmds)
  (CMD-VISUALIZATION-QUERY-VERSION FIRST-CMD-ID)
  CMD-VISUALIZATION-REGISTER
  CMD-VISUALIZATION-INIT-SHM
  CMD-VISUALIZATION-INIT-UDP
  CMD-VISUALIZATION-PROPERTY
  CMD-VISUALIZATION-PROPERTIES
  CMD-VISUALIZATION-SHUTDOWN)

(define-enum (<> xref-xform-cmds)
  (CMD-BROWSE FIRST-CMD-ID))

(define-enum (<> xref-courier-cmds)
  (CMD-SEND-MESSAGE FIRST-CMD-ID)
  CMD-REPLY-MESSAGE
  CMD-GET-CONNECTED-CLIENTS)

;; These are deprecated:
;;
;;  PLAYLIST-CHANGED-SHUFFLE
;;  PLAYLIST-CHANGED-CLEAR
;;  PLAYLIST-CHANGED-SORT
(define-enum (<> xref-playlist-signals)
  PLAYLIST-CHANGED-ADD
  PLAYLIST-CHANGED-INSERT
  PLAYLIST-CHANGED-SHUFFLE
  PLAYLIST-CHANGED-REMOVE
  PLAYLIST-CHANGED-CLEAR
  PLAYLIST-CHANGED-MOVE
  PLAYLIST-CHANGED-SORT
  PLAYLIST-CHANGED-UPDATE
  PLAYLIST-CHANGED-REPLACE)

(define-enum (<> xref-collection-signals)
  COLLECTION-CHANGED-ADD
  COLLECTION-CHANGED-UPDATE
  COLLECTION-CHANGED-RENAME
  COLLECTION-CHANGED-REMOVE)

(define-enum (<> xref-playlist-position-codes)
  PLAYLIST-CURRENT-ID-FORGET
  PLAYLIST-CURRENT-ID-KEEP
  PLAYLIST-CURRENT-ID-MOVE-TO-FRONT)

(define-enum (<> xref-playback-states)
  PLAYBACK-STATUS-STOP
  PLAYBACK-STATUS-PLAY
  PLAYBACK-STATUS-PAUSE)

;; Yes, this one starts at one. I don't know why.
(define-enum (<> xref-playback-seek-modes)
  (PLAYBACK-SEEK-CURRENT 1)
  PLAYBACK-SEEK-SET)

(define-enum (<> xref-mediainfo-reader-states)
  MEDIAINFO-READER-IDLE
  MEDIAINFO-READER-RUNNING)

(define-enum (<> xref-plugin-types)
  PLUGIN-TYPE-ALL
  PLUGIN-TYPE-OUTPUT
  PLUGIN-TYPE-XFORM)

(define-enum (<> xref-collection-types)
  COLLECTION-TYPE-REFERENCE
  COLLECTION-TYPE-UNIVERSE
  COLLECTION-TYPE-UNION
  COLLECTION-TYPE-INTERSECTION
  COLLECTION-TYPE-COMPLEMENT
  COLLECTION-TYPE-HAS
  COLLECTION-TYPE-MATCH
  COLLECTION-TYPE-TOKEN
  COLLECTION-TYPE-EQUAL
  COLLECTION-TYPE-NOT-EQUAL
  COLLECTION-TYPE-SMALLER
  COLLECTION-TYPE-SMALLER-OR-EQUAL
  COLLECTION-TYPE-GREATER
  COLLECTION-TYPE-GREATER-OR-EQUAl
  COLLECTION-TYPE-ORDER
  COLLECTION-TYPE-LIMIT
  COLLECTION-TYPE-MEDIASET
  COLLECTION-TYPE-IDLIST)

(define-enum (<> xref-medialib-entry-codes)
  MEDIALIB-ENTRY-NEW
  MEDIALIB-ENTRY-OK
  MEDIALIB-ENTRY-RESOLVING
  MEDIALIB-ENTRY-NOT-AVAILABLE
  MEDIALIB-ENTRY-REHASH)

(define-enum (<> xref-log-levels)
  LOG-LEVEL-UNKNOWN
  LOG-LEVEL-FATAL
  LOG-LEVEL-FAIL
  LOG-LEVEL-ERROR
  LOG-LEVEL-INFO
  LOG-LEVEL-DEBUG
  ;; This has to be the last log level value:
  LOG-LEVEL-COUNT)

(define-enum (<> xref-client2client-codes)
  C2C-POLICY-NO-REPLY
  C2C-POLICY-SINGLE-REPLY
  C2C-POLICY-MULTI-REPLY)
