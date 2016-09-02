;; Copyright (c) 2015 xmms2-guile workers, All rights reserved.
;;
;; Terms for redistribution and use can be found in LICENCE.

(define-module (xmms2 constants)
  #:use-module (rnrs bytevectors)
  #:use-module (xmms2 data-conversion))

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

;; ‘define-enum’ helps with defining a lot of variables for which the value is
;; a simple enumeration. The simplest case is:
;;
;;   (define-enum foo bar)
;;
;; Where ‘foo’ gets defined to 0 and ‘bar’ gets defined to 1. Whereas in:
;;
;;   (define-enum foo (bar 23) baz)
;;
;; ‘foo’ is still 0, ‘bar’ is 23 and ‘baz’ is 24. Using non-literal integer
;; values in offset fields is supported. Finally, you may define the form that
;; is used to define the variables that are enumerated:
;;
;;   (define-enum (=> define)
;;     (foo 12)
;;     bar
;;     baz)
;;
;; The default “definer” is ‘define-public’.
(define-syntax define-enum
  (lambda (x)
    (define (delayed-value kw increment syn)
      ;; This produces a piece of code, that looks like this:
      ;;
      ;;   (+ increment syn)
      ;;
      ;; where ‘increment’ is an integer and ‘syn’ is something potentially
      ;; complex. The procedure is called ‘delayed-value’ because at this point
      ;; we cannot compute the enumeration value while expanding the macro.
      ;; Instead we return code, that will yield the correct value when it is
      ;; evaluated at a later time.
      (datum->syntax kw (list #'+ increment syn)))

    (define (enum-new-offset kw offset)
      ;; If we're handed a new offset, this produces a version of that new
      ;; offset incremented by one.
      (let ((raw (syntax->datum offset)))
        (cond ((integer? raw) (+ 1 raw))
              (else (delayed-value kw 1 offset)))))

    (define (enum-increment kw iter)
      ;; If we're *not* handed a new offset, this produces a version of the old
      ;; iteration incremented by one. The old itertion is either an integer or
      ;; a piece of code that looks like this:
      ;;
      ;;   (+ increment syn)
      ;;
      ;; In which case the ‘cadr’ is the old increment, that needs to be
      ;; incremented yet again. The ‘caddr’ on the other hand is the piece of
      ;; code (that should upon evaluation yield an integer) that the
      ;; previously computed increment needs to be applied to.
      (cond ((integer? iter) (+ 1 iter))
            (else (let ((raw (syntax->datum iter)))
                    (delayed-value kw (+ 1 (cadr raw)) (caddr raw))))))

    (define (compute-current kw syn)
      ;; When we get handed a new offset in our enumeration, this procedure
      ;; computes the correct value, depending on whether we're looking at a
      ;; raw integer or something (potentially) complex.
      (let ((raw (syntax->datum syn)))
        (cond ((integer? raw) raw)
              (else (delayed-value kw 0 syn)))))

    (define (process-entry kw cur)
      ;; Process each entry that ‘process-enum’ throws at us.
      (syntax-case cur ()
        ((name value) (list #'name (compute-current kw #'value)
                            (lambda (x) (enum-new-offset kw #'value))))
        (name (list #'name #f (lambda (x) (enum-increment kw x))))))

    (define (produce-enum kw lst)
      ;; Iterate across ‘lst’ and keep records of the current iteration value
      ;; as well as the code we wish to return.
      (let loop ((rest lst) (iter 0) (acc '()))
        (if (null? rest)
            ;; Since we're consing the result recursively, ‘acc’ is in reverse
            ;; order so put it back i the correct order upon exit.
            (reverse acc)
            (with-syntax (((name cur-iter new-iter)
                           (process-entry kw (car rest))))
              ;; ‘name’ is obvious. ‘cur-iter’ is a syntax-object, if the
              ;; loop's ‘iter’ value needs to be discarded because we got
              ;; handed a new one; or #f if the old value is still good.
              ;; ‘new-iter’ is a procedure, that takes one argument (namely the
              ;; value of ‘iter’ or (if it's not #f) ‘cur-iter’ and produces
              ;; the correct ‘iter’ parameter for the next ‘loop’ call.
              (let ((cur-iter (or #'cur-iter iter))
                    (prc (syntax->datum #'new-iter)))
                (loop (cdr rest) (prc cur-iter)
                      (cons (cons #'name cur-iter) acc)))))))

    ;; Main entry point:
    (syntax-case x (=>)
      ((kw (=> definer) item ...)
       (with-syntax ((((variable . value) ...)
                      (produce-enum #'kw #'(item ...))))
         #'(begin (definer variable value) ...)))
      ((_ item ...)
       #'(define-enum (=> define-public) item ...)))))

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

(define-enum
  OBJECT-SIGNAL
  OBJECT-MAIN
  OBJECT-PLAYLIST
  OBJECT-CONFIG
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

(define-enum
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
(define-enum
  CMD-REPLY
  CMD-ERROR)

(define-enum
  (CMD-SIGNAL FIRST-CMD-ID)
  CMD-BROADCAST)

(define-enum
  (CMD-HELLO FIRST-CMD-ID)
  CMD-QUIT
  CMD-PLUGIN-LIST
  CMD-STATS)

(define-enum
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

(define-enum
  (CMD-GET-VALUE FIRST-CMD-ID)
  CMD-SET-VALUE
  CMD-REGISTER-VALUE
  CMD-LIST-VALUES)

(define-enum
  (CMD-START FIRST-CMD-ID)
  CMD-STOP
  CMD-PAUSE
  CMD-DECODER-KILL
  CMD-CURRENT-PLAYTIME
  CMD-SEEK-MS
  CMD-SEEK-SAMPLES
  CMD-PLAYBACK-STATUS
  CMD-CURRENT-ID
  CMD-VOLUME-SET
  CMD-VOLUME-GET)

(define-enum
  (CMD-PATH-IMPORT FIRST-CMD-ID)
  CMD-REHASH
  CMD-GET-ID
  CMD-REMOVE-ID
  CMD-PROPERTY-SET-STRING
  CMD-PROPERTY-SET-INTEGER
  CMD-PROPERTY-REMOVE
  CMD-MOVE-URL
  CMD-MEDIALIB-ADD-URL)

(define-enum
  (CMD-COLLECTION-SYNC FIRST-CMD-ID))

(define-enum
  (CMD-COLLECTION-GET FIRST-CMD-ID)
  CMD-COLLECTION-LIST
  CMD-COLLECTION-SAVE
  CMD-COLLECTION-REMOVE
  CMD-COLLECTION-FIND
  CMD-COLLECTION-RENAME
  CMD-QUERY
  CMD-QUERY-INFOS
  CMD-IDLIST-FROM-PLS)

(define-enum
  (CMD-GET-DATA FIRST-CMD-ID)
  CMD-ADD-DATA
  CMD-REMOVE-DATA
  CMD-LIST-DATA)

(define-enum
  (CMD-VISUALIZATION-QUERY-VERSION FIRST-CMD-ID)
  CMD-VISUALIZATION-REGISTER
  CMD-VISUALIZATION-INIT-SHM
  CMD-VISUALIZATION-INIT-UDP
  CMD-VISUALIZATION-PROPERTY
  CMD-VISUALIZATION-PROPERTIES
  CMD-VISUALIZATION-SHUTDOWN)

(define-enum
  (CMD-BROWSE FIRST-CMD-ID))

(define-enum
  (CMD-SEND-MESSAGE FIRST-CMD-ID)
  CMD-REPLY-MESSAGE
  CMD-GET-CONNECTED-CLIENTS)

;; These are deprecated:
;;
;;  PLAYLIST-CHANGED-SHUFFLE
;;  PLAYLIST-CHANGED-CLEAR
;;  PLAYLIST-CHANGED-SORT
(define-enum
  PLAYLIST-CHANGED-ADD
  PLAYLIST-CHANGED-INSERT
  PLAYLIST-CHANGED-SHUFFLE
  PLAYLIST-CHANGED-REMOVE
  PLAYLIST-CHANGED-CLEAR
  PLAYLIST-CHANGED-MOVE
  PLAYLIST-CHANGED-SORT
  PLAYLIST-CHANGED-UPDATE
  PLAYLIST-CHANGED-REPLACE)

(define-enum
  COLLECTION-CHANGED-ADD
  COLLECTION-CHANGED-UPDATE
  COLLECTION-CHANGED-RENAME
  COLLECTION-CHANGED-REMOVE)

(define-enum
  PLAYLIST-CURRENT-ID-FORGET
  PLAYLIST-CURRENT-ID-KEEP
  PLAYLIST-CURRENT-ID-MOVE-TO-FRONT)

(define-enum
  PLAYBACK-STATUS-STOP
  PLAYBACK-STATUS-PLAY
  PLAYBACK-STATUS-PAUSE)

;; Yes, this one starts at one. I don't know why.
(define-enum
  (PLAYBACK-SEEK-CURRENT 1)
  PLAYBACK-SEEK-SET)

(define-enum
  MEDIAINFO-READER-IDLE
  MEDIAINFO-READER-RUNNING)

(define-enum
  PLUGIN-TYPE-ALL
  PLUGIN-TYPE-OUTPUT
  PLUGIN-TYPE-XFORM)

(define-enum
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

(define-enum
  MEDIALIB-ENTRY-NEW
  MEDIALIB-ENTRY-OK
  MEDIALIB-ENTRY-RESOLVING
  MEDIALIB-ENTRY-NOT-AVAILABLE
  MEDIALIB-ENTRY-REHASH)

(define-enum
  LOG-LEVEL-UNKNOWN
  LOG-LEVEL-FATAL
  LOG-LEVEL-FAIL
  LOG-LEVEL-ERROR
  LOG-LEVEL-INFO
  LOG-LEVEL-DEBUG
  ;; This has to be the last log level value:
  LOG-LEVEL-COUNT)

(define-enum
  C2C-POLICY-NO-REPLY
  C2C-POLICY-SINGLE-REPLY
  C2C-POLICY-MULTI-REPLY)
