;; Copyright (c) 2017 xmms2-guile workers, All rights reserved.
;;
;; Terms for redistribution and use can be found in doc/LICENCE.

(define-module (genipc utilities)
  #:use-module (ice-9 format)
  #:use-module (ice-9 pretty-print)
  #:use-module (ice-9 regex)
  #:use-module (srfi srfi-1)
  #:export (activate-file-generation!
            file-generation-active?
            adjust-name
            adjust-name/constant
            adjust-name/object
            adjust-name/method
            adjust-name/b-or-s
            adjust-name/enum
            adjust-name/member
            adjust-name/namespace-hint
            adjust-name/arg
            bend-output
            notify
            module->file-name
            module->object
            name->identifier
            name->constants
            directory-exists?
            symbol-prefix?
            symbol-suffix?
            symbol-upcase
            cat
            pp
            am
            assq-ref*
            cleanup-documentation))

(define create-files? #f)
(define (activate-file-generation!)
  (set! create-files? #t))
(define (file-generation-active?)
  create-files?)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Output control: Depends on ‘create-files?’. The ‘notify’ helper can be used
;; like ‘format’ without the first argument to produce output indenpendently of
;; the value of said parameter.

(define (bend-output file thunk)
  (if create-files?
      (begin
        (format #t "Generating ~a...~%" file)
        (with-output-to-file file thunk))
      (thunk)))

(define (notify . args)
  (apply format (cons (current-error-port) args)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; File system related helpers:

(define (module->file-name module)
  (cat (string-join (cons "scheme" (map symbol->string module)) "/") ".scm"))

(define (directory-exists? name)
  "Return #t if a directory named NAME exist."
  (let ((data (stat name #f)))
    (if (not data)
        #f
        (eq? (stat:type data) 'directory))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; IPC namespace helpers:

(define (module->object module)
  (string->symbol (cat "OBJECT-"
                       (string-upcase (symbol->string (last module))))))

(define (name->identifier name)
  (string->symbol (cat "CMD-" (string-upcase (symbol->string name)))))

(define (name->constants name)
  (cat "scheme/xmms2/constants/" (symbol->string name) ".scm"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; API name adjustments:

(define (adjust-name name-map name)
  (define (maybe-replace name)
    (let ((replacement (assq-ref name-map name)))
      (or replacement name)))
  (maybe-replace (string->symbol (regexp-substitute/global
                                  #f "_" name 'pre "-" 'post))))

(define (adjust-name/constant name)
  (define name-map '((IPC-COMMAND-FIRST . FIRST-COMMAND-ID)))
  (adjust-name name-map name))

(define (adjust-name/object name)
  (define name-map '((bindata . binary-data)
                     (coll-sync . collection-sync)
                     (config . configuration)
                     (mediainfo-reader . media-info-reader)
                     (medialib . media-library)))
  (adjust-name name-map name))

(define (adjust-name/method name)
  (define name-map '((current-active . get-currently-active)
                     (current-id . get-current-identifier)
                     (current-pos . get-position)
                     (get-id . get-identifier)
                     (get-info . get-information)
                     (idlist-from-playlist . identifier-list-from-playlist)
                     (move-entry . update-entry-path)
                     (playtime . get-playtime)
                     (radd . add-url/recursive)
                     (replace . replace-entry)
                     (reply . send-reply)
                     (rinsert . insert-url/recursive)
                     (seek-ms . seek/milliseconds)
                     (seek-samples . seek/samples)
                     (set-next-rel . set-next/relative)
                     (set-property-int . set-property-integer)
                     (stats . statistics)
                     (status . get-status)
                     (tickle . kill-decoder)
                     (volume-get . get-volume)
                     (volume-set . set-volume)))
  (adjust-name name-map name))

(define (adjust-name/b-or-s name)
  (define name-map '((current-id . identifier-changed)
                     (current-pos . position-changed)
                     (playlist-changed . changed)
                     (playtime . current-playtime)
                     (status . status-changed)
                     (unindexed . unresolved-entries)))
  (adjust-name name-map name))

(define (adjust-name/enum name)
  (define name-map '((current-id . identifier-changed)
                     (current-pos . position-changed)))
  (adjust-name name-map name))

(define (adjust-name/member name)
  (define name-map '((current-id . identifier-changed)
                     (current-pos . position-changed)))
  (adjust-name name-map name))

(define (adjust-name/namespace-hint name)
  (map string->symbol (map string-downcase (string-split name #\_))))

(define (adjust-name/arg name)
  (define name-map '((client . client-name)
                     (offset-milliseconds . offset/ms)))
  (adjust-name name-map name))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Documentation processing:

(define (cleanup-documentation string)
  (filter (lambda (x)
            (not (string=? x "")))
          (map string-trim (string-split string #\newline))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Miscellaneous utilities:

(define (symbol-prefix? prefix sym)
  (string-prefix? (symbol->string prefix)
                  (symbol->string sym)))

(define (symbol-suffix? suffix sym)
  (string-suffix? (symbol->string suffix)
                  (symbol->string sym)))

(define (symbol-upcase sym)
  (string->symbol (string-upcase (symbol->string sym))))

(define (cat . lst)
  (string-concatenate lst))

(define (pp forms)
  (pretty-print forms
                (current-output-port)
                #:display? #f
                #:width 79
                #:max-expr-width 60))

;; Like append-map, but makes it possible to emit debugging messages.
(define (am f clist1 . rest)
  ;;(notify "clist1: ~a, rest: ~a~%" clist1 rest)
  (let ((rc (apply map f clist1 rest)))
    ;;(notify "rc: ~a~%" rc)
    (concatenate rc)))

(define (assq-ref* alist key)
  (car (assq-ref alist key)))
