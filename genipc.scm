;; -*- scheme -*-

(use-modules (ice-9 match)
             (ice-9 pretty-print)
             (ice-9 regex)
             (srfi srfi-1)
             (srfi srfi-19)
             (sxml match)
             (sxml simple))

(define (file-exists? name)
  "Return #t if a file (of any kind) named NAME exist."
  (access? name F_OK))

(define (notify . args)
  (apply format (cons (current-error-port) args)))

(define *source-file* (cadr (command-line)))

(unless (file-exists? *source-file*)
  (notify "Source XML file does not exist: ~a~%" *source-file*)
  (quit 1))

(define (pp forms)
  (pretty-print forms
                (current-output-port)
                #:display? #f
                #:width 79
                #:max-expr-width 60))

(define *source-xml*
  (with-input-from-file *source-file*
    (lambda ()
      (xml->sxml (current-input-port)
                 #:trim-whitespace? #t
                 #:namespaces '((xmms: . "https://xmms2.org/ipc.xsd"))))))

;; Like append-map, but makes it possible to emit debugging messages.
(define (am f clist1 . rest)
  ;;(notify "clist1: ~a, rest: ~a~%" clist1 rest)
  (let ((rc (apply map f clist1 rest)))
    ;;(notify "rc: ~a~%" rc)
    (concatenate rc)))

(define (cleanup-documentation string)
  (filter (lambda (x)
            (not (string=? x "")))
          (map string-trim (string-split string #\newline))))

(define (adjust-name name-map name)
  (define (maybe-replace name)
    (let ((replacement (assq-ref name-map name)))
      (or replacement name)))
  (maybe-replace (string->symbol (regexp-substitute/global
                                  #f "_" name 'pre "-" 'post))))

(define (adjust-name/object name)
  (define name-map '((bindata . binary-data)
                     (coll-sync . collection-sync)
                     (config . configuration)
                     (medialib . media-library)))
  (adjust-name name-map name))

(define (adjust-name/method name)
  (define name-map '((current-active . get-currently-active)
                     (current-id . get-current-identifier)
                     (current-pos . get-position)
                     (get-id . get-identifier)
                     (idlist-from-playlist . identifier-list-from-playlist)
                     (move-entry . update-entry-path)
                     (playtime . get-playtime)
                     (radd . add-url/recursive)
                     (replace . replace-entry)
                     (rinsert . insert-url/recursive)
                     (seek-ms . seek/milliseconds)
                     (seek-samples . seek/samples)
                     (send-message . send)
                     (set-next-rel . set-next/relative)
                     (set-property-int . set-property-integer)
                     (stats . statistics)
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

(define (adjust-name/arg name)
  (define name-map '((client . client-name)
                     (offset-milliseconds . offset/ms)))
  (adjust-name name-map name))

(define (adjust-type type)
  ;;(notify "type: ~a~%" type)
  (sxml-match type
    ((xmms::binary) '(binary))
    ((xmms::collection) '(collection))
    ((xmms::int) '(integer))
    ((xmms::string) '(string))
    ((xmms::unknown) '(unknown))
    ((xmms::list ,rest ...) `((list ,@(am adjust-type rest))))
    ((xmms::dictionary ,rest ...) `((dictionary ,@(am adjust-type rest))))))

(define (method-arg->sexp arg)
  ;;(notify "arg: ~a~%" arg)
  (sxml-match arg
    ((xmms::name ,name) `((name ,(adjust-name/arg name))))
    ((xmms::type ,type) `((type ,@(adjust-type type))))
    ((xmms::documentation ,doc) `((documentation ,(cleanup-documentation doc))))))

(define (return-value->sexp return-value)
  ;;(notify "return-value: ~a~%" return-value)
  (sxml-match return-value
    ((xmms::type ,type) `((type ,@(adjust-type type))))
    ((xmms::documentation ,doc) `((documentation ,(cleanup-documentation doc))))))

(define (method->sexp method)
  ;;(notify "method: ~a~%" method)
  (sxml-match method
    ((xmms::name ,name) `((name ,(adjust-name/method name))))
    ((xmms::documentation ,doc) `((documentation ,(cleanup-documentation doc))))
    ((xmms::argument ,rest ...) `((argument ,@(am method-arg->sexp rest))))
    ((xmms::return_value ,rest ...) `((return-value
                                       ,@(am return-value->sexp rest))))))

(define (broadcast-or-signal->sexp bs)
  ;;(notify "bs: ~a~%" bs)
  (sxml-match bs
    ((xmms::id ,id) `((identifier ,(string->number id))))
    ((xmms::name ,name) `((name ,(adjust-name/b-or-s name))))
    ((xmms::documentation ,doc) `((documentation ,(cleanup-documentation doc))))
    ((xmms::return_value ,rest ...) `((return-value
                                       ,@(am return-value->sexp rest))))))

(define (sxml->sexp tree)
  ;;(notify "tree: ~a~%" tree)
  (sxml-match tree
    ((*TOP* (*PI* ,stuff ...) ,things ...)
     `(xmms2-ipc-description ,@(am sxml->sexp things)))
    ((xmms::ipc (@ (version ,v)) ,objs ...) `((version ,v) ,@(am sxml->sexp objs)))
    ((xmms::object (xmms::name ,name) ,things ...)
     `((object (name ,(adjust-name/object name)) ,@(am sxml->sexp things))))
    ((xmms::method ,rest ...) `((method ,@(am method->sexp rest))))
    ((xmms::broadcast ,rest ...) `((broadcast ,@(am broadcast-or-signal->sexp rest))))
    ((xmms::signal ,rest ...) `((signal ,@(am broadcast-or-signal->sexp rest))))))

;;(pretty-print *source-xml*)
(define *sexp-stage-1* (sxml->sexp *source-xml*))
;;(pretty-print *sexp-stage-1*)

;; By now, the XML document is converted to an s-expression tree, that looks
;; like this:
;;
;;  (xmms2-ipc-description TOP-LEVEL-ENTITIES ...)
;;
;; Where TOP-LEVEL-ENTITIES are one of:
;;
;;   - (version <STRING>)
;;   - (object ...)
;;
;; “object” describes one of XMMS2's IPC objects. It may contain the following
;; forms:
;;
;;   - (name <SYMBOL>)
;;   - (method ...)
;;   - (broadcast ...)
;;   - (signal ...)
;;
;; The latter three of these need to be turned into scheme code. We will do
;; this by looping into the structure, accumulating data, rearranging it so it
;; will be easy to work with in a final generation step.

(define (build-argument arg)
  arg)

(define (handle-method forms)
  (let loop ((rest forms) (info '()) (args '()))
    (if (null? rest)
        (let ((rv (append (reverse info) (list (cons 'arguments args)))))
          ;;(format #t "method:~%")
          ;;(pp rv)
          rv)
        (let ((this (car rest)))
          (cond ((eq? (car this) 'argument)
                 (loop (cdr rest) info (append args (list (cdr this)))))
                (else (loop (cdr rest) (append (list this) info) args)))))))

(define (handle-broadcast forms)
  forms)

(define (handle-signal forms)
  forms)

(define (handle-object forms)
  (let loop ((rest forms)
             (meta '())
             (methods '())
             (signals '())
             (broadcasts '()))
    (if (null? rest)
        (list (list (cons 'meta meta)
                    (cons 'methods methods)
                    (cons 'broadcasts broadcasts)
                    (cons 'signals signals)))
        (let ((this (car rest))
              (rest (cdr rest)))
          (match this
            (('name name)
             (loop rest (append meta (list this)) methods signals broadcasts))
            (('method forms ...)
             (loop rest meta
                   (append methods (list (handle-method forms)))
                   signals broadcasts))
            (('signal forms ...)
             (loop rest meta methods
                   (append signals (list (handle-signal forms)))
                   broadcasts))
            (('broadcast forms ...)
             (loop rest meta methods signals
                   (append broadcasts (list (handle-broadcast forms))))))))))

(define *sexp-stage-2*
  (let loop ((rest *sexp-stage-1*)
             (meta '())
             (objects '()))
    (if (null? rest)
        (list (cons 'meta meta)
              (cons 'objects objects))
        (let ((this (car rest))
              (rest (cdr rest)))
          (match this
            ('xmms2-ipc-description (loop rest meta objects))
            (('version version)
             (loop rest
                   (append meta
                           (list (list 'version
                                       (map string->number
                                            (string-split version #\.)))))
                   objects))
            (('object forms ...) (loop rest meta
                                       (append objects
                                               (handle-object forms)))))))))

;;(pretty-print *sexp-stage-2*)

(define (ipc/module name . imports)
  (pp (let loop ((forms (list 'define-module (list 'xmms2 'ipc name)))
                 (rest imports))
        (if (null? rest)
            forms
            (loop (append forms (list #:use-module (car rest)))
                  (cdr rest))))))

(define (ipc/define-public name value)
  (pp (list 'define-public name value)))

(define (ipc/comment text)
  (if (string-null? text)
      (display ";;")
      (display ";; "))
  (display text)
  (newline))

(define (ipc/copyright)
  (ipc/comment (string-concatenate `("Copyright (c) "
                                     ,(number->string (date-year (current-date)))
                                     " xmms2-guile workers, All rights reserved.")))
  (ipc/comment "")
  (ipc/comment "Terms for redistribution and use can be found in LICENCE.")
  (ipc/comment "This file is generated from xmms2's ipc.xml definition file.")
  (newline))

(define (generate-ipc/module-name data)
  (let* ((meta (assq-ref data 'meta))
         (name (car (assq-ref meta 'name))))
    (list 'xmms2 'ipc name)))

(define (generate-ipc/meta data)
  (let* ((meta (assq-ref data 'meta))
         (version (car (assq-ref meta 'version)))
         (objects (assq-ref data 'objects)))
    (ipc/copyright)
    (ipc/comment "This module contains meta information taken from xmms2's IPC definition,")
    (ipc/comment "taken from the project's ipc.xml file. All method-, broadcast- and signal")
    (ipc/comment "definitions for the server's IPC objects reside in sub-modules named after")
    (ipc/comment "the respective object.")
    (newline)
    (ipc/module 'meta)
    (newline)
    (ipc/define-public 'ipc-version
                       (if (= (length version) 1)
                           (car version)
                           (list 'quote version)))
    (newline)
    (ipc/define-public 'ipc-name (quote 'xmms2-server-client-ipc))
    (newline)
    (ipc/define-public
     'ipc-generated-modules
     (list 'quote
           (let loop ((rest objects) (acc '()))
             (if (null? rest)
                 (sort (cons '(xmms2 ipc meta) acc)
                       (lambda (a b)
                         (let ((a3 (symbol->string (caddr a)))
                               (b3 (symbol->string (caddr b))))
                           (string< a3 b3))))
                 (loop (cdr rest)
                       (cons (generate-ipc/module-name (car rest))
                             acc))))))))

(define (generate-ipc/method data)
  (newline)
  (let ((arguments (assq-ref data 'arguments))
        (documentation (assq-ref data 'documentation))
        (name (assq-ref data 'name))
        (return-value (assq-ref data 'return-value)))
    (pp (list 'define-public
              (cons (symbol-append 'make- (car name))
                    (map (lambda (x)
                           (car (assq-ref x 'name)))
                         arguments))
              #t))))

(define (generate-ipc/broadcast data)
  #t)

(define (generate-ipc/signal data)
  #t)

(define (sort-thing lst)
  (sort lst
        (lambda (a b)
          (let ((name-a (symbol->string (car (assq-ref a 'name))))
                (name-b (symbol->string (car (assq-ref b 'name)))))
            (string< name-a name-b)))))

(define (generate-ipc/things what lst gen)
  (if (null? lst)
      (begin (newline)
             (ipc/comment (string-concatenate (list "There are no "
                                                    (string-downcase what)
                                                    " definitions"
                                                    " in this module."))))
      (begin (newline)
             (ipc/comment (string-concatenate (list (string-titlecase what)
                                                    " definitions")))
             (let loop ((rest (sort-thing lst)))
               (if (null? rest)
                   #t
                   (begin (gen (car rest))
                     (loop (cdr rest))))))))

(define (generate-ipc/object data)
  (ipc/copyright)
  (let* ((meta (assq-ref data 'meta))
         (name (car (assq-ref meta 'name)))
         (methods (assq-ref data 'methods))
         (broadcasts (assq-ref data 'broadcasts))
         (signals (assq-ref data 'signals)))
    (ipc/module name)
    (generate-ipc/things "method" methods generate-ipc/method)
    (generate-ipc/things "broadcast" broadcasts generate-ipc/broadcast)
    (generate-ipc/things "signal" signals generate-ipc/signal)))

(define (generate-ipc/objects data)
  (let loop ((rest (assq-ref data 'objects)))
    (if (null? rest)
        #t
        (begin (generate-ipc/object (car rest))
               (loop (cdr rest))))))

(generate-ipc/meta *sexp-stage-2*)
(generate-ipc/objects *sexp-stage-2*)
