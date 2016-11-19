;; -*- scheme -*-

(use-modules (ice-9 match)
             (ice-9 optargs)
             (ice-9 pretty-print)
             (ice-9 regex)
             (srfi srfi-1)
             (srfi srfi-19)
             (sxml match)
             (sxml simple))

;; If this is set to #t, the script will populate the scheme/xmms2/ipc/
;; directory. If set to #f, all generated code goes to stdout.
(define create-files? #t)

(define (bend-output file thunk)
  (if create-files?
      (begin
        (format #t "Generating ~a...~%" file)
        (with-output-to-file file thunk))
      (thunk)))

(define (module->file-name module)
  (cat (string-join (cons "scheme" (map symbol->string module)) "/") ".scm"))

(define (file-exists? name)
  "Return #t if a file (of any kind) named NAME exist."
  (access? name F_OK))

(define (directory-exists? name)
  "Return #t if a file (of any kind) named NAME exist."
  (let ((data (stat name #f)))
    (if (not data)
        #f
        (eq? (stat:type data) 'directory))))

(define (notify . args)
  (apply format (cons (current-error-port) args)))

(define *source-file* (cadr (command-line)))

(unless (file-exists? *source-file*)
  (notify "Source XML file does not exist: ~a~%" *source-file*)
  (quit 1))

(define (cat . lst)
  (string-concatenate lst))

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
  (define name-map '((current-id . identifier-changed)
                     (current-pos . position-changed)))
  (adjust-name name-map name))

(define (adjust-name/arg name)
  (define name-map '((client . client-name)
                     (offset-milliseconds . offset/ms)))
  (adjust-name name-map name))

(define (handle-unknown-xml name data)
  (notify "~a: Cannot handle XML entry: ~a~%" name data))

(define (adjust-type type)
  ;;(notify "type: ~a~%" type)
  (sxml-match type
    ((xmms::binary) '(binary))
    ((xmms::collection) '(collection))
    ((xmms::int) '(integer))
    ((xmms::string) '(string))
    ((xmms::unknown) '(unknown))
    ((xmms::list ,rest ...) `((list ,@(am adjust-type rest))))
    ((xmms::dictionary ,rest ...) `((dictionary ,@(am adjust-type rest))))
    ((xmms::enum-value (@ (name ,n))) `((enumeration ,(adjust-name/enum n))))
    (,otherwise (begin (handle-unknown-xml 'adjust-type otherwise)
                       (list type)))))

(define (method-arg->sexp arg)
  ;;(notify "arg: ~a~%" arg)
  (sxml-match arg
    ((xmms::name ,name) `((name ,(adjust-name/arg name))))
    ((xmms::type ,type) `((type ,@(adjust-type type))))
    ((xmms::documentation ,doc) `((documentation ,(cleanup-documentation doc))))
    ((xmms::default-hint ,hint) `((default-hint ,hint)))
    (,otherwise (begin (handle-unknown-xml 'method-arg->sexp otherwise)
                       (list arg)))))

(define (return-value->sexp return-value)
  ;;(notify "return-value: ~a~%" return-value)
  (sxml-match return-value
    ((xmms::type ,type) `((type ,@(adjust-type type))))
    ((xmms::documentation ,doc) `((documentation ,(cleanup-documentation doc))))
    (,otherwise (begin (handle-unknown-xml 'return-value->sexp otherwise)
                       (list return-value)))))

(define (method->sexp method)
  ;;(notify "method: ~a~%" method)
  (sxml-match method
    ((xmms::name ,name) `((name ,(adjust-name/method name))))
    ((xmms::documentation ,doc) `((documentation ,(cleanup-documentation doc))))
    ((xmms::argument ,rest ...) `((argument ,@(am method-arg->sexp rest))))
    ((xmms::return_value ,rest ...) `((return-value
                                       ,@(am return-value->sexp rest))))
    (,otherwise (begin (handle-unknown-xml 'method->sexp otherwise)
                       (list method)))))

(define (broadcast-or-signal->sexp bs)
  ;;(notify "bs: ~a~%" bs)
  (sxml-match bs
    ((xmms::id ,id) `((identifier ,(string->number id))))
    ((xmms::name ,name) `((name ,(adjust-name/b-or-s name))))
    ((xmms::documentation ,doc) `((documentation ,(cleanup-documentation doc))))
    ((xmms::return_value ,rest ...) `((return-value
                                       ,@(am return-value->sexp rest))))
    (,otherwise (begin (handle-unknown-xml 'broadcast-or-signal->sexp otherwise)
                       (list bs)))))

(define (enum->sexp elst)
  ;;(notify "elst: ~a~%" elst)
  (sxml-match elst
    ((xmms::name ,name) `((name ,(adjust-name/enum name))))
    ((xmms::member ,member) `((member ,(adjust-name/member member))))
    ((xmms::namespace-hint ,nsh)
     `((namespace-hint ,(adjust-name/namespace-hint nsh))))
    (,otherwise (begin (handle-unknown-xml 'enum->sexp otherwise)
                       (list elst)))))

(define (transform-value type value)
  (let* ((transformers `((integer . ,string->number)))
         (type (if (string? type)
                   (string->symbol type)
                   type))
         (transformer* (assq-ref transformers type))
         (transformer (or transformer* (lambda (x)
                                         (notify "transform-value: Unknown type: ~a (~a)~%"
                                                 type x)
                                         x))))
    (transformer value)))

(define (constant->sexp clst)
  ;;(notify "clst: ~a~%" clst)
  (sxml-match clst
    ((xmms::name ,name) `((name ,(adjust-name '() name))))
    ((xmms::value (@ (type ,t)) ,v) (list `(value ,(transform-value t v))))
    (,otherwise (begin (handle-unknown-xml 'enum->sexp otherwise)
                       (list clst)))))

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
    ((xmms::signal ,rest ...) `((signal ,@(am broadcast-or-signal->sexp rest))))
    ((xmms::enum ,rest ...) `((enum ,@(am enum->sexp rest))))
    ((xmms::constant ,rest ...) `((constant ,@(am constant->sexp rest))))
    (,otherwise (begin (handle-unknown-xml 'sxml->sexp otherwise)
                       (list tree)))))

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
;;   - (constant ...)
;;   - (enum ...)
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

(define (handle-unknown-sexp name data)
  (notify "~a: Cannot handle S-Expression: ~a~%" name data))

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

(define (handle-constants forms)
  (match forms
    ((('name name) ('value value)) (list (list name value)))
    ((xxx ...) (begin (handle-unknown-sexp 'handle-constants xxx)
                      xxx))))

(define (handle-enumerations forms)
  (let loop ((rest forms)
             (meta '())
             (members '()))
    (if (null? rest)
        (list (list (cons 'meta meta)
                    (cons 'members members)))
        (let ((this (car rest))
              (rest (cdr rest)))
          (match this
            (('name name) (loop rest (append meta (list this)) members))
            (('namespace-hint member) (loop rest (append meta (list this)) members))
            (('member member) (loop rest meta (append members (list member))))
            ((xxx ...) (begin (handle-unknown-sexp 'handle-enumerations xxx)
                              (loop rest meta members))))))))

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
                   (append broadcasts (list (handle-broadcast forms)))))
            ((xxx ...) (begin (handle-unknown-sexp 'handle-object xxx)
                              (loop rest meta methods signals broadcasts))))))))

(define *sexp-stage-2*
  (let loop ((rest *sexp-stage-1*)
             (meta '())
             (objects '())
             (constants '())
             (enums '()))
    (if (null? rest)
        (list (cons 'meta meta)
              (cons 'objects objects)
              (cons 'constants constants)
              (cons 'enumerations enums))
        (let ((this (car rest))
              (rest (cdr rest)))
          (match this
            ('xmms2-ipc-description (loop rest meta objects constants enums))
            (('version version)
             (loop rest
                   (append meta
                           (list (list 'version
                                       (map string->number
                                            (string-split version #\.)))))
                   objects
                   constants
                   enums))
            (('object forms ...) (loop rest meta
                                       (append objects
                                               (handle-object forms))
                                       constants
                                       enums))
            (('constant forms ...) (loop rest meta objects
                                         (append constants (handle-constants forms))
                                         enums))
            (('enum forms ...) (loop rest meta objects constants
                                     (append enums (handle-enumerations forms))))
            ((xxx ...) (begin (handle-unknown-sexp 'stage-2-loop xxx)
                              (loop rest meta objects))))))))

;;(pretty-print *sexp-stage-2*)

(define (ipc/module name . imports)
  (pp (let loop ((forms (list 'define-module name))
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
  (ipc/comment (cat "Copyright (c) "
                    (number->string (date-year (current-date)))
                    " xmms2-guile workers, All rights reserved."))
  (ipc/comment "")
  (ipc/comment "Terms for redistribution and use can be found in LICENCE.")
  (ipc/comment "This file is generated from xmms2's ipc.xml definition file.")
  (newline))

(define (generate-ipc/module-name data)
  (let* ((meta (assq-ref data 'meta))
         (name (car (assq-ref meta 'name))))
    (list `(xmms2 constants ,name)
          `(xmms2 ipc ,name))))

(define (generate-ipc/meta data)
  (let* ((meta (assq-ref data 'meta))
         (version (car (assq-ref meta 'version)))
         (objects (assq-ref data 'objects)))
    (bend-output
     (module->file-name '(xmms2 constants meta))
     (lambda ()
       (ipc/copyright)
       (ipc/comment "This module contains meta information taken from xmms2's IPC definition,")
       (ipc/comment "taken from the project's ipc.xml file. All method-, broadcast- and signal")
       (ipc/comment "definitions for the server's IPC objects reside in sub-modules named after")
       (ipc/comment "the respective object.")
       (newline)
       (ipc/module '(xmms2 constants meta))
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
                    (sort (cons '(xmms2 constants meta) acc)
                          (lambda (a b)
                            (let ((a3 (symbol->string (caddr a)))
                                  (b3 (symbol->string (caddr b))))
                              (string< a3 b3))))
                    (loop (cdr rest)
                          (append (generate-ipc/module-name (car rest))
                                  acc))))))))))

(define (module->object module)
  (string->symbol (cat "OBJECT-"
                       (string-upcase (symbol->string (last module))))))

(define (name->identifier name)
  (string->symbol (cat "CMD-" (string-upcase (symbol->string name)))))

(define* (ipc/packet-generator name module return-value
                               #:key
                               (arguments '())
                               (documentation #f)
                               (prefix #f))
  (pp (append
       (list 'define-ipc-packet-generator
             (let ((sym (symbol-append 'make- name)))
               (if prefix
                   (symbol-append prefix sym)
                   sym))
             'public
             (module->object module)
             (name->identifier name)
             ;; TODO: This should pretty print the documentation and also
             ;; include the documentation of the arguments and the return value
             ;; as well.
             (if documentation
                 (string-join (concatenate documentation) (format #f "~%"))
                 "Not documented yet."))
       (map (lambda (arg)
              (list (car (assq-ref arg 'type))
                    (car (assq-ref arg 'name))))
            arguments))))

(define (generate-ipc/method module data)
  (newline)
  (let ((arguments (assq-ref data 'arguments))
        (documentation (assq-ref data 'documentation))
        (name (car (assq-ref data 'name)))
        (return-value (assq-ref data 'return-value)))
    (ipc/packet-generator name module return-value
                          #:arguments arguments
                          #:documentation documentation)))

(define (generate-ipc/broadcast module data)
  #t)

(define (generate-ipc/signal module data)
  #t)

(define (sort-thing lst)
  (sort lst
        (lambda (a b)
          (let ((name-a (symbol->string (car (assq-ref a 'name))))
                (name-b (symbol->string (car (assq-ref b 'name)))))
            (string< name-a name-b)))))

(define (generate-ipc/things what lst gen name)
  (if (null? lst)
      (begin (newline)
             (ipc/comment (cat "There are no "
                               (string-downcase what)
                               " definitions"
                               " in this module.")))
      (begin (newline)
             (ipc/comment (cat (string-titlecase what) " definitions"))
             (let loop ((rest (sort-thing lst)))
               (if (null? rest)
                   #t
                   (begin (gen `(xmms2 ipc ,name) (car rest))
                          (loop (cdr rest))))))))

(define (name->constants name)
  (cat "scheme/xmms2/constants/" (symbol->string name) ".scm"))

(define (generate-ipc/object data)
  (ipc/copyright)
  (let* ((meta (assq-ref data 'meta))
         (name (car (assq-ref meta 'name)))
         (methods (assq-ref data 'methods))
         (broadcasts (assq-ref data 'broadcasts))
         (signals (assq-ref data 'signals))
         (std-libraries '((ice-9 optargs)
                          (xmms2 constants)
                          (xmms2 header)
                          (xmms2 ipc)
                          (xmms2 payload))))
    (apply ipc/module (cons* `(xmms2 ipc ,name)
                             `(xmms2 constants ,name)
                             std-libraries))
    (generate-ipc/things "method" methods generate-ipc/method name)
    (generate-ipc/things "broadcast" broadcasts generate-ipc/broadcast name)
    (generate-ipc/things "signal" signals generate-ipc/signal name)))

(define (generate-ipc/objects data)
  (let loop ((rest (assq-ref data 'objects)))
    (if (null? rest)
        #t
        (let ((this (car rest)))
          (bend-output
           (module->file-name
            `(xmms2 ipc ,(car (assq-ref (assq-ref this 'meta) 'name))))
           (lambda () (generate-ipc/object this)))
          (loop (cdr rest))))))

(define (generate-ipc/constant object stage-2)
  (ipc/copyright))

;; Constants are fun: We need to look at all methods, all broadcasts and
;; signals to generate numbers to match names. Then the explicit constants from
;; ipc.xml and finally handle the enumerations listed in that file as well.
(define (generate-ipc/constants data)
  (let loop ((rest (assq-ref data 'objects)))
    (if (null? rest)
        #t
        (let ((this (car rest))
              (rest (cdr rest)))
          (bend-output
           (module->file-name
            `(xmms2 constants ,(car (assq-ref (assq-ref this 'meta) 'name))))
           (lambda () (generate-ipc/constant this data)))
          (loop rest)))))

(when create-files?
  (for-each (lambda (dir)
              (or (directory-exists? dir)
                  (mkdir dir)))
            '("scheme"
              "scheme/xmms2"
              "scheme/xmms2/constants"
              "scheme/xmms2/ipc")))

(generate-ipc/meta *sexp-stage-2*)
(generate-ipc/constants *sexp-stage-2*)
(generate-ipc/objects *sexp-stage-2*)
