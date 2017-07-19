;; Copyright (c) 2017 xmms2-guile workers, All rights reserved.
;;
;; Terms for redistribution and use can be found in doc/LICENCE.

(define-module (genipc stage-3)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-19)
  #:use-module (genipc utilities)
  #:export (ensure-directories!
            generate-ipc/constants
            generate-ipc/meta
            generate-ipc/objects))

(define (ensure-directories!)
  (when (file-generation-active?)
    (for-each (lambda (dir)
                (or (directory-exists? dir)
                    (mkdir dir)))
              '("scheme"
                "scheme/xmms2"
                "scheme/xmms2/constants"
                "scheme/xmms2/ipc"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Stage 3: Generate ‘scheme/constants/*.scm’ as well as ‘scheme/ipc/*.sch’.

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
         (objects (assq-ref data 'objects))
         (enums (assq-ref data 'enumerations))
         (constants (assq-ref data 'constants))
         (bs (assq-ref data 'broadcasts-and-signals)))
    (bend-output
     (module->file-name '(xmms2 constants meta))
     (lambda ()
       (ipc/copyright)
       (ipc/comment "This module contains meta information taken from xmms2's IPC definition,")
       (ipc/comment "taken from the project's ipc.xml file. All method-, broadcast- and signal")
       (ipc/comment "definitions for the server's IPC objects reside in sub-modules named after")
       (ipc/comment "the respective object.")
       (newline)
       (ipc/module '(xmms2 constants meta) '(xmms2 enumeration))
       (newline)
       (ipc/define-public 'PROTOCOL-VERSION
                          (if (= (length version) 1)
                              (car version)
                              (list 'quote version)))
       (newline)
       (ipc/define-public 'ipc-name (quote 'xmms2-server-client-ipc))
       (newline)
       (ipc/comment "Constants:")
       (let loop ((rest constants))
         (if (null? rest)
             #t
             (let ((this (car rest)))
               (newline)
               (pp (cons 'define-public this))
               (loop (cdr rest)))))
       (newline)
       (ipc/comment "Enumerations:")
       (newline)
       (generate-ipc/object-table objects)
       (newline)
       (generate-ipc/broadcast&signal-table bs)
       (let loop ((rest enums))
         (if (null? rest)
             #t
             (let ((this (car rest)))
               (unless (claimed-enum? this)
                 (notify "  Including unclaimed enumeration: ~a~%"
                         (enum->name this))
                 (generate-ipc/enumeration this))
               (loop (cdr rest)))))
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

(define* (ipc/packet-generator name module return-value
                               #:key
                               (arguments '())
                               (documentation #f)
                               (prefix #f))
  (pp (append
       (list 'define-ipc-packet-generator
             (let ((sym (symbol-append 'ipc- name)))
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
  (define (name->id module name)
    (symbol-append 'BROADCAST-
                   (symbol-upcase (caddr module)) '-
                   (symbol-upcase name)))
  (define (name->generator name)
    (symbol-append 'make-broadcast- name))
  (let ((name (assq-ref* data 'name)))
    (newline)
    (pp `(define-public (,(name->generator name))
           (make-broadcast-generator ,(name->id module name))))))

(define (generate-ipc/signal module data)
  (define (name->id module name)
    (symbol-append 'SIGNAL-
                   (symbol-upcase (caddr module)) '-
                   (symbol-upcase name)))
  (define (name->generator name)
    (symbol-append 'make-signal- name))
  (let ((name (assq-ref* data 'name)))
    (newline)
    (pp `(define-public (,(name->generator name))
           (make-signal-generator ,(name->id module name))))))

(define (generate-ipc/introspection/method module data)
  (define (name->id name)
    (symbol-append 'CMD- (symbol-upcase name)))
  (define (name->generator name)
    (symbol-append 'ipc- name))
  (define (module->object module)
    (symbol-append 'OBJECT- (symbol-upcase (caddr module))))
  (let ((name (assq-ref* data 'name))
        (rv (or (assq-ref data 'return-value) '())))
    (newline)
    (pp `(define-public ,(symbol-append 'm: name)
           (make-ipc-method ',name
                            #:object ,(module->object module)
                            #:identifier ,(name->id name)
                            #:documentation ',(assq-ref* data 'documentation)
                            #:arguments ',(assq-ref data 'arguments)
                            #:return-value ',rv
                            #:generator ,(name->generator name))))))

(define (generate-ipc/introspection/broadcast module data)
  (define (name->id module name)
    (symbol-append 'BROADCAST-
                   (symbol-upcase (caddr module)) '-
                   (symbol-upcase name)))
  (define (module->object module)
    (symbol-append 'OBJECT- (symbol-upcase (caddr module))))
  (define (name->generator name)
    (symbol-append 'make-broadcast- name))
  (let ((name (assq-ref* data 'name))
        (rv (or (assq-ref data 'return-value) '())))
    (newline)
    (pp `(define-public ,(symbol-append 'b: name)
           (make-ipc-broadcast ',name
                               #:object ,(module->object module)
                               #:identifier ,(name->id module name)
                               #:documentation ',(assq-ref* data 'documentation)
                               #:return-value ',rv
                               #:generator ,(name->generator name))))))

(define (generate-ipc/introspection/signal module data)
  (define (name->id module name)
    (symbol-append 'SIGNAL-
                   (symbol-upcase (caddr module)) '-
                   (symbol-upcase name)))
  (define (module->object module)
    (symbol-append 'OBJECT- (symbol-upcase (caddr module))))
  (define (name->generator name)
    (symbol-append 'make-signal- name))
  (let ((name (assq-ref* data 'name))
        (rv (or (assq-ref data 'return-value) '())))
    (newline)
    (pp `(define-public ,(symbol-append 's: name)
           (make-ipc-signal ',name
                            #:object ,(module->object module)
                            #:identifier ,(name->id module name)
                            #:documentation ',(assq-ref* data 'documentation)
                            #:return-value ',rv
                            #:generator ,(name->generator name))))))

(define (sort-thing lst)
  (sort lst
        (lambda (a b)
          (let ((name-a (symbol->string (car (assq-ref a 'name))))
                (name-b (symbol->string (car (assq-ref b 'name)))))
            (string< name-a name-b)))))

(define (generate-ipc/things what lst gen introspec name)
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
                   (let ((modname `(xmms2 ipc ,name))
                         (this (car rest))
                         (rest (cdr rest)))
                     (gen modname this)
                     (introspec modname this)
                     (loop rest)))))))

(define (generate-ipc/object-table objects)
  (define (get-name x)
    (car (assq-ref (assq-ref x 'meta) 'name)))
  (define (prefix-name x)
    (symbol-append 'OBJECT- x))
  (pp (append `(define-enum (<> xref-objects))
              ;; The SIGNAL object isn't mentioned in ipc.xml, but that doesn't
              ;; mean we can leave it out.
              (cons 'OBJECT-SIGNAL
                    (map prefix-name
                         (map symbol-upcase
                              (map get-name objects)))))))

(define (generate-ipc/broadcast&signal-table lst)
  (pp (append `(define-enum (<> xref-broadcasts-and-signals)) lst)))

(define (generate-ipc/object data)
  (ipc/copyright)
  (let* ((meta (assq-ref data 'meta))
         (name (car (assq-ref meta 'name)))
         (methods (assq-ref data 'methods))
         (broadcasts (assq-ref data 'broadcasts))
         (signals (assq-ref data 'signals))
         (std-libraries '((ice-9 optargs)
                          (rnrs bytevectors)
                          (xmms2 constants)
                          (xmms2 constants meta)
                          (xmms2 header)
                          (xmms2 ipc)
                          (xmms2 payload)
                          (xmms2 types))))
    (apply ipc/module (cons* `(xmms2 ipc ,name)
                             `(xmms2 constants ,name)
                             std-libraries))
    (generate-ipc/things "method" methods
                         generate-ipc/method
                         generate-ipc/introspection/method
                         name)
    (generate-ipc/things "broadcast" broadcasts
                         generate-ipc/broadcast
                         generate-ipc/introspection/broadcast
                         name)
    (generate-ipc/things "signal" signals
                         generate-ipc/signal
                         generate-ipc/introspection/signal
                         name)))

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

(define (claimed-enum? enum)
  (not (not (assq-ref (assq-ref enum 'meta) 'claimed?))))

(define (claim-enum! enum module)
  (let ((claim (assq 'claimed? (assq-ref enum 'meta))))
    (when claim (set-cdr! claim module))))

(define (generate-ipc/method-table name methods)
  (define (xref x)
    (symbol-append 'xref- x '-cmds))
  (define (cname x)
    (symbol-append 'CMD-
                   (string->symbol (string-upcase (symbol->string x)))))
  (define (first-cmd x)
    `(,x FIRST-COMMAND-ID))
  (newline)
  (if (null? methods)
      (begin (ipc/comment (cat "The (xmms2 ipc " (symbol->string name)
                               ") module has no methods."))
             (ipc/comment "Therefore there is no command table."))
      (let ((names (map (lambda (x) (cname (car (assq-ref x 'name)))) methods)))
        (ipc/comment "Command table:")
        (pp (append `(define-enum (<> ,(xref name)))
                    (cons (first-cmd (car names))
                          (cdr names)))))))

(define (enum->thing enum thing)
  (assq-ref (assq-ref enum 'meta) thing))

(define (enum->name enum)
  (let ((value (enum->thing enum 'name)))
    (and value (car value))))

(define (enum->name-words enum)
  (enum->thing enum 'name-words))

(define (enum->namespace-hint enum)
  (let ((value (enum->thing enum 'namespace-hint)))
    (and value (car value))))

(define (does-enum-belong? enum obj-name)
  (if (claimed-enum? enum)
      #f
      (let ((nsh (enum->namespace-hint enum))
            (nw (enum->name-words enum)))
        (cond
         ;; If the first word in the namespace hint matches the object name,
         ;; this enum belongs to it.
         (nsh (cond ((eq? obj-name (car nsh)) #t)
                    (else #f)))
         ;; Similarly, if the first word of the name of the enumeration matches
         ;; the object name, the two belong together as well. After that, match
         ;; a few specific ones.
         (nw (cond ((eq? obj-name (car nw)) #t)
                   ((and (eq? obj-name 'courier)
                         (eq? (car nw) 'c2c)))
                   ((and (eq? obj-name 'media-library)
                         (eq? (car nw) 'medialib)))
                   ((and (eq? obj-name 'media-info-reader)
                         (eq? (car nw) 'mediainfo)))
                   (else #f)))
         (else #f)))))

(define (generate-ipc/enum-member prefix data)
  (define (prefixed-name prefix name)
    (symbol-append (symbol-upcase prefix) '- name))
  (if (symbol? data)
      (prefixed-name prefix data)
      (let* ((name (car data))
             (meta (cdr data))
             (ref-value (assq-ref meta 'ref-value))
             (ref-type (assq-ref meta 'ref-type))
             (value (assq-ref meta 'value)))
        (cond (ref-value (list (prefixed-name prefix name)
                               (if (and ref-type (eq? ref-type 'constant))
                                   ref-value
                                   (prefixed-name prefix ref-value))))
              (value (list (prefixed-name prefix name) value))
              (else name)))))

(define (generate-ipc/enumeration enum)
  (define (xref x)
    (symbol-append 'xref- x (if (symbol-suffix? 's x) 'es 's)))
  (let* ((name (enum->name enum))
         (members (assq-ref enum 'members))
         (transformer (lambda (x) (generate-ipc/enum-member name x))))
    (newline)
    (pp (append `(define-enum (<> ,(xref name)))
                (map transformer members)))))

(define (generate-ipc/constant object stage-2)
  (ipc/copyright)
  (let* ((meta (assq-ref object 'meta))
         (name (car (assq-ref meta 'name)))
         (methods (assq-ref object 'methods))
         (enums (assq-ref stage-2 'enumerations))
         (mod `(xmms2 constants ,name))
         (std-libraries '((xmms2 constants)
                          (xmms2 constants meta)
                          (xmms2 enumeration))))
    (apply ipc/module (cons mod std-libraries))
    (generate-ipc/method-table name methods)
    (let loop ((rest enums))
      (if (null? rest)
          #t
          (let ((this (car rest)))
            (when (does-enum-belong? this name)
              (claim-enum! this name)
              (generate-ipc/enumeration this))
            (loop (cdr rest)))))))

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
