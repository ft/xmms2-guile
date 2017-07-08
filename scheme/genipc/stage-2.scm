;; Copyright (c) 2017 xmms2-guile workers, All rights reserved.
;;
;; Terms for redistribution and use can be found in doc/LICENCE.

(define-module (genipc stage-2)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  #:use-module (genipc utilities)
  #:export (generate-stage-2))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Stage 2: Transform stage-1 data into s-expressions directly suitable for
;; compiling into ‘define-ipc-packet-generator’ and ‘make-ipc-*’ calls.

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
  (define (with-attributes a m)
    (cons m (map (lambda (x)
                   (let ((key (car x))
                         (value (cadr x)))
                     (cons key
                           (cond ((eq? key 'ref-value)
                                  (adjust-name/constant value))
                                 ((eq? key 'ref-type)
                                  (string->symbol value))
                                 ((eq? key 'value)
                                  (string->number value))
                                 (else value)))))
                 a)))
  (define (name-split data)
    (let ((name (car (assq-ref data 'name))))
      (map string->symbol (string-split (symbol->string name) #\-))))
  (let loop ((rest forms)
             (meta '())
             (members '()))
    (if (null? rest)
        (list (list (cons* 'meta
                           (cons 'claimed? #f)
                           (cons 'name-words (name-split meta))
                           meta)
                    (cons 'members members)))
        (let ((this (car rest))
              (rest (cdr rest)))
          (match this
            (('name name) (loop rest (append meta (list this)) members))
            (('namespace-hint member) (loop rest (append meta (list this)) members))
            (('member member) (loop rest meta (append members (list member))))
            (('member (attrs ...) member)
             (loop rest meta
                   (append members
                           (list (with-attributes attrs member)))))
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

(define (generate-stage-2 stage-1)
  (let loop ((rest stage-1)
             (meta '())
             (objects '())
             (constants '())
             (enums '()))
    (if (null? rest)
        (list (cons 'meta meta)
              (cons 'objects objects)
              (cons 'constants constants)
              (cons 'enumerations enums)
              ;; Maybe I shouldn't do it like this, but it was so much fun
              ;; writing it. Deconstructs stage-1 into a list of symbols
              ;; representing the different broadcasts and signals known to
              ;; XMMS2 in a <TYPE>-<OBJECT>-<NAME> format and in the correct
              ;; order, so an enumeration can be derived from it. All in one
              ;; go. :)
              (cons 'broadcasts-and-signals
                    ;; 9: This map produces a list of symbols, named
                    ;; appropriately to be used in a ‘define-enum’ call in the
                    ;; generated library code.
                    (map (lambda (x)
                           (match x
                             ((object type name)
                              (symbol-upcase (symbol-append type
                                                            '- object
                                                            '- name)))
                             ((xxx ...)
                              (begin (handle-unknown-sexp 'handle-object xxx)
                                     (quit 1)))))
                         ;; 8: Collapse all (object-name type name) tuples into
                         ;; one list, that looks like this:
                         ;;   ((object-name type name) ...)
                         ;; Note, that this now contains all signals and
                         ;; broadcasts, defined across the different objects.
                         (concatenate
                          ;; 7: This function performs step 6 for all objects
                          ;; that define signals or broadcasts (because that is
                          ;; what the ‘filter’ from step 5 puts into ‘data’.
                          ((lambda (data)
                             (map (lambda (lst)
                                    ;; 6: The inner map does this:
                                    ;;   (object-name (type name) ...)
                                    ;; ->
                                    ;;   ((object-name type name) ...)
                                    (let ((prefix (car lst)))
                                      (map (lambda (x) (cons prefix x))
                                           (cdr lst))))
                                  data))
                           ;; 5: Some objects don't define neither signals nor
                           ;; broadcasts. This weeds out all the empty lists
                           ;; (only an object name in it) that result from
                           ;; those objects.
                           (filter (lambda (x)
                                     (and (list? x)
                                          (> (length x)
                                             1)))
                                   ;; 4: This prefixes the (type name) lists
                                   ;; with the name of the object that is being
                                   ;; processed: (object-name (type name) ...)
                                   (map (lambda (x)
                                          (cons (car (assq-ref (cdr x)
                                                               'name))
                                                ;; 3: The ‘car’ here, is either
                                                ;; ‘signal’ or ‘broadcast’. So
                                                ;; this map returns a lists of
                                                ;; lists: (type name)
                                                (map (lambda (bs)
                                                       (cons (car bs)
                                                             (assq-ref (cdr bs)
                                                                       'name)))
                                                     ;; 2: Fetch all signal and
                                                     ;; broadcast entries from
                                                     ;; an object entry.
                                                     (filter (lambda (item)
                                                               (and (list? item)
                                                                    (let ((key (car item)))
                                                                      (or (eq? key 'signal)
                                                                          (eq? key 'broadcast)))))
                                                             x))))
                                        ;; 1: Here is the entry-point for the
                                        ;; whole expression: The map call this
                                        ;; is an argument to operates on object
                                        ;; entries in the stage-2 data. So this
                                        ;; gets those out of there; filters out
                                        ;; enumerations and constants for
                                        ;; examples.
                                        (filter (lambda (x)
                                                  (and (list? x)
                                                       (eq? 'object (car x))))
                                                stage-1))))))))
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
                              (loop rest meta objects constants enums))))))))
