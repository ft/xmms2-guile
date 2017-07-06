;; Copyright (c) 2017 xmms2-guile workers, All rights reserved.
;;
;; Terms for redistribution and use can be found in doc/LICENCE.

(define-module (documentation module)
  #:use-module (ice-9 session)
  #:use-module (srfi srfi-1)
  #:export (module->documentation))

(define (string->s-exp str)
  (with-input-from-string str
    (lambda ()
      (read (current-input-port)))))

(define (module->documentation mod)
  (sort (module->documentation* (or (and (string? mod) (string->s-exp mod)) mod))
        (lambda (a b)
          (let ((a-name (symbol->string (car a)))
                (b-name (symbol->string (car b))))
            (string< a-name b-name)))))

(define (module->documentation* mod)
  (let ((inf (module-obarray (resolve-interface mod))))
    (map (lambda (x) (process-interface mod x))
         (hash-map->list cons inf))))

(define (additional-documentation mod item)
  (catch #t
    (lambda ()
      (or (variable-ref (module-variable (resolve-module (cons* 'documentation
                                                                'more
                                                                (cdr mod)))
                                         item))
          'undocumented))
    (lambda (k . a)
      'undocumented)))

(define (maybe-proc-doc mod proc)
  (or (procedure-documentation proc)
      (additional-documentation mod (procedure-name proc))))

(define (expand-for-value mod name value)
  (cond ((procedure? value)
         (list name 'procedure
               (maybe-proc-doc mod value)
               (procedure-arguments value)
               (procedure-property value 'arity)))
        ((macro? value)
         (let* ((tf (macro-transformer value))
                (doc (maybe-proc-doc mod tf)))
           (list name 'macro
                 ;; If there is documentation for the transformer, use that. If
                 ;; not, try the additional documentation for the macro name.
                 (if (eq? doc 'undocumented)
                     (additional-documentation mod name)
                     doc)
                 (procedure-arguments tf)
                 (procedure-property tf 'arity))))
        ((integer? value)
         (list name 'integer
               (additional-documentation mod name)))
        (else (list name 'unknown-datum))))

(define (process-interface mod item)
  (let* ((name (car item))
         (value (cdr item)))
    (if (and (variable? value) (not (variable-bound? value)))
        (list name 'unbound-parameter)
        (expand-for-value mod name (if (variable? value)
                                       (variable-ref value)
                                       value)))))
