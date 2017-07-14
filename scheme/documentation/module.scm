;; Copyright (c) 2017 xmms2-guile workers, All rights reserved.
;;
;; Terms for redistribution and use can be found in doc/LICENCE.

(define-module (documentation module)
  #:use-module (ice-9 session)
  #:use-module (srfi srfi-1)
  #:use-module (documentation more)
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

(define (expand-for-value mod name value)
  (cond ((procedure? value)
         (list name 'procedure
               (or (procedure-documentation value) 'undocumented)
               (procedure-arguments value)
               (procedure-minimum-arity value)))
        ((macro? value)
         (let ((tf (macro-transformer value))
               (inlined (inlinable? mod name)))
           (list name (if inlined 'procedure 'macro)
                 (or (procedure-documentation tf) 'undocumented)
                 (procedure-arguments (or inlined tf))
                 (procedure-minimum-arity (or inlined tf)))))
        ((integer? value)
         (list name 'integer
               (variable-documentation mod name)))
        (else (list name 'unknown-datum))))

(define (process-interface mod item)
  (let* ((name (car item))
         (value (cdr item)))
    (if (and (variable? value) (not (variable-bound? value)))
        (list name 'unbound-parameter)
        (expand-for-value mod name (if (variable? value)
                                       (variable-ref value)
                                       value)))))
