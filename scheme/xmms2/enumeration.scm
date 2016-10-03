;; Copyright (c) 2016 xmms2-guile workers, All rights reserved.
;;
;; Terms for redistribution and use can be found in LICENCE.

(define-module (xmms2 enumeration)
  #:export (define-enum))

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
    (syntax-case x (=> <>)
      ((kw (<> xref-name) (=> definer) item ...)
       (with-syntax ((((variable . value) ...)
                      (produce-enum #'kw #'(item ...))))
         #'(begin (definer variable value) ...
                  (definer xref-name (list (cons value 'variable) ...)))))
      ((kw (=> definer) (<> xref-name) item ...)
       #'(define-enum (<> xref-name) (=> definer) item ...))
      ((kw (=> definer) item ...)
       (with-syntax ((((variable . value) ...)
                      (produce-enum #'kw #'(item ...))))
         #'(begin (definer variable value) ...)))
      ((_ item ...)
       #'(define-enum (=> define-public) item ...)))))
