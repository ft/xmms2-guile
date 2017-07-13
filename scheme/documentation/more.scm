;; Copyright (c) 2017 xmms2-guile workers, All rights reserved.
;;
;; Terms for redistribution and use can be found in doc/LICENCE.

(define-module (documentation more)
  #:export (inlinable?
            add-macro-docstring
            define-variable))

(define (inlinable? mod name)
  (catch #t
    (lambda ()
      (eval-string (symbol->string name) (resolve-module mod)))
    (lambda (k . a)
      #f)))

(define (add-macro-docstring name docstring)
  (let ((var (module-variable (current-module)
                              name)))
    (set-procedure-property! (macro-transformer (variable-ref var))
                             'documentation
                             docstring)))

(define-syntax define-variable
  (lambda (x)
    (syntax-case x ()
      ((kw name value docstring)
       (with-syntax ((varname
                      (datum->syntax #'kw
                                     (symbol-append 'x2/docstring:
                                                    (syntax->datum #'name)))))
         #'(begin (define name value)
                  (define varname "")
                  (set! varname docstring)))))))
