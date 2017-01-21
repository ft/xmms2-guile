;; Copyright (c) 2017 xmms2-guile workers, All rights reserved.
;;
;; Terms for redistribution and use can be found in LICENCE.

(define-module (xmms2 fetch-spec)
  #:use-module (xmms2 types)
  #:export (fetch-spec))

(define process-argument (@@ (xmms2 types) process-argument))

(define-syntax fetch-spec
  (lambda (x)
    (define (process-arguments kw lst)
      (let loop ((rest lst) (skip? #t) (acc '()))
        (syntax-case rest (-)
          (() acc)
          (((- exp ...) . args)
           (loop #'args #t #`(#,@acc #,#'(fetch-spec exp ...))))
          ((key . args) (and (not skip?) (keyword? (syntax->datum #'key)))
           (loop #'args #t #`(#,@acc key)))
          ((value . args) (not skip?)
           (loop #'args #t #`(#,@acc #,(process-argument #'value))))
          ((key . args) skip?
           (loop #'args #f #`(#,@acc key)))
          ((key ...)
           (syntax-violation 'fetch-spec "Invalid fetch-spec expression" rest kw)))))
    (syntax-case x ()
      ((kw exp ...) (property-list? (syntax->datum #'(exp ...)))
       #`(property-list->dictionary #,@(process-arguments #'kw #'(exp ...))))
      ((kw exp ...)
       (syntax-violation 'fetch-spec "Invalid fetch-spec expression" x #'kw)))))
