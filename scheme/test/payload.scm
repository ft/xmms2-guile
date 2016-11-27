;; Copyright (c) 2016 xmms2-guile workers, All rights reserved.
;;
;; Terms for redistribution and use can be found in LICENCE.

(define-module (test payload)
  #:use-module (test tap)
  #:use-module (xmms2 payload)
  #:export (pass-if-payload-equal?
            pass-if-payload-not-equal?
            perform-payload-><-test))

(define-syntax pass-if-payload-equal?
  (syntax-rules ()
    ((_ a b)
     (pass-if-equal? (payload-combine a) (payload-combine b)))))

(define-syntax pass-if-payload-not-equal?
  (syntax-rules ()
    ((_ a b)
     (pass-if-not-equal? (payload-combine a) (payload-combine b)))))

(define-syntax perform-payload-><-test
  (syntax-rules ()
    ((_ generator disector value tap-test rest* ...)
     (let ((v value))
       (define-test (format #f "~a><~a: ~a" 'generator 'disector v)
         (tap-test (disector (generator v)) v rest* ...))))))
