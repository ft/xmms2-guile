;; -*- scheme -*-

;; Copyright (c) 2015 xmms2-guile workers, All rights reserved.
;;
;; Terms for redistribution and use can be found in LICENCE.

(use-modules (test tap))

(primitive-load "tests/test-tap-cfg.scm")

(force-import (xmms2 payload) log2)
(force-import (xmms2 payload) frexp)

(define-syntax define-float-util-test
  (syntax-rules ()
    ((_ expr value)
     (define-test (format #f "~a => ~a" 'expr value)
       (pass-if-= expr value)))
    ((_ expr value eps)
     (define-test (format #f "~a => ~a" 'expr value)
       (pass-if-~= expr value eps)))))

(define fractional car)
(define exponent cdr)

(with-fs-test-bundle
 (plan 13)
 (define-float-util-test (log2 1) 0)
 (define-float-util-test (log2 2) 1)
 (define-float-util-test (log2 16) 4)
 (define-float-util-test (log2 2.5) 1.3219281 1e-5)
 (define-float-util-test (log2 5690) 12.474212 1e-5)
 (define-float-util-test (exponent (frexp 8)) 4)
 (define-float-util-test (fractional (frexp 8)) 0.5 1e-5)
 (define-float-util-test (exponent (frexp 5690)) 13)
 (define-float-util-test (fractional (frexp 5690)) 0.694578 1e-5)
 (define-float-util-test (exponent (frexp -4)) 3)
 (define-float-util-test (fractional (frexp -4)) -0.5 1e-5)
 (define-float-util-test (exponent (frexp 0)) 0)
 (define-float-util-test (fractional (frexp 0)) 0 1e-5))
