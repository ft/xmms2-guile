;; -*- scheme -*-

;; Copyright (c) 2015-2016 xmms2-guile workers, All rights reserved.
;;
;; Terms for redistribution and use can be found in LICENCE.

(use-modules (test tap)
             (test payload)
             (xmms2 payload))

(primitive-load "tests/test-tap-cfg.scm")

(with-fs-test-bundle
 (plan 3)
 (define-test "int64 payload 0 looks good"
   (pass-if-payload-equal? (make-int64-payload 0)
                           #vu8(0 0 0 2 0 0 0 0 0 0 0 0)))
 (define-test "int64 payload 255 looks good"
   (pass-if-payload-equal? (make-int64-payload 255)
                           #vu8(0 0 0 2 0 0 0 0 0 0 0 255)))
 (define-test "int64 payload 256 looks good"
   (pass-if-payload-equal? (make-int64-payload 256)
                           #vu8(0 0 0 2 0 0 0 0 0 0 1 0))))
