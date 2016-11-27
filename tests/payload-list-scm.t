;; -*- scheme -*-

;; Copyright (c) 2015 xmms2-guile workers, All rights reserved.
;;
;; Terms for redistribution and use can be found in LICENCE.

(use-modules (test tap)
             (test payload)
             (xmms2 payload)
             (xmms2 constants))

(primitive-load "tests/test-tap-cfg.scm")

(with-fs-test-bundle
 (plan 4)

 (define-test "List payload '() looks good"
   (pass-if-payload-equal? (make-list-payload '())
                           #vu8(0 0 0 6 0 0 0 0 0 0 0 0)))

 (define-test "List payload '(23) looks good"
   (pass-if-payload-equal?
    (make-list-payload '(23))
    #vu8(0 0 0 6 0 0 0 0 0 0 0 1 0 0 0 2 0 0 0 0 0 0 0 23)))

 (define-test "List payload '(23 \"cmc\") looks good"
   (pass-if-payload-equal?
    (make-list-payload '(23 "cmc"))
    #vu8(0 0 0 6 0 0 0 0 0 0 0 2 0 0 0 2 0 0 0 0 0 0 0 23
                                 0 0 0 3 0 0 0 4 99 109 99 0)))

 (define-test "Restricted (int) list payload '(23 42 666) looks good"
   (pass-if-payload-equal?
    (make-list-payload '(23 42 666)
                       #:restricted TAG-INT64)
    #vu8(0 0 0 6 0 0 0 2 0 0 0 3 0 0 0 2 0 0 0 0 0 0 0 23
                                 0 0 0 2 0 0 0 0 0 0 0 42
                                 0 0 0 2 0 0 0 0 0 0 2 154))))
