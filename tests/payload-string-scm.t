;; -*- scheme -*-

;; Copyright (c) 2015 xmms2-guile workers, All rights reserved.
;;
;; Terms for redistribution and use can be found in LICENCE.

(use-modules (test tap)
             (test payload)
             (test setup)
             (xmms2 payload))

(init-test-tap!)

(with-fs-test-bundle
 (plan 3)
 (define-test "empty string payload looks good"
   (pass-if-payload-equal? (make-string-payload "")
                           #vu8(0 0 0 3 0 0 0 1 0)))
 (define-test "Short string \"A\" payload looks good"
   (pass-if-payload-equal? (make-string-payload "A")
                           #vu8(0 0 0 3 0 0 0 2 65 0)))
 (define-test "String payload \"Hello World.\" looks good"
   (pass-if-payload-equal?
    (make-string-payload "Hello World.")
    #vu8(0 0 0 3 0 0 0 13 72 101 108 108 111 32 87 111 114 108 100 46 0))))
