;; -*- scheme -*-

;; Copyright (c) 2016 xmms2-guile workers, All rights reserved.
;;
;; Terms for redistribution and use can be found in LICENCE.

(use-modules (test tap)
             (test payload)
             (test setup)
             (xmms2 payload)
             (xmms2 constants))

(init-test-tap!)

(define-syntax test-><-binary?
  (syntax-rules ()
    ((_ vec)
     (begin (perform-payload-><-test make-binary-payload
                                     payload->binary
                                     vec pass-if-equal?)
            (perform-payload-><-test make-value-payload
                                     payload->binary
                                     vec pass-if-equal?)
            (perform-payload-><-test make-binary-payload
                                     payload->value
                                     vec pass-if-equal?)
            (perform-payload-><-test make-value-payload
                                     payload->value
                                     vec pass-if-equal?)))))

(define *tests-per-back-and-forth* 4)

(with-fs-test-bundle
 (plan (+ 3 (* 3 *tests-per-back-and-forth*)))

 (define-test "Binary payload #vu8() looks good"
   (pass-if-payload-equal? (make-binary-payload #vu8())
                           #vu8(0 0 0 5 0 0 0 0)))

 (define-test "Binary payload #vu8(23) looks good"
   (pass-if-payload-equal?
    (make-binary-payload #vu8(23))
    #vu8(0 0 0 5 0 0 0 1 23)))

 (define-test "Binary payload #vu8(23 42 7 66 6) looks good"
   (pass-if-payload-equal?
    (make-binary-payload #vu8(23 42 7 66 6))
    #vu8(0 0 0 5 0 0 0 5 23 42 7 66 6)))

 (test-><-binary? #vu8())
 (test-><-binary? #vu8(23))
 (test-><-binary? #vu8(23 42 7 66 6)))
