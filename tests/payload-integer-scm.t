;; -*- scheme -*-

;; Copyright (c) 2015-2016 xmms2-guile workers, All rights reserved.
;;
;; Terms for redistribution and use can be found in LICENCE.

(use-modules (test tap)
             (test payload)
             (test setup)
             (xmms2 payload))

(init-test-tap!)

;; Here are a couple of tests, that generate integer payload and then turns
;; that payload back into an integer. If the generator works (and that is
;; tested for beforehand), this makes sure the disectors work, too. This uses
;; four tests per transformation, since we have the specific generators and
;; disectors per data type and then there are the frontends that take a look at
;; the data passed into them and then choose the correct generator/disector
;; pair to use.

(define-syntax test-><-int64?
  (syntax-rules ()
    ((_ n)
     (begin (perform-payload-><-test make-int64-payload
                                     payload->int64
                                     n pass-if-=)
            (perform-payload-><-test make-value-payload
                                     payload->int64
                                     n pass-if-=)
            (perform-payload-><-test make-int64-payload
                                     payload->value
                                     n pass-if-=)
            (perform-payload-><-test make-value-payload
                                     payload->value
                                     n pass-if-=)))))

(define *tests-per-back-and-forth* 4)

(with-fs-test-bundle
 (plan (* 3 (+ 1 *tests-per-back-and-forth*)))
 (define-test "int64 payload 0 looks good"
   (pass-if-payload-equal? (make-int64-payload 0)
                           #vu8(0 0 0 2 0 0 0 0 0 0 0 0)))
 (define-test "int64 payload 255 looks good"
   (pass-if-payload-equal? (make-int64-payload 255)
                           #vu8(0 0 0 2 0 0 0 0 0 0 0 255)))
 (define-test "int64 payload 256 looks good"
   (pass-if-payload-equal? (make-int64-payload 256)
                           #vu8(0 0 0 2 0 0 0 0 0 0 1 0)))
 (test-><-int64? 0)
 (test-><-int64? 255)
 (test-><-int64? 256))
