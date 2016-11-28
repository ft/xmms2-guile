;; -*- scheme -*-

;; Copyright (c) 2015 xmms2-guile workers, All rights reserved.
;;
;; Terms for redistribution and use can be found in LICENCE.

(use-modules (test tap)
             (test payload)
             (test setup)
             (xmms2 payload)
             (xmms2 constants))

(init-test-tap!)

(define (make-intlist-payload lst)
  (make-list-payload lst #:restricted TAG-INT64))

(define-syntax test-><-list?
  (syntax-rules ()
    ((_ lst)
     (begin (perform-payload-><-test make-list-payload
                                     payload->list
                                     lst pass-if-equal?)
            (perform-payload-><-test make-value-payload
                                     payload->list
                                     lst pass-if-equal?)
            (perform-payload-><-test make-list-payload
                                     payload->value
                                     lst pass-if-equal?)
            (perform-payload-><-test make-value-payload
                                     payload->value
                                     lst pass-if-equal?)))))

(define *tests-per-back-and-forth* 4)

(with-fs-test-bundle
 (plan (+ 2 4 (* 4 *tests-per-back-and-forth*)))

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
                                 0 0 0 2 0 0 0 0 0 0 2 154)))

 (test-><-list? '())
 (test-><-list? '(23))
 (test-><-list? '(23 "cmc"))
 (test-><-list? '(23 (12 #vu8(88 77 66 55 44 33) 42) "cmc"))
 (perform-payload-><-test make-intlist-payload payload->list
                          '(1 2 3 4 5) pass-if-equal?)
 (perform-payload-><-test make-intlist-payload payload->value
                          '(23 42 666 007) pass-if-equal?))
