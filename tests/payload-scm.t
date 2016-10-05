;; -*- scheme -*-

;; Copyright (c) 2015 xmms2-guile workers, All rights reserved.
;;
;; Terms for redistribution and use can be found in LICENCE.

(use-modules (test tap)
             (srfi srfi-1)
             (rnrs bytevectors)
             (xmms2 constants)
             (xmms2 payload))

(primitive-load "tests/test-tap-cfg.scm")

;; The payload generators may produce byte-vectors, or lists of byte-vectors.
;; The following turns both those structures into into a single byte-vector.
;; This allows the generator to produce either form without the test failing.
(define (maybe-collapse data)
  (cond
   ((bytevector? data) data)
   ((and (list? data) (bytevector? (car data)))
    (u8-list->bytevector (concatenate (map bytevector->u8-list data))))
   (else (throw 'tests:xmms2/unknown-data data))))

(with-fs-test-bundle
 (plan 10)
 (define-test "int64 payload 0 looks good"
   (pass-if-equal? (maybe-collapse (make-int64-payload 0))
                   #vu8(0 0 0 2 0 0 0 0 0 0 0 0)))
 (define-test "int64 payload 255 looks good"
   (pass-if-equal? (maybe-collapse (make-int64-payload 255))
                   #vu8(0 0 0 2 0 0 0 0 0 0 0 255)))
 (define-test "int64 payload 256 looks good"
   (pass-if-equal? (maybe-collapse (make-int64-payload 256))
                   #vu8(0 0 0 2 0 0 0 0 0 0 1 0)))
 (define-test "empty string payload looks good"
   (pass-if-equal? (maybe-collapse (make-string-payload ""))
                   #vu8(0 0 0 3 0 0 0 1 0)))
 (define-test "Short string \"A\" payload looks good"
   (pass-if-equal? (maybe-collapse (make-string-payload "A"))
                   #vu8(0 0 0 3 0 0 0 2 65 0)))
 (define-test "String payload \"Hello World.\" looks good"
   (pass-if-equal? (maybe-collapse (make-string-payload "Hello World."))
                   #vu8(0 0 0 3 0 0 0 13 72 101 108 108 111
                          32 87 111 114 108 100 46 0)))
 (define-test "List payload '() looks good"
   (pass-if-equal? (maybe-collapse (make-list-payload '()))
                   #vu8(0 0 0 6 0 0 0 0 0 0 0 0)))
 (define-test "List payload '(23) looks good"
   (pass-if-equal? (maybe-collapse (make-list-payload '(23)))
                   #vu8(0 0 0 6 0 0 0 0 0 0 0 1 0 0 0 2 0 0 0 0 0 0 0 23)))
 (define-test "List payload '(23 \"cmc\") looks good"
   (pass-if-equal? (maybe-collapse (make-list-payload '(23 "cmc")))
                   #vu8(0 0 0 6 0 0 0 0 0 0 0 2 0 0 0 2 0 0 0 0 0 0 0 23
                          0 0 0 3 0 0 0 4 99 109 99 0)))
 (define-test "Restricted (int) list payload '(23 42 666) looks good"
   (pass-if-equal? (maybe-collapse (make-list-payload '(23 42 666)
                                                      #:restricted TAG-INT64))
                   #vu8(0 0 0 6 0 0 0 2 0 0 0 3 0 0 0 2 0 0 0 0 0 0 0 23
                          0 0 0 2 0 0 0 0 0 0 0 42 0 0 0 2 0 0 0 0 0 0 2 154))))
