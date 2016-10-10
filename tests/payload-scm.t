;; -*- scheme -*-

;; Copyright (c) 2015 xmms2-guile workers, All rights reserved.
;;
;; Terms for redistribution and use can be found in LICENCE.

(use-modules (test tap)
             (srfi srfi-1)
             (rnrs bytevectors)
             (xmms2 constants)
             (xmms2 data-conversion)
             (xmms2 payload))

(primitive-load "tests/test-tap-cfg.scm")

(force-import (xmms2 payload) log2)
(force-import (xmms2 payload) frexp)

;; The payload generators may produce byte-vectors, or lists of byte-vectors.
;; The following turns both those structures into into a single byte-vector.
;; This allows the generator to produce either form without the test failing.
(define (maybe-collapse data)
  (cond
   ((bytevector? data) data)
   ((and (list? data) (bytevector? (car data)))
    (u8-list->bytevector (concatenate (map bytevector->u8-list data))))
   (else (throw 'tests:xmms2/unknown-data data))))

(define (test-float-payload n expected)
  (let* ((data (make-float-payload n))
         (name (format #f "(make-float-payload ~a)" n))
         (type (uint32-ref data 0))
         (mantissa-is (int32-ref data 4))
         (mantissa-ex (int32-ref expected 4))
         (exp-is (int32-ref data 8))
         (exp-ex (int32-ref expected 8))
         (back-is (payload->float data 4))
         (back-ex (payload->float expected 4))
         (error-is (abs (- n back-is)))
         (error-ex (abs (- n back-ex))))
    ;; The tag should trivially be correct.
    (define-test (format #f "~a: Type looks good." name)
      (pass-if-= type TYPE-FLOAT))
    ;; The exponent has to exactly correct.
    (define-test (format #f "~a: Exponent looks good." name)
      (pass-if-= exp-is exp-ex))
    ;; The mantissa may be a small bit off, due to rounding.
    (define-test (format #f "~a: Mantissa looks good." name)
      (pass-if-~= mantissa-is mantissa-ex 65))
    ;; Check if out if our implementation is at least as near to the original
    ;; value than the reference implementation.
    (define-test (format #f "~a: We are as good as or better than the reference"
                         name error-is error-ex)
      (pass-if-true (<= error-is error-ex)))))

(with-fs-test-bundle
 (plan (+ 23
          ;; ‘test-float-payload’ emitted tests:
          (* 18 4)))
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
                          0 0 0 2 0 0 0 0 0 0 0 42 0 0 0 2 0 0 0 0 0 0 2 154)))

 ;; Test a bunch of utilities that are used to produce floating point numbers:
 (define-test "(log2 1) => 0"
   (pass-if-= (log2 1) 0))
 (define-test "(log2 2) => 1"
   (pass-if-= (log2 2) 1))
 (define-test "(log2 16) => 4"
   (pass-if-= (log2 16) 4))
 (define-test "(log2 2.5) => 1.3219281"
   (pass-if-~= (log2 2.5) 1.3219281 1e-5))
 (define-test "(log2 5690) => 12.47421294"
   (pass-if-~= (log2 5690) 12.474212 1e-5))
 (define-test "(frexp 8), exponent: 4"
   (pass-if-= (cdr (frexp 8)) 4))
 (define-test "(frexp 8), fractional => -0.5"
   (pass-if-~= (car (frexp 8)) 0.5 1e-5))
 (define-test "(frexp 5690), exponent: 13"
   (pass-if-= (cdr (frexp 5690)) 13))
 (define-test "(frexp 5690), fractional => 0.694580"
   (pass-if-~= (car (frexp 5690)) 0.694578 1e-5))
 (define-test "(frexp -4), exponent: 3"
   (pass-if-= (cdr (frexp -4)) 3))
 (define-test "(frexp -4), fractional => -0.5"
   (pass-if-~= (car (frexp -4)) -0.5 1e-5))
 (define-test "(frexp 0), exponent: 0"
   (pass-if-= (cdr (frexp 0)) 0))
 (define-test "(frexp 0), fractional: 0"
   (pass-if-~= (car (frexp 0)) 0 1e-5))

 ;; The expected values come from a reference implementation in C that was
 ;; taken right out of xmms2's core. The code was compiled on an AMD64 machine
 ;; running Linux 4.x using frexp(3) from GNU libc 2.24 Compiled by GNU CC
 ;; version 6.2.0 20160914.
 (test-float-payload 0.5 #vu8(0 0 0 9 64 0 0 0 0 0 0 0))
 (test-float-payload 1.000000e-01 #vu8(0 0 0 9 102 102 102 128 255 255 255 253))
 (test-float-payload 1.000000e-02 #vu8(0 0 0 9 81 235 133 0 255 255 255 250))
 (test-float-payload 1.000000e-03 #vu8(0 0 0 9 65 137 55 128 255 255 255 247))
 (test-float-payload 9.000000e-01 #vu8(0 0 0 9 115 51 51 0 0 0 0 0))
 (test-float-payload 9.000000e-02 #vu8(0 0 0 9 92 40 246 0 255 255 255 253))
 (test-float-payload 9.000000e-03 #vu8(0 0 0 9 73 186 94 0 255 255 255 250))
 (test-float-payload 1.000000e+01 #vu8(0 0 0 9 80 0 0 0 0 0 0 4))
 (test-float-payload 1.000000e+02 #vu8(0 0 0 9 100 0 0 0 0 0 0 7))
 (test-float-payload 1.000000e+03 #vu8(0 0 0 9 125 0 0 0 0 0 0 10))
 (test-float-payload 9.000000e+06 #vu8(0 0 0 9 68 170 32 0 0 0 0 24))
 (test-float-payload 2.000000e+09 #vu8(0 0 0 9 119 53 148 0 0 0 0 31))
 (test-float-payload -5.000000e-01 #vu8(0 0 0 9 192 0 0 0 0 0 0 0))
 (test-float-payload -1.000000e+03 #vu8(0 0 0 9 131 0 0 0 0 0 0 10))
 (test-float-payload -1.000000e+09 #vu8(0 0 0 9 136 202 108 0 0 0 0 30))
 (test-float-payload -1.000000e-09 #vu8(0 0 0 9 187 71 208 128 255 255 255 227))
 (test-float-payload 1.200000e+22 #vu8(0 0 0 9 81 80 174 128 0 0 0 74))
 (test-float-payload -1.200000e-20 #vu8(0 0 0 9 142 169 200 0 255 255 255 190)))
