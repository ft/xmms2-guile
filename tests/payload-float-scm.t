;; -*- scheme -*-

;; Copyright (c) 2015 xmms2-guile workers, All rights reserved.
;;
;; Terms for redistribution and use can be found in LICENCE.

(use-modules (test tap)
             (xmms2 constants)
             (xmms2 data-conversion)
             (xmms2 payload))

(primitive-load "tests/test-tap-cfg.scm")

(define (test-float-payload n expected)
  (let* ((data (make-float-payload n))
         (name (format #f "(make-float-payload ~a)" n))
         (type (uint32-ref data 0))
         (mantissa-is (int32-ref data 4))
         (mantissa-ex (int32-ref expected 4))
         (exp-is (int32-ref data 8))
         (exp-ex (int32-ref expected 8))
         (back-is (payload->float data))
         (back-ex (payload->float expected))
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

(define *tests-per-payload-test* 4)

(with-fs-test-bundle
 (plan (* 18 *tests-per-payload-test*))
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
