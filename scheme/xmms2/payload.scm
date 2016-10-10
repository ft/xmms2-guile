;; Copyright (c) 2015 xmms2-guile workers, All rights reserved.
;;
;; Terms for redistribution and use can be found in LICENCE.

(define-module (xmms2 payload)
  #:use-module (ice-9 optargs)
  #:use-module (rnrs bytevectors)
  #:use-module (xmms2 constants)
  #:use-module (xmms2 data-conversion)
  #:export (make-float-payload
            make-int64-payload
            make-string-payload
            make-list-payload
            payload-length
            payload->float))

(define-syntax-rule (missing-generator name args ...)
  (define-public (name args ...)
    (throw 'xmms2/payload-generator-not-implemented 'name)))

(missing-generator make-binary-payload data)
(missing-generator make-collection-payload data)
(missing-generator make-dictionary-payload data)
(missing-generator make-error-payload data)

(define-public (make-unknown-payload data)
  (throw 'xmms2/incomplete-library-code
         '(The library has emitted make-unknown-payload which
               is completely useless and hints at the library
               missing support for a data type in the
               define-ipc-packet-generator macro from the
               (xmms2 ipc) module. Please report this bug!)))

(define *payload-tag-size* 4)
(define *payload-integer-size* 8)
(define *payload-float-size* 8)

(define (make-int64-payload value)
  (let ((rv (make-bytevector (+ *payload-integer-size* *payload-tag-size*) 0)))
    (bytevector-copy! TAG-INT64 0 rv 0 *payload-tag-size*)
    (int64-set! rv *payload-tag-size* value)
    rv))

(define (log2 value)
  (/ (log10 value) (log10 2)))

(define (frexp value)
  "Converts floating-point number to fractional and integral parts and returns
a pair containing the two: (fractional . exponent)"
  (if (zero? value)
      (cons 0 0)
      (let* ((value* (abs value))
             (exponent (inexact->exact (ceiling (log2 value*))))
             (divisor (if (< value* 1)
                          (/ 1 (ash 1 (* -1 exponent)))
                          (ash 1 exponent)))
             (fractional (exact->inexact (/ value divisor))))
        (if (>= (abs fractional) 1.0)
            (cons (/ fractional 2) (+ exponent 1))
            (cons fractional exponent)))))

(define INT32_MAX (- (ash 1 31) 1))
(define INT32_MIN (* -1 (ash 1 31)))

(define (make-float-payload value)
  (let* ((fe (frexp value))
         (fractional (car fe))
         (exponent (cdr fe))
         (mantissa (inexact->exact (truncate
                                    (if (positive? fractional)
                                        (* fractional INT32_MAX)
                                        (* -1 fractional INT32_MIN)))))
         (rv (make-bytevector (+ *payload-tag-size* *payload-float-size*))))
    (bytevector-copy! TAG-FLOAT 0 rv 0 *payload-tag-size*)
    (int32-set! rv 4 mantissa)
    (int32-set! rv 8 exponent)
    rv))

(define (payload->float bv offset)
  (let* ((m (int32-ref bv offset))
         (e (int32-ref bv (+ 4 offset)))
         (sign (if (>= m 0) 1 -1))
         (fractional (/ m (if (>= m 0) INT32_MAX INT32_MIN)))
         (factor (if (>= e 0) (ash 1 e) (/ (ash 1 (* -1 e))))))
    (exact->inexact (* sign fractional factor))))

(define (make-string-payload value)
  (let* ((str (string->utf8 value))
         (len (bytevector-length str))
         (data-offset (+ *payload-tag-size* 4))
         (rv (make-bytevector (+ data-offset 1 len) 0)))
    (uint32-set! rv *payload-tag-size* (+ 1 len))
    (bytevector-copy! TAG-STRING 0 rv 0 *payload-tag-size*)
    (bytevector-copy! str 0 rv data-offset len)
    rv))

(define* (make-list-payload lst #:key (restricted #f))
  (let loop ((rest (reverse lst))
             (acc '()))
    (if (null? rest)
        (let* ((size-offset (* 2 *payload-tag-size*))
               (length-size 4)
               (tag (make-bytevector (+ size-offset length-size))))
          (bytevector-copy! TAG-LIST 0 tag 0 *payload-tag-size*)
          (bytevector-copy! (or restricted TAG-NONE) 0
                            tag *payload-tag-size* *payload-tag-size*)
          (uint32-set! tag size-offset (length lst))
          (cons tag acc))
        (let ((cur (car rest)))
          (loop (cdr rest)
                (cond ((integer? cur)
                       (cons (make-int64-payload cur) acc))
                      ((string? cur)
                       (cons (make-string-payload cur) acc))
                      (else (throw 'xmms2/unknown-data-type cur))))))))

(define (payload-length p)
  (if (bytevector? p)
      (bytevector-length p)
      (apply + (map bytevector-length p))))
