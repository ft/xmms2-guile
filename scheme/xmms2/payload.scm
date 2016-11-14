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
            make-dictionary-payload
            payload-length
            payload->float*
            payload->float))

(define-syntax-rule (missing-generator name args ...)
  (define-public (name args ...)
    (throw 'xmms2/payload-generator-not-implemented 'name)))

(missing-generator make-binary-payload data)
(missing-generator make-collection-payload data)
(missing-generator make-error-payload data)

(define-public (make-unknown-payload data)
  (throw 'xmms2/incomplete-library-code
         '(The library has emitted make-unknown-payload which
               is completely useless and hints at the library
               missing support for a data type in the
               define-ipc-packet-generator macro from the
               (xmms2 ipc) module. Please report this bug!)))

(define *payload-tag-size* 4)
(define *payload-size-size* 4)
(define *payload-integer-size* 8)
(define *payload-float-size* 8)

(define (dictionary? data)
  (and (list? data)
       (pair? (car data))
       (string? (caar data))))

(define (non-complex-number? data)
  (and (number? data) (not (complex? data))))

(define (make-value-payload data)
  (cond ((int64? data) (make-int64-payload data))
        ((non-complex-number? data) (make-float-payload data))
        ((string? data) (make-string-payload data))
        ((dictionary? data) (make-dictionary-payload data))
        ((list? data) (make-list-payload data))
        (else (throw 'xmms2/unknown-data-type data))))

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

(define (make-float-payload value)
  (let* ((fe (frexp value))
         (fractional (car fe))
         (exponent (cdr fe))
         (mantissa (inexact->exact (truncate
                                    (if (positive? fractional)
                                        (* fractional *int32-max*)
                                        (* -1 fractional *int32-min*)))))
         (rv (make-bytevector (+ *payload-tag-size* *payload-float-size*))))
    (bytevector-copy! TAG-FLOAT 0 rv 0 *payload-tag-size*)
    (int32-set! rv 4 mantissa)
    (int32-set! rv 8 exponent)
    rv))

(define (payload->float* bv offset)
  (let* ((m (int32-ref bv offset))
         (e (int32-ref bv (+ 4 offset)))
         (sign (if (>= m 0) 1 -1))
         (fractional (/ m (if (>= m 0) *int32-max* *int32-min*)))
         (factor (if (>= e 0) (ash 1 e) (/ (ash 1 (* -1 e))))))
    (exact->inexact (* sign fractional factor))))

(define (payload->float bv)
  (payload->float* bv *payload-tag-size*))

(define (make-string-payload value)
  (let* ((str (string->utf8 value))
         (len (bytevector-length str))
         (data-offset (+ *payload-tag-size* *payload-size-size*))
         (rv (make-bytevector (+ data-offset 1 len) 0)))
    (uint32-set! rv *payload-tag-size* (+ 1 len))
    (bytevector-copy! TAG-STRING 0 rv 0 *payload-tag-size*)
    (bytevector-copy! str 0 rv data-offset len)
    rv))

(define (make-list-header len type)
  (let* ((size-offset (* 2 *payload-tag-size*))
         (header (make-bytevector (+ size-offset *payload-size-size*))))
    (bytevector-copy! TAG-LIST 0 header 0 *payload-tag-size*)
    (bytevector-copy! (or type TAG-NONE) 0
                      header *payload-tag-size* *payload-tag-size*)
    (uint32-set! header size-offset len)
    header))

(define (cons-or-append! a b)
  (if (list? a)
      (append! a b)
      (cons a b)))

(define* (make-list-payload lst #:key (restricted #f))
  ;; Restricted list payload should filter the input list for the desired type
  ;; elements first.
  (let loop ((rest (reverse lst)) (acc '()))
    (if (null? rest)
        (cons (make-list-header (length lst) (or restricted TAG-NONE)) acc)
        (loop (cdr rest)
              (cons-or-append! (make-value-payload (car rest)) acc)))))

(define (make-dictionary-header len)
  (let* ((header (make-bytevector (+ *payload-tag-size* *payload-size-size*))))
    (bytevector-copy! TAG-DICTIONARY 0 header 0 *payload-tag-size*)
    (uint32-set! header *payload-tag-size* len)
    header))

(define* (make-dictionary-payload alist #:key (restricted #f))
  ;; There is no restricted tag in xmms2's dictionary implementation, but lets
  ;; just give the serializer the same API as the list type, for symmetry.
  (let loop ((rest (reverse alist)) (acc '()))
    (if (null? rest)
        (cons (make-dictionary-header (length alist)) acc)
        (let* ((key (caar rest))
               (value (cdar rest)))
          (loop (cdr rest)
                (cons (make-string-payload key)
                      (cons-or-append! (make-value-payload value) acc)))))))

(define (payload-length p)
  (if (bytevector? p)
      (bytevector-length p)
      (apply + (map bytevector-length p))))
