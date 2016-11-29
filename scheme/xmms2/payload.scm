;; Copyright (c) 2015 xmms2-guile workers, All rights reserved.
;;
;; Terms for redistribution and use can be found in LICENCE.

(define-module (xmms2 payload)
  #:use-module (ice-9 optargs)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-11)
  #:use-module (rnrs bytevectors)
  #:use-module (xmms2 constants)
  #:use-module (xmms2 data-conversion)
  #:use-module (xmms2 jump-table)
  #:export (make-float-payload
            make-int64-payload
            make-string-payload
            make-list-payload
            make-dictionary-payload
            make-error-payload
            make-binary-payload
            make-value-payload
            payload-combine
            payload-length
            payload->value*
            payload->value
            payload->dictionary*
            payload->dictionary
            payload->error*
            payload->error
            payload->binary*
            payload->binary
            payload->float*
            payload->float
            payload->int64*
            payload->int64
            payload->string*
            payload->string
            payload->list*
            payload->list))

(define-syntax-rule (missing-generator name args ...)
  (define-public (name args ...)
    (throw 'xmms2/payload-generator-not-implemented 'name)))

(missing-generator make-collection-payload data)

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

(define (bytevector-looks-reasonable? bv min expected-type)
  (and (>= (bytevector-length bv) (+ *payload-tag-size* min))
       (let ((actual-type (uint32-ref bv 0)))
         (= actual-type expected-type))))

(define (dictionary? data)
  (and (list? data)
       (not (null? data))
       (pair? (car data))
       (symbol? (caar data))))

(define (non-complex-number? data)
  (and (number? data) (zero? (imag-part data))))

(define (make-value-payload data)
  (cond ((int64? data) (make-int64-payload data))
        ((non-complex-number? data) (make-float-payload data))
        ((string? data) (make-string-payload data))
        ((dictionary? data) (make-dictionary-payload data))
        ((list? data) (make-list-payload data))
        ((bytevector? data) (make-binary-payload data))
        (else (throw 'xmms2/unknown-data-type data))))

(define (make-binary-payload value)
  (let ((hdr (make-bytevector (+ *payload-tag-size* *payload-size-size*)
                              0)))
    (bytevector-copy! TAG-BINARY 0 hdr 0 *payload-tag-size*)
    (uint32-set! hdr *payload-tag-size* (bytevector-length value))
    (list hdr value)))

(define (payload->binary* bv offset)
  (let* ((offset (+ *payload-tag-size* offset))
         (size (uint32-ref bv offset))
         (rv (make-bytevector size)))
    (bytevector-copy! bv (+ offset *payload-size-size*) rv 0 size)
    (values rv (+ *payload-tag-size* *payload-size-size* size))))

(define (payload->binary bv)
  (if (bytevector-looks-reasonable? bv *payload-size-size* TYPE-BINARY)
      (let-values (((value . rest) (payload->binary* bv 0)))
        value)
      0))

(define (make-int64-payload value)
  (let ((rv (make-bytevector (+ *payload-integer-size* *payload-tag-size*) 0)))
    (bytevector-copy! TAG-INT64 0 rv 0 *payload-tag-size*)
    (int64-set! rv *payload-tag-size* value)
    rv))

(define (payload->int64* bv offset)
  (values (uint64-ref bv (+ *payload-tag-size* offset))
          (+ *payload-tag-size* *uint64-size*)))

(define (payload->int64 bv)
  (if (bytevector-looks-reasonable? bv *int64-size* TYPE-INT64)
      (let-values (((value . rest) (payload->int64* bv 0)))
        value)
      0))

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
         (mantissa (inexact->exact (round
                                    (if (positive? fractional)
                                        (* fractional *int32-max*)
                                        (* -1 fractional *int32-min*)))))
         (rv (make-bytevector (+ *payload-tag-size* *payload-float-size*))))
    (bytevector-copy! TAG-FLOAT 0 rv 0 *payload-tag-size*)
    (int32-set! rv 4 mantissa)
    (int32-set! rv 8 exponent)
    rv))

(define (payload->float* bv offset)
  (let* ((offset (+ *payload-tag-size* offset))
         (m (int32-ref bv offset))
         (e (int32-ref bv (+ 4 offset)))
         (sign (if (>= m 0) 1 -1))
         (fractional (/ m (if (>= m 0) *int32-max* *int32-min*)))
         (factor (if (>= e 0) (ash 1 e) (/ (ash 1 (* -1 e))))))
    (values (exact->inexact (* sign fractional factor))
            (+ *payload-tag-size* *payload-float-size*))))

(define (payload->float bv)
  (if (bytevector-looks-reasonable? bv (* 2 *int32-size*) TYPE-FLOAT)
      (let-values (((value . rest) (payload->float* bv 0)))
        value)
      0.0))

(define (make-dict-key-payload value)
  (let* ((str (string->utf8 (symbol->string value)))
         (len (bytevector-length str))
         (rv (make-bytevector (+ *payload-size-size* 1 len) 0)))
    (uint32-set! rv 0 (+ 1 len))
    (bytevector-copy! str 0 rv *payload-size-size* len)
    rv))

(define (make-string-payload value)
  (let* ((str (string->utf8 value))
         (len (bytevector-length str))
         (data-offset (+ *payload-tag-size* *payload-size-size*))
         (rv (make-bytevector (+ data-offset 1 len) 0)))
    (uint32-set! rv *payload-tag-size* (+ 1 len))
    (bytevector-copy! TAG-STRING 0 rv 0 *payload-tag-size*)
    (bytevector-copy! str 0 rv data-offset len)
    rv))

(define (make-error-payload value)
  (let ((bv (make-string-payload value)))
    (bytevector-copy! TAG-ERROR 0 bv 0 *payload-tag-size*)
    bv))

(define (payload->dict-key bv offset)
  (let* ((pl (uint32-ref bv offset))
         (len (- pl 1)))
    (values
     (if (<= len 0) #f
         (string->symbol
          (utf8->string
           (bytevector-ref bv (+ *payload-size-size* offset) len))))
     (+ *payload-size-size* pl))))

(define (payload->string* bv offset)
  (let* ((offset (+ *payload-tag-size* offset))
         (pl (uint32-ref bv offset))
         (len (- pl 1)))
    (values
     (if (<= len 0) ""
         (utf8->string (bytevector-ref bv (+ *payload-size-size* offset) len)))
     (+ *payload-tag-size* *payload-size-size* pl))))

(define (payload->string bv)
  (if (bytevector-looks-reasonable? bv (+ *payload-size-size* 1) TYPE-STRING)
      (let-values (((value . rest) (payload->string* bv 0)))
        value)
      ""))

(define payload->error* payload->string*)

(define (payload->error bv)
  (if (bytevector-looks-reasonable? bv (+ *payload-size-size* 1) TYPE-ERROR)
      (let-values (((value . rest) (payload->error* bv 0)))
        value)
      ""))

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

(define (payload->list* bv offset)
  (let ((offset (+ *payload-tag-size* offset))
        (bl (bytevector-length bv)))
    (let loop ((left (uint32-ref bv (+ offset *payload-size-size*)))
               (consumed (+ *payload-tag-size* *payload-size-size*))
               (acc '()))
      (if (or (zero? left) (>= (+ offset consumed) bl))
          (values (reverse acc) (+ *payload-tag-size* consumed))
          (let-values (((value consumed-bytes)
                        (payload->value* bv (+ consumed offset))))
            (loop (- left 1) (+ consumed consumed-bytes) (cons value acc)))))))

(define (payload->list bv)
  (if (bytevector-looks-reasonable? bv
                                    (+ *payload-tag-size* *payload-size-size*)
                                    TYPE-LIST)
      (let-values (((value . rest) (payload->list* bv 0)))
        value)
      '()))

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
                (cons (make-dict-key-payload key)
                      (cons-or-append! (make-value-payload value) acc)))))))

(define (payload->dictionary* bv offset)
  (let ((offset (+ *payload-tag-size* offset))
        (bl (bytevector-length bv)))
    (let loop ((left (uint32-ref bv offset))
               (consumed *payload-size-size*)
               (acc '()))
      (if (or (zero? left) (>= (+ offset consumed) bl))
          (values (reverse acc) (+ *payload-tag-size* consumed))
          (let-values (((key key-bytes)
                        (payload->dict-key bv (+ consumed offset))))
            (let-values (((value value-bytes)
                          (payload->value* bv (+ consumed key-bytes offset))))
              (loop (- left 1)
                    (+ consumed key-bytes value-bytes)
                    (cons (cons key value) acc))))))))

(define (payload->dictionary bv)
  (if (bytevector-looks-reasonable? bv *payload-size-size* TYPE-DICTIONARY)
      (let-values (((value . rest) (payload->dictionary* bv 0)))
        value)
      '()))

(define (payload-length p)
  (if (bytevector? p)
      (bytevector-length p)
      (apply + (map bytevector-length p))))

(define (payload-combine p)
  (if (bytevector? p) p
      (let ((value (make-bytevector (payload-length p))))
        (let loop ((rest p) (offset 0))
          (if (null? rest)
              value
              (let* ((this (car rest))
                     (bl (bytevector-length this)))
                (bytevector-copy! this 0 value offset bl)
                (loop (cdr rest) (+ offset bl))))))))

(define deserializer-table
  (make-jump-table (table (TYPE-INT64 payload->int64*)
                          (TYPE-FLOAT payload->float*)
                          (TYPE-STRING payload->string*)
                          (TYPE-DICTIONARY payload->dictionary*)
                          (TYPE-LIST payload->list*)
                          (TYPE-BINARY payload->binary*)
                          (TYPE-ERROR payload->error*))
                   #:others (lambda (a b) (values #f 1))
                   #:out-of-range (lambda (a b) (values #f 1))))

(define (payload->value* data offset)
  (let ((type (uint32-ref data offset)))
    (apply-jump-table deserializer-table
                      type data offset)))

(define (payload->value data)
  (let-values (((value . rest) (payload->value* data 0)))
    value))
