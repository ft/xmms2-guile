;; Copyright (c) 2015 xmms2-guile workers, All rights reserved.
;;
;; Terms for redistribution and use can be found in LICENCE.

(define-module (xmms2 payload)
  #:use-module (ice-9 optargs)
  #:use-module (rnrs bytevectors)
  #:use-module (xmms2 constants)
  #:use-module (xmms2 data-conversion)
  #:export (make-int64-payload
            make-string-payload
            make-list-payload
            payload-length
            payload-length*
            *payload-tag-size*
            *integer-size*))

(define *payload-tag-size* 4)
(define *integer-size* 8)

(define (make-int64-payload value)
  (let ((rv (make-bytevector 8 0)))
    (uint64-set! rv 0 value)
    rv))

(define (make-string-payload value)
  (let* ((str (string->utf8 value))
         (len (bytevector-length str))
         (rv (make-bytevector (+ 5 len) 0)))
    (uint32-set! rv 0 (+ 1 len))
    (bytevector-copy! str 0 rv 4 len)
    rv))

(define* (make-list-payload lst #:key (restricted #f))
  (let loop ((rest (reverse lst))
             (acc '()))
    (if (null? rest)
        (if (null? acc)
            '()
            (if restricted
                (cons* restricted (make-int64-payload (length lst)) acc)
                (cons* (make-int64-payload (length lst)) acc)))
        (let ((cur (car rest)))
          (loop (cdr rest)
                (cond ((integer? cur)
                       (cons* TAG-INT64 (make-int64-payload cur) acc))
                      ((string? cur)
                       (cons* TAG-STRING (make-string-payload cur) acc))
                      (else (throw 'xmms2/unknown-data-type cur))))))))

(define (payload-length p)
  (if (bytevector? p)
      (bytevector-length p)
      (apply + (map bytevector-length p))))

(define (payload-length* p)
  (+ *payload-tag-size* (payload-length p)))
