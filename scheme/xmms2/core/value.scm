;; Copyright (c) 2013 xmms2-guile workers, All rights reserved.
;;
;; Terms for redistribution and use can be found in LICENCE.

(define-module (xmms2 core value)
  #:use-module (xmms2 core primitives)
  #:export (make-xmms2-value
            integer->value-type
            xmms2-result->value
            xmms2-value->scheme-data))

(define (make-xmms2-value)
  (xmms2:type/make-value))

(define type-map
  `((,XMMS2-VALUE-NONE . XMMS2-VALUE-NONE)
    (,XMMS2-VALUE-ERROR . XMMS2-VALUE-ERROR)
    (,XMMS2-VALUE-INTEGER . XMMS2-VALUE-INTEGER)))

(define (integer->value-type x)
  (let ((type (assv x type-map)))
    (if type
        (cdr type)
        'XMMS2-VALUE-UNKNOWN)))

(define (type-of-value x)
  (integer->value-type (xmms2:primitive/type-of-value x)))

;; When this is used, it is important that the return value exits scope with
;; the parent result. You MUST NOT do this:
;;
;; (define v (xmms2-result->value (xmms2:primitive/status connection)))
;;
;; Because the XMMS2 client library frees the together with the parent result
;; container.
(define (xmms2-result->value x)
  (xmms2:primitive/result->scheme x))

(define (trivial-type? x)
  (or (eq? x 'XMMS2-VALUE-NONE)
      (eq? x 'XMMS2-VALUE-ERROR)
      (eq? x 'XMMS2-VALUE-UNKNOWN)))

(define (xmms2-value->scheme-data x)
  (let ((type (type-of-value x)))
    (cond
      ((trivial-type? type) type)
      ((eq? type 'XMMS2-VALUE-INTEGER)
       (xmms2:primitive/value->integer x))
      (else 'XMMS2-UNSUPPORTED-DATA-TYPE))))
