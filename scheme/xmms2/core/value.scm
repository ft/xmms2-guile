;; Copyright (c) 2013 xmms2-guile workers, All rights reserved.
;;
;; Terms for redistribution and use can be found in LICENCE.

(define-module (xmms2 core value)
  #:use-module (xmms2 core primitives)
  #:export (make-xmms2-value
            integer->status
            integer->value-type
            xmms2-decode-url
            xmms2-result->value
            xmms2-value->scheme-data))

(define (make-xmms2-value)
  (xmms2:type/make-value))

(define (from-symbol-map map key unknown-symbol)
  (let ((value (assv key map)))
    (if value
        (cdr value)
        unknown-symbol)))

(define status-map
  `((,XMMS2-STATUS-PLAYING . playing)
    (,XMMS2-STATUS-PAUSED . paused)
    (,XMMS2-STATUS-STOPPED . stopped)))

(define (integer->status x)
  (from-symbol-map status-map x 'unknown-status))

(define type-map
  `((,XMMS2-VALUE-BINARY . binary)
    (,XMMS2-VALUE-BITBUFFER . bitbuffer)
    (,XMMS2-VALUE-COLLECTION . collection)
    (,XMMS2-VALUE-DICTIONARY . dictionary)
    (,XMMS2-VALUE-ERROR . erroneous-value)
    (,XMMS2-VALUE-FLOAT . float)
    (,XMMS2-VALUE-INTEGER . integer)
    (,XMMS2-VALUE-LIST . list)
    (,XMMS2-VALUE-NONE . empty-value)
    (,XMMS2-VALUE-STRING . string)))

(define (integer->value-type x)
  (from-symbol-map type-map x 'unknown-value-type))

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
  (or (eq? x 'empty-value)
      (eq? x 'erroneous-value)
      (eq? x 'unknown-value-type)))

(define (xmms2-value->scheme-data x)
  (let ((type (type-of-value x)))
    (cond
      ((trivial-type? type) type)
      ((eq? type 'integer)
       (xmms2:primitive/value->integer x))
      ((eq? type 'string)
       (xmms2:primitive/value->string x))
      ((eq? type 'list)
       (xmms2:primitive/value->list x))
      ((eq? type 'dictionary)
       (xmms2:primitive/value->dictionary x))
      ((eq? type 'binary)
       (xmms2:primitive/value->binary x))
      (else `(XMMS2-UNSUPPORTED-DATA-TYPE . ,type)))))

(define (xmms2-decode-url url)
  (xmms2-value->scheme-data (xmms2:primitive/decode-url url)))
