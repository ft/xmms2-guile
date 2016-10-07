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
            payload-length*))

(define-syntax-rule (missing-generator name args ...)
  (define-public (name args ...)
    (throw 'xmms2/payload-generator-not-implemented 'name)))

(missing-generator make-binary-payload data)
(missing-generator make-collection-payload data)
(missing-generator make-dictionary-payload data)
(missing-generator make-error-payload data)
(missing-generator make-float-payload data)

(define-public (make-unknown-payload data)
  (throw 'xmms2/incomplete-library-code
         '(The library has emitted make-unknown-payload which
               is completely useless and hints at the library
               missing support for a data type in the
               define-ipc-packet-generator macro from the
               (xmms2 ipc) module. Please report this bug!)))

(define *payload-tag-size* 4)
(define *integer-size* 8)

(define (make-int64-payload value)
  (let ((rv (make-bytevector (+ *integer-size* *payload-tag-size*) 0)))
    (bytevector-copy! TAG-INT64 0 rv 0 *payload-tag-size*)
    (int64-set! rv *payload-tag-size* value)
    rv))

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

(define (payload-length* p)
  (+ *payload-tag-size* (payload-length p)))
