;; Copyright (c) 2015 xmms2-guile workers, All rights reserved.
;;
;; Terms for redistribution and use can be found in LICENCE.

(define-module (xmms2 header)
  #:use-module (rnrs bytevectors)
  #:use-module (xmms2 constants)
  #:use-module (xmms2 data-conversion)
  #:use-module (xmms2 payload)
  #:export (make-protocol-header
            header->payload-length
            header->object-type
            header->command-id))

(define *offset-object-type* 0)
(define *offset-command-id* 4)
(define *offset-cookie* 8)
(define *offset-payload-length* 12)

(define (header->object-type header)
  "Return the object-type portion of a byte-vector, carrying an XMMS2 protocol
header. The return value is an unsigned integer."
  (uint32-ref header *offset-object-type*))

(define (header->command-id header)
  "Return the command-identifier portion of a byte-vector, carrying an XMMS2
protocol header. The return value is an unsigned integer."
  (uint32-ref header *offset-command-id*))

(define (header->cookie result)
  "Return the cookie portion of a byte-vector, carrying an XMMS2 protocol
header. The return value is an unsigned integer."
  (uint32-ref header *offset-cookie*))

(define (header->payload-length header)
  "Return the payload-length portion of a byte-vector, carrying an XMMS2
protocol header. The return value is an unsigned integer."
  (uint32-ref header *offset-payload-length*))

(define (make-protocol-header type cmd-id pl-length)
  "Return an XMMS2 protocol header, using TYPE, CMD-ID and PL-LENGTH to fill
the type, command-identifier and payload-length information."
  (let ((bv (make-bytevector 16 0)))
    (uint32-set! bv *offset-object-type* type)
    (uint32-set! bv *offset-command-id* cmd-id)
    (uint32-set! bv *offset-payload-length* pl-length)
    bv))
