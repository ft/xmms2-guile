;; Copyright (c) 2016 xmms2-guile workers, All rights reserved.
;;
;; Terms for redistribution and use can be found in LICENCE.

(define-module (xmms2 data-conversion)
  #:use-module (rnrs bytevectors)
  #:export (uint32-ref
            uint32-set!
            uint64-ref
            uint64-set!))

;; The octet order in XMMS2's protocol is big-endian. If it matters, all
;; procedures from this module will indeed use that particular order.
(define *endianness* 'big)

(define (uint32-ref bv offset)
"Return an unsigned 32 bit integer from a byte-vector starting at the byte at
OFFSET."
  (bytevector-u32-ref bv offset *endianness*))

(define (uint32-set! bv offset data)
"Set an unsigned 32 bit integer in a byte-vector starting at the byte at OFFSET
to DATA, which is that source unsigned integer."
  (bytevector-u32-set! bv offset data *endianness*))

(define (uint64-ref bv offset)
"Return an unsigned 64 bit integer from a byte-vector starting at the byte at
OFFSET."
  (bytevector-u64-ref bv offset *endianness*))

(define (uint64-set! bv offset data)
"Set an unsigned 64 bit integer in a byte-vector starting at the byte at OFFSET
to DATA, which is that source unsigned integer."
  (bytevector-u64-set! bv offset data *endianness*))
