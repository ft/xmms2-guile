;; Copyright (c) 2016 xmms2-guile workers, All rights reserved.
;;
;; Terms for redistribution and use can be found in LICENCE.

(define-module (xmms2 data-conversion)
  #:use-module (rnrs bytevectors)
  #:export (int32-ref
            int32-set!
            uint32-ref
            uint32-set!
            int64-ref
            int64-set!
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

(define (int64-ref bv offset)
"Return an 64 bit integer from a byte-vector starting at the byte at OFFSET."
  (bytevector-s64-ref bv offset *endianness*))

(define (int64-set! bv offset data)
"Set an 64 bit integer in a byte-vector starting at the byte at OFFSET to DATA,
which is that source integer."
  (bytevector-s64-set! bv offset data *endianness*))

(define (int32-ref bv offset)
  "Return an 32 bit integer from a byte-vector starting at the byte at OFFSET."
  (bytevector-s32-ref bv offset *endianness*))

(define (int32-set! bv offset data)
  "Set an 32 bit integer in a byte-vector starting at the byte at OFFSET to DATA,
which is that source integer."
  (bytevector-s32-set! bv offset data *endianness*))
