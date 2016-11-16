;; Copyright (c) 2016 xmms2-guile workers, All rights reserved.
;;
;; Terms for redistribution and use can be found in LICENCE.

(define-module (xmms2 data-conversion)
  #:use-module (rnrs bytevectors)
  #:export (bytevector-ref
            int32-ref
            int32-set!
            int32?
            uint32-ref
            uint32-set!
            uint32?
            int64-ref
            int64-set!
            int64?
            uint64-ref
            uint64-set!
            uint64?
            *uint32-max*
            *uint32-size*
            *uint64-max*
            *uint64-size*
            *int32-max*
            *int32-min*
            *int32-size*
            *int64-max*
            *int64-min*
            *int64-size*))

;; The octet order in XMMS2's protocol is big-endian. If it matters, all
;; procedures from this module will indeed use that particular order.
(define *endianness* 'big)

;; uint32_t

(define *uint32-max* (- (ash 1 32) 1))
(define *uint32-size* 4)

(define (uint32? data)
  (and (exact-integer? data)
       (<= data *uint32-max*)))

(define (uint32-ref bv offset)
"Return an unsigned 32 bit integer from a byte-vector starting at the byte at
OFFSET."
  (bytevector-u32-ref bv offset *endianness*))

(define (uint32-set! bv offset data)
"Set an unsigned 32 bit integer in a byte-vector starting at the byte at OFFSET
to DATA, which is that source unsigned integer."
  (bytevector-u32-set! bv offset data *endianness*))

;; uint64_t

(define *uint64-max* (- (ash 1 64) 1))
(define *uint64-size* 8)

(define (uint64? data)
  (and (exact-integer? data)
       (<= data *uint64-max*)))

(define (uint64-ref bv offset)
"Return an unsigned 64 bit integer from a byte-vector starting at the byte at
OFFSET."
  (bytevector-u64-ref bv offset *endianness*))

(define (uint64-set! bv offset data)
"Set an unsigned 64 bit integer in a byte-vector starting at the byte at OFFSET
to DATA, which is that source unsigned integer."
  (bytevector-u64-set! bv offset data *endianness*))

;; int32_t

(define *int32-max* (- (ash 1 31) 1))
(define *int32-min* (* -1 (ash 1 31)))
(define *int32-size* 4)

(define (int32? data)
  (and (exact-integer? data)
       (<= data *int32-max*)
       (>= data *int32-min*)))

(define (int32-ref bv offset)
  "Return an 32 bit integer from a byte-vector starting at the byte at OFFSET."
  (bytevector-s32-ref bv offset *endianness*))

(define (int32-set! bv offset data)
  "Set an 32 bit integer in a byte-vector starting at the byte at OFFSET to DATA,
which is that source integer."
  (bytevector-s32-set! bv offset data *endianness*))

;; int64_t

(define *int64-max* (- (ash 1 63) 1))
(define *int64-min* (* -1 (ash 1 63)))
(define *int64-size* 8)

(define (int64? data)
  (and (exact-integer? data)
       (<= data *int64-max*)
       (>= data *int64-min*)))

(define (int64-ref bv offset)
"Return an 64 bit integer from a byte-vector starting at the byte at OFFSET."
  (bytevector-s64-ref bv offset *endianness*))

(define (int64-set! bv offset data)
"Set an 64 bit integer in a byte-vector starting at the byte at OFFSET to DATA,
which is that source integer."
  (bytevector-s64-set! bv offset data *endianness*))

(define (bytevector-ref bv offset len)
  (let* ((bl (bytevector-length bv))
         (final-length (if (<= bl offset)
                           0
                           (min (- bl offset) len)))
         (value (make-bytevector final-length)))
    (bytevector-copy! bv offset value 0 final-length)
    value))
