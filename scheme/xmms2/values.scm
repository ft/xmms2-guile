;; Copyright (c) 2016 xmms2-guile workers, All rights reserved.
;;
;; Terms for redistribution and use can be found in LICENCE.

(define-module (xmms2 values)
  #:use-module (srfi srfi-1)
  #:use-module (ice-9 iconv)
  #:use-module (rnrs bytevectors)
  #:use-module (xmms2 data-conversion)
  #:export (decode-url
            decode-and-strip
            encode-url
            strip-file))

(define *good-char*
  (char-set-union (ucs-range->char-set (char->integer #\a)
                                       (+ 1 (char->integer #\z)))
                  (ucs-range->char-set (char->integer #\A)
                                       (+ 1 (char->integer #\Z)))
                  (ucs-range->char-set (char->integer #\0)
                                       (+ 1 (char->integer #\9)))
                  (char-set #\:)
                  (char-set #\/)
                  (char-set #\-)
                  (char-set #\.)
                  (char-set #\_)))

(define (good-char? ch)
  (if (integer? ch)
      (or (and (>= ch #x61)
               (<= ch #x7a))
          (and (>= ch #x41)
               (<= ch #x5a))
          (= ch #x3a)
          (= ch #x2d)
          (= ch #x2e)
          (= ch #x2f)
          (= ch #x5f))
      (char-set-contains? *good-char* ch)))

(define (encode-url file)
  (let loop ((rest ((if (string? file)
                        (lambda (x) (bytevector->u8-list (string->utf8 x)))
                        bytevector->u8-list) file))
             (acc '()))
    (if (null? rest)
        (utf8->string (u8-list->bytevector (reverse acc)))
        (let ((this (car rest))
              (rest (cdr rest)))
          (cond ((= #x20 this)
                 (loop rest (cons #x2b acc)))
                ((good-char? this)
                 (loop rest (cons this acc)))
                (else (loop rest
                            (append! (reverse
                                      (append! (list #x25)
                                               (bytevector->u8-list
                                                (string->utf8
                                                 (number->string this 16)))))
                                     acc))))))))

(define (decode-url url)
  (let loop ((rest (string->list url)) (acc '()))
    (if (null? rest)
        (u8-list->bytevector (reverse acc))
        (let ((this (car rest))
              (rest (cdr rest)))
          (cond ((char=? this #\+) (loop rest (cons #x20 acc)))
                ((char=? this #\%)
                 (if (< (length rest) 2)
                     (throw x2/url-percent-escape-lacks-data (cons #\% rest)))
                 (let ((str (list->string (take rest 2))))
                   (loop (drop rest 2)
                         (cons (string->number str 16) acc))))
                (else (loop rest (cons (char->integer this) acc))))))))

(define (strip-file str)
  (if (string? str)
      (if (string-prefix? "file://" str)
          (substring str 7)
          str)
      (if (equal? (bytevector-ref str 0 7)
                  #vu8(102 105 108 101 58 47 47))
          (bytevector-ref str 7 (- (bytevector-length str) 7))
          str)))

(define (decode-and-strip url)
  (strip-file (decode-url url)))
