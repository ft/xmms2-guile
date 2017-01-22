;; Copyright (c) 2016 xmms2-guile workers, All rights reserved.
;;
;; Terms for redistribution and use can be found in LICENCE.

(define-module (xmms2 values)
  #:use-module (srfi srfi-1)
  #:export (decode-url
            decode-and-strip
            encode-url
            strip-file))

(define *good-char*
  (char-set-union (ucs-range->char-set (char->integer #\a)
                                       (+ 1 (char->integer #\z)))
                  (ucs-range->char-set (char->integer #\A)
                                       (+ 1 (char->integer #\Z)))
                  (char-set #\:)
                  (char-set #\/)
                  (char-set #\-)
                  (char-set #\.)
                  (char-set #\_)))

(define (encode-url file)
  (let loop ((rest (string->list file)) (acc '()))
    (if (null? rest)
        (list->string (reverse acc))
        (let ((this (car rest))
              (rest (cdr rest)))
          (cond ((char=? #\space this)
                 (loop rest (cons #\+ acc)))
                ((char-set-contains? *good-char* this)
                 (loop rest (cons this acc)))
                (else (loop rest (append! (reverse
                                           (append!
                                            (list #\%)
                                            (string->list
                                             (number->string (char->integer this)
                                                             16))))
                                          acc))))))))

(define (decode-url url)
  (let loop ((rest (string->list url)) (acc '()))
    (if (null? rest)
        (list->string (reverse acc))
        (let ((this (car rest))
              (rest (cdr rest)))
          (cond ((char=? this #\+) (loop rest (cons #\space acc)))
                ((char=? this #\%)
                 (if (< (length rest) 2)
                     (throw x2/url-percent-escape-lacks-data (cons #\% rest)))
                 (let ((str (list->string (take rest 2))))
                   (loop (drop rest 2)
                         (cons (integer->char (string->number str 16)) acc))))
                (else (loop rest (cons this acc))))))))

(define (strip-file str)
  (if (string-prefix? "file://" str)
      (substring str 7)
      str))

(define (decode-and-strip url)
  (strip-file (decode-url url)))
