;; Copyright (c) 2017 xmms2-guile workers, All rights reserved.
;;
;; Terms for redistribution and use can be found in doc/LICENCE.

(define-module (documentation combine-markdown)
  #:use-module (rnrs io ports)
  #:use-module (ice-9 optargs)
  #:use-module (genipc utilities)
  #:export (combine-markdown))

(define (last-char str)
  (let ((len (string-length str)))
    (substring str (- len 1) len)))

(define (include-tag? str)
  (and (>= (string-length str) 9)
       (string= (substring str 0 9)
                "@include ")))

(define (include? str)
  (and (include-tag? str)
       (string= (last-char str) "@")))

(define (include-file str)
  (substring str 9 (- (string-length str) 1)))

(define (get-stdin-line)
  (get-line (current-input-port)))

(define* (cat-file input)
  (with-input-from-file input
    (lambda ()
      (let loop ((line (get-stdin-line)))
        (cond ((eof-object? line) #t)
              (else (display line) (newline)
                    (loop (get-stdin-line))))))))

(define* (combine-markdown input #:key (topdir "."))
  (with-input-from-file input
    (lambda ()
      (let loop ((line (get-stdin-line)))
        (cond ((eof-object? line) #t)
              ((include? line)
               (cat-file (cat topdir "/" (include-file line)))
               (loop (get-stdin-line)))
              (else (display line) (newline)
                    (loop (get-stdin-line))))))))
