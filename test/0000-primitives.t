;; -*- scheme -*-
;;
;; Copyright (c) 2013 xmms2-guile workers, All rights reserved.
;;
;; Terms for redistribution and use can be found in LICENCE.

(use-modules (taptest)
             (xmms2 core primitives))

;; Short-hand for producing ('foo . foo)
(define-syntax xx
  (lambda (x)
    (syntax-case x ()
      ((_ name)
       #'(cons (quote name) name)))))

(define status-constants (list (xx XMMS2-STATUS-PAUSED)
                               (xx XMMS2-STATUS-PLAYING)
                               (xx XMMS2-STATUS-STOPPED)))

(define type-constants (list (xx XMMS2-VALUE-BINARY)
                             (xx XMMS2-VALUE-COLLECTION)
                             (xx XMMS2-VALUE-DICTIONARY)
                             (xx XMMS2-VALUE-ERROR)
                             (xx XMMS2-VALUE-FLOAT)
                             (xx XMMS2-VALUE-INTEGER)
                             (xx XMMS2-VALUE-LIST)
                             (xx XMMS2-VALUE-NONE)
                             (xx XMMS2-VALUE-STRING)))

;; Helper, that combines values properly to define a set of tests, that check
;; that - for example - every integer constant in `type-constants' is unique.
;;
;; Those constants are "magic values" in the XMMS2 C library and _must_ be
;; unique, because they identify different types of data.
(define (permute-tests alist title guard test)
  (let next ((head alist))
    (cond
     ((null? head) #t)
     (else
      (for-each (lambda (y)
                  (let ((xkey (car (car head)))
                        (xval (cdr (car head)))
                        (ykey (car y))
                        (yval (cdr y)))
                    (if (guard xkey ykey)
                        (define-test (title xkey xval ykey yval)
                          (pass-if-true (test xval yval))))))
                head)
      (next (cdr head))))))

(define (different-keys? x y)
  (not (eq? x y)))

(define (not-equal? x y)
  (not (= x y)))

(define (title-constants-are-not-equal xkey xval ykey yval)
  (format #f "Constants are not equal: ~a (~d) <> ~a (~d)"
          xkey xval ykey yval))

;; Some math to calculate the number of possible combinations.
(define (fact n)
  (facti 1 n))

(define (facti acc n)
  (if (zero? n) acc
      (facti (* n acc) (1- n))))

(define (nok n k)
  (/ (fact n)
     (* (fact k) (fact (- n k)))))

(define (no2 n)
  (nok n 2))

(with-test-bundle (primitives constants)
  (let ((len-sc (length status-constants))
        (len-tc (length type-constants)))
    (plan (+ len-sc
             len-tc
             (no2 len-sc)
             (no2 len-tc))))
  (map (lambda (x)
         (let ((key (car x))
               (val (cdr x)))
           (define-test (format #f "constant: ~a" key)
             (pass-if-true (integer? val)))))
       (append status-constants type-constants))
  (permute-tests status-constants
                 title-constants-are-not-equal
                 different-keys?
                 not-equal?)
  (permute-tests type-constants
                 title-constants-are-not-equal
                 different-keys?
                 not-equal?))
