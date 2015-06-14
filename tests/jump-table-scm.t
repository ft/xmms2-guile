;; -*- scheme -*-

;; Copyright (c) 2015 xmms2-guile workers, All rights reserved.
;;
;; Terms for redistribution and use can be found in LICENCE.

(use-modules (test tap)
             (xmms2 jump-table))

(primitive-load "tests/test-tap-cfg.scm")

(define simple (make-jump-table (table (0 car)
                                       (3 cadr))
                                #:others cons))

(define large (make-jump-table (table (0 car)
                                      (5 cadr))
                               #:length 64
                               #:others cons
                               #:out-of-range list))

(define with-offset (make-jump-table (table (offset 4)
                                            (4 car)
                                            (5 cadr))
                                     #:others cons))

(define with-transformer (make-jump-table (table (512 car)
                                                 (1024 cadr))
                                          #:others cons
                                          #:index-transformer (lambda (x)
                                                                (ash x -10))))

(force-import (xmms2 jump-table) jt/table)

(with-fs-test-bundle
 (plan 19)
 (define-test "simple: check size"
   (pass-if-= (vector-length (jt/table simple))
              4))
 (define-test "simple: try and find car"
   (pass-if-equal? (apply-jump-table simple 0 '(a b))
                   'a))
 (define-test "simple: try and find cadr"
   (pass-if-equal? (apply-jump-table simple 3 '(a b))
                   'b))
 (define-test "simple: miss both car and cadr (#:others)"
   (pass-if-equal? (apply-jump-table simple 1 'a 'b)
                   '(a . b)))
 (define-test "simple: miss both car and cadr by a lot (#:others)"
   (pass-if-equal? (apply-jump-table simple 666 'a 'b)
                   '(a . b)))
 (define-test "large: check size"
   (pass-if-= (vector-length (jt/table large))
              64))
 (define-test "large: try and find car"
   (pass-if-equal? (apply-jump-table large 0 '(a b))
                   'a))
 (define-test "large: try and find cadr"
   (pass-if-equal? (apply-jump-table large 5 '(a b))
                   'b))
 (define-test "large: miss both car and cadr (#:others)"
   (pass-if-equal? (apply-jump-table large 3 'a 'b)
                   '(a . b)))
 (define-test "large: miss both car and cadr by a bit more (#:others)"
   (pass-if-equal? (apply-jump-table large 56 'a 'b)
                   '(a . b)))
 (define-test "large: miss both car and cadr by a lot (#:out-of-range)"
   (pass-if-equal? (apply-jump-table large 666 'a 'b)
                   '(a b)))
 (define-test "with-offset: check size"
   (pass-if-= (vector-length (jt/table with-offset))
              2))
 (define-test "with-offset: try and find car"
   (pass-if-equal? (apply-jump-table with-offset 0 '(a b))
                   'a))
 (define-test "with-offset: try and find cadr"
   (pass-if-equal? (apply-jump-table with-offset 1 '(a b))
                   'b))
 (define-test "with-offset: miss both car and cadr (#:others)"
   (pass-if-equal? (apply-jump-table with-offset 2 'a 'b)
                   '(a . b)))
 (define-test "with-transformer: check size"
   (pass-if-= (vector-length (jt/table with-transformer))
              2))
 (define-test "with-transformer: try and find car"
   (pass-if-equal? (apply-jump-table with-transformer 512 '(a b))
                   'a))
 (define-test "with-transformer: try and find cadr"
   (pass-if-equal? (apply-jump-table with-transformer 1024 '(a b))
                   'b))
 (define-test "with-transformer: miss both car and cadr (#:others)"
   (pass-if-equal? (apply-jump-table with-transformer 2048 'a 'b)
                   '(a . b))))
