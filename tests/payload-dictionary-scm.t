;; -*- scheme -*-

;; Copyright (c) 2016 xmms2-guile workers, All rights reserved.
;;
;; Terms for redistribution and use can be found in LICENCE.

(use-modules (test tap)
             (test payload)
             (test setup)
             (xmms2 payload))

(init-test-tap!)

(define-syntax test-><-dictionary?
  (syntax-rules ()
    ((_ lst)
     (begin (perform-payload-><-test make-dictionary-payload
                                     payload->dictionary
                                     lst pass-if-equal?)
            (perform-payload-><-test make-value-payload
                                     payload->dictionary
                                     lst pass-if-equal?)
            (perform-payload-><-test make-dictionary-payload
                                     payload->value
                                     lst pass-if-equal?)
            (perform-payload-><-test make-value-payload
                                     payload->value
                                     lst pass-if-equal?)))))

(define *tests-per-back-and-forth* 4)

(with-fs-test-bundle
 (plan (+ 4 (* 3 *tests-per-back-and-forth*)))

 (define-test "Dictionary payload '() looks good"
   (pass-if-payload-equal? (make-dictionary-payload '())
                           #vu8(0 0 0 7 0 0 0 0)))

 (define-test "Dictionary payload '((foo . 23)) looks good"
   (pass-if-payload-equal?
    (make-dictionary-payload '((foo . 23)))
    #vu8(0 0 0 7 0 0 0 1 0 0 0 4 102 111 111 0 0 0 0 2 0 0 0 0 0 0 0 23)))

 (define-test "Dictionary payload '((foo . 23) (bar . \"bar\")) looks good"
   (pass-if-payload-equal?
    (make-dictionary-payload '((foo . 23) (bar . "bar")))
    #vu8(0 0 0 7 0 0 0 2 0 0 0 4 102 111 111 0
                         0 0 0 2 0 0 0 0 0 0 0 23
                         0 0 0 4 98 97 114
                         0 0 0 0 3 0 0 0 4 98 97 114 0)))

 (define-test "Dictionary payload '((foo . 23) (bar . \"bar\") (baz 42 23 666)) looks good"
   (pass-if-payload-equal?
    (make-dictionary-payload '((foo . 23) (bar . "bar") (baz 42 23 666)))
    #vu8(0 0 0 7 0 0 0 3 0 0 0 4 102 111 111 0
                         0 0 0 2 0 0 0 0 0 0 0 23
                         0 0 0 4 98 97 114 0
                         0 0 0 3 0 0 0 4 98 97 114 0
                         0 0 0 4 98 97 122
                         0 0 0 0 6 0 0 0 0 0 0 0 3
                         0 0 0 2 0 0 0 0 0 0 0 42
                         0 0 0 2 0 0 0 0 0 0 0 23
                         0 0 0 2 0 0 0 0 0 0 2 154)))

 (test-><-dictionary? '((foo . 23)))
 (test-><-dictionary? '((foo . 23) (bar . "bar")))
 (test-><-dictionary? '((foo . 23) (bar . "bar") (baz 42 23 666))))
