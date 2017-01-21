;; -*- scheme -*-

;; Copyright (c) 2017 xmms2-guile workers, All rights reserved.
;;
;; Terms for redistribution and use can be found in LICENCE.

(use-modules (test tap)
             (test setup)
             (xmms2 fetch-spec)
             (xmms2 types))

(init-test-tap!)
(setlocale LC_ALL "")

(with-fs-test-bundle
 (plan 3)
 (define-test "Simple fetch spec looks good"
   (pass-if-equal? (dictionary-data (fetch-spec #:foo bar))
                   '((foo . "bar"))))
 (define-test "Evaluating arguments works like in collections"
   (pass-if-equal? (dictionary-data (fetch-spec #:foo (| (+ 1 2))))
                   '((foo . 3))))
 (define-test "Sub-expressions work"
   (pass-if-equal? (dictionary-data/deep (fetch-spec #:foo (- #:type count)))
                   '((foo . ((type . "count")))))))
