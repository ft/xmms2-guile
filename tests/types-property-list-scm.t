;; -*- scheme -*-

;; Copyright (c) 2017 xmms2-guile workers, All rights reserved.
;;
;; Terms for redistribution and use can be found in LICENCE.

(use-modules (test tap)
             (test setup)
             (xmms2 types))

(init-test-tap!)
(setlocale LC_ALL "")

(with-fs-test-bundle
 (plan 18)
 (define-test "empty list is a property list"
   (pass-if-true (property-list? '())))
 (define-test "simple property list"
   (pass-if-true (property-list? '(#:foo bar))))
 (define-test "longer property list example"
   (pass-if-true (property-list? '(#:foo bar #:baz quux))))
 (define-test "example with more complex value field"
   (pass-if-true (property-list? '(#:foo (thing fish) #:baz quux))))
 (define-test "integers are no property lists"
   (pass-if-false (property-list? 1)))
 (define-test "symbols are no property lists"
   (pass-if-false (property-list? 'foo)))
 (define-test "strings are no property lists"
   (pass-if-false (property-list? "foo")))
 (define-test "vectors are no property lists"
   (pass-if-false (property-list? #vu8(1 2 3 4))))
 (define-test "missing value field #1"
   (pass-if-false (property-list? '(#:foo))))
 (define-test "missing value field #2"
   (pass-if-false (property-list? '(#:foo bar #:baz))))
 (define-test "two value fields are not allowed"
   (pass-if-false (property-list? '(#:foo 1 2 #:baz quux))))
 (define-test "keys must be keywords"
   (pass-if-false (property-list? '(foo bar))))

 (define-test "property list transforms to correct association list #1"
   (pass-if-equal? (property-list->association-list #:foo 'bar)
                   '((foo . bar))))
 (define-test "property list transforms to correct association list #2"
   (pass-if-equal? (property-list->association-list #:foo 'bar
                                                    #:baz 'quux)
                   '((foo . bar)
                     (baz . quux))))
 (define-test "property list transforms to correct association list #3"
   (pass-if-equal? (property-list->association-list #:foo 'bar
                                                    #:list '(with some stuff in it)
                                                    #:baz 'quux)
                   '((foo . bar)
                     (list with some stuff in it)
                     (baz . quux))))
 (define-test "property list transforms to correct dictionary"
   (pass-if-equal? (dictionary-data
                    (property-list->dictionary #:foo 'bar
                                               #:list '(with some stuff in it)
                                               #:baz 'quux))
                   '((foo . bar)
                     (list with some stuff in it)
                     (baz . quux))))
 (define-test "transformer throws up on invalid data #1"
   (pass-if-exception 'x2/not-a-property-list
                      (property-list->association-list 'foobar)))
 (define-test "transformer throws up on invalid data #2"
   (pass-if-exception 'x2/not-a-property-list
                      (property-list->dictionary 'foobar))))
