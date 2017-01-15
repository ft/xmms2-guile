;; -*- scheme -*-

;; Copyright (c) 2016 xmms2-guile workers, All rights reserved.
;;
;; Terms for redistribution and use can be found in LICENCE.

(use-modules (test tap)
             (test setup)
             (xmms2 constants collection)
             (xmms2 types))

(init-test-tap!)
(setlocale LC_ALL "")

(define *universe* (make-universe))

(define *big-three* (collection (∪ (artist = Slayer)
                                   (artist = Metallica)
                                   (artist = Anthrax))))

(define *complex* (collection (∩ (∪ (¬ (artist = Slayer))
                                    (artist = Megadeth)
                                    (artist = Chopin))
                                 (∪ (album = One)
                                    (∩ (artist = Björk)
                                       (artist = "Tori Amos"))
                                    (album = "Call of the Mastodon"))
                                 (¬ (artist = Mozart)))))

(define-syntax-rule (simple-equals-tests desc src ...)
  (let ((c (collection (artist = "Slayer")))
        (fmt (string-concatenate (list "simple" desc ", equals: ~a is ~s"))))
    (define-test (format #f fmt 'operator 'COLLECTION-TYPE-EQUALS)
      (pass-if-= (collection-operator c) COLLECTION-TYPE-EQUALS))
    (define-test (format #f fmt 'field "artist")
      (pass-if-string=? (collection-attribute c 'field) "artist"))
    (define-test (format #f fmt 'value "Slayer")
      (pass-if-string=? (collection-attribute c 'value) "Slayer"))
    (define-test (format #f fmt 'source '*universe*)
      (pass-if-equal? (car (collection-children c)) *universe*))))

(define *tests-per-simple-equals* 4)

(define-syntax-rule (simple-has-tests desc src ...)
  (let ((c (collection (has artist)))
        (fmt-1 (string-concatenate (list "simple" desc ", has: ~a")))
        (fmt-2 (string-concatenate (list "simple" desc ", has: ~a is ~s"))))
    (define-test (format #f fmt-2 'operator 'COLLECTION-TYPE-HAS)
      (pass-if-= (collection-operator c) COLLECTION-TYPE-HAS))
    (define-test (format #f fmt-1 "artist")
      (pass-if-string=? (collection-attribute c 'field) "artist"))
    (define-test (format #f fmt-2 'source '*universe*)
      (pass-if-equal? (car (collection-children c)) *universe*))))

(define *tests-per-simple-has* 3)

(with-fs-test-bundle
 (plan (+ (* 3 *tests-per-simple-equals*)
          (* 3 *tests-per-simple-has*)
          21))

 (simple-equals-tests "")
 (simple-equals-tests " (universe keyword)" #:from universe)
 (simple-equals-tests " (universe variable)" #:from *universe*)

 (simple-has-tests "")
 (simple-has-tests " (universe keyword)" #:from universe)
 (simple-has-tests " (universe variable)" #:from *universe*)

 (let* ((has-artist (collection (has artist)))
        (missing-artist (collection (¬ has-artist))))

   (define-test "not: operator is COLLECTION-TYPE-COMPLEMENT"
     (pass-if-= (collection-operator missing-artist)
                COLLECTION-TYPE-COMPLEMENT))

   (define-test "not: source of missing-artist is has-artist"
     (pass-if-equal? (car (collection-children missing-artist))
                     has-artist)))

 (define-test "id-lists work"
   (let ((lst '(1 2 3 4 5 6 7)))
     (pass-if-equal? (collection-idlist (collection (‣ lst)))
                     lst)))

 (define-test "id-lists can set type"
   (let ((lst '(1 2 3 4 5 6 7)))
     (pass-if-equal? (collection-attribute (collection (‣ lst #:type pshuffle))
                                           'type)
                     "pshuffle")))

 (define-test "variables as arguments work"
   (let ((band "Slayer"))
     (pass-if-equal? (collection-attribute (collection (artist = (band))) 'value)
                     band)))

 (define-test "expression may be complex in argument position"
   (let* ((band "Slayer")
          (stuff `((thing . "fish") (band . ,band))))
     (pass-if-equal? (collection-attribute
                      (collection (artist = ((assq-ref stuff 'band)))) 'value)
                     band)))

 (define-test "key expression may be complex as well"
   (let* ((band "Slayer")
          (stuff `((thing . "fish") (band . ,band)))
          (key "artist")
          (wat `((value . band) (key . ,key))))
     (pass-if-equal? (collection-attribute
                      (collection ((assq-ref wat 'key)
                                   =
                                   ((assq-ref stuff 'band))))
                      'field)
                     "artist")))

 (define-test "#:namespace argument can be evaluated as well"
   (let ((ns "Collectionation"))
     (pass-if-equal? (collection-attribute (collection (→ Slayer #:namespace (ns)))
                                           'namespace)
                     ns)))

 (define-test "#:case-sensitive? works #1"
   (pass-if-= (collection-attribute
               (collection (artist = Slayer #:case-sensitive? #f))
               'case-sensitive)
              0))

 (define-test "#:case-sensitive? works #2"
   (pass-if-= (collection-attribute
               (collection (artist = Slayer #:case-sensitive? #t))
               'case-sensitive)
              1))

 (define-test "#:case-sensitive? can use evaluated expressions"
   (let ((active? #t))
     (pass-if-= (collection-attribute
                 (collection (artist = Slayer #:case-sensitive? (active?)))
                 'case-sensitive)
                1)))

 (define-test "#:collation works"
   (pass-if-string=? (collection-attribute
                      (collection (artist = Slayer #:collation BINARY))
                      'collation)
                     "BINARY"))

 (define-test "#:collation upcases symbols"
   (pass-if-string=? (collection-attribute
                      (collection (artist = Slayer #:collation binary))
                      'collation)
                     "BINARY"))

 (define-test "#:collation upcases strings"
   (pass-if-string=? (collection-attribute
                      (collection (artist = Slayer #:collation "binary"))
                      'collation)
                     "BINARY"))

 (define-test "#:source-preference works"
   (pass-if-string=? (collection-attribute
                      (collection (artist = Slayer #:source-preference all))
                      'source-preference)
                     "all"))

 (define-test "#:source works"
   (pass-if-string=? (collection-attribute
                      (collection (artist = Slayer #:source plugin/vorbis))
                      'source)
                     "plugin/vorbis"))

 (define-test "#:order works"
   (pass-if-string=? (collection-attribute
                      (collection (artist = Slayer #:order ASC))
                      'order)
                     "ASC"))

 (define-test "#:order upcases symbols"
   (pass-if-string=? (collection-attribute
                      (collection (artist = Slayer #:order asc))
                      'order)
                     "ASC"))

 (define-test "#:order upcases strings"
   (pass-if-string=? (collection-attribute
                      (collection (artist = Slayer #:order "desc"))
                      'order)
                     "DESC"))

 (define-test "#:start works"
   (pass-if-= (collection-attribute
               (collection (artist = Slayer #:start 23))
               'start)
              23))

 (define-test "#:length works"
   (pass-if-= (collection-attribute
               (collection (artist = Slayer #:length 42))
               'length)
              42)))
