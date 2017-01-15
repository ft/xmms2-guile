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

(with-fs-test-bundle
 (plan 9)

 (define-test "universe has one node"
   (pass-if-= 1 (collection-fold (lambda (x acc) (+ acc 1))
                                 0 *universe*)))

 (define-test "big-three has seven nodes"
   ;; The root node and three child nodes, that each have one child themselves.
   (pass-if-= 7 (collection-fold (lambda (x acc) (+ acc 1))
                                 0 *big-three*)))

 (define-test "complex has twenty-two nodes"
   (pass-if-= 22 (collection-fold (lambda (x acc) (+ acc 1))
                                  0 *complex*)))

 (let ((pre-lr '(COLLECTION-TYPE-INTERSECTION
                 COLLECTION-TYPE-UNION
                 COLLECTION-TYPE-COMPLEMENT
                 COLLECTION-TYPE-EQUALS
                 COLLECTION-TYPE-UNIVERSE
                 COLLECTION-TYPE-EQUALS
                 COLLECTION-TYPE-UNIVERSE
                 COLLECTION-TYPE-EQUALS
                 COLLECTION-TYPE-UNIVERSE
                 COLLECTION-TYPE-UNION
                 COLLECTION-TYPE-EQUALS
                 COLLECTION-TYPE-UNIVERSE
                 COLLECTION-TYPE-INTERSECTION
                 COLLECTION-TYPE-EQUALS
                 COLLECTION-TYPE-UNIVERSE
                 COLLECTION-TYPE-EQUALS
                 COLLECTION-TYPE-UNIVERSE
                 COLLECTION-TYPE-EQUALS
                 COLLECTION-TYPE-UNIVERSE
                 COLLECTION-TYPE-COMPLEMENT
                 COLLECTION-TYPE-EQUALS
                 COLLECTION-TYPE-UNIVERSE))
       (pre-rl '(COLLECTION-TYPE-INTERSECTION
                 COLLECTION-TYPE-COMPLEMENT
                 COLLECTION-TYPE-EQUALS
                 COLLECTION-TYPE-UNIVERSE
                 COLLECTION-TYPE-UNION
                 COLLECTION-TYPE-EQUALS
                 COLLECTION-TYPE-UNIVERSE
                 COLLECTION-TYPE-INTERSECTION
                 COLLECTION-TYPE-EQUALS
                 COLLECTION-TYPE-UNIVERSE
                 COLLECTION-TYPE-EQUALS
                 COLLECTION-TYPE-UNIVERSE
                 COLLECTION-TYPE-EQUALS
                 COLLECTION-TYPE-UNIVERSE
                 COLLECTION-TYPE-UNION
                 COLLECTION-TYPE-EQUALS
                 COLLECTION-TYPE-UNIVERSE
                 COLLECTION-TYPE-EQUALS
                 COLLECTION-TYPE-UNIVERSE
                 COLLECTION-TYPE-COMPLEMENT
                 COLLECTION-TYPE-EQUALS
                 COLLECTION-TYPE-UNIVERSE))
       (post-lr '(COLLECTION-TYPE-UNIVERSE
                  COLLECTION-TYPE-EQUALS
                  COLLECTION-TYPE-COMPLEMENT
                  COLLECTION-TYPE-UNIVERSE
                  COLLECTION-TYPE-EQUALS
                  COLLECTION-TYPE-UNIVERSE
                  COLLECTION-TYPE-EQUALS
                  COLLECTION-TYPE-UNION
                  COLLECTION-TYPE-UNIVERSE
                  COLLECTION-TYPE-EQUALS
                  COLLECTION-TYPE-UNIVERSE
                  COLLECTION-TYPE-EQUALS
                  COLLECTION-TYPE-UNIVERSE
                  COLLECTION-TYPE-EQUALS
                  COLLECTION-TYPE-INTERSECTION
                  COLLECTION-TYPE-UNIVERSE
                  COLLECTION-TYPE-EQUALS
                  COLLECTION-TYPE-UNION
                  COLLECTION-TYPE-UNIVERSE
                  COLLECTION-TYPE-EQUALS
                  COLLECTION-TYPE-COMPLEMENT
                  COLLECTION-TYPE-INTERSECTION))
       (post-rl '(COLLECTION-TYPE-UNIVERSE
                  COLLECTION-TYPE-EQUALS
                  COLLECTION-TYPE-COMPLEMENT
                  COLLECTION-TYPE-UNIVERSE
                  COLLECTION-TYPE-EQUALS
                  COLLECTION-TYPE-UNIVERSE
                  COLLECTION-TYPE-EQUALS
                  COLLECTION-TYPE-UNIVERSE
                  COLLECTION-TYPE-EQUALS
                  COLLECTION-TYPE-INTERSECTION
                  COLLECTION-TYPE-UNIVERSE
                  COLLECTION-TYPE-EQUALS
                  COLLECTION-TYPE-UNION
                  COLLECTION-TYPE-UNIVERSE
                  COLLECTION-TYPE-EQUALS
                  COLLECTION-TYPE-UNIVERSE
                  COLLECTION-TYPE-EQUALS
                  COLLECTION-TYPE-UNIVERSE
                  COLLECTION-TYPE-EQUALS
                  COLLECTION-TYPE-COMPLEMENT
                  COLLECTION-TYPE-UNION
                  COLLECTION-TYPE-INTERSECTION))
       (level-lr '(COLLECTION-TYPE-INTERSECTION
                   COLLECTION-TYPE-UNION
                   COLLECTION-TYPE-UNION
                   COLLECTION-TYPE-COMPLEMENT
                   COLLECTION-TYPE-COMPLEMENT
                   COLLECTION-TYPE-EQUALS
                   COLLECTION-TYPE-EQUALS
                   COLLECTION-TYPE-EQUALS
                   COLLECTION-TYPE-INTERSECTION
                   COLLECTION-TYPE-EQUALS
                   COLLECTION-TYPE-EQUALS
                   COLLECTION-TYPE-EQUALS
                   COLLECTION-TYPE-UNIVERSE
                   COLLECTION-TYPE-UNIVERSE
                   COLLECTION-TYPE-UNIVERSE
                   COLLECTION-TYPE-EQUALS
                   COLLECTION-TYPE-EQUALS
                   COLLECTION-TYPE-UNIVERSE
                   COLLECTION-TYPE-UNIVERSE
                   COLLECTION-TYPE-UNIVERSE
                   COLLECTION-TYPE-UNIVERSE
                   COLLECTION-TYPE-UNIVERSE))
       (level-rl '(COLLECTION-TYPE-INTERSECTION
                   COLLECTION-TYPE-COMPLEMENT
                   COLLECTION-TYPE-UNION
                   COLLECTION-TYPE-UNION
                   COLLECTION-TYPE-EQUALS
                   COLLECTION-TYPE-EQUALS
                   COLLECTION-TYPE-INTERSECTION
                   COLLECTION-TYPE-EQUALS
                   COLLECTION-TYPE-EQUALS
                   COLLECTION-TYPE-EQUALS
                   COLLECTION-TYPE-COMPLEMENT
                   COLLECTION-TYPE-UNIVERSE
                   COLLECTION-TYPE-UNIVERSE
                   COLLECTION-TYPE-EQUALS
                   COLLECTION-TYPE-EQUALS
                   COLLECTION-TYPE-UNIVERSE
                   COLLECTION-TYPE-UNIVERSE
                   COLLECTION-TYPE-UNIVERSE
                   COLLECTION-TYPE-EQUALS
                   COLLECTION-TYPE-UNIVERSE
                   COLLECTION-TYPE-UNIVERSE
                   COLLECTION-TYPE-UNIVERSE))
       (append-op (lambda (c acc)
                    (append acc (list (assq-ref xref-collection-types
                                                (collection-operator c)))))))
   (define-test "collection-fold, order: pre, left-to-right on *complex* works"
     (pass-if-equal? (collection-fold append-op '() *complex*
                                      #:left-to-right? #t
                                      #:order 'pre)
                     pre-lr))
   (define-test "collection-fold, order: pre, right-to-left on *complex* works"
     (pass-if-equal? (collection-fold append-op '() *complex*
                                      #:left-to-right? #f
                                      #:order 'pre)
                     pre-rl))
   (define-test "collection-fold, order: post, left-to-right on *complex* works"
     (pass-if-equal? (collection-fold append-op '() *complex*
                                      #:left-to-right? #t
                                      #:order 'post)
                     post-lr))
   (define-test "collection-fold, order: post, right-to-left on *complex* works"
     (pass-if-equal? (collection-fold append-op '() *complex*
                                      #:left-to-right? #f
                                      #:order 'post)
                     post-rl))
   (define-test "collection-fold, order: level, left-to-right on *complex* works"
     (pass-if-equal? (collection-fold append-op '() *complex*
                                      #:left-to-right? #t
                                      #:order 'level)
                     level-lr))
   (define-test "collection-fold, order: level, right-to-left on *complex* works"
     (pass-if-equal? (collection-fold append-op '() *complex*
                                      #:left-to-right? #f
                                      #:order 'level)
                     level-rl))))
