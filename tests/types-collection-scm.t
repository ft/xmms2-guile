;; -*- scheme -*-

;; Copyright (c) 2016 xmms2-guile workers, All rights reserved.
;;
;; Terms for redistribution and use can be found in LICENCE.

(use-modules (test tap)
             (test setup)
             (xmms2 constants collection)
             (xmms2 types))

(init-test-tap!)

(define *universe* (make-universe))

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
          2))

 (simple-equals-tests "")
 (simple-equals-tests " (universe keyword)" from universe)
 (simple-equals-tests " (universe variable)" from *universe*)

 (simple-has-tests "")
 (simple-has-tests " (universe keyword)" from universe)
 (simple-has-tests " (universe variable)" from *universe*)

 (let* ((has-artist (collection (has artist)))
        (missing-artist (collection (¬ has-artist))))
   (define-test "not: operator is COLLECTION-TYPE-COMPLEMENT"
     (pass-if-= (collection-operator missing-artist)
                COLLECTION-TYPE-COMPLEMENT))
   (define-test "not: source of missing-artist is has-artist"
     (pass-if-equal? (car (collection-children missing-artist))
                     has-artist))))
