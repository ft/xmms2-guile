;; Copyright (c) 2016 xmms2-guile workers, All rights reserved.
;;
;; Terms for redistribution and use can be found in LICENCE.

(define-module (test payload)
  #:use-module (test tap)
  #:use-module (xmms2 payload)
  #:export (pass-if-payload-equal?
            pass-if-payload-not-equal?))

(define-tap-test (pass-if-payload-equal? a b)
  (equal? (payload-combine a)
          (payload-combine b)))

(define-tap-test (pass-if-payload-not-equal? a b)
  (not (equal? (payload-combine a)
               (payload-combine b))))
