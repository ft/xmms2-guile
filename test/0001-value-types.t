;; -*- scheme -*-

(use-modules (taptest)
             (xmms2 core value))

(define trivial-type? (@@ (xmms2 core value) trivial-type?))

(with-test-bundle (value types)
  (plan 3)
  (map
   (lambda (x)
     (define-test (format #f "trivial-type: ~a" x)
       (pass-if-true (trivial-type? x))))
   '(empty-value
     erroneous-value
     unknown-value-type)))
