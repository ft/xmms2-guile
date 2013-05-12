;; -*- scheme -*-

(use-modules (taptest)
             (xmms2 core value))

(define trivial-type? (@@ (xmms2 core value) trivial-type?))
(define from-symbol-map (@@ (xmms2 core value) from-symbol-map))

(define some-map '((1 . one)
                   (2 . two)
                   (3 . three)
                   (4 . four)
                   (5 . five)
                   (6 . six)))

(with-test-bundle (value types)
  (plan (+ 4 (length some-map)))
  (map
   (lambda (x)
     (define-test (format #f "trivial-type: ~a" x)
       (pass-if-true (trivial-type? x))))
   '(empty-value
     erroneous-value
     unknown-value-type))
  (map (lambda (x)
         (let ((key (car x))
               (val (cdr x)))
           (define-test (format #f "from-symbol-map (~d)" key)
             (pass-if-eq? val
                          (from-symbol-map some-map key 'unknown-symbol)))))
       some-map)
  (define-test "from-symbol-map (unknown)"
    (pass-if-eq? 'unknown-symbol
                 (from-symbol-map some-map 23 'unknown-symbol))))
