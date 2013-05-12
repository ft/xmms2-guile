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

(define trivial-types '(empty-value
                        erroneous-value
                        unknown-value-type))

(define unknown-mapping 'unknown-symbol)

(with-test-bundle (value types)
  (plan (+ 1
           (length trivial-types)
           (length some-map)))

  ;; Check trivial types:
  (map
   (lambda (x)
     (define-test (format #f "trivial-type: ~a" x)
       (pass-if-true (trivial-type? x))))
   trivial-types)

  ;; Check if `from-symbol-map' works. If it does, `integer->status' and
  ;; `integer->value-type' will work as well.
  (map (lambda (x)
         (let ((key (car x))
               (val (cdr x)))
           (define-test (format #f "from-symbol-map (~d)" key)
             (pass-if-eq? val
                          (from-symbol-map some-map key 'unknown-symbol)))))
       some-map)
  ;; If called with an unknown key, `from-symbol-map' should return its third
  ;; argument to signal, that the key doesn't have a corresponding value in the
  ;; given mapping.
  (define-test "from-symbol-map (unknown)"
    (pass-if-eq? unknown-mapping
                 (from-symbol-map some-map 23 unknown-mapping))))
