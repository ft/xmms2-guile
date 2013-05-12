;; -*- scheme -*-
;;
;; Copyright (c) 2013 xmms2-guile workers, All rights reserved.
;;
;; Terms for redistribution and use can be found in LICENCE.

(use-modules (taptest)
             (xmms2 core primitives))

;; Short-hand for producing ('foo . foo)
(define-syntax xx
  (lambda (x)
    (syntax-case x ()
      ((_ name)
       #'(cons (quote name) name)))))

(define constants (list (xx XMMS2-STATUS-PAUSED)
                        (xx XMMS2-STATUS-PLAYING)
                        (xx XMMS2-STATUS-STOPPED)
                        (xx XMMS2-VALUE-ERROR)
                        (xx XMMS2-VALUE-INTEGER)
                        (xx XMMS2-VALUE-NONE)))

(with-test-bundle (primitives constants)
  (plan (length constants))
  (map (lambda (x)
         (let ((key (car x))
               (val (cdr x)))
           (define-test (format #f "constant: ~a" key)
             (pass-if-true (integer? val)))))
       constants))
