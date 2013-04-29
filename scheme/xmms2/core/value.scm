;; Copyright (c) 2013 xmms2-guile workers, All rights reserved.
;;
;; Terms for redistribution and use can be found in LICENCE.

(define-module (xmms2 core value)
  #:use-module (xmms2 core primitives)
  #:export (make-xmms2-value))

(define (make-xmms2-value)
  (xmms2:type/make-value))
