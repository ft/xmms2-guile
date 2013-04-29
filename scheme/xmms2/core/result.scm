;; Copyright (c) 2013 xmms2-guile workers, All rights reserved.
;;
;; Terms for redistribution and use can be found in LICENCE.

(define-module (xmms2 core result)
  #:use-module (xmms2 core primitives)
  #:export (make-xmms2-result))

(define (make-xmms2-result)
  (xmms2:primitives/make-result))
