;; Copyright (c) 2016 xmms2-guile workers, All rights reserved.
;;
;; Terms for redistribution and use can be found in LICENCE.

(define-module (xmms2 types)
  #:use-module (srfi srfi-9)
  #:export (dictionary?
            non-complex-number?))

(define (dictionary? data)
  (and (list? data)
       (not (null? data))
       (pair? (car data))
       (symbol? (caar data))))

(define (non-complex-number? data)
  (and (number? data) (zero? (imag-part data))))

(define-record-type <collection>
  (make-collection operator attributes idlist children)
  collection?
  (operator collection-operator)
  (attributes collection-attributes)
  (idlist collection-idlist)
  (children collection-children))
