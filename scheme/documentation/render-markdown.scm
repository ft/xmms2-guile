;; Copyright (c) 2017 xmms2-guile workers, All rights reserved.
;;
;; Terms for redistribution and use can be found in doc/LICENCE.

(define-module (documentation render-markdown)
  #:export (output-markdown))

(define (output-markdown source)
  (format #t "~s~%" source))
