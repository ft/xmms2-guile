;; Copyright (c) 2017 xmms2-guile workers, All rights reserved.
;;
;; Terms for redistribution and use can be found in doc/LICENCE.

(define-module (documentation module constants)
  #:use-module (srfi srfi-1)
  #:use-module (genipc utilities)
  #:export (expand-constants-integer))

(define (expand-constants-integer mod name value)
  (list name 'integer
        (cond ((symbol-prefix? 'CMD- name)
               (format #f "Command identifier for the ‘~a’ command of the ‘~a’ object."
                       (substring (string-downcase (symbol->string name)) 4)
                       (last mod)))
              (else 'undocumented))))
