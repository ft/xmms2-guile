;; Copyright (c) 2017 xmms2-guile workers, All rights reserved.
;;
;; Terms for redistribution and use can be found in doc/LICENCE.

(define-module (genipc stage-0)
  #:use-module (sxml simple)
  #:export (generate-sxml))

;; Stage 0: Read XML definition into a SXML structure.

(define (generate-sxml file)
  (with-input-from-file file
    (lambda ()
      (xml->sxml (current-input-port)
                 #:trim-whitespace? #t
                 #:namespaces '((xmms: . "https://xmms2.org/ipc.xsd"))))))
