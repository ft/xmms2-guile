;; Copyright (c) 2016 xmms2-guile workers, All rights reserved.
;;
;; Terms for redistribution and use can be found in LICENCE.

(define-module (xmms2 constants binary-data)
  #:use-module (xmms2 constants)
  #:use-module (xmms2 enumeration))

(define-enum (<> xref-binary-data-cmds)
  (CMD-GET-DATA FIRST-CMD-ID)
  CMD-ADD-DATA
  CMD-REMOVE-DATA
  CMD-LIST-DATA)
