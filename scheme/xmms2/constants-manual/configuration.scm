;; Copyright (c) 2016 xmms2-guile workers, All rights reserved.
;;
;; Terms for redistribution and use can be found in LICENCE.

(define-module (xmms2 constants configuration)
  #:use-module (xmms2 constants)
  #:use-module (xmms2 enumeration))

(define-enum (<> xref-configuration-cmds)
  (CMD-GET-VALUE FIRST-CMD-ID)
  CMD-SET-VALUE
  CMD-REGISTER-VALUE
  CMD-LIST-VALUES)
