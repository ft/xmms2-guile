;; Copyright (c) 2016 xmms2-guile workers, All rights reserved.
;;
;; Terms for redistribution and use can be found in LICENCE.

(define-module (xmms2 constants main)
  #:use-module (xmms2 constants)
  #:use-module (xmms2 enumeration))

(define-enum (<> xref-main-cmds)
  (CMD-HELLO FIRST-CMD-ID)
  CMD-QUIT
  CMD-LIST-PLUGINS
  CMD-STATISTICS)
