;; Copyright (c) 2016 xmms2-guile workers, All rights reserved.
;;
;; Terms for redistribution and use can be found in LICENCE.

(define-module (xmms2 constants visualization)
  #:use-module (xmms2 constants)
  #:use-module (xmms2 enumeration))

(define-enum (<> xref-visualisation-cmds)
  (CMD-QUERY-VERSION FIRST-CMD-ID)
  CMD-REGISTER
  CMD-INIT-SHM
  CMD-INIT-UDP
  CMD-SET-PROPERTY
  CMD-SET-PROPERTIES
  CMD-SHUTDOWN)
