;; Copyright (c) 2016 xmms2-guile workers, All rights reserved.
;;
;; Terms for redistribution and use can be found in LICENCE.

(define-module (xmms2 constants collection-sync)
  #:use-module (xmms2 constants)
  #:use-module (xmms2 enumeration))

(define-enum (<> xref-collection-sync-cmds)
  (CMD-SYNC FIRST-CMD-ID))
