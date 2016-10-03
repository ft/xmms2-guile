;; Copyright (c) 2016 xmms2-guile workers, All rights reserved.
;;
;; Terms for redistribution and use can be found in LICENCE.

(define-module (xmms2 constants mediainfo-reader)
  #:use-module (xmms2 enumeration))

(define-enum (<> xref-mediainfo-reader-states)
  MEDIAINFO-READER-IDLE
  MEDIAINFO-READER-RUNNING)
