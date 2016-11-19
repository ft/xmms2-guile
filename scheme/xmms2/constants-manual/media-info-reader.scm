;; Copyright (c) 2016 xmms2-guile workers, All rights reserved.
;;
;; Terms for redistribution and use can be found in LICENCE.

(define-module (xmms2 constants media-info-reader)
  #:use-module (xmms2 constants)
  #:use-module (xmms2 enumeration))

(define-enum (<> xref-media-info-reader-states)
  MEDIA-INFO-READER-IDLE
  MEDIA-INFO-READER-RUNNING)
