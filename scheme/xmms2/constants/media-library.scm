;; Copyright (c) 2016 xmms2-guile workers, All rights reserved.
;;
;; Terms for redistribution and use can be found in LICENCE.

(define-module (xmms2 constants media-library)
  #:use-module (xmms2 enumeration))

(define-enum (<> xref-media-library-cmds)
  (CMD-PATH-IMPORT FIRST-CMD-ID)
  CMD-REHASH
  CMD-GET-ID
  CMD-REMOVE-ID
  CMD-PROPERTY-SET-STRING
  CMD-PROPERTY-SET-INTEGER
  CMD-PROPERTY-REMOVE
  CMD-MOVE-URL
  CMD-MEDIALIB-ADD-URL)

(define-enum (<> xref-media-library-entry-codes)
  MEDIA-LIBRARY-ENTRY-NEW
  MEDIA-LIBRARY-ENTRY-OK
  MEDIA-LIBRARY-ENTRY-RESOLVING
  MEDIA-LIBRARY-ENTRY-NOT-AVAILABLE
  MEDIA-LIBRARY-ENTRY-REHASH)
