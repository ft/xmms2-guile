;; Copyright (c) 2016 xmms2-guile workers, All rights reserved.
;;
;; Terms for redistribution and use can be found in LICENCE.

(define-module (xmms2 constants media-library)
  #:use-module (xmms2 constants)
  #:use-module (xmms2 enumeration))

(define-enum (<> xref-media-library-cmds)
  (CMD-GET-INFORMATION FIRST-CMD-ID)
  CMD-IMPORT-PATH
  CMD-REHASH
  CMD-GET-IDENTIFIER
  CMD-REMOVE-ENTRY
  CMD-SET-PROPERTY-STRING
  CMD-SET-PROPERTY-INTEGER
  CMD-REMOVE-PROPERTY
  CMD-UPDATE-ENTRY-PATH
  CMD-ADD-ENTRY)

(define-enum (<> xref-media-library-entry-codes)
  MEDIA-LIBRARY-ENTRY-NEW
  MEDIA-LIBRARY-ENTRY-OK
  MEDIA-LIBRARY-ENTRY-RESOLVING
  MEDIA-LIBRARY-ENTRY-NOT-AVAILABLE
  MEDIA-LIBRARY-ENTRY-REHASH)
