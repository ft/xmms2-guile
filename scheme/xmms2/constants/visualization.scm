;; Copyright (c) 2016 xmms2-guile workers, All rights reserved.
;;
;; Terms for redistribution and use can be found in LICENCE.

(define-module (xmms2 constants visualisation)
  #:use-module (xmms2 constants)
  #:use-module (xmms2 enumeration))

(define-enum (<> xref-visualisation-cmds)
  (CMD-VISUALIZATION-QUERY-VERSION FIRST-CMD-ID)
  CMD-VISUALIZATION-REGISTER
  CMD-VISUALIZATION-INIT-SHM
  CMD-VISUALIZATION-INIT-UDP
  CMD-VISUALIZATION-PROPERTY
  CMD-VISUALIZATION-PROPERTIES
  CMD-VISUALIZATION-SHUTDOWN)
