;; Copyright (c) 2016 xmms2-guile workers, All rights reserved.
;;
;; Terms for redistribution and use can be found in LICENCE.

(define-module (xmms2 constants courier)
  #:use-module (xmms2 constants)
  #:use-module (xmms2 enumeration))

(define-enum (<> xref-courier-cmds)
  (CMD-SEND-MESSAGE FIRST-CMD-ID)
  CMD-REPLY-MESSAGE
  CMD-GET-CONNECTED-CLIENTS)

(define-enum (<> xref-client2client-codes)
  C2C-POLICY-NO-REPLY
  C2C-POLICY-SINGLE-REPLY
  C2C-POLICY-MULTI-REPLY)
