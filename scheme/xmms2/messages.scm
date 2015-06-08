;; Copyright (c) 2015 xmms2-guile workers, All rights reserved.
;;
;; Terms for redistribution and use can be found in LICENCE.

(define-module (xmms2 messages)
  #:use-module (rnrs bytevectors)
  #:use-module (xmms2 constants)
  #:use-module (xmms2 header)
  #:use-module (xmms2 payload))

(define-public (make-hello client-name)
  ;; So how does this work? First, we're preparing the payload we're going to
  ;; stuff down the server's pipe. With that, it's possible the construct a
  ;; header (because you need the payload-length in that). Then we're
  ;; constructing the complete message by correctly marking the payload (it's a
  ;; list-payload, so we're prefixing it with TAG-LIST when we're consing the
  ;; full message).
  ;;
  ;; The reply to HELLO looks like this:
  ;;
  ;; Header:
  ;;   OBJECT-MAIN              4 Bytes
  ;;   CMD-REPLY | CMD-ERROR    4 Bytes
  ;;   0x00000000 (Use unknown) 4 Bytes
  ;;   Payload-Length           4 Bytes
  ;; Payload:
  ;;   INT64                    4 Bytes
  ;;   Client-ID                8 Bytes
  ;;
  ;; So, Header: 00 00 00 01 00 00 00 00 00 00 00 0c
  ;;    Payload: 00 00 00 02 00 00 00 00 00 00 00 04
  ;;
  ;; That is:
  ;;   - Object kind: MAIN
  ;;   - 12 Bytes of payload
  ;;   - Payload is an int64
  ;;   - Value of payload is: 4
  (let ((payload (make-list-payload (list PROTOCOL-VERSION client-name))))
    (cons* (make-protocol-header OBJECT-MAIN
                                 CMD-HELLO
                                 (payload-length* payload))
           TAG-LIST
           payload)))
