;; Copyright (c) 2013 xmms2-guile workers, All rights reserved.
;;
;; Terms for redistribution and use can be found in LICENCE.

(define-module (xmms2 core connection)
  #:use-module (xmms2 core primitives)
  #:use-module (srfi srfi-9) ;; record types
  #:export (xmms2-connect
            make-xmms2-connection
            xmms2-connection?
            get-xmms2-connection-container
            get-xmms2-connection-uri
            get-xmms2-connection-status
            set-xmms2-connection-status!))

(define-record-type <xmms2-connection>
  (make-bare-xmms2-connection uri)
  xmms2-connection?
  (uri get-xmms2-connection-uri set-xmms2-connection-uri!)
  (status get-xmms2-connection-status set-xmms2-connection-status!)
  (container get-xmms2-connection-container set-xmms2-connection-container!))

(define (make-xmms2-connection client uri)
  (let ((new (make-bare-xmms2-connection uri)))
    (set-xmms2-connection-container! new
                                     (xmms2:type/make-connection client))
    (set-xmms2-connection-status! new 'down)
    new))

(define (xmms2-connect client uri)
  "Take a client-tag in `client' and a server location in `uri' and connect to
the server in question. Returns a <xmms2-connection> record, that encapsulates
the connection."
  (let ((connection (make-xmms2-connection client uri)))
    (xmms2:primitive/connect (get-xmms2-connection-container connection)
                             (get-xmms2-connection-uri connection))
    (set-xmms2-connection-status! connection 'up)
    connection))
