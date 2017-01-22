;; Copyright (c) 2017 xmms2-guile workers, All rights reserved.
;;
;; Terms for redistribution and use can be found in LICENCE.

(define-module (xmms2 client)
  #:use-module (ice-9 optargs)
  #:use-module (xmms2 constants meta)
  #:use-module (xmms2 header)
  #:use-module (xmms2 io)
  #:use-module (xmms2 ipc main)
  #:use-module (xmms2 payload)
  #:export (default-uri
            with-xmms2-connection))

(define (default-uri)
  "Returns the default IPC uri used by xmms2 servers."
  (string-concatenate `("unix:///tmp/xmms-ipc-"
                        ,(passwd:name (getpwuid (geteuid))))))

(define (default-failure-handler exception arguments)
  (format (current-error-port)
          "xmms2-connection failed: ~s (~s)~%"
          exception arguments)
  (quit 1))

(define* (with-xmms2-connection #:key
                                (handler (lambda (cl id co) #t))
                                (server (default-uri))
                                (client "xmms2-guile")
                                (failure default-failure-handler))
  (catch #t
    (lambda ()
      (let ((connection (make-xmms2-connection server))
            (return-value #f))
        (xmms2-connect connection)
        (xmms2-send connection (ipc-hello PROTOCOL-VERSION client))
        (let* ((reply (xmms2-recv connection))
               (cookie (header->cookie (car reply)))
               (client-id (payload->value (caddr reply))))
          (set! return-value (handler connection client-id cookie)))
        (xmms2-disconnect connection)
        return-value))
    (lambda (key . rest)
      (failure key rest))))
