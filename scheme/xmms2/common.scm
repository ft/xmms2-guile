;; Copyright (c) 2013 xmms2-guile workers, All rights reserved.
;;
;; Terms for redistribution and use can be found in LICENCE.

(define-module (xmms2 common)
  #:use-module (xmms2 core connection)
  #:use-module (xmms2 core primitives)
  #:export (default-client-id
            default-fail-handler
            default-uri
            with-xmms2-connection))

(define default-client-id "xmms2-guile")

(define (default-fail-handler msg)
  (format #t "xmms2-connection failed: ~a~%" msg)
  (quit 1))

(define default-uri
  (string-concatenate `("unix:///tmp/xmms-ipc-"
                        ,(passwd:name (getpwuid (geteuid))))))

(define* (with-xmms2-connection
          #:key
          handler
          (server default-uri)
          (client default-client-id)
          (fail default-fail-handler))
  (catch 'xmms2:sync/connection-failure
    (lambda ()
      (let ((connection (xmms2-connect client server)))
        (handler connection)
        (xmms2:primitive/unref-connection
         (get-xmms2-connection-container connection))))
    (lambda (key . args)
      (fail (car args)))))
