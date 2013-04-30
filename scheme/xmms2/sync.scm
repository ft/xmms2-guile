;; Copyright (c) 2013 xmms2-guile workers, All rights reserved.
;;
;; Terms for redistribution and use can be found in LICENCE.

(define-module (xmms2 sync)
  #:use-module (ice-9 optargs)
  #:use-module (xmms2 core primitives)
  #:use-module (xmms2 core connection)
  #:export (with-xmms2-connection))

(define* (with-xmms2-connection
          #:key
          handler
          (server (string-concatenate `("unix:///tmp/xmms-ipc-"
                                        ,(passwd:name (getpwuid (geteuid))))))
          (client "xmms2-guile")
          (fail (lambda (msg)
                  (format #t "xmms2-connection failed: ~a~%" msg)
                  (quit 1))))
  (catch 'xmms2:sync/connection-failure
    (lambda ()
      (let ((connection (xmms2-connect client server)))
        (handler connection)))
    (lambda (key . args)
      (fail (car args)))))

(define (synchronous-action action connection)
  (let ((result (action (get-xmms2-connection-container connection))))
    (xmms2:primitive/sync-wait result)))

(define-syntax define-synchronous-action
  (lambda (x)
    (syntax-case x ()
      ((_ synchronous-function primitive-function)
       #'(define-public (synchronous-function connection)
           (synchronous-action primitive-function connection))))))

(define-synchronous-action xmms2/pause xmms2:primitive/pause)
(define-synchronous-action xmms2/play xmms2:primitive/play)
(define-synchronous-action xmms2/stop xmms2:primitive/stop)
