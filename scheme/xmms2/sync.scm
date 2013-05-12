;; Copyright (c) 2013 xmms2-guile workers, All rights reserved.
;;
;; Terms for redistribution and use can be found in LICENCE.

(define-module (xmms2 sync)
  #:use-module (ice-9 optargs)
  #:use-module (xmms2 core connection)
  #:use-module (xmms2 core primitives)
  #:use-module (xmms2 core value)
  #:export (with-xmms2-connection))

(define default-uri
  (string-concatenate `("unix:///tmp/xmms-ipc-"
                        ,(passwd:name (getpwuid (geteuid))))))

(define (default-fail-handler msg)
  (format #t "xmms2-connection failed: ~a~%" msg)
  (quit 1))

(define* (with-xmms2-connection
          #:key
          handler
          (server default-uri)
          (client "xmms2-guile")
          (fail default-fail-handler))
  (catch 'xmms2:sync/connection-failure
    (lambda ()
      (let ((connection (xmms2-connect client server)))
        (handler connection)
        (xmms2:primitive/unref-connection connection)))
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
           (let* ((result (synchronous-action primitive-function connection))
                  (value (xmms2-result->value result)))
             (xmms2-value->scheme-data value))))
      ((_ synchronous-function primitive-function post-process-fcn)
       #'(define-public (synchronous-function connection)
           (let* ((result (synchronous-action primitive-function connection))
                  (value (xmms2-result->value result)))
             (post-process-fcn (xmms2-value->scheme-data value))))))))

(define-synchronous-action xmms2/current-id xmms2:primitive/current-id)
(define-synchronous-action xmms2/pause xmms2:primitive/pause)
(define-synchronous-action xmms2/play xmms2:primitive/play)
(define-synchronous-action xmms2/status xmms2:primitive/status integer->status)
(define-synchronous-action xmms2/stop xmms2:primitive/stop)
(define-synchronous-action xmms2/tickle xmms2:primitive/tickle)
