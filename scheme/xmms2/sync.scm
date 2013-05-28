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
        (xmms2:primitive/unref-connection
         (get-xmms2-connection-container connection))))
    (lambda (key . args)
      (fail (car args)))))

(define (synchronous-action action connection . args)
  (let ((result (apply action
                       (cons (get-xmms2-connection-container connection)
                             args))))
    (xmms2:primitive/sync-wait result)))

;; TODO: This is four times about the same expansion. There must be a way to
;; write this in a less redundant way.
(define-syntax define-synchronous-action
  (lambda (x)
    (syntax-case x ()
      ((_ synchronous-function (a0 ...) primitive-function)
       #'(define-public (synchronous-function connection a0 ...)
           (let* ((result (synchronous-action primitive-function
                                              connection
                                              a0 ...))
                  (value (xmms2-result->value result)))
             (xmms2-value->scheme-data value))))

      ((_ synchronous-function primitive-function)
       #'(define-public (synchronous-function connection)
           (let* ((result (synchronous-action primitive-function connection))
                  (value (xmms2-result->value result)))
             (xmms2-value->scheme-data value))))

      ((_ synchronous-function primitive-function (a0 ...) post-process-fcn)
       #'(define-public (synchronous-function connection a0 ...)
           (let* ((result (synchronous-action primitive-function
                                              connection
                                              a0 ...))
                  (value (xmms2-result->value result)))
             (post-process-fcn (xmms2-value->scheme-data value)))))

      ((_ synchronous-function primitive-function post-process-fcn)
       #'(define-public (synchronous-function connection)
           (let* ((result (synchronous-action primitive-function connection))
                  (value (xmms2-result->value result)))
             (post-process-fcn (xmms2-value->scheme-data value))))))))

(define-synchronous-action xmms2/current-id xmms2:primitive/current-id)
(define-synchronous-action xmms2/pause xmms2:primitive/pause)
(define-synchronous-action xmms2/play xmms2:primitive/play)
(define-synchronous-action xmms2/playtime xmms2:primitive/playtime)
(define-synchronous-action xmms2/status xmms2:primitive/status integer->status)
(define-synchronous-action xmms2/stop xmms2:primitive/stop)
(define-synchronous-action xmms2/tickle xmms2:primitive/tickle)
(define-synchronous-action xmms2/volume-get xmms2:primitive/volume-get)

(define-synchronous-action xmms2/active-playlist xmms2:primitive/active-playlist)
(define-synchronous-action xmms2/get-playlists xmms2:primitive/get-playlists)
(define-synchronous-action xmms2/playlist-entries (playlist)
                           xmms2:primitive/playlist-entries)

(define-synchronous-action xmms2/medialib-get-info (id)
                           xmms2:primitive/medialib-get-info)

(define-synchronous-action xmms2/config-list xmms2:primitive/config-list)
