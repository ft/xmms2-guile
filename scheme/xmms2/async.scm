;; Copyright (c) 2013 xmms2-guile workers, All rights reserved.
;;
;; Terms for redistribution and use can be found in LICENCE.

;; In contrast to synchronous connections, in asynchronous connections you do
;; NOT wait for server responses after submitting a request. Instead, you
;; register a callback procedure, that is run whenever a reply to the request
;; in question arrives from the server.

(define-module (xmms2 sync)
  #:use-module (ice-9 optargs)
  #:use-module (xmms2 common)
  #:use-module (xmms2 core connection)
  #:use-module (xmms2 core primitives)
  #:use-module (xmms2 core value))

;; Alist that maps event names to values (those that came in the server reply).
(define *async-values* '())

;; Alist that maps event names to a list of callback procedures. When a server
;; reply arrives, all callbacks for an event name are called in succession.
(define *async-callbacks* '())

(define (async-run-callbacks event)
  (let ((cb-list (assq-ref *async-callbacks* event))
        (value (assq-ref *async-values* event)))
    (if cb-list
        (for-each (lambda (cb)
                    (cb event value))
                  cb-list))))

(define (register-callback event cb)
  (let ((old (assq-ref *async-callbacks* event)))
    (unless old (xmms2:primitive/async-initialise-event event))
    (assq-set! *async-callbacks* event (cons cb (or old '())))))

(define (call-io-and-set-status connection function)
  (if (= 0 (function (get-xmms2-connection-container connection)))
      (set-xmms2-connection-status! connection 'down)))

(define (xmms2-server-read connection)
  (call-io-and-set-status connection xmms2:primitive/server-read))

(define (xmms2-server-write connection)
  (if (xmms2:primitive/server-data-pending
       (get-xmms2-connection-container connection))
      (call-io-and-set-status connection xmms2:primitive/server-write)))

(define (xmms2-server-io connection)
  (xmms2-server-read connection)
  (xmms2-server-write connection))

(define* (xmms2-select-loop #:key
                            (read-handler (lambda (c) (xmms2-server-io c)))
                            (except-handler (lambda (c) #t))
                            (post-handler (lambda (c) #t))
                            (init-handler (lambda (c) #t))
                            (seconds #f)
                            (micro-seconds #f)
                            (server default-uri)
                            (client default-client-id)
                            (fail default-fail-handler))
  (with-xmms2-connection
   #:server server
   #:client client
   #:fail fail
   #:handler
   (lambda (c)
     (init-handler c)
     (let ((fds (list (xmms2:primitive/get-connection-fd c))))
       (let next ((continue? #t))
         (if continue?
             (let* ((result (select fds '() fds seconds micro-seconds))
                    (got-reads? (not (null? (car result))))
                    (got-excepts? (not (null? (caddr result)))))
               (and got-excepts? (except-handler c))
               (and got-reads? (read-handler c))
               (async-run-all-callbacks)
               (next (post-handler c)))))))))
