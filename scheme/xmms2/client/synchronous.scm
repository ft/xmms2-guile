;; Copyright (c) 2017 xmms2-guile workers, All rights reserved.
;;
;; Terms for redistribution and use can be found in LICENCE.

(define-module (xmms2 client synchronous)
  #:use-module (xmms2 client)
  #:use-module (xmms2 io)
  #:export (request->reply
            request->value))

(define (request->reply connection data)
  (xmms2-send connection data)
  (xmms2-recv connection))

(define (request->value connection data)
  (reply->value (request->reply connection data)))
