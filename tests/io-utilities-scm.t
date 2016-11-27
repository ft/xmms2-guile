;; -*- scheme -*-

;; Copyright (c) 2015 xmms2-guile workers, All rights reserved.
;;
;; Terms for redistribution and use can be found in LICENCE.

(use-modules (test tap)
             (test setup)
             (rnrs bytevectors)
             (xmms2 io))

(init-test-tap!)

(force-import (xmms2 io) packet->data)
(define hello-world (string->utf8 "Hello World."))

(with-fs-test-bundle
 (plan 2)
 (define-test "packet->data: primitive packet"
   (pass-if-equal? (packet->data hello-world)
                   hello-world))
 (define-test "packet->data: list packet"
   (pass-if-equal? (packet->data (list (string->utf8 "Hello")
                                       (string->utf8 " ")
                                       (string->utf8 "World")
                                       (string->utf8 ".")))
                   hello-world)))
