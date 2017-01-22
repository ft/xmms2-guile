;; Copyright (c) 2017 xmms2-guile workers, All rights reserved.
;;
;; Terms for redistribution and use can be found in LICENCE.

(define-module (xmms2 client)
  #:use-module (ice-9 optargs)
  #:use-module (xmms2 io)
  #:export (default-uri))

(define (default-uri)
  "Returns the default IPC uri used by xmms2 servers."
  (string-concatenate `("unix:///tmp/xmms-ipc-"
                        ,(passwd:name (getpwuid (geteuid))))))
