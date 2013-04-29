;; Copyright (c) 2013 xmms2-guile workers, All rights reserved.
;;
;; Terms for redistribution and use can be found in LICENCE.

(define-module (xmms2 core primitives)
  #:use-module (ice-9 format))

(define (extension-path library-name)
  "Returns the proper value for the directory in which to look for extension
modules (i.e. dynamic libraries that add features to guile).

This is either the value of the `$XMMS2_GUILE_EXTENSION_PATH' environment
variable, if it is set, or the value of the compile-time option
`build-cfg/extension-path'."
  (let ((ext-path (getenv "XMMS2_GUILE_EXTENSION_PATH")))
    (if ext-path
        (format #f "~a/~a" ext-path library-name)
        library-name)))

(define (load-xmms2-extension)
  (load-extension (extension-path "libxmms2-guile")
                  "xmms2_guile_ext_init"))

(load-xmms2-extension)
