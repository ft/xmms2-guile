;; Copyright (c) 2015 xmms2-guile workers, All rights reserved.
;;
;; Terms for redistribution and use can be found in LICENCE.

(define-module (xmms2 constants)
  #:use-module (rnrs bytevectors)
  #:use-module (xmms2 data-conversion)
  #:use-module (xmms2 enumeration))

;; Some utilities:
(define (int32-bv value)
  (let ((rv (make-bytevector 4 0)))
    (uint32-set! rv 0 value)
    rv))

;; ‘define-type-constant’ defines two variables given a name FOO and
;; a value 1234:
;;
;; (define-public TYPE-FOO 1234)
;; (define-public TAG-FOO #vu(0 0 4 210))
(define-syntax define-type-constant
  (lambda (x)
    (define (prefixed-name kw prefix name)
      (datum->syntax kw (symbol-append prefix
                                   (syntax->datum name))))
    (syntax-case x ()
      ((kw name value)
       (with-syntax ((type-id-name (prefixed-name #'kw 'TYPE- #'name))
                     (tag-name (prefixed-name #'kw 'TAG- #'name)))
         #'(begin (define-public type-id-name value)
                  (define-public tag-name (int32-bv type-id-name))))))))

(define-syntax value-name-pair
  (syntax-rules ()
    ((_ value) (cons value (quote value)))))

;; Constant definitions below:

(define-public XMMS2-HEADER-SIZE 16)
(define-public XMMS2-DEFAULT-PORT 9667)
(define-public COLLECTION_NAMESPACE_ALL "*")
(define-public COLLECTION-NAMESPACE-COLLECTIONS "Collections")
(define-public COLLECTION-NAMESPACE-PLAYLISTS "Playlists")
(define-public NAME-OF-ACTIVE-PLAYLIST "_active")

(define-enum (=> define-type-constant)
  NONE
  ERROR
  INT64
  STRING
  COLLECTION
  BINARY
  LIST
  DICTIONARY
  BITBUFFER
  FLOAT)

(define-public xref-types
  (list (value-name-pair TYPE-NONE)
        (value-name-pair TYPE-ERROR)
        (value-name-pair TYPE-INT64)
        (value-name-pair TYPE-STRING)
        (value-name-pair TYPE-COLLECTION)
        (value-name-pair TYPE-BINARY)
        (value-name-pair TYPE-LIST)
        (value-name-pair TYPE-DICTIONARY)
        (value-name-pair TYPE-BITBUFFER)
        (value-name-pair TYPE-FLOAT)))
