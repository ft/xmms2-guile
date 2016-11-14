;; Copyright (c) 2016 xmms2-guile workers, All rights reserved.
;;
;; Terms for redistribution and use can be found in LICENCE.

(define-module (xmms2 ipc)
  #:use-module (ice-9 optargs)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:use-module (xmms2 constants)
  #:use-module (xmms2 payload)
  #:export (define-ipc-packet-generator
            make-ipc-broadcast
            make-ipc-method
            make-ipc-signal
            ipc-broadcast?
            ipc-method?
            ipc-signal?
            broadcast-name
            broadcast-object
            broadcast-identifier
            broadcast-documentation
            broadcast-return-value
            broadcast-generator
            method-name
            method-object
            method-identifier
            method-documentation
            method-arguments
            method-return-value
            method-generator
            signal-name
            signal-object
            signal-identifier
            signal-documentation
            signal-return-value
            signal-generator))

;; The following macro can be used to define functions, that generate XMMS2
;; protocol data from scheme data structures. Example:
;;
;; (define-ipc-packet-generator make-hello public OBJECT-MAIN CMD-HELLO
;;   "Say 'Hello' to the server."
;;   (integer protocol-version) (string client-name))
;;
;; This will define a function (using ‘define-public’, because of the ‘public’
;; tag), that does the following:
;;
;; (make-hello PROTOCOL-VERSION "cmc")
;;
;;   => (#vu8(0 0 0 1 0 0 0 32 0 0 0 0 0 0 0 36)    ; Header
;;       #vu8(0 0 0 6)                              ; Tag: List
;;       #vu8(0 0 0 0 0 0 0 2)                      ; List length: 2
;;       #vu8(0 0 0 2)                              ; Tag: Integer
;;       #vu8(0 0 0 0 0 0 0 23)                     ; Integer: 23
;;       #vu8(0 0 0 3)                              ; Tag: String
;;       #vu8(0 0 0 4 99 109 99 0))                 ; XMMS2-String: "cmc"
;;
;; ...which is indeed a valid message in XMMS2's protocol.
;;
;; The generated function does perform type checks on the arguments passed to
;; it and throws ‘type-error’ in case an invalid datum was handed to it in
;; either of the required arguments.
;;
;; All generated ipc code in (xmms2 ipc *) uses this macro to create its
;; protocol generators.
(define-syntax define-ipc-packet-generator
  (lambda (ctx)

    (define (generate-predicate-checks kw lst)
      (let loop ((rest lst) (return-value '()))
        (if (null? rest)
            (datum->syntax kw (reverse return-value))
            (loop (cdr rest)
                  (cons (let ((type (syntax->datum (caar rest)))
                              (argument (syntax->datum (cadar rest))))
                          (cond
                           ((eq? type 'integer) `(integer? ,argument))
                           ((eq? type 'string) `(string? ,argument))
                           ;; We could perform much stricter tests with lists
                           ;; and dicts here, but that would impact performance
                           ;; with large data structures. Maybe we could make
                           ;; this optional via a #:keyword flag, similar to
                           ;; #:cookie.
                           ((and (list? type) (eq? 'dictionary (car type)))
                            `(and (list? ,argument)
                                  (pair? (car ,argument))
                                  (string? (caar ,argument))))
                           ((and (list? type) (eq? 'list (car type)))
                            `(list? ,argument))
                           (else
                            ;; This should be fatal in the future.
                            ;;(throw 'xmms2/unknown-type type 'with argument)
                            (format (current-error-port)
                                    "WARNING: Unknown type `~a' with argument `~a'~%"
                                    type argument))))
                        return-value)))))

    (syntax-case ctx (public private)
      ((kw ipc private object identifier (type name) ...)
       #'(kw define* ipc object identifier "Documentation missing." (type name) ...))
      ((kw ipc public object identifier (type name) ...)
       #'(kw define*-public ipc object identifier "Documentation missing." (type name) ...))
      ((kw ipc private object identifier documentation (type name) ...)
       #'(kw define* ipc object identifier documentation (type name) ...))
      ((kw ipc public object identifier documentation (type name) ...)
       #'(kw define*-public ipc object identifier documentation (type name) ...))
      ((kw definer ipc object identifier documentation (type name) ...)
       (with-syntax (((payload types arguments acc)
                      (generate-temporaries '(payload types arguments acc))))
         #`(definer (ipc name ... #:key (cookie 0))
             documentation
             (if (not (and #,@(generate-predicate-checks #'kw #'((type name) ...))))
                 (apply throw 'xmms2/type-error
                        (let zip ((types '(type ...))
                                  (arguments (list name ...))
                                  (acc '()))
                          (if (or (null? types)
                                  (null? arguments))
                              (reverse acc)
                              (zip (cdr types)
                                   (cdr arguments)
                                   (cons (cons (car types)
                                               (car arguments))
                                         acc)))))
                 (let ((payload (make-list-payload (list name ...))))
                   (cons (make-protocol-header object identifier cookie
                                               (payload-length payload))
                         payload)))))))))

;; The record types are used to pass around methods, broadcasts and signals
;; with all the available information for full introspection.

(define-record-type <ipc-broadcast>
  (make-ipc-broadcast name object identifier documentation return-value generator)
  ipc-broadcast?
  (name broadcast-name)
  (object broadcast-object)
  (identifier broadcast-identifier)
  (documentation broadcast-documentation)
  (return-value broadcast-return-value)
  (generator broadcast-generator))

(define-record-type <ipc-method>
  (make-ipc-method name object identifier documentation arguments return-value generator)
  ipc-method?
  (name method-name)
  (object method-object)
  (identifier method-identifier)
  (documentation method-documentation)
  (arguments method-arguments)
  (return-value method-return-value)
  (generator method-generator))

(define-record-type <ipc-signal>
  (make-ipc-signal name object identifier documentation return-value generator)
  ipc-signal?
  (name signal-name)
  (object signal-object)
  (identifier signal-identifier)
  (documentation signal-documentation)
  (return-value signal-return-value)
  (generator signal-generator))
