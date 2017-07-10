;; Copyright (c) 2017 xmms2-guile workers, All rights reserved.
;;
;; Terms for redistribution and use can be found in doc/LICENCE.

(define-module (genipc stage-1)
  #:use-module (ice-9 optargs)
  #:use-module (sxml match)
  #:use-module (genipc utilities)
  #:export (sxml->sexp))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Stage 1: Destructure SXML into s-expressions. This massages the data from
;; XML into something that the following stages can work with easier. This also
;; does most of the name adjustments (it really should to all of them). This
;; stage makes heavy use of ‘sxml-match’.

(define (handle-unknown-xml name data)
  (notify "~a: Cannot handle XML entry: ~a~%" name data))

(define (type->sexp type)
  ;;(notify "type: ~a~%" type)
  (sxml-match type
    ((xmms::binary) '(binary))
    ((xmms::collection) '(collection))
    ((xmms::int) '(integer))
    ((xmms::string) '(string))
    ((xmms::unknown) '(unknown))
    ((xmms::list ,rest ...) `((list ,@(am type->sexp rest))))
    ((xmms::dictionary ,rest ...) `((dictionary ,@(am type->sexp rest))))
    ((xmms::enum-value (@ (name ,n))) `((enumeration ,(adjust-name/enum n))))
    (,otherwise (begin (handle-unknown-xml 'type->sexp otherwise)
                       (list type)))))

(define (method-arg->sexp arg)
  ;;(notify "arg: ~a~%" arg)
  (sxml-match arg
    ((xmms::name ,name) `((name ,(adjust-name/arg name))))
    ((xmms::type ,type) `((type ,@(type->sexp type))))
    ((xmms::documentation ,doc) `((documentation ,(cleanup-documentation doc))))
    ((xmms::default-hint ,hint) `((default-hint ,hint)))
    (,otherwise (begin (handle-unknown-xml 'method-arg->sexp otherwise)
                       (list arg)))))

(define (return-value->sexp return-value)
  ;;(notify "return-value: ~a~%" return-value)
  (sxml-match return-value
    ((xmms::type ,type) `((type ,@(type->sexp type))))
    ((xmms::documentation ,doc) `((documentation ,(cleanup-documentation doc))))
    (,otherwise (begin (handle-unknown-xml 'return-value->sexp otherwise)
                       (list return-value)))))

(define (method->sexp method)
  ;;(notify "method: ~a~%" method)
  (sxml-match method
    ((xmms::name ,name) `((name ,(adjust-name/method name))))
    ((xmms::documentation ,doc) `((documentation ,(cleanup-documentation doc))))
    ((xmms::argument ,rest ...) `((argument ,@(am method-arg->sexp rest))))
    ((xmms::return_value ,rest ...) `((return-value
                                       ,@(am return-value->sexp rest))))
    (,otherwise (begin (handle-unknown-xml 'method->sexp otherwise)
                       (list method)))))

(define (broadcast-or-signal->sexp bs)
  ;;(notify "bs: ~a~%" bs)
  (sxml-match bs
    ((xmms::id ,id) `((identifier ,(string->number id))))
    ((xmms::name ,name) `((name ,(adjust-name/b-or-s name))))
    ((xmms::documentation ,doc) `((documentation ,(cleanup-documentation doc))))
    ((xmms::return_value ,rest ...) `((return-value
                                       ,@(am return-value->sexp rest))))
    (,otherwise (begin (handle-unknown-xml 'broadcast-or-signal->sexp otherwise)
                       (list bs)))))

(define (enum->sexp elst)
  ;;(notify "elst: ~a~%" elst)
  (sxml-match elst
    ((xmms::name ,name) `((name ,(adjust-name/enum name))))
    ((xmms::member (@ . ,attr) ,member) (if (null? attr)
                                            `((member ,(adjust-name/member member)))
                                            `((member ,attr ,(adjust-name/member member)))))
    ((xmms::namespace-hint ,nsh)
     `((namespace-hint ,(adjust-name/namespace-hint nsh))))
    (,otherwise (begin (handle-unknown-xml 'enum->sexp otherwise)
                       (list elst)))))

(define (transform-value type value)
  (let* ((transformers `((integer . ,string->number)))
         (type (if (string? type)
                   (string->symbol type)
                   type))
         (transformer* (assq-ref transformers type))
         (transformer (or transformer* (lambda (x)
                                         (notify "transform-value: Unknown type: ~a (~a)~%"
                                                 type x)
                                         x))))
    (transformer value)))

(define (constant->sexp clst)
  ;;(notify "clst: ~a~%" clst)
  (sxml-match clst
    ((xmms::name ,name) `((name ,(adjust-name/constant name))))
    ((xmms::value (@ (type ,t)) ,v) (list `(value ,(transform-value t v))))
    (,otherwise (begin (handle-unknown-xml 'enum->sexp otherwise)
                       (list clst)))))

(define (sxml->sexp tree)
  ;;(notify "tree: ~a~%" tree)
  (sxml-match tree
    ((*TOP* (*PI* ,stuff ...) ,things ...)
     `(xmms2-ipc-description ,@(am sxml->sexp things)))
    ((xmms::ipc (@ (version ,v)) ,objs ...) `((version ,v) ,@(am sxml->sexp objs)))
    ((xmms::object (xmms::name ,name) ,things ...)
     `((object (name ,(adjust-name/object name)) ,@(am sxml->sexp things))))
    ((xmms::method ,rest ...) `((method ,@(am method->sexp rest))))
    ((xmms::broadcast ,rest ...) `((broadcast ,@(am broadcast-or-signal->sexp rest))))
    ((xmms::signal ,rest ...) `((signal ,@(am broadcast-or-signal->sexp rest))))
    ((xmms::enum ,rest ...) `((enum ,@(am enum->sexp rest))))
    ((xmms::constant ,rest ...) `((constant ,@(am constant->sexp rest))))
    (,otherwise (begin (handle-unknown-xml 'sxml->sexp otherwise)
                       (list tree)))))
