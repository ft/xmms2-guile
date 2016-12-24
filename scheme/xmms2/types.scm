;; Copyright (c) 2016 xmms2-guile workers, All rights reserved.
;;
;; Terms for redistribution and use can be found in LICENCE.

(define-module (xmms2 types)
  #:use-module (srfi srfi-9)
  #:use-module (xmms2 constants collection)
  #:export (dictionary?
            non-complex-number?))

(define (dictionary? data)
  (and (list? data)
       (not (null? data))
       (pair? (car data))
       (symbol? (caar data))))

(define (non-complex-number? data)
  (and (number? data) (zero? (imag-part data))))

(define-record-type <collection>
  (make-collection operator attributes idlist children)
  collection?
  (operator collection-operator)
  (attributes collection-attributes)
  (idlist collection-idlist)
  (children collection-children))

(define-syntax expand-collection-dsl
  (lambda (x)

    (define (set-operator? x)
      (let ((op (syntax->datum x)))
        (not (not (memq op '(∪ ∩ UNION INTERSECTION or and ¬ not COMPLEMENT))))))

    (define (unary-operator? x)
      (let ((op (syntax->datum x)))
        (not (not (memq op '(has))))))

    (define (binary-operator? x)
      (let ((op (syntax->datum x)))
        (not (not (memq op '(= != ≠ < ≤ <= > ≥ >= ~ match))))))

    (define (process-operator x)
      (let ((op (syntax->datum x)))
        (assq-ref (list (cons '= #'COLLECTION-TYPE-EQUALS)
                        (cons '≠ #'COLLECTION-TYPE-NOTEQUAL)
                        (cons '!= #'COLLECTION-TYPE-NOTEQUAL)
                        (cons '∩ #'COLLECTION-TYPE-INTERSECTION)
                        (cons 'INTERSECTION #'COLLECTION-TYPE-INTERSECTION)
                        (cons 'or #'COLLECTION-TYPE-INTERSECTION)
                        (cons '∪ #'COLLECTION-TYPE-UNION)
                        (cons 'UNION #'COLLECTION-TYPE-UNION)
                        (cons 'and #'COLLECTION-TYPE-UNION)
                        (cons '> #'COLLECTION-TYPE-GREATER)
                        (cons '≥ #'COLLECTION-TYPE-GREATEREQ)
                        (cons '>= #'COLLECTION-TYPE-GREATEREQ)
                        (cons '< #'COLLECTION-TYPE-SMALLER)
                        (cons '≤ #'COLLECTION-TYPE-SMALLEREQ)
                        (cons '<= #'COLLECTION-TYPE-SMALLEREQ)
                        (cons '¬ #'COLLECTION-TYPE-COMPLEMENT)
                        (cons 'not #'COLLECTION-TYPE-COMPLEMENT)
                        (cons 'COMPLEMENT #'COLLECTION-TYPE-COMPLEMENT)
                        (cons '~ #'COLLECTION-TYPE-MATCH)
                        (cons 'match #'COLLECTION-TYPE-MATCH)
                        (cons 'has #'COLLECTION-TYPE-HAS))
                  op)))

    (define (process-argument x)
      (let ((arg (syntax->datum x)))
        (cond ((symbol? arg) (symbol->string arg))
              (else x))))

    (define (process-source x)
      (let ((source (syntax->datum x)))
        (cond ((eq? source 'universe)
               #'(make-collection COLLECTION-TYPE-UNIVERSE '() '() '()))
              (else x))))

    (syntax-case x (from universe)
      ((_ (op exp ...)) (set-operator? #'op)
       (with-syntax ((operator (process-operator #'op)))
         #'(make-collection operator
                            '() '() (list (expand-collection-dsl exp) ...))))
      ((_ (op a from s)) (unary-operator? #'op)
       (with-syntax ((operator (process-operator #'op))
                     (arg-a (process-argument #'a))
                     (source (process-source #'s)))
         #'(make-collection operator '(field . arg-a) '() (list source))))
      ((_ (op a)) (unary-operator? #'op)
       (with-syntax ((operator (process-operator #'op))
                     (arg-a (process-argument #'a)))
         #'(expand-collection-dsl (op a from universe))))
      ((_ (a op b from s)) (binary-operator? #'op)
       (with-syntax ((operator (process-operator #'op))
                     (arg-a (process-argument #'a))
                     (arg-b (process-argument #'b))
                     (source (process-source #'s)))
         #'(make-collection operator '((field . arg-a) (value . arg-b)) '()
                            (list source))))
      ((_ (a op b)) (binary-operator? #'op)
       #'(expand-collection-dsl (a op b from universe)))
      ((_ exp ...) #'(syntax-error "Invalid collection expression:" exp ...)))))

(define-syntax collection
  (lambda (x)
    (syntax-case x ()
      ;; If the DSL is entered with exactly one expression, expand that
      ;; expression and return it. If it was called with multiple expressions,
      ;; expand all of them and wrap them into (list ...).
      ((_ exp) #'(expand-collection-dsl exp))
      ((_ exp0 expn ...) #'(list (collection exp0) (collection expn) ...)))))
