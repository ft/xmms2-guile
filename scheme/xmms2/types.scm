;; Copyright (c) 2016 xmms2-guile workers, All rights reserved.
;;
;; Terms for redistribution and use can be found in LICENCE.

(define-module (xmms2 types)
  #:use-module (ice-9 optargs)
  #:use-module (srfi srfi-9)
  #:use-module (xmms2 constants collection)
  #:export (collection
            collection?
            collection-fold
            make-collection
            make-universe
            collection-operator
            collection-attributes
            collection-attribute
            collection-idlist
            collection-children
            dictionary?
            dictionary-data
            dictionary-type
            make-dictionary
            association-list?
            non-complex-number?))

(define (association-list? data)
  (and (list? data)
       (not (null? data))
       (pair? (car data))
       (symbol? (caar data))))

(define-record-type <dictionary>
  (make-dictionary* data type)
  dictionary?
  (data dictionary-data)
  (type dictionary-type))

(define* (make-dictionary data #:key (type #f))
  (make-dictionary* data type))

(define (non-complex-number? data)
  (and (number? data) (zero? (imag-part data))))

(define-record-type <collection>
  (make-collection operator attributes idlist children)
  collection?
  (operator collection-operator)
  (attributes collection-attributes)
  (idlist collection-idlist)
  (children collection-children))

(define (collection-attribute c a)
  (assq-ref (collection-attributes c) a))

(define (make-universe)
  (make-collection COLLECTION-TYPE-UNIVERSE '() '() '()))

(define (node-is-leaf? c)
  (zero? (length (collection-children c))))

(define* (collection-fold proc
                          init
                          collection
                          #:key
                          (left-to-right? #t)
                          (order 'pre)
                          (pick #f))
  "This function implements `fold'-like functionality for the collection data
type.

It walks `collection' and calls `proc' with two arguments: The current node's
payload and the return value of the last call to `proc'. On the first call of
`proc' `init' is used as the second argument. The function returns the proc's
last return value.

The function supports a number of keyword-style arguments to alter its course
of action:

  #:order           One of: pre (the default, and fallback in case of any
                    other value), post or level

  #:left-to-right?  Boolean (defaults to #t); controls the order in which
                    child-nodes are processed.

  #:pick            Enables the caller to pick a set of information contained
                    within the collection: operator, attributes, idlist,
                    children. Any other value (including #f, which is the
                    default) passes the entire collection structure to proc."

  (define (recurse node acc)
    (collection-fold proc acc node
                     #:left-to-right? left-to-right?
                     #:order order))

  (define (children node)
    (let ((cl (collection-children node)))
      (if left-to-right? cl (reverse cl))))

  (define (pick-node node)
    (case pick
      ((operator) (collection-operator node))
      ((attributes) (collection-attributes node))
      ((idlist) (collection-idlist node))
      ((children) (collection-children node))
      (else node)))

  (define (recursive-traversal nodes acc)
    (if (null? nodes)
        acc
        (let ((node (car nodes)))
          (recursive-traversal (cdr nodes)
                               (if (node-is-leaf? node)
                                   (proc (pick-node node) acc)
                                   (recurse node acc))))))

  (case order
    ((post)
     (proc (pick-node collection)
           (recursive-traversal (children collection) init)))
    ((level)
     (let continue ((queue (list collection))
                    (acc init))
       (if (null? queue)
           acc
           (let ((rem (cdr queue))
                 (node (car queue)))
             (continue (append rem (children node))
                       (proc (pick-node node) acc))))))
    (else
     (recursive-traversal (children collection)
                          (proc (pick-node collection) init)))))

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
        (cond ((eq? source 'universe) #'(make-universe))
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
         #'(make-collection operator '((field . arg-a)) '() (list source))))
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
      ((_ exp) (identifier? #'exp) #'exp)
      ((_ exp ...) #'(syntax-error "Invalid collection expression:" exp ...)))))

(define-syntax collection
  (lambda (x)
    (syntax-case x ()
      ;; If the DSL is entered with exactly one expression, expand that
      ;; expression and return it. If it was called with multiple expressions,
      ;; expand all of them and wrap them into (list ...).
      ((_ exp) #'(expand-collection-dsl exp))
      ((_ exp0 expn ...) #'(list (collection exp0) (collection expn) ...)))))
