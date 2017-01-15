;; Copyright (c) 2016 xmms2-guile workers, All rights reserved.
;;
;; Terms for redistribution and use can be found in LICENCE.

(define-module (xmms2 types)
  #:use-module (ice-9 optargs)
  #:use-module (srfi srfi-9)
  #:use-module (xmms2 constants)
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
            dict
            dict-ref
            make-dictionary
            association-list?
            non-complex-number?))

(define (non-complex-number? data)
  (and (number? data) (zero? (imag-part data))))

(define (association-list? data)
  (and (list? data)
       (not (null? data))
       (pair? (car data))
       (symbol? (caar data))))

(define-record-type <dictionary>
  (make-dictionary data)
  dictionary?
  (data dictionary-data))

(define (dict key dict)
  (assq key (dictionary-data dict)))

(define (dict-ref key dict)
  (assq-ref (dictionary-data dict) key))

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

    (define (id-list-operator? x)
      (let ((op (syntax->datum x)))
        (not (not (memq op '(‣ ID-LIST id-list))))))

    (define (unary-operator? x)
      (let ((op (syntax->datum x)))
        (not (not (memq op '(has REFERENCE reference →))))))

    (define (binary-operator? x)
      (let ((op (syntax->datum x)))
        (not (not (memq op '(= != ≠ < ≤ <= > ≥ >= ~ match))))))

    (define (process-operator operator args)
      (define (unary-field lst)
        #`(list (cons 'field #,(car lst))))

      (define (binary-field-value lst)
        #`(list (cons 'field #,(car lst))
                (cons 'value #,(cadr lst))))

      (define (coll:reference lst)
        #`(list (cons 'reference #,(car lst))
                (cons 'namespace COLLECTION-NAMESPACE-COLLECTIONS)))

      (define (coll:id-list lst)
        #'(list))

      ;; TODO:
      ;;
      ;;   COLLECTION-TYPE-TOKEN
      ;;   COLLECTION-TYPE-ORDER
      ;;   COLLECTION-TYPE-LIMIT
      ;;   COLLECTION-TYPE-MEDIASET
      (let* ((id (assq-ref
                  (list (cons '= #'COLLECTION-TYPE-EQUALS)
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
                        (cons 'has #'COLLECTION-TYPE-HAS)
                        (cons '→ #'COLLECTION-TYPE-REFERENCE)
                        (cons 'reference #'COLLECTION-TYPE-REFERENCE)
                        (cons 'REFERENCE #'COLLECTION-TYPE-REFERENCE)
                        (cons '‣ #'COLLECTION-TYPE-IDLIST)
                        (cons 'id-list #'COLLECTION-TYPE-IDLIST)
                        (cons 'ID-LIST #'COLLECTION-TYPE-IDLIST))
                  (syntax->datum operator)))
             (proc (assoc
                    (syntax->datum id)
                    (list (cons 'COLLECTION-TYPE-INTERSECTION identity)
                          (cons 'COLLECTION-TYPE-UNION identity)
                          (cons 'COLLECTION-TYPE-COMPLEMENT identity)
                          (cons 'COLLECTION-TYPE-HAS unary-field)
                          (cons 'COLLECTION-TYPE-REFERENCE coll:reference)
                          (cons 'COLLECTION-TYPE-IDLIST coll:id-list)))))
        (list id ((if proc (cdr proc) binary-field-value) args))))

    ;; This expands the operand sub-language
    (define* (process-argument x #:key (post identity))
      (syntax-case x (|)
        ;; If an argument is a parenthesized expression starting with a bar
        ;; character, strip away one level of parentheses and the bar symbol
        ;; and insert the inner expression for evaluation. This way the DSL
        ;; supports arbitrarily complex expressions in its arguments while
        ;; allowing the user to express collections with the least amount of
        ;; bother.
        ((| exp) #'exp)
        ;; If a non-parenthesized expression looks like an identifier, turn it
        ;; into a string.
        (exp (identifier? #'exp)
             (with-syntax ((str (post (symbol->string (syntax->datum #'exp)))))
               #'str))
        (exp (string? (syntax->datum #'exp))
             (with-syntax ((str (post (syntax->datum #'exp))))
               #'str))
        ;; Insert everything else verbatim.
        (exp #'exp)))

    (define (add-attribute attributes a)
      (let loop ((rest attributes) (acc '()) (append? #t))
        (syntax-case rest (list cons)
          (() (if append? (append acc (list a)) acc))
          ((list . args) (loop #'args #`(#,@acc list) append?))
          (((cons key value) . args) (equal? (syntax->datum #'key)
                                             (syntax->datum (cadr a)))
           (loop #'args #`(#,@acc (cons key #,(caddr a))) #f))
          (((cons key value) . args)
           (loop #'args #`(#,@acc (cons key value)) append?)))))

    ;; This expands the property-list sub-language
    (define* (process-prop-list kw attributes lst
                                #:key (default-source #'(list (make-universe))))
      (unless (zero? (modulo (length lst) 2))
        (syntax-violation 'collection
                          (format #f "Property list has to have an even number of elements! ~a"
                                  (syntax->datum lst))
                          x kw))
      (let loop ((rest lst) (attr attributes) (source default-source))
        (syntax-case rest (universe)
          (() (list attr source))
          ((#:from universe . args)
           (loop #'args attr #'(list (make-universe))))
          ((#:from place . args) (identifier? #'place)
           (loop #'args attr #'(list place)))
          ((#:from place . args)
           (loop #'args attr #'(list (expand-collection-dsl place))))
          ((#:case-sensitive? active? . args)
           (begin (unless (let ((datum (syntax->datum #'active?)))
                            (or (boolean? datum) (list? datum)))
                    (syntax-violation 'collection
                                      "#:case-sensitive? expects boolean argument!"
                                      x kw))
                  (with-syntax ((value (if (syntax->datum #'active?) #'1 #'0)))
                    (loop #'args (add-attribute attr #'(cons 'case-sensitive value))
                          source))))
          ((#:namespace ns . args)
           (loop #'args (add-attribute attr #`(cons 'namespace #,(process-argument #'ns))) #''()))
          ((#:source s . args)
           (loop #'args
                 (add-attribute attr #`(cons 'source #,(process-argument #'s)))
                 source))
          ((#:source-preference s . args)
           (loop #'args
                 (add-attribute attr #`(cons 'source-preference
                                             #,(process-argument #'s)))
                 source))
          ((#:start s . args)
           (loop #'args
                 (add-attribute attr #`(cons 'start #,(process-argument #'s)))
                 source))
          ((#:length l . args)
           (loop #'args
                 (add-attribute attr #`(cons 'length #,(process-argument #'l)))
                 source))
          ((#:type t . args)
           (loop #'args (add-attribute attr #`(cons 'type #,(process-argument #'t))) source))
          ((#:order o . args)
           (loop #'args
                 (add-attribute attr
                                #`(cons 'order
                                        #,(process-argument #'o
                                                            #:post string-upcase)))
                 source))
          ((#:collation c . args)
           (loop #'args
                 (add-attribute attr
                                #`(cons 'collation
                                        #,(process-argument #'c
                                                            #:post string-upcase)))
                 source))
          ((key . args) (keyword? (syntax->datum #'key))
           (syntax-violation 'collection
                             (format #f "Unknown keyword ~a" (syntax->datum #'key))
                             x kw))
          ((key . args)
           (syntax-violation 'collection
                             (format #f "Expected keyword at `~s'" (syntax->datum #'key))
                             x kw)))))

    ;; This is the expansion for the main language
    (syntax-case x ()
      ((kw (op exp ...)) (set-operator? #'op)
       (with-syntax (((operator attributes) (process-operator #'op #'())))
         #'(make-collection operator
                            '()
                            '()
                            (list (expand-collection-dsl exp) ...))))

      ((kw (op a rest ...)) (id-list-operator? #'op)
       (with-syntax (((operator attributes) (process-operator #'op '())))
         (with-syntax (((attr source)
                        (process-prop-list #'kw #'attributes #'(rest ...)
                                           #:default-source #''())))
           #'(make-collection operator attr a source))))

      ((kw (op a rest ...)) (unary-operator? #'op)
       (with-syntax (((operator attributes)
                      (process-operator #'op (list (process-argument #'a)))))
         (with-syntax (((attr source) (process-prop-list #'kw #'attributes
                                                         #'(rest ...))))
           #'(make-collection operator attr '() source))))

      ((kw (a op b rest ...)) (binary-operator? #'op)
       (with-syntax (((operator attributes)
                      (process-operator #'op (map process-argument #'(a b)))))
         (with-syntax (((attr source)
                        (process-prop-list #'kw #'attributes #'(rest ...))))
           #'(make-collection operator attr '() source))))

      ((kw exp) (identifier? #'exp) #'exp)
      ((kw exp ...) (syntax-violation 'collection
                                      "Invalid collection expression!"
                                      x
                                      #'kw)))))

(define-syntax collection
  (lambda (x)
    (syntax-case x ()
      ;; If the DSL is entered with exactly one expression, expand that
      ;; expression and return it. If it was called with multiple expressions,
      ;; expand all of them and wrap them into (list ...).
      ((_ exp) #'(expand-collection-dsl exp))
      ((_ exp0 expn ...) #'(list (collection exp0) (collection expn) ...)))))
