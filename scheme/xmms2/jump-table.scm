;; Copyright (c) 2015 xmms2-guile workers, All rights reserved.
;;
;; Terms for redistribution and use can be found in LICENCE.

;; If the keys are from an enum starting at zero, creating a jump-table works
;; like this:
;;
;;(make-jump-table (table (TYPE-NONE none-handler)
;;                        (TYPE-ERROR error-handler)
;;                        (TYPE-INT64 integer-handler)
;;                        (TYPE-STRING string-handler))
;;                 #:others handle-fallback)
;;
;; The result will be a vector, that is filled with ‘handle-fallback’ in all
;; places the table above doesn't specify otherwise. When an entry beyond the
;; last entry is tried to be accessed, the ‘#:others’ handler is used as well.
;;
;; Is no ‘#:others’ handler is specified, an exception is thrown.
;;
;; In case an enum has an offset (or performs more changes in its members), the
;; ‘#:index-transformer’ handler may be used in order to keep the table more
;; packed than it be otherwise.
;;
;; Imagine ‘foo’ being set to 512 and ‘bar’ being set to 1024, the resulting
;; vector would be pretty massive. You could define this function:
;;
;; (define (foobar-offset-transform idx) (ash idx -10))
;;
;; ...and use it like this:
;;
;;(make-jump-table (table (foo foo-handler)
;;                        (bar bar-handler)
;;                 #:index-transformer foobar-offset-transform
;;                 #:others handle-fallback)
;;
;; Then the resulting jump-table vector would look like this:
;;
;;   [foo-handler          ; Index 0
;;    bar-handler]         ; Index 1

(define-module (xmms2 jump-table)
  #:use-module (ice-9 optargs)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-43)
  #:export (apply-jump-table
            make-jump-table
            table))

(define-record-type <jump-table>
  (make-jump-table* table fallback out-of-range index-transformer)
  jump-table?
  (table jt/table)
  (fallback jt/fallback)
  (out-of-range jt/out-of-range)
  (index-transformer jt/index-transformer))

(define-syntax table
  (lambda (x)
    (syntax-case x (offset)
      ((_ (offset OFFS) (idx cb) ...)
       #'(quasiquote (((unquote (- idx OFFS)) . (unquote cb)) ...)))
      ((_ (idx cb) ...)
       #'(quasiquote (((unquote idx) . (unquote cb)) ...))))))

(define (vlength tab transformer len)
  (if len len
      (fold (lambda (x acc)
              (if (not x)
                  acc
                  (let ((idx (transformer (car x))))
                    (if (> idx acc)
                        (+ 1 idx)
                        acc))))
            0
            tab)))

(define* (make-jump-table table
                          #:key
                          length
                          others
                          out-of-range
                          (index-transformer identity))
  (make-jump-table* (let ((len (vlength table index-transformer length)))
                      (vector-unfold (lambda (i x)
                                       (let ((v (assoc i x)))
                                         (values (if v (cdr v) others) x)))
                                     len
                                     (map (lambda (x)
                                            (cons (index-transformer (car x))
                                                  (cdr x)))
                                          table)))
                    others
                    out-of-range
                    index-transformer))

(define (apply-jump-table jt idx . args)
  (catch 'out-of-range
    (lambda () (apply (vector-ref (jt/table jt)
                                  ((jt/index-transformer jt) idx))
                      args))
    (lambda (k . a)
      (apply (or (jt/out-of-range jt) (jt/fallback jt)) args))))
