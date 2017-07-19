;; Copyright (c) 2017 xmms2-guile workers, All rights reserved.
;;
;; Terms for redistribution and use can be found in doc/LICENCE.

(define-module (documentation module constants)
  #:use-module (ice-9 format)
  #:use-module (srfi srfi-1)
  #:use-module (genipc utilities)
  #:use-module (xmms2 types)
  #:export (expand-constants-integer
            expand-constants-xref))

(define (expand-constants-integer mod name value)
  (list name 'integer
        (cond ((symbol-prefix? 'CMD- name)
               (format #f "Identifier for the ‘~a’ command of the ‘~a’ object."
                       (substring (string-downcase (symbol->string name)) 4)
                       (last mod)))
              (else 'undocumented))))

(define (xref-example name key value)
  (cat (format #f "    (assq-ref ~a ~d)~%" name key)
       (format #f "        → ~a~%" value)))

(define (xref-cmd-docstring name object key value)
  (cat (format #f "This is a cross-reference list for command IDs of the ")
       (format #f "‘~a’ object. This allows users to decode numeric " object)
       (format #f "values to symbols. For example:~%~%")
       (xref-example name key value)))

(define (xref-action-docstring name object action key value)
  (cat (format #f "This is a cross-reference list for ‘~a’ action IDs of within "
               action)
       (format #f "the ‘~a’ object. This allows users to decode numeric " object)
       (format #f "values to symbols. For example:~%~%")
       (xref-example name key value)))

(define (expand-constants-xref mod name value)
  (list name 'xref-list
        (cond ((and (symbol-prefix? 'xref- name)
                    (symbol-suffix? '-cmds name))
               (let* ((str (symbol->string name))
                      (object (substring str 5 (- (string-length str) 5))))
                 (xref-cmd-docstring name object (caar value) (cdar value))))
              ((and (symbol-prefix? 'xref- name)
                    (symbol-suffix? '-actions name))
               (let* ((str (symbol->string name))
                      (action (substring str 5 (- (string-length str) 8))))
                 (xref-action-docstring name (last mod) action
                                        (caar value) (cdar value))))
              (else 'undocumented))))
