;; -*- scheme -*-

(use-modules (ice-9 regex) (srfi srfi-1)
             (sxml match)
             (sxml simple))

(define (file-exists? name)
  "Return #t if a file (of any kind) named NAME exist."
  (access? name F_OK))

(define (notify . args)
  (apply format (cons (current-error-port) args)))

(define *source-file* (cadr (command-line)))

(unless (file-exists? *source-file*)
  (notify "Source XML file does not exist: ~a~%" *source-file*)
  (quit 1))

(define *source-xml*
  (with-input-from-file *source-file*
    (lambda ()
      (xml->sxml (current-input-port)
                 #:trim-whitespace? #t
                 #:namespaces '((xmms: . "https://xmms2.org/ipc.xsd"))))))

;; Like append-map, but makes it possible to emit debugging messages.
(define (am f clist1 . rest)
  ;;(notify "clist1: ~a, rest: ~a~%" clist1 rest)
  (let ((rc (apply map f clist1 rest)))
    ;;(notify "rc: ~a~%" rc)
    (concatenate rc)))

(define (cleanup-documentation string)
  (filter (lambda (x)
            (not (string=? x "")))
          (map string-trim (string-split string #\newline))))

(define (adjust-name name)
  (define name-map '((stats . statistics)
                     (set-next-rel . set-next/relative)
                     (current-pos . current-position)
                     (radd . add/recursive)
                     (rinsert . insert/recursive)
                     (seek-ms . seek-milliseconds)
                     (current-id . current-identifier)
                     (get-id . get-identifier)
                     (idlist-from-playlist . identifier-list-from-playlist)
                     (current-pos . current-position)
                     (playlist-changed . changed)))
  (define (maybe-replace name)
    (let ((replacement (assq-ref name-map name)))
      (or replacement name)))
  (maybe-replace (string->symbol (regexp-substitute/global
                                  #f "_" name 'pre "-" 'post))))

(define (adjust-type type)
  ;;(notify "type: ~a~%" type)
  (sxml-match type
    ((xmms::binary) '(binary))
    ((xmms::collection) '(collection))
    ((xmms::int) '(integer))
    ((xmms::string) '(string))
    ((xmms::unknown) '(unknown))
    ((xmms::list ,rest ...) `((list ,@(am adjust-type rest))))
    ((xmms::dictionary ,rest ...) `((dictionary ,@(am adjust-type rest))))))

(define (method-arg->sexp arg)
  ;;(notify "arg: ~a~%" arg)
  (sxml-match arg
    ((xmms::name ,name) `((name ,(adjust-name name))))
    ((xmms::type ,type) `((type ,@(adjust-type type))))
    ((xmms::documentation ,doc) `((documentation ,(cleanup-documentation doc))))))

(define (return-value->sexp return-value)
  ;;(notify "return-value: ~a~%" return-value)
  (sxml-match return-value
    ((xmms::type ,type) `((type ,@(adjust-type type))))
    ((xmms::documentation ,doc) `((documentation ,(cleanup-documentation doc))))))

(define (method->sexp method)
  ;;(notify "method: ~a~%" method)
  (sxml-match method
    ((xmms::name ,name) `((name ,(adjust-name name))))
    ((xmms::documentation ,doc) `((documentation ,(cleanup-documentation doc))))
    ((xmms::argument ,rest ...) `((argument ,@(am method-arg->sexp rest))))
    ((xmms::return_value ,rest ...) `((return-value
                                       ,@(am return-value->sexp rest))))))

(define (broadcast-or-signal->sexp bs)
  ;;(notify "bs: ~a~%" bs)
  (sxml-match bs
    ((xmms::id ,id) `((identifier ,(string->number id))))
    ((xmms::name ,name) `((name ,(adjust-name name))))
    ((xmms::documentation ,doc) `((documentation ,(cleanup-documentation doc))))
    ((xmms::return_value ,rest ...) `((return-value
                                       ,@(am return-value->sexp rest))))))

(define (sxml->sexp tree)
  ;;(notify "tree: ~a~%" tree)
  (sxml-match tree
    ((*TOP* (*PI* ,stuff ...) ,things ...)
     `(xmms2-ipc-description ,@(am sxml->sexp things)))
    ((xmms::ipc (@ (version ,v)) ,objs ...) `((version . ,v) ,@(am sxml->sexp objs)))
    ((xmms::object (xmms::name ,name) ,things ...)
     `((object (name ,(adjust-name name)) ,@(am sxml->sexp things))))
    ((xmms::method ,rest ...) `((method ,@(am method->sexp rest))))
    ((xmms::broadcast ,rest ...) `((broadcast ,@(am broadcast-or-signal->sexp rest))))
    ((xmms::signal ,rest ...) `((signal ,@(am broadcast-or-signal->sexp rest))))))

;((@ (ice-9 pretty-print) pretty-print) *source-xml*)
((@ (ice-9 pretty-print) pretty-print) (sxml->sexp *source-xml*))
