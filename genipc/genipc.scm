;; -*- scheme -*-

(use-modules (genipc stage-0)
             (genipc stage-1)
             (genipc stage-2)
             (genipc stage-3)
             (genipc utilities))

(define debug? #f)
(activate-file-generation!)

(define *source-file* (cadr (command-line)))

(unless (file-exists? *source-file*)
  (notify "Source XML file does not exist: ~a~%" *source-file*)
  (quit 1))

(define *source-xml* (generate-sxml *source-file*))
(if debug? (pp *source-xml*))

(define *sexp-stage-1* (sxml->sexp *source-xml*))
(if debug? (pp *sexp-stage-1*))

;; By now, the XML document is converted to an s-expression tree, that looks
;; like this:
;;
;;  (xmms2-ipc-description TOP-LEVEL-ENTITIES ...)
;;
;; Where TOP-LEVEL-ENTITIES are one of:
;;
;;   - (version <STRING>)
;;   - (object ...)
;;   - (constant ...)
;;   - (enum ...)
;;
;; “object” describes one of XMMS2's IPC objects. It may contain the following
;; forms:
;;
;;   - (name <SYMBOL>)
;;   - (method ...)
;;   - (broadcast ...)
;;   - (signal ...)
;;
;; The latter three of these need to be turned into scheme code. We will do
;; this by looping into the structure, accumulating data, rearranging it so it
;; will be easy to work with in a final generation step.

(define *sexp-stage-2* (generate-stage-2 *sexp-stage-1*))
(if debug? (pp *sexp-stage-2*))

(ensure-directories!)
(generate-ipc/constants *sexp-stage-2*)
(generate-ipc/meta *sexp-stage-2*)
(generate-ipc/objects *sexp-stage-2*)
