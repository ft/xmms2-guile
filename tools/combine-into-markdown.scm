;; -*- scheme -*-

(use-modules (documentation more)
             (documentation combine-markdown))
(setlocale LC_ALL "")
(define *input* (cadr (command-line)))
(combine-markdown *input* #:topdir (maybe-topdir "."))
