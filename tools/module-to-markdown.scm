;; -*- scheme -*-

(use-modules (documentation module)
             (documentation render-markdown))
(setlocale LC_ALL "")
(define *module* (cadr (command-line)))
(list->markdown (module->documentation *module*))
