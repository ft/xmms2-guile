;; -*- scheme -*-

(use-modules (documentation module)
             (documentation render-markdown))
(define *module* (cadr (command-line)))
(output-markdown (module->documentation *module*))
