
(add-load-path "." :relative)
(use vgen.compiler)
(use gauche.parseopt)

(define (main args)
  (let1 loadpath '()
    (let-args (cdr args)
      ()
      (let ([in-port (standard-input-port)])
        (vise-repl in-port
                   :script-output-port (current-error-port)
                   :prompt-output-port (current-output-port)))))
  0)

