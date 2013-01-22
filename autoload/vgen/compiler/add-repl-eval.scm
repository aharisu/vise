(define-module vgen.compiler.add-repl-eval
  (use vgen.util)
  (use vgen.common)
  (export vise-phase-add-repl-eval
    ))

(select-module vgen.compiler.add-repl-eval)

(define (vise-phase-add-repl-eval form-list)
  (sexp-traverse 
    form-list
    `((lambda . ,add-repl-eval)
      (if . ,add-repl-eval)
      (let . ,add-repl-eval) ;;???
      (begin . ,add-repl-eval)
      (and . ,add-repl-eval)
      (or . ,add-repl-eval)
      (ary . ,add-repl-eval)
      (dict . ,add-repl-eval)
      (,traverse-apply-function-hook . ,add-repl-eval)
      (,traverse-symbol-ref . ,add-repl-eval)
      )))

(define (add-repl-eval form ctx loop)
  (if (and (eq? ctx 'toplevel) (not (statement-expression? form)))
    (list
      (make <vsymbol>
            :exp 'eval-expression
            :env (toplevel-env))
      form)
    form))

