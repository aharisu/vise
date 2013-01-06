
(define-module vgen.compiler 
  (use gauche.parameter)

  (use vgen.util)
  (use vgen.common)
  
  (use vgen.compiler.read)
  (use vgen.compiler.include)
  (use vgen.compiler.expand)
  (use vgen.compiler.check)
  (use vgen.compiler.add-return)
  (use vgen.compiler.render)

  (use vgen.compiler.lambda-expand)
  (use vgen.compiler.self-recursion)
  (use vgen.compiler.erase)

  (use vgen.compiler.add-repl-eval)
  (export vise-compile-from-string vise-compile
    vise-repl
    )
  )

(select-module vgen.compiler)

(define (vise-compile-from-string str)
  (vise-compile (open-input-string str)))

(define (get-file-path in-port)
  (if (eq? (port-type in-port) 'file)
    (sys-dirname (port-name in-port))
    (sys-normalize-pathname "." :absolute #t :expand #t :canonicalize #t)))

(define (make-global-env)
  (rlet1 env (make-env #f)
    (hash-table-for-each
      renderer-table
      (lambda (k v) (env-add-symbol&exp env k 'syntax v)))
    (for-each
      (lambda (sym) (env-add-symbol env sym 'syntax))
      vim-symbol-list)))


(define (vise-compile in-port 
                      :key (out-port (current-output-port))
                      (load-path '())
                      (prologue "")
                      (epilogue ""))
  (let1 global-env (make-global-env)
    (parameterize ([toplevel-env global-env])
      (let1 exp-list ((.$
                        vise-phase-render
                        vise-phase-erase
                        vise-phase-lambda-expand
                        vise-phase-self-recursion
                        vise-phase-add-return
                        (pa$ vise-phase-check global-env)
                        (pa$ vise-phase-expand global-env)
                        (pa$ vise-phase-include (append load-path (cons (get-file-path in-port) '())))
                        vise-phase-read)
                      in-port)
        (with-output-to-port
          out-port
          (lambda ()
            (display prologue)
            (for-each print exp-list)
            (display epilogue)))))))

(define (vise-repl in-port
                   :key (script-output-port (current-output-port))
                   (prompt-output-port (current-output-port))
                   (load-path '())
                   )
  (let* ([global-env (make-global-env)]
         [translator (.$
                       vise-phase-render
                       vise-phase-lambda-expand
                       vise-phase-self-recursion
                       vise-phase-add-return
                       (pa$ vise-phase-check global-env)
                       (pa$ vise-phase-expand global-env)
                       vise-phase-add-repl-eval
                       (pa$ vise-phase-include (append load-path (cons (get-file-path in-port) '()))))])
    (parameterize ([toplevel-env global-env]
                   [script-prefix "b:"])
      (letrec ((repl (lambda ()
                       (guard (e [(<vise-error> e) 
                                  (display (@ e.message) prompt-output-port)
                                  (newline prompt-output-port)
                                  (repl)])
                         (port-for-each
                           (lambda (e)
                             (for-each
                               (lambda (exp)
                                 (display exp script-output-port)
                                 (flush script-output-port))
                               (translator (list e))))
                           (lambda ()
                             (display "vise> " prompt-output-port)
                             (flush prompt-output-port)
                             (vise-phase-read-repl in-port)))))))
        (repl)))))

