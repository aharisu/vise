
(define-module vgen.compiler 
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
  (export-all)
  )

(select-module vgen.compiler)

(define (vise-compile-from-string str)
  (vise-compile (open-input-string str)))

(define (get-file-path in-port)
  (if (eq? (port-type in-port) 'file)
    (sys-dirname (port-name in-port))
    (sys-normalize-pathname "." :absolute #t :expand #t :canonicalize #t)))

(define (vise-compile in-port 
                      :key (out-port (current-output-port))
                      (load-path '())
                      (prologue "")
                      (epilogue ""))
  (let* ((global-env (rlet1 env (make-env #f)
                       (hash-table-for-each
                         renderer-table
                         (lambda (k v) (env-add-symbol&exp env k 'syntax v)))
                       (for-each
                         (lambda (sym) (env-add-symbol env sym 'syntax))
                         vim-symbol-list)))
         (exp-list ((.$
                      vise-phase-render
                      vise-phase-erase
                      vise-phase-lambda-expand
                      vise-phase-self-recursion
                      vise-phase-add-return
                      (pa$ vise-phase-check global-env)
                      (pa$ vise-phase-expand global-env)
                      (pa$ vise-phase-include (append load-path (cons (get-file-path in-port) '())))
                      vise-phase-read)
                    in-port)))
    (with-output-to-port
      out-port
      (lambda ()
        (display prologue)
        (for-each print exp-list)
        (display epilogue)))))

