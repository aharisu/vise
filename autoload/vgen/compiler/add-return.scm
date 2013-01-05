(define-module vgen.compiler.add-return
  (use gauche.parameter)
  (use util.match)

  (use vgen.util)
  (use vgen.common)
  (export vise-phase-add-return
    return-add? add-return-defun
    ))

(select-module vgen.compiler.add-return)

(define (vise-phase-add-return form-list)
  (sexp-traverse 
    form-list
    `((defun . ,(pa$ add-return #f))
      (lambda . ,(pa$ add-return #t))
      (let . ,(pa$ add-return #t)))))

(define (return-add? exp ctx)
  (and (eq? ctx 'stmt) 
    (or (not (list? exp)) 
      (and (not (eq? (vexp (car exp)) 'raw-vimscript))
        (or (not (eq? (vise-lookup-renderer-ctx (car exp)) 'stmt))
          (find (pa$ eq? (vexp (car exp))) vim-symbol-list))))))

(define (add-return-defun form)
  (add-return #f form #f #f))

(define (add-return lambda? form ctx loop)
  (let1 ret (let1 env (slot-ref (car form) 'env)
              `(,@(drop-right form 1)
                 ,(find-tail-exp
                    (lambda (exp ctx)
                      (if (return-add? exp ctx)
                        (list (make <vsymbol> :exp 'return :env env) exp)
                        exp))
                    'stmt
                    (last form))))
    (if lambda?
      `(,(car ret)
         ,(cadr ret)
         ,@(sexp-traverse
             (cddr ret)
             `((lambda . ,(pa$ add-return #t)))))
      `(,(car ret)
         ,(cadr ret)
         ,(caddr form)
         ,(cadddr form)
         ,@(sexp-traverse
             (cddddr ret)
             `((lambda . ,(pa$ add-return #t))))))))

