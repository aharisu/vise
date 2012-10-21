
(define (vise-phase-add-return form-list)
  (sexp-traverse 
    form-list
    `((defun . ,(pa$ add-return #f))
      (lambda . ,(pa$ add-return #t))
      (let . ,(pa$ add-return #t)))))

(define (add-return lambda? form loop)
  (rlet1 ret (let1 env (slot-ref (car form) 'env)
               `(,@(drop-right form 1)
                  ,(find-tail-exp
                     (lambda (exp) 
                       (list (make <vsymbol> :exp 'return :env env) exp))
                     (car (last-pair form)))))
    (if lambda?
      `(,(car ret)
         ,(cadr ret)
         ,@(map loop (cddr ret)))
      `(,(car ret)
         ,(cadr ret)
         ,(caddr form)
         ,(cadddr form)
         ,@(map loop (cddddr ret))))))

