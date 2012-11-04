
(define (vise-phase-add-return form-list)
  (sexp-traverse 
    form-list
    `((defun . ,(pa$ add-return #f))
      (lambda . ,(pa$ add-return #t))
      (let . ,(pa$ add-return #t)))))

(define (return-add? exp ctx)
  (and (eq? ctx 'stmt) 
    (or (not (list? exp)) 
      (not (eq? (vise-lookup-renderer-ctx (car exp)) 'stmt))
      (find (pa$ eq? (vexp (car exp))) vim-symbol-list))))

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
             (drop-right (cddr ret) 1)
             `((lambda . ,(pa$ add-return #t))))
         ,(loop 'stmt (last (cddr ret))))
      `(,(car ret)
         ,(cadr ret)
         ,(caddr form)
         ,(cadddr form)
         ,@(sexp-traverse
             (drop-right (cddddr ret) 1)
             `((lambda . ,(pa$ add-return #t))))
         ,(loop 'stmt (last (cddddr ret)))))))

