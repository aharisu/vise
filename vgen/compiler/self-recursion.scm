

(define (vise-phase-self-recursion form-list)
  (sexp-traverse 
    form-list
    `((defun . ,self-recursion-defun)
      (defvar . ,self-recursion-var)
      (set! . ,self-recursion-var)
      (let . ,self-recursion-let))))

(define (self-recursion-defun form loop)
  (let1 form (self-recursion-optimize (cadr form) form #f)
    (append
      (list
        (car form) ;defun
        (cadr form) ;name
        (caddr form);args
        (cadddr form));mofier
      (map loop (cddddr form)))))

(define (self-recursion-var form loop)
  (list
    (car form)
    (cadr form)
    (let ((sym (cadr form))
          (init (caddr form)))
      (loop (if (and (vsymbol? sym)
                  (list? init)
                  (eq? 'lambda (vexp (car init))))
              (self-recursion-optimize sym init #t)
              init)))))

(define (self-recursion-let form loop)
  (append
    (list
      (car form) ;let
      (map
        (lambda (clause) 
          (list 
            (car clause)
            (let ((sym (car clause))
                  (init (cadr clause)))
              (loop (if (and (vsymbol? sym )
                          (list? init)
                          (eq? 'lambda (vexp (car init))))
                      (self-recursion-optimize sym init #t)
                      init)))))
        (cadr form)))
    (map loop (cddr form))))


;;; original
;;(letrec ((loop (lambda (a) 
;;                 (if (> a 100)
;;                   a 
;;                   (loop (+ a 10))))))
;;  (loop 10))
;;
;;; replace to
;;(letrec ((loop (lambda (a)
;;; added
;;                 (let ((a a)
;;                       (recursion #t))
;;                   (while recursion
;;                     (set! recursion #f)
;;; ;;;;;
;;                     (if (> a 100)
;;                       a
;;; added
;;                       (begin
;;                         (set! recursion #t)
;;                         (set! a (+ a 10))))))))) ;;replace
;;; ;;;;;
;;  (loop 10))
(define (self-recursion-optimize sym init lambda?)
  (define (find-tail-exp action exp)
    (cond
      [(list? exp)
       (case (get-symbol (car exp))
         [(defun let lambda begin and or) 
          `(,@(drop-right exp 1)
             ,(find-tail-exp action (car (last-pair (cddr exp)))))]
         [(if)
          (if (null? (cdddr exp))
            (list (car exp) (cadr exp)
                  (find-tail-exp action (caddr exp))) ;then
            (list (car exp) (cadr exp)
                  (find-tail-exp action (caddr exp)) ;then
                  (find-tail-exp action (cadddr exp))))] ;else
         [(set!)
          (list (car exp) (cadr exp)
                (find-tail-exp action (caddr exp)))]
         [(return) (find-tail-exp action (cadr exp))]
         [(while) exp]
         [(quasiquote) exp] ;;TODO
         [else 
           (if (any (pa$ eq? (get-symbol (car exp))) vim-cmd-list)
             exp
             (action exp))])]
      [else (action exp)]))

  (let ([args (filter-map 
                (lambda (arg) (and (vsymbol? arg) (@ arg.exp)))
                ((if lambda? cadr caddr) init))]
        [self-data (env-find-data (@ sym.env) sym)]
        [body-env (assq-ref (slot-ref (car init) 'prop) 'body-env)]
        [injection-env (assq-ref (slot-ref (car init) 'prop) 'injection-env)]
        [has-tail-recursion? #f])
    (let1 exp (find-tail-exp
                (lambda (exp)
                  (if (and (list? exp) (not (eq? (vexp (car exp)) 'quote))
                        (eq? self-data (env-find-data (slot-ref (car exp) 'env) (car exp))))
                    (begin
                      (set! has-tail-recursion? #t)
                      (expand-expression
                        injection-env 
                        '()
                        `(begin 
                           (set! recursion #t)
                           ,@(map 
                               (lambda (arg bind) `(set! ,arg ,bind))
                               args (cdr exp)))))
                    (list 
                      (make <vsymbol> :exp 'return :env injection-env)
                      exp)))
                init)
      (if has-tail-recursion?
        `(,@(take exp (if lambda? 2 4))
           ,(list
              (make <vsymbol> :exp 'let :env injection-env)
              (append
                (map
                  (lambda (arg)
                    (list
                      (rlet1 sym (make <vsymbol> :exp arg :env injection-env)
                        (env-add-symbol injection-env sym 'local))
                      (make <vsymbol> :exp arg :env body-env)))
                  args)
                (list (list
                        (rlet1 sym (make <vsymbol> :exp 'recursion :env injection-env)
                          (env-add-symbol injection-env sym 'local))
                        #t)))
              (append
                (list
                  (make <vsymbol> :exp 'while :env injection-env)
                  (make <vsymbol> :exp 'recursion :env injection-env)
                  (list
                    (make <vsymbol> :exp 'set! :env injection-env)
                    (make <vsymbol> :exp 'recursion :env injection-env)
                    #f))
                ((if lambda? cddr cddddr) exp))))
        exp))))


