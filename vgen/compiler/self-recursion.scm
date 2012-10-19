

(define (vise-phase-self-recursion form-list)
  (sexp-traverse 
    form-list
    `((defvar . ,self-recursion-var)
      (set! . ,self-recursion-var)
      (let . ,self-recursion-let))))

(define (self-recursion-var form loop)
  (list
    (car form)
    (cadr form)
    (loop (self-recursion-optimize (cadr form) (caddr form)))))

(define (self-recursion-let form loop)
  (append
    (list
      (car form) ;let
      (map
        (lambda (clause) 
          (list 
            (car clause)
            (loop (self-recursion-optimize (car clause) (cadr clause)))))
        (cadr form)))
    (map loop (cddr form))))

(define (self-recursion-optimize sym init)
  (define (find-tail-exp action exp)
    (cond
      [(list? exp)
       (case (get-symbol (car exp))
         [(lambda begin and or) 
          (append
            (drop-right exp 1)
            (cons (find-tail-exp action (car (last-pair (cddr exp)))) '()))]
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
         [(while echo) exp]
         [(quasiquote) exp] ;;TODO
         [else (action exp)])]
      [else (action exp)]))

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
  (if (not (and (vsymbol? sym) (list? init) (eq? 'lambda (vexp (car init)))))
    init
    (let ([args (reverse!
                  (fold 
                    (lambda (arg acc) (if (vsymbol? arg) (cons (@ arg.exp) acc) acc))
                    '()
                    (cadr init)))]
          [self-data (env-find-data (@ sym.env) sym)]
          [lambda-env (assq-ref (slot-ref (car init) 'prop) 'lambda-env)]
          [injection-env (assq-ref (slot-ref (car init) 'prop) 'injection-env)]
          [has-tail-recursion? #f])
      (let1 exp (find-tail-exp
                  (lambda (exp)
                    (if (and (list? exp) (not (eq? (vexp (car exp)) 'quote))
                          (receive (d outside?) (env-find-data-with-outside-lambda? (slot-ref (car exp) 'env) (car exp))
                            (and d (has-attr? d 'free) outside? (eq? self-data d))))
                      (begin
                        (set! has-tail-recursion? #t)
                        (expand-expression
                          injection-env 
                          `(begin 
                             (set! recursion #t)
                             ,@(map 
                                 (lambda (arg bind) `(set! ,arg ,bind))
                                 args (cdr exp)))))
                      (list 'return exp)))
                  init)
        (if has-tail-recursion?
          (list
            (car exp)
            (cadr exp)
            (list
              (make <vsymbol> :exp 'let :env injection-env)
              (append
                (map
                  (lambda (arg)
                    (list
                      (rlet1 sym (make <vsymbol> :exp arg :env injection-env)
                        (env-add-symbol injection-env sym 'local))
                      (make <vsymbol> :exp arg :env lambda-env)))
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
                (cddr exp))))
          exp)))))


