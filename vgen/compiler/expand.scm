
(define (vise-phase-expand env exp-list)
  (init-phase-expand env)
  (let loop ((exp-list exp-list)
             (acc '()))
    (if (null? exp-list)
      (reverse! (filter identity acc))
      (loop (cdr exp-list)
            (cons (expand-expression env (car exp-list)) acc)))))

(define (expand-expression env exp)
  (cond
    [(list? exp)
     (case (car exp)
       [(quote) exp]
       [(defmacro) ;top level
        (unless (env-toplevel? env)
          (errorf <vise-error> "Compiler: defmacro can only be defined at top level:~a" exp))
        (register-macro env (cdr exp))
        #f]
       [(defun) ;top level
        (unless (env-toplevel? env)
          (errorf <vise-error> "Compiler: defun can only be defined at top level:~a" exp))
        (expand-defun env exp)]
       [(defvar)
        ]
       [(lambda) (expand-lambda env exp)]
       [(if) (expand-if env exp)]
       [(set!) (expand-set! env exp)]
       [(let*) (expand-let* env exp)]
       [(dolist) ]
       [(while begin and or quasiquote unquote unquote-splicing)
        (cons 
          (make <vsymbol> :exp (car exp) :env env)
          (map (pa$ expand-expression env) (cdr exp)))]
       [else (expand-apply env exp)])]
    [(symbol? exp)
     (rlet1 sym (make <vsymbol> 
                      :exp exp
                      :env env)
       (if-let1 d (env-find-data env sym)
         (@inc! d.ref-count)))]
    [else exp]))

(define-syntax register-macro
  (syntax-rules (match)
    ;; recursion
    [(_ env "clauses" op clauses (:where defs ...))
     (env-add-symbol&exp env 'op 'macro 
                         (make <vmacro>
                               :exp (lambda (form) 
                                      defs ...
                                      (match form . clauses))
                               :env env))]
    [(_ env "clauses" op clauses ())
     (register-macro env "clauses" op clauses (:where))]
    [(_ env "clauses" op (clause ...) (x . y))
     (register-macro env "clauses" op (clause ... x) y)]
    ;; entry
    [(_ env (op . args) . body)       ; single pattern case
     (register-macro env "clauses" op (((_ . args) . body)) ())]
    [(_ env op (match (pat . body) . clauses)) ; (pat . body) rules out a single symbol
     (register-macro env "clauses" op ((pat . body)) clauses)]
    [(_ env op (arg ...) . body)
     (register-macro env (op arg ...) . body)]))

(define (init-phase-expand env)
  ;;add global macro
  (register-macro 
    env
    (when test . body)
    `(if ,test (begin ,@body)))
  (register-macro 
    env
    (unless test . body)
    `(if (not ,test) (begin ,@body)))
  )

(define (constract-proc-args proc-env arg)
  (map ;args
    (lambda (arg)
      (if (symbol? arg)
        (rlet1 sym (make <vsymbol> :exp arg :env proc-env)
          (env-add-symbol proc-env sym 'arg))
        arg))
    (cond
      [(symbol? arg) (list :rest arg)]
      [(list? arg) arg]
      [(pair? arg)
       (let loop ((arg arg)
                  (ret '()))
         (if (pair? (cdr arg))
           (loop (cdr arg) (cons (car arg) ret))
           (reverse!  (cons (cdr arg) (cons :rest (cons (car arg) ret))))))]
      [else (errorf <vise-error> "Compiler: Illegal argument:~a" arg)])))

(define (expand-defun env exp)
  (when (< (length exp) 4)
    (errorf <vise-error> "Compiler: Bad syntax:~a" exp))
  (let1 fn-env (make-env env)
    (append
      (list
        (make <vsymbol> :exp (car exp) :env env) ;defun
        (if (symbol? (cadr exp)) ;name symbol
          (rlet1 sym (make <vsymbol> :exp (cadr exp) :env env)
            ;;TODO parse name
            (env-add-symbol env sym 'script))
          (cadr exp))
        (constract-proc-args fn-env (caddr exp))) ;args
      (map ;body
        (pa$ expand-expression fn-env)
        (cdddr exp)))))

(define (expand-lambda env exp)
  (when (< (length exp) 3)
    (errorf <vise-error> "Compiler: Bad syntax:~a" exp))
  (let1 lambda-env (make-env env)
    (append
      (list
        (make <vsymbol> :exp (car exp) :env env) ;lambda
        (constract-proc-args lambda-env (cadr exp))) ;args
      (map ;body
        (pa$ expand-expression lambda-env)
        (cddr exp)))))

(define (expand-if env exp)
  (match (cdr exp)
    [(pred then)
     (list (make <vsymbol> :exp (car exp) :env env) ;if
           (expand-expression env (cadr exp)) ;pred
           (expand-expression env (caddr exp)))] ;than
    [(pred then else)
     (list (make <vsymbol> :exp (car exp) :env env) ;if
           (expand-expression env (cadr exp)) ;pred
           (expand-expression env (caddr exp)) ;than
           (expand-expression env (cadddr exp)))] ;else
    [else (error <vise-error> "Compiler: Bad if syntax:" exp)]))

(define (expand-set! env exp)
  (match (cdr exp)
    [(sym e)
     (list
       (make <vsymbol> :exp (car exp) :env env) ;set!
       (if (symbol? sym) ;;symbol
         (rlet1 sym (make <vsymbol> :exp sym :env env)
           (if-let1 d (env-find-data env sym)
             (@inc! d.set-count)))
         sym)
       (expand-expression env e))]
    [else (error <vise-error> "Compiler: Bad set! syntax:" exp)]))

(define (expand-let* env exp)
  (match exp
    [(_ vars . body)
     (receive (vars env)
       (let loop ((vars vars)
                  (env (make-env env))
                  (acc '()))
         (if (null? vars)
           (values (reverse! acc) env)
           (loop (cdr vars)
                 (make-env env)
                 (cons
                   (list
                     (if (symbol? (caar vars))
                       (rlet1 sym (make <vsymbol> :exp (caar vars) :env env)
                         ;;TODO context
                         (env-add-symbol env sym 'local))
                       (caar vars))
                     (expand-expression (@ env.parent) (cadar vars)))
                   acc))))
       `(,(make <vsymbol> :exp (car exp) :env env) ;let*
          ,vars
          ,@(map ;body
              (pa$ expand-expression env)
              (cddr exp))))]
    [_ (error "Bad let* syntax:" exp)]))

(define (expand-apply env exp)
  (let1 e (env-find-exp env (car exp))
    (if (vmacro? e)
      ;;macro expand
      (expand-expression env ((@ e.exp) exp))
      ;;function call
      (map (pa$ expand-expression env) exp))))

