
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
       [(defvar) (expand-defvar env exp)]
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
    [(symbol? exp) (expand-refer-symbol env exp)]
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

(define (parse-def-scope form)
  (let1 scope (cadr form)
    (if (keyword? scope)
      (if (or (eq? scope :script) (eq? scope :global) (eq? scope :window) (eq? scope :buffer)) 
        (values (string->symbol (keyword->string scope)) (caddr form) (cdddr form))
        (errorf <vise-error> "Compiler: Illegal scope:~a ~a" scope form))
      (values 'script scope (cddr form)))))

(define (expand-defun env exp)
  (define (parse-body fn-env body)
    (if (null? body)
      body
      (receive (modify body) (if (keyword? (car body))
                               (values (car body) (cdr body))
                               (values :normal body))
        (cons modify 
              (map
                (pa$ expand-expression fn-env)
                body)))))

  (when (< (length exp) 4)
    (errorf <vise-error> "Compiler: Bad syntax:~a" exp))
  (let1 fn-env (make-env env)
    (receive (scope name rest) (parse-def-scope exp)
      (when (< (length rest) 2)
        (errorf <vise-error> "Compiler: Bad syntax:~a" exp))
      (append
        (list
          (make <vsymbol> :exp (car exp) :env env) ;defun
          (if (symbol? name) ;name symbol
            (rlet1 sym (make <vsymbol> :exp name :env env)
              (env-add-symbol env sym scope)
              (env-data-attr-push! (env-find-data env sym) 'function))
            name)
          (constract-proc-args fn-env (car rest))) ;args
        (parse-body fn-env (cdr rest))))))  ;body

(define (expand-defvar env exp)
  (when (< (length exp) 3)
    (errorf <vise-error> "Compiler: Bad syntax:~a" exp))
  (receive (scope name rest) (parse-def-scope exp)
    (unless (= (length rest) 1)
      (errorf <vise-error> "Compiler: Bad syntax:~a" exp))
    (list
      (make <vsymbol> :exp (car exp) :env env) ;defvar
      (if (symbol? name) ;;name symbol
        (rlet1 sym (make <vsymbol> :exp name :env env)
          (env-add-symbol env sym scope))
        name)
      (expand-expression env (car rest)))))

(define (expand-lambda env exp)
  (when (< (length exp) 3)
    (errorf <vise-error> "Compiler: Bad syntax:~a" exp))
  (let1 lambda-env (make-env env #t)
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
                         (env-add-symbol 
                           env sym 
                           (if (and (list? (cadar vars))
                                 (eq? (caadar vars) 'lambda))
                             'lambda 
                             'local)))
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

(define (expand-refer-symbol env exp)
  (let1 sym (make <vsymbol> 
                  :exp exp
                  :env env)
    (receive (d outside?) (env-find-data-with-outside-lambda? env sym)
      (if d
        (begin
          (@inc! d.ref-count)
          (if (and outside? (eq? (@ d.scope) 'local))
            (begin 
              (env-data-attr-push! d 'free)
              (list 'ref-display-var sym))
            sym))
        sym))))


