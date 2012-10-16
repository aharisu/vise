
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
       [(let* let letrec) (expand-let env exp)]
       [(dolist) (expand-dolist env exp)]
       [(while begin and or quasiquote unquote unquote-splicing)
        (cons 
          (make <vsymbol> :exp (car exp) :env env)
          (map (pa$ expand-expression env) (cdr exp)))]
       [(list-func)
        (list
          (make <vsymbol> :exp (car exp) :env env)
          (cadr exp) ;keyword
          (expand-expression env (caddr exp))
          (expand-expression env (cadddr exp)))]
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

(define (check-lambda-for-list-fnction form proc . require-args)
  (if (and (list? proc) (eq? (car proc) 'lambda))
    (match proc
      [(_ args . body)
       (if (and (list? args) (any (pa$ = (length args)) require-args))
         #t
         (errorf <vise-error> "Compiler: Bad syntax:~a ~a" proc form))]
      [else (errorf <vise-error> "Compiler: Bad syntax:~a ~a" proc form)])
    #f))

(define (is-expand-lambda-body? proc)
  (and (= 1 (length (cddr proc)))
    (list? (caddr proc))
    (not (or* eq? (caaddr proc)
              'lambda 'if 'set! 'let 'let* 'letrec 'dolist 'while 'begin
              'quasiquote 'unquote 'unquote-splicing 'list-func))))

(define (replace-symbol sym-from sym-to body)
  (cond
    ((list? body) (map (pa$ replace-symbol sym-from sym-to) body))
    ((symbol? body) (if (eq? sym-from body) sym-to body))
    (else body)))

(define (init-phase-expand env)
  ;;add global macro
  ;;when
  (register-macro 
    env
    (when test . body)
    `(if ,test (begin ,@body)))
  ;;unless
  (register-macro 
    env
    (unless test . body)
    `(if (not ,test) (begin ,@body)))
  ;;map
  (register-macro
    env
    (map proc l)
    (if (check-lambda-for-list-fnction `(map ,proc ,l) proc 1 2)
      (if (is-expand-lambda-body? proc)
        `(list-func 
           :map ,l 
           ,@(if (= 1 (length (cadr proc)))
               (replace-symbol (caadr proc) 'v:val (cddr proc))
               ((.$
                  (pa$ replace-symbol (cadadr proc) 'v:val)
                  (pa$ replace-symbol (caadr proc) 'v:key))
                (cddr proc))))
        `(list-func :map ,l ,proc))
      `(list-func :map ,l (,proc v:val))))
  ;;for-each
  (register-macro 
    env
    (for-each proc l)
    (if (check-lambda-for-list-fnction `(for-each ,proc ,l) proc 1 2)
      `(dolist (,(caadr proc) ,l) ,@(cddr proc))
      `(dolist (val ,l) (,proc val))))
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
              (attr-push! (env-find-data env sym) 'function))
            name)
          (constract-proc-args fn-env (car rest))) ;args
        (parse-body fn-env (cdr rest))))))  ;body

(define (expand-symbol-bind sym init scope env)
  (define (to-vsymbol sym)
    (if (symbol? sym)
      (rlet1 sym (make <vsymbol> :exp sym :env env)
        (env-add-symbol env sym scope)
        (when (and (list? init) (eq? (car init) 'lambda))
          (attr-push! (env-find-data env sym) 'lambda)
          scope))
      sym))
  (cond
    ((symbol? sym) (to-vsymbol sym))
    ((pair? sym)
     (map
       to-vsymbol
       (if (list? sym)
         sym
         (let loop ((arg sym)
                    (ret '()))
           (if (pair? (cdr arg))
             (loop (cdr arg) (cons (car arg) ret))
             (reverse!  (cons (cdr arg) (cons :rest (cons (car arg) ret)))))))))))

(define (expand-defvar env exp)
  (when (< (length exp) 3)
    (errorf <vise-error> "Compiler: Bad syntax:~a" exp))
  (receive (scope name rest) (parse-def-scope exp)
    (unless (= (length rest) 1)
      (errorf <vise-error> "Compiler: Bad syntax:~a" exp))
    (list
      (make <vsymbol> :exp (car exp) :env env) ;defvar
      (expand-symbol-bind name (car rest) scope env)
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

(define (expand-dolist env exp)
  (match exp
    [(_ (var expr) . body)
     (let1 dolist-env (make-env env)
       (append
         (list
           (make <vsymbol> :exp (car exp) :env env) ;dolist
           (list
             (expand-symbol-bind var expr 'local dolist-env)
             (expand-expression env expr)))
         (map
           (pa$ expand-expression dolist-env)
           body)))]
    [else (errorf <vise-error> "Compiler: Bad syntax:~a" exp)]))

(define (expand-let env exp)
  (match exp
    [(_ (? list? vars) . body)
     (receive (vars env)
       (if (eq? (car exp) 'letrec)
         (let ([env (make-env env)]
               [var-loop (lambda (f)
                           (let loop ((vars vars)
                                      (acc '()))
                             (if (null? vars)
                               (reverse! acc)
                               (loop (cdr vars) (cons (f vars) acc)))))])
           (values
             (map
               list
               (var-loop (lambda (var) (expand-symbol-bind (caar var) (cadar var) 'local env)))
               (var-loop (lambda (var) (expand-expression env (cadar var)))))
             env))
         (let loop ((vars vars)
                    (env (make-env env))
                    (acc '()))
           (if (null? vars)
             (values (reverse! acc) env)
             (loop (cdr vars)
                   (if (eq? (car exp) 'let*) (make-env env) env)
                   (cons
                     (list
                       (expand-symbol-bind (caar vars) (cadar vars) 'local env)
                       (expand-expression (@ env.parent) (cadar vars)))
                     acc)))))
       `(,(make <vsymbol> :exp 'let :env env) ;let or let* or letrec
          ,vars
          ,@(map ;body
              (pa$ expand-expression env)
              (cddr exp))))]

    [(let (? symbol? name) ((var . spec) ...) . body)
     (expand-expression env `(letrec ((,name (lambda ,var ,@body)))
                               (,name ,@(map car spec))))]
    [_ (error "Bad let syntax:" exp)]))

(define (expand-apply env exp)
  (let1 e (env-find-exp env (car exp))
    (if (vmacro? e)
      ;;macro expand
      (expand-expression env ((@ e.exp) exp))
      ;;function call
      (map (pa$ expand-expression env) exp))))

(define (expand-refer-symbol env exp)
  (rlet1 sym (make <vsymbol> 
                   :exp exp
                   :env env)
    (receive (d outside?) (env-find-data-with-outside-lambda? env sym)
      (when d
        (@inc! d.ref-count)
        (when (and outside? (eq? (@ d.scope) 'local))
          (attr-push! d 'free))))))


