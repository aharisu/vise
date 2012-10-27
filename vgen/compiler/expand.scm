
(define (vise-phase-expand env exp-list)
  (init-phase-expand env)
  (let loop ((exp-list exp-list)
             (acc '()))
    (if (null? exp-list)
      (reverse! (filter identity acc))
      (loop (cdr exp-list)
            (cons (expand-expression env '() (car exp-list)) acc)))))

(define (expand-expression env parent exp)
  (cond
    [(list? exp)
     (case (car exp)
       [(quote) exp]
       [(defmacro) ;top level
        (unless (env-toplevel? env)
          (vise-error "Compiler: defmacro can only be defined at top level:~a" exp))
        (register-macro env (cdr exp))
        #f]
       [(defun) ;top level
        (unless (env-toplevel? env)
          (vise-error "Compiler: defun can only be defined at top level:~a" exp))
        (expand-defun env parent exp)]
       [(defvar) (expand-defvar env parent exp)]
       [(lambda) (expand-lambda env parent exp)]
       [(if) (expand-if env parent exp)]
       [(set!) (expand-set! env parent exp)]
       [(let* let letrec) (expand-let env parent exp)]
       [(dolist) (expand-dolist env parent exp)]
       [(while begin and or quasiquote unquote unquote-splicing augroup)
        (cons 
          (make <vsymbol> :exp (car exp) :env env
                :debug-info (debug-source-info exp))
          (map (pa$ expand-expression env exp) (cdr exp)))]
       [(list-func)
        (list
          (make <vsymbol> :exp (car exp) :env env
                :debug-info (debug-source-info exp))
          (cadr exp) ;keyword
          (expand-expression env exp (caddr exp))
          (expand-expression env exp (cadddr exp)))]
       [(dict) (expand-dict env parent exp)]
       [(try) (expand-try env parent exp)]
       [(autocmd) (expand-autocmd env parent exp)]
       [else (expand-apply env parent exp)])]
    [(symbol? exp) (expand-refer-symbol env parent exp)]
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
         (vise-error "Compiler: Bad syntax:~a ~a" proc form))]
      [else (vise-error "Compiler: Bad syntax:~a ~a" proc form)])
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
  (define (expand-list-func type proc l)
    (if (check-lambda-for-list-fnction `(,type ,proc ,l) proc 1 2)
      (if (is-expand-lambda-body? proc)
        `(list-func 
           ,type ,l 
           ,@(if (= 1 (length (cadr proc)))
               (replace-symbol (caadr proc) 'v:val (cddr proc))
               ((.$
                  (pa$ replace-symbol (cadadr proc) 'v:val)
                  (pa$ replace-symbol (caadr proc) 'v:key))
                (cddr proc))))
        `(list-func ,type ,l ,proc))
      `(list-func ,type ,l (,proc v:val)))
    )

  ;;add global macro
  ;;cond
  (register-macro
    env
    cond
    (match
      [(_ ('else . rest)) `(begin ,@rest)]
      [(_ (test)) test]
      [(_ (test) . clause)
       (let1 temp (gensym "temp")
         `(let ((,temp ,test))
            (if ,temp
              ,temp
              (cond ,@clause))))]
      [(_ (test . rest))
       `(if ,test (begin ,@rest))]
      [(_ (test . rest) . clause)
       `(if ,test (begin ,@rest) (cond ,@clause))]))
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
    (expand-list-func :map proc l))
  ;;filter
  (register-macro
    env
    (filter proc l)
    (expand-list-func :filter proc l))
  ;;for-each
  (register-macro 
    env
    (for-each proc l)
    (if (check-lambda-for-list-fnction `(for-each ,proc ,l) proc 1 2)
      `(dolist (,(caadr proc) ,l) ,@(cddr proc))
      `(dolist (val ,l) (,proc val))))
  ;;length
  (register-macro
    env
    (length seq)
    `(len ,seq))
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
      [else (vise-error "Compiler: Illegal argument:~a" arg)])))

(define (parse-def-scope form)
  (let1 scope (cadr form)
    (if (keyword? scope)
      (if (or (eq? scope :script) (eq? scope :global) (eq? scope :window) (eq? scope :buffer)) 
        (values (string->symbol (keyword->string scope)) (caddr form) (cdddr form))
        (vise-error "Compiler: Illegal scope:~a ~a" scope form))
      (values 'script scope (cddr form)))))

(define (expand-defun env parent exp)
  (define (parse-body fn-env body)
    (if (null? body)
      body
      (receive (modify body) (if (keyword? (car body))
                               (values (car body) (cdr body))
                               (values :normal body))
        (cons modify 
              (map
                (pa$ expand-expression fn-env exp)
                body)))))

  (when (< (length exp) 4)
    (vise-error "Compiler: Bad syntax:~a" exp))
  (let* ((fn-env (make-env env))
         (injection-env (make-env fn-env)))
    (receive (scope name rest) (parse-def-scope exp)
      (when (< (length rest) 2)
        (vise-error "Compiler: Bad syntax:~a" exp))
      (append
        (list
          (make <vsymbol> :exp (car exp) :env env ;defun
                :debug-info (debug-source-info exp)
                :prop `((body-env . ,fn-env) (injection-env . ,injection-env)))
          (if (symbol? name) ;name symbol
            (rlet1 sym (make <vsymbol> :exp name :env env)
              (env-add-symbol env sym scope)
              (attr-push! (env-find-data env sym) 'function))
            name)
          (constract-proc-args fn-env (car rest))) ;args
        (parse-body injection-env (cdr rest))))))  ;body

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

(define (expand-defvar env parent exp)
  (when (< (length exp) 3)
    (vise-error "Compiler: Bad syntax:~a" exp))
  (receive (scope name rest) (parse-def-scope exp)
    (unless (= (length rest) 1)
      (vise-error "Compiler: Bad syntax:~a" exp))
    (list
      (make <vsymbol> :exp (car exp) :env env ;defvar
                :debug-info (debug-source-info exp))
      (expand-symbol-bind name (car rest) scope env)
      (expand-expression env exp (car rest)))))

(define (expand-lambda env parent exp)
  (when (< (length exp) 3)
    (vise-error "Compiler: Bad syntax:~a" exp))
  (let* ([lambda-env (make-env env #t)]
         [injection-env (make-env lambda-env)])
    (append
      (list
        (make <vsymbol> :exp (car exp) :env env  ;lambda
              :debug-info (debug-source-info exp)
              :prop `((body-env . ,lambda-env) (injection-env . ,injection-env)))
        (constract-proc-args lambda-env (cadr exp))) ;args
      (map ;body
        (pa$ expand-expression injection-env exp)
        (cddr exp)))))

(define (expand-if env parent exp)
  (match (cdr exp)
    [(pred then)
     (list (make <vsymbol> :exp (car exp) :env env  ;if
                 :debug-info (debug-source-info exp))
           (expand-expression env exp (cadr exp)) ;pred
           (expand-expression env exp (caddr exp)))] ;than
    [(pred then else)
     (list (make <vsymbol> :exp (car exp) :env env ;if
                 :debug-info (debug-source-info exp))
           (expand-expression env exp (cadr exp)) ;pred
           (expand-expression env exp (caddr exp)) ;than
           (expand-expression env exp (cadddr exp)))] ;else
    [else (vise-error "Compiler: Bad if syntax:~a" exp)]))

(define (expand-set! env parent exp)
  (match (cdr exp)
    [(sym e)
     (list
       (make <vsymbol> :exp (car exp) :env env ;set!
             :debug-info (debug-source-info exp))
       (if (symbol? sym) ;;symbol
         (rlet1 sym (make <vsymbol> :exp sym :env env)
           (if-let1 d (env-find-data env sym)
             (@inc! d.set-count)))
         ;;TODO ref
         (expand-expression env exp sym))
       (expand-expression env exp e))]
    [else (vise-error "Compiler: Bad set! syntax:~a" exp)]))

(define (expand-dolist env parent exp)
  (match exp
    [(_ (var expr) . body)
     (let1 dolist-env (make-env env)
       (append
         (list
           (make <vsymbol> :exp (car exp) :env env ;dolist 
                 :debug-info (debug-source-info exp))
           (list
             (expand-symbol-bind var expr 'local dolist-env)
             (expand-expression env exp expr)))
         (map
           (pa$ expand-expression dolist-env exp)
           body)))]
    [else (vise-error "Compiler: Bad syntax:~a" exp)]))

(define (expand-let env parent exp)
  (match exp
    [(_ (? list? vars) . body)
     (receive (vars let-env)
       (if (eq? (car exp) 'letrec)
         (let ([let-env (make-env env)]
               [var-loop (lambda (f)
                           (let loop ((vars vars)
                                      (acc '()))
                             (if (null? vars)
                               (reverse! acc)
                               (loop (cdr vars) (cons (f vars) acc)))))])
           (values
             (map
               list
               (var-loop (lambda (var) (expand-symbol-bind (caar var) (cadar var) 'local let-env)))
               (var-loop (lambda (var) (expand-expression let-env exp (cadar var)))))
             let-env))
         (let loop ((vars vars)
                    (let-env (make-env env))
                    (acc '()))
           (if (null? vars)
             (values (reverse! acc) let-env)
             (loop (cdr vars)
                   (if (eq? (car exp) 'let*) (make-env let-env) let-env)
                   (cons
                     (list
                       (expand-symbol-bind (caar vars) (cadar vars) 'local let-env)
                       (expand-expression (@ let-env.parent) exp (cadar vars)))
                     acc)))))
       (let1 injection-env (make-env let-env)
         `(,(make <vsymbol> :exp 'let :env env ;let or let* or letrec
                  :debug-info (debug-source-info exp)
                  :prop `((body-env . ,let-env) (injection-env . ,injection-env)))
            ,vars
            ,@(map ;body
                (pa$ expand-expression injection-env exp)
                (cddr exp)))))]
    ;;named-let
    [(let (? symbol? name) ((var . spec) ...) . body)
     (expand-expression env parent
                        `(letrec ((,name (lambda ,var ,@body)))
                           (,name ,@(map car spec))))]
    [_ (vise-error "Bad let syntax:~a" exp)]))

(define (expand-dict env parent exp)
  (append
    (cons (make <vsymbol> :exp (car exp) :env env) '())
    (match (cdr exp)
      [((sym init) ...)
       (map
         (lambda (sym init)
           (list
             (if (symbol? sym)
               (make <vsymbol> :exp sym :env env
                     :debug-info (debug-source-info exp))
               sym)
             (expand-expression env exp init)))
         sym init)]
      [_ (vise-error "dict format error")])))

(define (expand-try env parent exp)
  `(,(make <vsymbol> :exp (car exp) :env env ;dolist 
           :debug-info (debug-source-info exp))
     ,(expand-expression env exp (cadr exp))
     ,@(map
         (lambda (clause) 
           (if (list? clause)
             (map (pa$ expand-expression env clause) clause)
             clause))
         (cddr exp))))

(define (expand-autocmd env parent exp)
  (define (err)
    (vise-error "Bad syntax. autocmd format (autocmd [group] (event1 event2 ...) pat [:nested] cmd).\n~a" exp))
  (receive (group events pat nest cmd)
    (match exp
      [(_ (? symbol? group) (? list? events) pat (? keyword? nest) cmd)
       (values group events pat nest cmd)]
      [(_ (? list? events) pat (? keyword? nest) cmd)
       (values 'default events pat nest cmd)]
      [(_ (? symbol? group) (? list? events) pat cmd)
       (values group events pat :normal cmd)]
      [(_ (? list? events) pat cmd)
       (values 'default events pat :normal cmd)]
      [else (err)])
    (let1 pat (cond
                [(string? pat) pat]
                [(symbol? pat) (symbol->string pat)]
                [else (err)])
      (list
        (make <vsymbol> :exp (car exp) :env env ;autocmd
              :debug-info (debug-source-info exp))
        (expand-expression env exp group)
        (map (pa$ expand-expression env exp) events)
        pat
        nest
        (expand-expression env exp cmd)))))

(define (expand-apply env parent exp)
  (let1 e (env-find-exp env (car exp))
    (if (vmacro? e)
      ;;macro expand
      (expand-expression env parent ((@ e.exp) exp))
      ;;function call
      (rlet1 form (map (pa$ expand-expression env exp) exp)
        (when (is-a? (car form) <vexp>)
          (slot-set! (car form) 'debug-info (debug-source-info exp)))))))

(define (expand-refer-symbol env parent exp)
  (rlet1 sym (make <vsymbol> 
                   :exp exp
                   :env env
                   :parent parent)
    (receive (d outside?) (env-find-data-with-outside-lambda? env sym)
      (when d
        (@inc! d.ref-count)
        (when (and outside? (eq? (@ d.scope) 'local))
          (attr-push! d 'free))))))


