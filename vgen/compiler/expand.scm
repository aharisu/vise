
(define (vise-phase-expand env exp-list)
  (init-phase-expand env)
  (let loop ((exp-list exp-list)
             (acc '()))
    (if (null? exp-list)
      (reverse! (filter identity acc))
      (loop (cdr exp-list)
            (cons (expand-expression env 'toplevel '() (car exp-list)) acc)))))

(define (expand-expression env ctx parent exp)
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
        (expand-defun env ctx parent exp)]
       [(defvar) (expand-defvar env ctx parent exp)]
       [(lambda) (expand-lambda env ctx parent exp)]
       [(if) (expand-if env ctx parent exp)]
       [(set!) (expand-set! env ctx parent exp)]
       [(let* let letrec) (expand-let env ctx parent exp)]
       [(dolist) (expand-dolist env ctx parent exp)]
       [(while)
        (append
          (list
            (make <vsymbol> :exp (car exp) :env env
                :debug-info (debug-source-info exp))
            (expand-expression env 'expr exp (cadr exp)))
          (map (pa$ expand-expression env 'stmt exp) (cddr exp)))]
       [(and or quasiquote unquote unquote-splicing augroup)
        (cons 
          (make <vsymbol> :exp (car exp) :env env
                :debug-info (debug-source-info exp))
          (map (pa$ expand-expression env 
                    (if (or* eq? (car exp) 'and 'or) 'expr ctx)
                    exp)
               (cdr exp)))]
       [(begin) (expand-begin env ctx parent exp)]
       [(list-func)
        (list
          (make <vsymbol> :exp (car exp) :env env
                :debug-info (debug-source-info exp))
          (cadr exp) ;keyword
          (expand-expression env 'expr exp (caddr exp))
          (expand-expression env 'expr exp (cadddr exp)))]
       [(dict) (expand-dict env ctx parent exp)]
       [(try) (expand-try env ctx parent exp)]
       [(autocmd) (expand-autocmd env ctx parent exp)]
       [else (expand-apply env ctx parent exp)])]
    [(symbol? exp) (expand-refer-symbol env ctx parent exp)]
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
  ;;let1
  (register-macro
    env
    (let1 sym expr . body)
    `(let ((,sym ,expr)) ,@body))
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

(define (get-name-scope exp name)
  (let1 name (symbol->string name)
    (cond
      [(or (char-upper-case? (string-ref name 0)) (cmd-symbol? name)) 'cmd]
      [(and (< 1 (string-length name)) (eq? (string-ref name 1) #\:))
       (let1 prefix (string-ref name 0)
         (cond
           [(eq? prefix #\g) 'global]
           [(eq? prefix #\s) 'script]
           [(eq? prefix #\w) 'window]
           [(eq? prefix #\b) 'buffer]
           [else (vise-error "Unkown name scope:~a ~a" name exp)]))]
      [else 'script])))

(define (expand-defun env ctx parent exp)
  (define (parse-body fn-env body)
    (if (null? body)
      body
      (receive (modify body) (if (keyword? (car body))
                               (values (car body) (cdr body))
                               (values :normal body))
        (cons modify 
              (map
                (pa$ expand-expression fn-env 'stmt exp)
                body)))))

  (when (< (length exp) 4)
    (vise-error "Compiler: Bad syntax:~a" exp))
  (let* ((fn-env (make-env env))
         (injection-env (make-env fn-env)))
    (append
      (list
        (make <vsymbol> :exp (car exp) :env env ;defun
              :debug-info (debug-source-info exp)
              :prop `((body-env . ,fn-env) (injection-env . ,injection-env)))
        (if (symbol? (cadr exp)) ;name symbol
          (rlet1 sym (make <vsymbol> :exp (cadr exp) :env env)
            (env-add-symbol env sym (get-name-scope exp (cadr exp)))
            (attr-push! (env-find-data env sym) 'function))
          (cadr exp))
        (constract-proc-args fn-env (caddr exp))) ;args
      (parse-body injection-env (cdddr exp)))))  ;body

(define (expand-symbol-bind sym init scope env)
  (define (to-vsymbol sym)
    (if (symbol? sym)
      (rlet1 sym (make <vsymbol> :exp sym :env env)
        (env-add-symbol env sym scope)
        (when (and (list? init) (eq? (car init) 'lambda))
          (attr-push! (env-find-data env sym) 'lambda)))
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

(define (expand-defvar env ctx parent exp)
  (when (< (length exp) 3)
    (vise-error "Compiler: Bad syntax:~a" exp))
  (list
    (make <vsymbol> :exp (car exp) :env env ;defvar
          :debug-info (debug-source-info exp))
    (expand-symbol-bind (cadr exp) (caddr exp) (get-name-scope exp (cadr exp)) env)
    (expand-expression env 'expr exp (caddr exp))))

(define (expand-lambda env ctx parent exp)
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
        (pa$ expand-expression injection-env 'stmt exp)
        (cddr exp)))))

(define (expand-if env ctx parent exp)
  (let1 cctx (if (eq? ctx 'expr) 'expr ctx)
    (match (cdr exp)
      [(pred then)
       (list (make <vsymbol> :exp (car exp) :env env  ;if
                   :debug-info (debug-source-info exp))
             (expand-expression env 'expr exp (cadr exp)) ;pred
             (expand-expression env cctx exp (caddr exp)))] ;than
      [(pred then else)
       (list (make <vsymbol> :exp (car exp) :env env ;if
                   :debug-info (debug-source-info exp))
             (expand-expression env 'expr exp (cadr exp)) ;pred
             (expand-expression env cctx exp (caddr exp)) ;than
             (expand-expression env cctx exp (cadddr exp)))] ;else
      [else (vise-error "Compiler: Bad if syntax:~a" exp)])))

(define (expand-set! env ctx parent exp)
  (match (cdr exp)
    [(sym e)
     (list
       (make <vsymbol> :exp (car exp) :env env ;set!
             :debug-info (debug-source-info exp))
       (if (symbol? sym) ;;symbol
         (rlet1 sym (make <vsymbol> :exp sym :env env)
           (if-let1 d (env-find-data env sym)
             (@inc! d.set-count)))
         (expand-expression env 'expr exp sym))
       (expand-expression env 'expr exp e))]
    [else (vise-error "Compiler: Bad set! syntax:~a" exp)]))

(define (expand-dolist env ctx parent exp)
  (match exp
    [(_ (var expr) . body)
     (let1 dolist-env (make-env env)
       (append
         (list
           (make <vsymbol> :exp (car exp) :env env ;dolist 
                 :debug-info (debug-source-info exp))
           (list
             (expand-symbol-bind var expr 'local dolist-env)
             (expand-expression env 'expr exp expr)))
         (map
           (pa$ expand-expression dolist-env 'stmt exp)
           body)))]
    [else (vise-error "Compiler: Bad syntax:~a" exp)]))

(define (expand-let env ctx parent exp)
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
               (var-loop (lambda (var) (expand-expression let-env 'expr exp (cadar var)))))
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
                       (expand-expression (@ let-env.parent) 'expr exp (cadar vars)))
                     acc)))))
       (let1 injection-env (make-env let-env)
         `(,(make <vsymbol> :exp 'let :env env ;let or let* or letrec
                  :debug-info (debug-source-info exp)
                  :prop `((body-env . ,let-env) (injection-env . ,injection-env)))
            ,vars
            ,@(map ;body
                (pa$ expand-expression injection-env 'stmt exp)
                (cddr exp)))))]
    ;;named-let
    [(let (? symbol? name) ((var . spec) ...) . body)
     (expand-expression env ctx parent
                        `(letrec ((,name (lambda ,var ,@body)))
                           (,name ,@(map car spec))))]
    [_ (vise-error "Bad let syntax:~a" exp)]))

(define (expand-begin env ctx parent exp)
  (if (eq? ctx 'expr)
    (expand-expression env 'expr parent `((lambda () ,@(cdr exp))))
    (cons 
      (make <vsymbol> :exp (car exp) :env env
            :debug-info (debug-source-info exp))
      (map (pa$ expand-expression env ctx exp)
           (cdr exp)))))

(define (expand-dict env ctx parent exp)
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
             (expand-expression env 'expr exp init)))
         sym init)]
      [_ (vise-error "dict format error")])))

(define (expand-try env ctx parent exp)
  `(,(make <vsymbol> :exp (car exp) :env env ;dolist 
           :debug-info (debug-source-info exp))
     ,(expand-expression env 'stmt exp (cadr exp))
     ,@(map
         (lambda (clause) 
           (if (list? clause)
             (map (pa$ expand-expression env 'stmt clause) clause)
             clause))
         (cddr exp))))

(define (expand-autocmd env ctx parent exp)
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
        (expand-expression env 'expr exp group)
        (map (pa$ expand-expression env 'expr exp) events)
        pat
        nest
        (expand-expression env 'stmt exp cmd)))))

(define (expand-apply env ctx parent exp)
  (let1 e (env-find-exp env (car exp))
    (if (vmacro? e)
      ;;macro expand
      (expand-expression env ctx parent ((@ e.exp) exp))
      ;;function call
      (rlet1 form (map (pa$ expand-expression env 'expr exp) exp)
        (when (is-a? (car form) <vexp>)
          (slot-set! (car form) 'debug-info (debug-source-info exp)))))))

(define (expand-refer-symbol env ctx parent exp)
  (rlet1 sym (make <vsymbol> 
                   :exp exp
                   :env env
                   :parent parent)
    (if-let1 d (env-find-data env sym)
      (@inc! d.ref-count))))


