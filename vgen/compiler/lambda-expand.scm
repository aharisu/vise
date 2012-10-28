
(define (vise-phase-lambda-expand form-list)
  (sexp-traverse
    form-list
    `((let . ,lambda-expand-let)
      (defvar . ,lambda-expand-defvar)))
  (sexp-traverse 
    form-list
    `((,traverse-apply-function-hook . ,lambda-expand-apply-func))))

(define (lambda-expand-let form ctx loop)
  (append
    (list
      (car form) ;let
      (map
        (lambda (clause) 
          (lambda-expand-symbol-bind (car clause) (cadr clause))
          (list (car clause) (loop 'expr (cadr clause))))
        (cadr form)))
    (map (pa$ loop 'stmt) (cddr form))))

(define (lambda-expand-defvar form ctx loop)
  (lambda-expand-symbol-bind (cadr form) (caddr form))
  (list
    (car form)
    (cadr form)
    (loop 'expr (caddr form))))

(define (lambda-expand-symbol-bind sym init)
  (when (and (vsymbol? sym) (list? init) (eq? (vexp (car init)) 'lambda))
    (let1 d (env-find-data (@ sym.env) sym)
      (@! d.prop (cons (cons 'init-expr init) (@ d.prop))))))


(define (lambda-expand-apply-func form ctx loop)
  (define (gen-let-clause env args init)
    (let* ([rest? (and (not (null? args))
                    (let1 last (last args)
                      (has-attr? (env-find-data (@ last.env) last) 'rest)))]
           [len-args (- (length args) (if rest? 1 0))]
           [len-init (length init)])
      (unless ((if rest? <= =) len-args len-init)
        (vise-error "Wrong number of arguments. (required ~a, got ~a): ~a" len-args len-init form))
      (append
        (map
          (lambda (arg init)
            (list 
              (rlet1 sym (make <vsymbol> :exp (vexp arg) :env env)
                (env-add-symbol env sym 'local))
              init))
          args (take init len-args))
        (if rest?
          (list (list
                  (rlet1 sym (make <vsymbol> :exp (vexp (last args)) :env env)
                    (env-add-symbol env sym 'local))
                  (list 'quote (drop init len-args))))
          '()))))
  (define (merge-let form)
    (if (and (eq? (vexp (car form)) 'let) (eq? (vexp (caaddr form)) 'let))
      `(,(caaddr form)
         ;;外側letと内側letのvarsをUnionする
         ,(fold-right
            (lambda (v acc)
              (if-let1 inner-var (find (.$ (pa$ eq? (vexp (car v))) vexp car) acc)
                  ;;内側letの初期化式の中で使用されている外側letのシンボルを置換する
                  (let* ([self (env-find-data (slot-ref (car v) 'env) (car v))]
                         [init (sexp-traverse 
                                 (cadr inner-var)
                                 `((,traverse-symbol-ref
                                     . ,(lambda (form ctx loop)
                                          (if (eq? self (and (vsymbol? form)
                                                          (env-find-data (@ form.env) form)))
                                            (cadr v)
                                            form)))))])
                    ;;初期化式を置換する
                    (let loop ((vars acc)
                               (acc '()))
                      (if (null? vars)
                        (reverse! acc)
                        (loop (cdr vars)
                              (cons 
                                (if (eq? (vexp (caar vars)) (vexp (car v))) 
                                  (list (caar vars) init)
                                  (car vars))
                                acc)))))
                  (cons v acc)))
            (cadr (caddr form)) ;;inner vars
            (cadr form)) ;;outer vars
         ,@(cddr (caddr form))) ;;inner body
      form))

  (let* ([sym (car form)]
         [env (and (vsymbol? sym) (slot-ref sym 'env))]
         [d (and (vsymbol? sym) (env-find-data env sym))])
    (if (and d (has-attr? d 'lambda)
          (zero? (@ d.set-count)) (= 1 (@ d.ref-count)))
      (let* ([init (assq-ref (@ d.prop) 'init-expr)]
             [env (assq-ref (slot-ref (car init) 'prop) 'injection-env)]
             [new-injection-env (env-injection env)])
        ;make new injection-env
        (assq-set! (slot-ref (car init) 'prop) 'injection-env new-injection-env)
        ;;decrease ref-count
        (@dec! d.ref-count)
        ;;merge outer and inner
        (merge-let
          `(,(make <vsymbol> :exp 'let :env (@ env.parent)
                   :prop `((body-env . ,env) (injection-env . ,new-injection-env)))
             ,(gen-let-clause env (filter vsymbol? (cadr init)) (cdr form))
             ,@(cddr init))))
      (map (pa$ loop 'expr) form))))

