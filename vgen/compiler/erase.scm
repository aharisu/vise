
(define (vise-phase-erase form-list)
  (sexp-traverse
    form-list
    `((defvar . ,erase-sym-bind-defvar)
      (let . ,erase-sym-bind-let)))
  (parameterize ([erased-flag #f])
    (let1 form-list (filter-map
                      (erase-filter 'toplevel #f)
                      form-list)
      (if (erased-flag)
        (vise-phase-erase form-list)
        form-list))))

(define (erase-sym-bind-defvar form ctx loop)
  (erase-sym-bind (cadr form) (caddr form))
  (loop 'expr (caddr form)))

(define (erase-sym-bind-let form ctx loop)
  (for-each
    (lambda (clause) 
      (erase-sym-bind (car clause) (cadr clause))
      (list (car clause) (loop 'expr (cadr clause))))
    (cadr form))
  (for-each (pa$ loop 'stmt) (cddr form)))

(define (erase-is-literal? form)
  (cond
    [(list? form) 
     (case (vexp (car form))
       [(quote lambda) #t]
       [else #f])]
    [(vsymbol? form) 
     (if-let1 d (env-find-data form)
       (if (vsymbol? (@ d.value))
         (erase-is-literal? (@ d.value))
         (not (eq? env-data-none-value (@ d.value))))
       #f)]
    [(symbol? form) #f]
    [else #t]))

(define (erase-sym-bind sym exp)
  (let1  d (and (vsymbol? sym) (env-find-data sym))
    (when (and d (env-data-ref-only? d))
      (@! d.value exp))))

(define-constant exp-erased (gensym))
(define erased-flag (make-parameter #f))

(define (mark-erase exp)
  (erased-flag #t)
  exp)

(define (erase-filter ctx parent)
  (lambda (exp)
    (let1 e (erase-expression ctx parent exp)
      (if (eq? exp-erased e)
        #f
        e))))

(define (erase-expression ctx parent form)
  (cond
    [(list? form)
     (case (vexp (car form))
       [(quote) form]
       [(defun) (erase-defun ctx parent form)]
       [(defvar) (erase-defvar ctx parent form)]
       [(lambda) (erase-lambda ctx parent form)]
       [(if) (erase-if ctx parent form)]
       [(set!) (erase-set! ctx parent form)]
       [(return) (erase-return ctx parent form)]
       [(let) (erase-let ctx parent form)]
       [(dolist) (erase-dolist ctx parent form)]
       [(while) (erase-while ctx parent form)]
       [(begin and or) (erase-begin-or-and ctx parent form)]
       [(augroup) (erase-augroup ctx parent form)]
       [(list-func) (erase-list-func ctx parent form)]
       [(dict) (erase-dict ctx parent form)]
       [(try) (erase-try ctx parent form)]
       [(autocmd) (erase-autocmd ctx parent form)]
       [else (erase-apply ctx parent form)])]
    [(vsymbol? form) (erase-refer-symbol ctx parent form)]
    [else form]))

(define (erase-defun ctx parent form)
  `(,(car form);defun
     ,(cadr form);name
     ,(caddr form);args
     ,(cadddr form);modifier
     ,@(filter-map
         (erase-filter 'stmt form)
         (cddddr form))))

(define (erase-defvar ctx parent form)
  (list
    (car form);defvar
    (cadr form);name
    (erase-expression 'expr form (caddr form))))

(define (erase-lambda ctx parent form)
  `(,(car form);lambda
     ,(cadr form);args
     ,@(filter-map (erase-filter 'stmt form) (cddr form))))

(define (erase-if ctx parent form)
  (let1 frm (filter
              (lambda (e)
                (if (eq? exp-erased e)
                  (mark-erase #f)
                  e))
              (let1 cctx (if (eq? ctx 'expr) 'expr ctx)
                (if (null? (cdddr form))
                  (list
                    (car form)
                    (erase-expression 'expr form (cadr form))
                    (erase-expression cctx form (caddr form)))
                  (list
                    (car form)
                    (erase-expression 'expr form (cadr form))
                    (erase-expression cctx form (caddr form))
                    (erase-expression cctx form (cadddr form))))))
    (if (erase-is-literal? (cadr frm))
      (mark-erase
        (begin
          (if-let1 d (and (vsymbol? (cadr frm)) (env-find-data (cadr frm)))
            (@dec! d.ref-count))
          (if (or* eq? (get-evaluated-exp (cadr frm)) #f 0)
            (if (null? (cdddr frm)) exp-erased (cadddr frm)) ;;get else
            (caddr frm)))) ;;get then
      frm)))

(define (erase-set! ctx parent form)
  (list
    (car form);set!
    (cadr form) ;name
    (erase-expression 'expr form (caddr form))))

(define  (erase-return ctx parent form)
  (if (null? (cdr form))
    form 
    (list
      (car form) ;return
      (erase-expression 'expr form (cadr form)))))

(define (erase-dolist ctx parent form)
  `(,(car form)
     ,(list
        (caadr form)
        (erase-expression 'expr form (cadadr form)))
     ,@(filter-map (erase-filter 'stmt form) (cddr form))))

(define (erase-while ctx parent form)
  `(,(car form) ;name
     ,(erase-expression 'expr form (cadr form))
     ,@(filter-map (erase-filter 'stmt form) (cddr form))))

(define (erase-begin-or-and ctx parent form)
  `(,(car form) ;name
     ,@(filter-map 
         (pa$ erase-expression (if (or* eq? (vexp (car form)) 'and 'or) 'expr ctx) form) 
         (cdr form))))

(define (erase-augroup ctx parent form)
  form)

(define (erase-list-func ctx parent form)
  (list
    (car form) ;list
    (cadr form) ;function
    (erase-expression 'expr form (caddr form))
    (erase-expression 'expr form (cadddr form))))

(define (erase-let ctx parent form)
  `(,(car form)
     ,(let loop ([vars (cadr form)]
                 [acc '()])
        (if (null? vars)
          (reverse! acc)
          (let1 d (and (vsymbol? (caar vars)) (env-find-data (caar vars)))
            (loop (cdr vars)
                  (if (and d (env-data-not-use? d) (erase-is-literal? (cadar vars)))
                    (mark-erase acc) ;erase var decl
                    (cons 
                      (list
                        (caar vars)
                        (erase-expression 'expr (car vars) (cadar vars)))
                      acc))))))
     ,@(filter-map
         (erase-filter 'stmt form)
         (cddr form))))

(define (erase-dict ctx parent form)
  `(,(car form)
     ,@(map
         (lambda (pair) (list (car pair) (erase-expression 'expr form (cadr pair))))
         (cdr form))))

(define (erase-try ctx parent form)
  form)

(define (erase-autocmd ctx parent form)
  (list
    (car form) ;autocmd
    (cadr form);group
    (caddr form) ;events
    (cadddr form) ;pat
    (car (cddddr form)) ;nest
    (erase-expression 'stmt form (cadr (cddddr form)))))

(define (erase-apply ctx parent form)
  (map (pa$ erase-expression 'expr form) form))

(define (erase-refer-symbol ctx parent form)
  form)

