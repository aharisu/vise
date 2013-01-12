(define-module vgen.common
  (use gauche.parameter)
  (use gauche.vport)
  (use srfi-1)
  (use srfi-13)
  (use vgen.util)
  (export-all)
  )

(select-module vgen.common)

(define-condition-type <vise-error> <error> #f)

(define script-prefix (make-parameter "s:"))

;;-------------
;;Util Type
;;-------------

(define (is-function-call? exp)
  (and (list? exp) (not (eq? (car exp) 'quote))))

(define (get-evaluated-exp e)
  (cond
    [(and (list? e) (eq? (vexp (car e)) 'quote)
       (not (symbol? (vexp (cadr e)))))
     (cadr e)]
    [(vsymbol? e) 
     (let1 d (env-find-data e)
       (if (and d (not (eq? env-data-none-value (@ d.value))))
         (get-evaluated-exp (@ d.value))
         e))]
    [else e]))

;;
;; <vexp>
(define-class <vexp> ()
  (
   (exp :init-keyword :exp)
   (attr :init-value '())
   (prop :init-keyword :prop  :init-value '())
   (parent :init-keyword :parent :init-value #f)
   (debug-info :init-keyword :debug-info :init-value #f)
   )
  )

(define-method write-object ((sym <vexp>) port)
  (format port "~a" (@ sym.exp)))

(define (vexp o)
  (if (is-a? o <vexp>)
    (@ o.exp)
    o))

;;
;; <vsymbol>
(define-class <vsymbol> (<vexp>)
  (
   (env :init-keyword :env)
   )
  )

(define vsymbol? (cut is-a? <> <vsymbol>))

(define (get-symbol sym)
  (cond 
    [(vsymbol? sym) (@ sym.exp)]
    [(symbol? sym) sym]
    [else #f]))

;;
;;<vmacro>
(define-class <vmacro> (<vexp>)
  (
   (env :init-keyword :env)
   )
  )

(define vmacro? (cut is-a? <> <vmacro>))

(define-constant env-data-none-value (gensym))

(define-constant toplevel-env (make-parameter #f))

;;;;;
;;refer data
;;@slot scope {@ 'arg 'script 'global 'window 'buffer 'syntax}
(define-class <env-data> ()
  (
   (symbol :init-keyword :symbol)
   (exp :init-keyword :exp)
   (value :init-value env-data-none-value) ;evaluated value
   (scope :init-keyword :scope)
   (attr :init-keyword :attr :init-value '())
   (vim-name :init-value #f)
   (ref-count :init-value 0)
   (set-count :init-value 0)
   (prop :init-keyword :prop  :init-value '())
   )
  )

(define (env-data-not-use? d) 
  (and (not (has-attr? d 'auto-gen))
    (zero? (slot-ref d 'set-count)) (zero? (slot-ref d 'ref-count))))
(define (env-data-ref-only? d) 
  (and (not (has-attr? d 'auto-gen))
    (zero? (slot-ref d 'set-count))))
(define (env-data-set-only? d) 
  (and (not (has-attr? d 'auto-gen))
    (zero? (slot-ref d 'ref-count))))

(define (attr-push! o attr)
  (@! o.attr (set-cons (@ o.attr) attr)))

(define (attr-remove! o attr)
  (@! o.attr (remove! (pa$ equal? attr) (@ o.attr))))

(define (has-attr? o attr)
  (and (slot-exists? o 'attr) (set-exists (@ o.attr) attr)))

(define (get-vim-name env-data)
  (if (@ env-data.vim-name)
    (@ env-data.vim-name)
    (rlet1 name (vise-gensym (@ env-data.symbol) (@ env-data.scope) (@ env-data.attr))
      (@! env-data.vim-name name))))

(define (cmd-symbol? symbol)
  (string-scan (x->string (vexp symbol)) #\#))


;;;;;
;;@param scope {@ 'arg 'script 'global 'window 'buffer 'syntax}
;;@param attr {@ set-list}
(define (vise-gensym sym scope attr)
  (cond
    [(or (eq? scope 'syntax) (cmd-symbol? sym)) (x->string sym)]
    [(and (eq? scope 'arg) (set-exists attr 'rest)) "a:000"]
    [(string=? (substring* (x->string sym) 0 2) "l:") (x->string sym)]
    [else 
      (let ((prefix (case scope
                      ((arg) #\a)
                      ((script) (string-ref (script-prefix) 0))
                      ((global) #\g)
                      ((window) #\w)
                      ((buffer) #\b)
                      (else #f)))
            (name (if (set-exists attr 'func-call)
                    (string-titlecase (x->string sym))
                    (x->string sym))))
        (if prefix
          (let1 sym (x->string sym)
            (if (and (< 1 (string-length sym)) (eq? (string-ref name 1) #\:)
                  (eq? (string-ref name 0) prefix))
              name 
              (string-append (x->string prefix) ":" name)))
          (symbol->string (gensym (string-append name "_")))))]))

(define (remove-symbol-prefix symbol)
  (if-let1 ret (string-scan (x->string symbol) ":" 'after)
    ret
    symbol))

;;
;;environment
(define-class <env> ()
  (
   (symbols :init-value '())
   (children :init-value '())
   (parent :init-keyword :parent)
   (lambda-border? :init-keyword :lambda-border?)
   )
  )

(define (make-env parent :optional (lambda-border? #f))
  (rlet1 env (make <env> 
                   :parent parent
                   :lambda-border? lambda-border?)
    (when parent
      (@push! env.parent.children env))))

(define env-toplevel?  (.$ not (cut slot-ref <> 'parent)))

(define (env-add-symbol env symbol scope :key (attr '()))
  (env-add-symbol&exp env symbol scope #f :attr attr))

(define (env-add-symbol&exp env symbol scope exp :key (attr '()))
  (let* ([symbol (get-symbol symbol)]
         [data (make <env-data>
                    :symbol symbol
                    :exp exp
                    :scope scope
                    :attr attr)])
    (@push! env.symbols (cons symbol data))
    data))

(define (env-find-data-with-outside-lambda? env symbol)
  (let1 symbol (get-symbol symbol)
    (let loop ((env env)
               (outside? #f))
      (if-let1 d (assq-ref (@ env.symbols) symbol)
        (values d outside?)
        (if (@ env.parent)
          (loop (@ env.parent) (or outside? (@ env.lambda-border?)))
          (values #f #f))))))

(define (env-find-data symbol :optional env)
  (let1 env (if (undefined? env) (@ symbol.env) env)
    (receive (d outside?) (env-find-data-with-outside-lambda? env symbol)
      d)))

(define (env-find-exp symbol :optional env)
  (let1 env (if (undefined? env) (@ symbol.env) env)
    (if-let1 d (env-find-data symbol env)
      (@ d.exp)
      #f)))

(define (allow-rebound? vsymbol)
  (if-let1 d (and (vsymbol? vsymbol) (env-find-data vsymbol))
    (not (or (eq? (@ d.scope) 'arg) (eq? (@ d.scope) 'syntax)))
    #t))

(define (env-injection env :optional inject)
  (let* ([p (@ env.parent)]
         [inject (if (undefined? inject) 
                   (make-env p)
                   (begin
                     (@! inject.parent p)
                     inject))])
    (@! p.children (remove! (pa$ eq? env) (@ p.children)))
    (@push! p.children inject)
    (@! env.parent inject)
    inject))

(define (print-env-table env)
  (let loop ((env env))
    (for-each
      (lambda (item) (print (car item) ":" (cdr item)))
      (@ env.symbols))
    (when (@ env.parent)
      (print "->")
      (loop (@ env.parent))))
  (newline))

;;
;;virtual output port
(define-class <vise-output-port> (<virtual-output-port>)
  (
   (raw :init-keyword :raw)
   (prev :init-keyword :prev)
   (indent :init-keyword :indent)
   ))

(define (replace-indent prev str indent-level)
  (let* ([num (* indent-level 2)]
         [indent-str (if (zero? num)
                       ""
                       (format 
                         (string-append "~" (number->string num) ",,,' a")
                         " "))])
    (string-append 
      (if (rxmatch #/\n$/ prev) indent-str "")
      (regexp-replace #/\n +$/
                      (regexp-replace-all #/\n/ str (string-append "\n" indent-str))
                      "\n"))))

(define (make-vise-output-port port)
  (let ([prev (cons "" (undefined))]
        [indent (cons 0 (undefined))])
    (make <vise-output-port>
          :raw port
          :prev prev
          :indent indent
          :putc (lambda (c) 
                  (let* ([c (x->string c)]
                         [str (replace-indent (car prev) c (car indent))])
                    (set-car! prev c)
                    (display str port)))
          :puts (lambda (x)
                  (let* ([x-str (x->string x)]
                         [str (replace-indent (car prev) x-str (car indent))])
                    (set-car! prev x-str)
                    (display str port)))
          :flush  (pa$ flush port)))) 

(define (add-new-line :optional (port (current-output-port)))
  (unless (rxmatch #/\n$/ (car (@ port.prev)))
    (newline port)))

(define (get-indent-level :optional (port (current-output-port)))
  (car (@ port.indent)))

(define (set-indent-level! indent :optional (port (current-output-port)))
  (set-car! (@ port.indent) indent))

(define-macro (add-indent . body)
  `(unwind-protect
     (begin 
       (inc! (car (slot-ref (current-output-port) 'indent)))
       ,@body)
     (dec! (car (slot-ref (current-output-port) 'indent)))))

;;
;;set
(define (set-exists set obj)
  (any (.$ (pa$ equal? (vexp obj)) vexp) set))

(define (set-cons set obj)
  (if (set-exists set obj)
    set
    (cons obj set)))


;;----------
;;Compile
;;----------

(define (p-ret e)
  (display e (standard-output-port))
  (newline (standard-output-port))
  e)

(define (vise-error msg . obj)
  (let1 obj (map
              (lambda (obj)
                (if-let1 info (or (debug-source-info obj)
                                (and (pair? obj) (is-a? (car obj) <vexp>) (slot-ref (car obj) 'debug-info)))
                  (format "file:~a line:~a form:~a" (car info) (cadr info) obj)
                  obj))
              obj)
    (apply errorf <vise-error> msg obj)))


(define-constant traverse-apply-function-hook (gensym))
(define-constant traverse-symbol-ref (gensym))

(define (sexp-traverse form hook)
  (define (call-symbol-ref-hook ctx sym)
    (if-let1 func (assq-ref hook traverse-symbol-ref)
      (func sym ctx loop)
      sym))
  (define (loop ctx form)
    (if (list? form)
      (if-let1 func (assq-ref hook (vexp (car form)))
        (func form ctx loop)
        (case (vexp (car form))
          [(quote) form]
          [(defun) 
           (append
             (list
               (car form) ;defun
               (cadr form) ;name
               (caddr form) ;args
               (cadddr form)) ;modifier
             (map (pa$ loop 'stmt) (cddddr form)))]
          [(defvar)
           (list
             (car form) ;defvar
             (cadr form) ;name
             (loop 'expr (caddr form)))]
          [(lambda) 
           (append
             (list
               (car form) ;lambda
               (cadr form)) ;args
             (map (pa$ loop 'stmt) (cddr form)))]
          [(if) 
           (let1 cctx (if (eq? ctx 'expr) 'expr ctx)
             (if (null? (cdddr form))
               (list
                 (car form)
                 (loop 'expr (cadr form))
                 (loop cctx (caddr form)))
               (list
                 (car form)
                 (loop 'expr (cadr form))
                 (loop cctx (caddr form))
                 (loop cctx (cadddr form)))))]
          [(return)
           (if (null? (cdr form))
             form 
             (list
               (car form) ;return
               (loop 'expr (cadr form))))]
          [(set!) 
           (list
             (car form);set!
             (call-symbol-ref-hook ctx (cadr form)) ;name
             (loop 'expr (caddr form)))]
          [(let)
           (append
             (list
               (car form) ;let
               (map
                 (lambda (clause) (list (car clause) (loop 'expr (cadr clause))))
                 (cadr form)))
             (map (pa$ loop 'stmt) (cddr form)))]
          [(dolist)
           (append
             (list
               (car form)
               (list
                 (caadr form)
                 (loop 'expr (cadadr form))))
             (map (pa$ loop 'stmt) (cddr form)))]
          [(while)
           (append
             (list
               (car form) ;name
               (loop 'expr (cadr form)))
             (map (pa$ loop 'stmt) (cddr form)))]
          [(begin and or quasiquote unquote unquote-splicing augroup raw-vimscript)
           (append
             (cons (car form) '()) ;name
             (map (pa$ loop (if (or* eq? (vexp (car form)) 'and 'or) 'expr ctx)) 
                  (cdr form)))]
          [(try) 
           `(,(car form);try
              ,(loop 'stmt (cadr form)) ;body
              ,@(map  ;catch - finally clause
                  (lambda (clause)
                    `(,(car clause) ,@(map (pa$ loop 'stmt) (cdr clause))))
                   (cddr form)))]
          [(list-func)
           (list
             (car form) ;list
             (cadr form) ;function
             (loop 'expr (caddr form))
             (loop 'expr (cadddr form)))]
          [(autocmd)
           (list
             (car form) ;autocmd
             (cadr form);group
             (caddr form) ;events
             (cadddr form) ;pat
             (car (cddddr form)) ;nest
             (loop 'stmt (cadr (cddddr form))))] ;cmd
          [(dict)
           (append
             (cons (car form) '())
             (map
               (lambda (pair) (list (car pair) (loop 'expr (cadr pair))))
               (cdr form)))]
          [else 
            (if-let1 func (assq-ref hook traverse-apply-function-hook)
              (func form ctx loop)
              (map (pa$ loop 'expr) form))]))
      (call-symbol-ref-hook ctx form)))

  (if (list? form)
    (map (pa$ loop 'toplevel) form)
    (loop 'toplevel form)))

(define vim-cmd-list '())

(define (find-tail-exp action ctx exp)
  (cond
    [(list? exp)
     (case (get-symbol (car exp))
       [(defun let)
        `(,@(drop-right exp 1)
           ,(find-tail-exp action 'stmt (last exp)))]
       [(begin) 
        `(,@(drop-right exp 1)
           ,(find-tail-exp action
                           (if (eq? 'begin (get-symbol (car exp))) ctx 'expr)
                           (last (cdr exp))))]
       [(if)
        (let1 cctx (if (eq? ctx 'expr) 'expr ctx)
          (if (null? (cdddr exp))
            (list (car exp) (cadr exp)
                  (find-tail-exp action cctx (caddr exp))) ;then
            (list (car exp) (cadr exp)
                  (find-tail-exp action cctx (caddr exp)) ;then
                  (find-tail-exp action cctx (cadddr exp)))))] ;else
       [(set!)
        (list (car exp) (cadr exp)
              (find-tail-exp action 'expr (caddr exp)))]
       [(return) (find-tail-exp action 'stmt (cadr exp))]
       [(while augroup autocmd) exp]
       [(quasiquote) exp] ;;TODO
       [(try)
        `(,(car exp)
           ,(find-tail-exp action ctx (cadr exp))
           ,@(map
               (lambda (clause)
                 (if (< 1 (length clause))
                   `(,@(drop-right clause 1)
                      ,(find-tail-exp action 'stmt (last clause)))
                   clause))
               (cddr exp)))]
       [else 
         (if (or (any (pa$ eq? (get-symbol (car exp))) vim-cmd-list)
               (eq? 'raw-vimscript (get-symbol (car exp))))
           exp
           (action exp ctx))])]
    [else (action exp ctx)]))

(define (statement-expression? form)
  (and (pair? form)
    (let1 func (get-symbol (car form))
      (case func
        [(defun defvar return set! dolist while augroup try autocmd)
         #t]
        [else (any (pa$ eq? func) vim-cmd-list)]))))

(define-constant renderer-table (make-hash-table 'eq?))

(define (vise-register-renderer! name renderer)
  (hash-table-put! renderer-table name renderer))

(define (vise-lookup-renderer sym)
  (if-let1 v (vise-lookup-renderer-value sym)
    (cdr v)
    #f))

(define (vise-lookup-renderer-ctx sym)
  (if-let1 v (vise-lookup-renderer-value sym)
    (car v)
    #f))

(define (vise-lookup-renderer-value sym)
  (cond 
    [(vsymbol? sym)
     (if-let1 d (env-find-data sym)
       (and (eq? (@ d.scope) 'syntax) (@ d.exp))
       #f)]
    [(symbol? sym) (hash-table-get renderer-table sym #f)]
    [else #f]))

;;syntax name symbol -> (lambda () ... "func-name")
;;registered in tha vgen.compiler.render module
(define-constant syntax-function-ref-table (make-hash-table 'eq?))

(define-constant vim-symbol-list (include "vim-function.scm"))

