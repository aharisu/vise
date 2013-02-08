(define-module vgen.compiler.render
  (use gauche.parameter)
  (use util.match)

  (use vgen.util)
  (use vgen.common)
  (use vgen.compiler.expand)
  (use vgen.compiler.add-return)
  (export vise-phase-render
    ))

(select-module vgen.compiler.render)

(define auto-generate-exp (make-parameter '()))

(define (add-auto-generate-exp sym exp)
  (let1 l (auto-generate-exp)
    (unless (assoc sym l)
      (auto-generate-exp (acons sym exp l)))))

(define (vise-phase-render exp)
  (let loop ((exp exp)
             (acc '()))
    ;;mark free variable
    (sexp-traverse
      exp
      `((,traverse-symbol-ref 
          . ,(lambda (form ctx loop)
               (when (vsymbol? form)
                 (receive (d outside?) (env-find-data-with-outside-lambda? (@ form.env) form)
                   (when d
                     (if (and outside? (or* eq? (@ d.scope) 'local 'arg))
                       (attr-push! d 'free)
                       (attr-remove! d 'free)))))
               form))
        (,traverse-apply-function-hook
          . ,(lambda (form ctx loop)
               (when (vsymbol? (car form))
                 (attr-push! (car form) 'function-call))
               (map (pa$ loop 'expr) form)))))
    (parameterize ([auto-generate-exp '()])
      (let1 ret (append
                  (map 
                    (lambda (form)
                      (let1 str-port (make-vise-output-port (open-output-string))
                        (vise-render 'toplevel form str-port)
                        (get-output-string (@ str-port.raw))))
                    exp)
                  acc)
        (if (null? (auto-generate-exp))
          ret
          (loop (map cdr (auto-generate-exp)) ret))))))

(define (vise-render ctx exp :optional (out-port (current-output-port)))
  (with-output-to-port
    out-port
    (lambda ()
      (cond
        [(vsymbol? exp)
         (display (vim-ref-symbol exp))]
        [(list? exp)
         (if-let1 renderer (vise-lookup-renderer (car exp))
           (let1 ret (renderer exp ctx)
             (unless (undefined? ret) (vise-render ctx ret out-port)))
           (render-func-call ctx exp))]
        [else (render-literal exp ctx)]))));literal

(define (vim-symbol symbol)
  (vise-render-identifier
    (if-let1 d (and (vsymbol? symbol) (env-find-data symbol))
      (get-vim-name d)
      (x->string symbol))))

(define (vim-unboxing vim-symbol)
  (string-append vim-symbol "[0]"))

(define (boxing? symbol)
  (let1 d (and (vsymbol? symbol) (env-find-data symbol))
    (and d 
      (or* eq? (@ d.scope) 'local 'arg)
      (has-attr? d 'free)
      (not (env-data-ref-only? d)))))

(define (vim-function-ref func-name)
  #`"function(,(get-sid-prefix-symbol)() . ',(remove-symbol-prefix func-name)')")

(define-macro (define-syntax-function-ref op . body)
  `(hash-table-put! syntax-function-ref-table ,op
                    (let ([func-name #f])
                      (lambda ()
                        (unless func-name
                          (set! func-name (gensym (string-append (script-prefix) (vim-symbol ,op))))
                          (add-auto-generate-exp
                            ,op
                            (add-return-defun
                              (expand-toplevel-expression
                                `(defun ,func-name args :normal ,,@body)))))
                        func-name))))

(define (vim-ref-symbol symbol :optional (unboxing? #t) env)
  (receive (d outside?) (if (vsymbol? symbol)
                          (env-find-data-with-outside-lambda? 
                            (if (undefined? env) (@ symbol.env) env)
                            symbol)
                          (values #f #f))
    ($ (lambda (sym)
         (if (and unboxing? (boxing? symbol))
           (vim-unboxing sym)
           sym))
      $ (lambda (sym)
          (cond
            [(and d (has-attr? d 'function) (not (has-attr? symbol 'function-call)))
             (vim-function-ref sym)]
            [(and d (eq? (@ d.scope) 'syntax) (not (has-attr? symbol 'function-call)))
             (vim-function-ref ((hash-table-get syntax-function-ref-table (vexp symbol))))]
            [else sym]))
      $ (lambda (sym)
          (if (has-attr? symbol 'self-recursion)
            "self"
            (if (and d outside? (or* eq? (@ d.scope) 'local 'arg))
              (string-append "self['" sym "']")
              sym)))
      $ vim-symbol
      symbol)))

(define dict-type-symbol #f)
(define (get-dict-type-symbol)
  (unless dict-type-symbol
    (set! dict-type-symbol (gensym (string-append (script-prefix) "dict_type_")))
    (add-auto-generate-exp 'dict-type
                           `(defvar ,dict-type-symbol (type (dict)))))
  dict-type-symbol)

(define sid-prefix-symbol #f)
(define (get-sid-prefix-symbol)
  (unless sid-prefix-symbol
    (set! sid-prefix-symbol (gensym (string-append (script-prefix) "SID_PREFIX_")))
    (add-auto-generate-exp 
      'sid-prefix
      `(defun ,sid-prefix-symbol () :normal
              ,(if (string=? "s:" (script-prefix))
                 `(return (matchstr (expand (sq-str "<sfile>"))
                                    (sq-str ,#`"<SNR>\\d\\+_\\ze,(remove-symbol-prefix sid-prefix-symbol)$")))
                 `(return ,(script-prefix))))))
  sid-prefix-symbol)

(define-constant temp-symbol (gensym "temp_"))

(define (render-func-call ctx form)
  (define (is-direct-call? d form)
    (or (and (list? (car form)) (has-attr? (caar form) 'lambda-no-free-vars))
      (and d (has-attr? d 'lambda-no-free-vars))))

  (let ((params #`"(,(string-join (map (pa$ vise-render-to-string 'expr) (cdr form)) \",\"))")
        (sym (vise-render-to-string 'expr (car form)))
        (d (and (vsymbol? (car form)) (env-find-data (car form)))))
    (cond
      [(and (symbol? (vexp (car form)))
         (or (not d) (eq? (@ d.scope) 'syntax) (has-attr? d 'function)))
       (when (or (stmt-ctx? ctx) (toplevel-ctx? ctx))
         (display "call "))
       (display (string-append sym params))]
      [(or (and (list? (car form)) (eq? 'lambda (vexp (caar form))))
         (and d (has-attr? d 'lambda)))
       (when (or (stmt-ctx? ctx) (toplevel-ctx? ctx))
         (if (or (symbol? (vexp (car form))) (is-direct-call? d form))
           (display "call ")
           (begin (display "let ") (display temp-symbol) (display " = "))))
       (if (is-direct-call? d form)
         (display (string-append sym params))
         (display (string-append sym ".func" params)))]
      [else 
        (display
          (if (or (stmt-ctx? ctx) (toplevel-ctx? ctx))
            #`"if (type(,sym)==,(get-dict-type-symbol))\n  call ,|sym|.func,|params|\nelse\n  call ,|sym|,|params|\nendif"
            #`"(type(,sym)==,(get-dict-type-symbol)) ? ,|sym|.func,|params| : ,|sym|,|params|"))]))
  (when (or (stmt-ctx? ctx) (toplevel-ctx? ctx))
    (add-new-line)))


(define (vise-render-to-string ctx form)
  (let1 port (make-vise-output-port (open-output-string))
    (vise-render ctx form port)
    (get-output-string (@ port.raw))))

(define (expr-ctx? ctx) (eq? ctx 'expr))
(define (stmt-ctx? ctx) (eq? ctx 'stmt))
(define (toplevel-ctx? ctx) (eq? ctx 'toplevel))

(define (ensure-expr-ctx form ctx)
  (unless (expr-ctx? ctx)
    (if (stmt-ctx? ctx)
      (vise-error "vise: expression appears in an statment context:~a" form)
      (vise-error "vise: expression appears in a toplevel context:~a" form))))

(define (ensure-stmt-ctx form ctx)
  (unless (stmt-ctx? ctx)
    (if (expr-ctx? ctx)
      (vise-error "vise: statment appears in an expression context:~a" form)
      (vise-error "vise: statment appears in a toplevel context:~a" form))))

(define (ensure-toplevel-ctx form ctx)
  (unless (toplevel-ctx? ctx)
    (vise-error "vise: form can only appear in toplevel:~a" form)))
(define (ensure-stmt-or-toplevel-ctx form ctx)
  (unless (or (toplevel-ctx? ctx) (stmt-ctx? ctx))
    (vise-error "vise: form can only appear in toplevel or statment context:~a" form)))

;;------------------------------------------------------------
;; Renderer
;;
(define-syntax define-vise-renderer
  (syntax-rules ()
    [(_ (op form ctx) req-ctx (:definition defs ...) body ...)
     (vise-register-renderer! 'op (cons 'req-ctx 
                                        (lambda (form ctx) 
                                          defs ... 
                                          (when (and (eq? ctx 'toplevel)
                                                  (not (eq? 'op 'eval-expression))
                                                  (not (eq? 'op 'begin))
                                                  (string=? (script-prefix) "b:"))
                                            (display "exec_statement "))
                                          body ...
                                          (when (and (eq? ctx 'toplevel)
                                                  (not (eq? 'op 'eval-expression))
                                                  (not (eq? 'op 'begin))
                                                  (string=? (script-prefix) "b:"))
                                            (print " endexec_statement")))))]
    [(_ (op form ctx) req-ctx body ...)
     (define-vise-renderer (op form ctx) req-ctx (:definition) body ...)]))

;;------------------------------------------------------------
;; Syntax
;;

(define (render-literal exp ctx)
  (ensure-expr-ctx exp ctx)
  (display
    (cond
      [(list? exp)
       (string-append
         "["
         (string-join 
           (map (pa$ vise-render-to-string 'expr) exp) 
           ",")
         "]")]
      [(regexp? exp) (string-append "'" (regexp->string exp) "'")]
      [(string? exp) (write-to-string exp)]
      [(boolean? exp) (if exp 1 0)]
      [else exp])))

(define-vise-renderer (quote form ctx) expr
  (render-literal (cadr form) ctx))

(define-vise-renderer (defun form ctx) stmt
  (:definition
    (define (gen-args args)
      ($ (cut string-join <> ",")
        $ map (lambda (sym) 
                (let* ([d (env-find-data sym)]
                       [sym (remove-symbol-prefix (get-vim-name d))])
                  (if (has-attr? d 'rest)
                    "..."
                    (vise-render-identifier sym))))
        $ filter vsymbol? 
        args))
    (define (render-body body)
      (add-indent (vise-render 'stmt `(begin ,@body)))
      (add-new-line))
    (define (gen-vfn name args modify body)
      (display "function! ")
      (display (vim-symbol name))
      (display "(")
      (display (gen-args args))
      (display ")")
      (unless (eq? modify :normal)
        (display modify))
      (newline)
      (render-body body)
      (print "endfunction")))
  (ensure-toplevel-ctx form ctx)
  (match form
    [(_ name (args ...) modify . body) (gen-vfn name args modify body)]))

(define (find-symbol-recursion action arg exp)
  (define (find exp arg)
    (cond
      [(vsymbol? exp)
       (action exp arg)]
      [(pair? exp)
       (case (get-symbol (car exp))
         ((quote) arg)
         ((lambda) (fold find arg (cddr exp)))
         ((if)
          (if (null? (cdddr exp))
            ($ find (cadr exp) ;test
              $ find (caddr exp)  ;then
              arg)
            ($ find (cadr exp) ;test
              $ find (caddr exp) ;then
              $ find (cadddr exp) ;else
              arg))) ;else
         ((set!)
          ($ find (cadr exp)
            $ find (caddr exp)
            arg))
         ((while begin and or)
          (fold find arg (cdr exp)))
         ((quasiquote) ) ;;TODO
         (else 
           (fold find arg exp)))]
      [else arg]))
  (find exp arg))

(define (find-self-recursion sym init)
  (if (and (list? init) (eq? 'lambda (vexp (car init))))
    (if (not (vsymbol? sym))
      (vise-error "Not allow distribute binding:~a ~a" sym init)
      (let1 self-data (env-find-data sym)
        (slot-set! (car init) 'prop
                   (acons 'self-data self-data (slot-ref (car init) 'prop)))
        (if-let1 free-vars (find-free init self-data)
          (when (null? free-vars)
            (attr-push! self-data 'lambda-no-free-vars)))
        (rlet1 self-rec (find-symbol-recursion
                           (lambda (exp vars)
                             (receive (d num-outside) (env-find-data-with-outside-lambda-count (@ exp.env) exp)
                               (if (and (not (zero? num-outside))
                                     d (has-attr? d 'free) 
                                     (eq? self-data d))
                                 (begin
                                   (if (= 1 num-outside)
                                     (attr-push! exp 'self-recursion))
                                   (set-cons vars exp))
                                 vars)))
                           '() 
                           init)
          (unless (null? self-rec)
            ;;mark self-recursion to lambda symbol
            (attr-push! (car init) 'self-recursion))
          self-rec)))
    '()))

(define (find-free&self-rec form self-data :optional (found-action identity))
  (if (eq? 'lambda (vexp (car form)))
    (fold
      (lambda (exp free&self-rec)
        (find-symbol-recursion
          (lambda (exp free&self-rec)
            (receive (d outside?) (env-find-data-with-outside-lambda? (@ exp.env) exp)
              (if (and outside?
                    d (has-attr? d 'free))
                (if (eq? self-data d)
                  (cons (car free&self-rec)
                        (set-cons (cdr free&self-rec) exp))
                  (cons (set-cons (car free&self-rec) exp)
                        (cdr free&self-rec)))
                free&self-rec)))
          free&self-rec exp))
      (cons '() '())
      (cddr form))
    #f))

(define (find-free form self-data :optional (found-action identity))
  (let1 free&self-rec (find-free&self-rec form self-data found-action)
    (append (car free&self-rec) (cdr free&self-rec))))

(define (render-symbol-bind sym init :optional (for-rendering? #f))
  ;;boxing
  (for-each
    (lambda (sym)
      (when (and (vsymbol? sym) (boxing? sym))
        (display "let ")
        (display (vim-symbol sym))
        (print " = [0]")))
    (if (list? sym) sym (list sym)))
  ;;cmd
  (display (if for-rendering?  "for " "let "))
  ;;symbols
  (if (list? sym)
    (begin
      (display "[")
      (let loop ((sym sym))
        (unless (null? sym)
          (display (if (vsymbol? (car sym))
                     (vim-ref-symbol (car sym))
                     ";"))
          (when (not (or (null? (cdr sym)) (eq? (cadr sym) :rest) (eq? (car sym) :rest)))
            (display ","))
          (loop (cdr sym))))
      (display "]"))
    (display (vim-ref-symbol sym)))

  ;;binding expr
  (let1 self-rec (find-self-recursion sym init)
    (display (if for-rendering?  " in " " = "))
    (vise-render 'expr init)
    (display (if for-rendering?  "" ""))
    (add-new-line)
    (unless (null? self-rec)
      (print #`"let ,(vim-symbol sym)[',(vim-symbol sym)'] = ,(vim-symbol sym)"))))

(define-vise-renderer (defvar form ctx) stmt
  (ensure-stmt-or-toplevel-ctx form ctx)
  (render-symbol-bind (cadr form) (caddr form)))

(define-vise-renderer (lambda form ctx) expr
  (ensure-expr-ctx form ctx)
  (let* ([func-name (symbol->string (gensym (string-append (script-prefix) "display")))]
         [free&self-rec (find-free&self-rec form (assq-ref (slot-ref (car form) 'prop) 'self-data #f))]
         [no-free-var? (and (null? (car free&self-rec)) (null? (cdr free&self-rec)))])
    (add-auto-generate-exp 
      func-name
      `(defun ,func-name ,(cadr form) ,(if no-free-var? :normal :dict) ,@(cddr form)))
    (if no-free-var?
      (begin
        (attr-push! (car form) 'lambda-no-free-vars)
        (display (vim-function-ref func-name)))
      (begin
        (display "{'func':")
        (display (vim-function-ref func-name))
        (display
          (string-join
            (map
              (lambda (var) #`"',(vim-symbol var)':,(vim-ref-symbol var #f (slot-ref (car form) 'env))")
              (car free&self-rec))
            "," 'prefix))
        (display "}")))))

(define-vise-renderer (let form ctx) stmt
  (:definition
    (define (all-ref-only? vars)
      (every
        (lambda (var) (env-data-ref-only? (env-find-data var)))
        vars)))

  (if (expr-ctx? ctx)
    (let1 func-name (gensym (string-append (script-prefix) "let_func"))
      ;;check distribute binding
      (when (any (.$ list? car) (cadr form))
        (vise-error "Expression context let, distribute binding not allow:~a" form))
      (for-each
        (lambda (vars) (find-self-recursion (car vars) (cadr vars)))
        (cadr form))
      (add-auto-generate-exp
        func-name
        (add-return-defun
          ;;constract auto generate function
          `(,(make <vsymbol> :exp 'defun :env (slot-ref (car form) 'env))
             ,func-name
             ,(map 
                (lambda (vars)
                  ;;chage scope local -> arg
                  (slot-set! (env-find-data (car vars)) 'scope 'arg)
                  (car vars))
                (cadr form))
             :normal
             ,@(if (all-ref-only? (map car (cadr form)))
                 (cddr form)
                 ;;surrounded by let
                 (list (surround-let (slot-ref (car form) 'prop) 
                                     (map car (cadr form)) ;args
                                     (cddr form))))))) ;body
      ;;call auto generate function
      (vise-render 'expr `(,func-name ,@(map cadr (cadr form)))))
    ;;render stetment context let
    (begin
      ;;render vars declare
      (for-each
        (lambda (vars)
          (render-symbol-bind (car vars) (cadr vars)))
        (cadr form))
      ;;render body
      (vise-render 'stmt `(begin ,@(cddr form))))))


(define-vise-renderer (begin form ctx) stmt
  (for-each
    (lambda (exp)
      (vise-render ctx exp)
      (unless (eq? ctx 'expr)
        (add-new-line)))
    (cdr form)))

(define-vise-renderer (if form ctx) stmt
  (if (expr-ctx? ctx)
    (match form
      [(_ test then else)
       (display "((")
       (vise-render 'expr test)
       (display ")?")
       (vise-render-expr then)
       (display " : ")
       (vise-render-expr else)
       (display ")")]
      [_ (vise-error "Expression context if, else clause require:~a" form)])
    (match form
      [(_ test then)
       (display "if ")
       (vise-render 'expr test)
       (add-new-line)
       (add-indent (vise-render ctx then))
       (add-new-line)
       (print "endif")]
      [(_ test then else)
       (display "if ")
       (vise-render 'expr test)
       (add-new-line)
       (add-indent (vise-render ctx then))
       (add-new-line)
       (if (and (list? else) (eq? (vexp (car else)) 'if))
         (begin
           (display "else")
           (vise-render ctx else))
         (begin
           (print "else")
           (add-indent (vise-render ctx else))
           (add-new-line)
           (print "endif")))])))

(define-vise-renderer (try form ctx) stmt
  (ensure-stmt-or-toplevel-ctx form ctx)
  (print "try")
  (add-indent (vise-render 'stmt (cadr form)))
  (add-new-line)
  (for-each
    (lambda (clause)
      (let1 pat (car clause)
        (cond
          [(string? pat)
           (display "catch ")
           (display (string-append "/" (write-to-string pat display) "/"))]
          [(regexp? pat)
           (display "catch ")
           (display (string-append "/" (regexp->string pat) "/"))]
          [(eq? (vexp pat) 'else)
           (display "catch")]
          [else
            (display "finally")]))
      (add-new-line)
      (add-indent
        (vise-render 'stmt `(begin ,@(cdr clause)))))
    (cddr form))
  (add-new-line)
  (print "endtry"))

(define-vise-renderer (dolist form ctx) stmt
  (ensure-stmt-or-toplevel-ctx form ctx)
  (render-symbol-bind (caadr form) (cadadr form) #t)
  (add-indent
    (vise-render 'stmt `(begin ,@(cddr form))))
  (add-new-line)
  (print "endfor"))

(define-vise-renderer (while form ctx) stmt
  (ensure-stmt-or-toplevel-ctx form ctx)
  (match form
    [(_ test . body)
     (display "while ")
     (vise-render 'expr test)
     (add-new-line)
     (add-indent
       (vise-render 'stmt `(begin ,@body)))
     (add-new-line)
     (print "endwhile")]))

(define-vise-renderer (list-func form ctx) expr
  (:definition
    (define (string-right str index)
      (let1 i (- (string-length str) 1 index)
        (if (< i 0)
          #f
          (string-ref str i))))
    (define (get-func-name type)
      (let1 type (x->string type)
        (if (eq? (string-right type 0) #\!)
          (substring type 0 (- (string-length type) 1))
          type)))
    )

  ;(ensure-expr-ctx form ctx)
  (when (or (stmt-ctx? ctx) (toplevel-ctx? ctx))
    (display "call "))
  (display (get-func-name (cadr form)))
  (display "(")
  (let1 need-copy? (not (eq? (string-right (x->string (cadr form)) 0) #\!))
    (when need-copy?
      (display "copy("))
    (vise-render 'expr (caddr form))
    (when need-copy?
      (display ")")))
  (display ",\'")
  (if (eq? 'lambda (vexp (car (cadddr form))))
    (let* ([lambda-form (cadddr form)]
           [lambda-env (slot-ref (caadr lambda-form) 'env)]
           [free (find-free
                   lambda-form #f
                   (lambda (exp) 
                     (rlet1 sym (make <vsymbol> :exp (@ exp.exp) :env lambda-env)
                       (env-add-symbol lambda-env sym 'arg))))]
           [func-name (gensym (string-append (script-prefix) "list_func"))])
      (add-auto-generate-exp
        func-name
        `(defun ,func-name 
                ,(append (cadr lambda-form) free) :normal
                ,@(cddr lambda-form)))
      (vise-render 'expr 
                   `(,func-name 
                      v:val
                      ,@(map
                          (lambda (o) 
                            (make <vsymbol> 
                                  :exp (vexp o) 
                                  :env (slot-ref (car form) 'env)))
                          free))))
    (vise-render 'expr (cadddr form)))
  (display "\')")
  (add-new-line))

(define-vise-renderer (return form ctx) stmt
  (ensure-stmt-ctx form ctx)
  (match form
    [(_ expr) 
     (display "return ")
     (vise-render 'expr expr)
     (add-new-line)]
    [(_)
     (print "return")]))

(define-vise-renderer (break form ctx) stmt
  (ensure-stmt-ctx form ctx)
  (match form
    [(_) (print "break")]))

(define-vise-renderer (continue form ctx) stmt
  (ensure-stmt-ctx form ctx)
  (match form
    [(_) (print "continue")]))

(define-vise-renderer (set! form ctx) stmt
  (ensure-stmt-or-toplevel-ctx form ctx)
  (match form
    [(_ var val)
     (display "let ")
     (vise-render 'expr var)
     (display " = ")
     (vise-render 'expr val)]
    [_   (vise-error "uneven args for set!:~a" form)]))

(define-vise-renderer (raw-vimscript form ctx) expr
  (when (eq? ctx 'stmt)
    (add-new-line))
  (let1 indent-level (get-indent-level)
    (set-indent-level! 0)
    (for-each
      (lambda (exp)
        (if (string? exp)
          (display exp)
          (vise-render-expr exp)))
      (cdr form))
    (set-indent-level! indent-level))
  (when (eq? ctx 'stmt)
    (add-new-line)))

(define-macro (define-vim-cmd op sop)
  `(begin
     (set! vim-cmd-list (cons (string->symbol ,sop) vim-cmd-list))
     (define-vise-renderer (,op form ctx) stmt
       (ensure-stmt-or-toplevel-ctx form ctx)
       (display ,sop)
       (display (string-join
                  (map
                    (pa$ vise-render-to-string 'expr)
                    (cdr form))
                  " " 'prefix))
       (add-new-line))))

(define-vim-cmd echo "echo")
(define-vim-cmd echon "echon")
(define-vim-cmd echohl "echohl")
(define-vim-cmd echomsg "echomsg")
(define-vim-cmd execute "execute")
(define-vim-cmd normal "normal")
(define-vim-cmd normal! "normal!")
(define-vim-cmd tag "tag")
(define-vim-cmd sleep "sleep")
(define-vim-cmd enew "enew")
(define-vim-cmd silent "silent")
(define-vim-cmd silent! "silent!")
(define-vim-cmd unlet "unlet")
(define-vim-cmd unlet! "unlet!")
(define-vim-cmd startinsert "startinsert")
(define-vim-cmd startinsert! "startinsert!")
(define-vim-cmd stopinsert "stopinsert")
(define-vim-cmd edit "edit")
(define-vim-cmd setlocal "setlocal")
(define-vim-cmd source "source")
(define-vim-cmd autocmd! "autocmd!")
(define-vim-cmd finish "finish")
(define-vim-cmd throw "throw")
;;TODO
;(define-vim-cmd keymap "key")

(set! vim-cmd-list (cons 'vim-cmd vim-cmd-list))
(define-vise-renderer (vim-cmd form ctx) stmt
  (ensure-stmt-or-toplevel-ctx form ctx)
  (vise-render 'expr (cadr form))
  (display (string-join
             (map
               (pa$ vise-render-to-string 'expr)
               (cddr form))
             " " 'prefix))
  (add-new-line))

(set! vim-cmd-list (cons 'eval-expression vim-cmd-list))
(define-vise-renderer (eval-expression form ctx) stmt
  (ensure-toplevel-ctx form ctx)
  (display "eval_expression ")
  (vise-render 'expr (cadr form))
  (display " endeval_expression")
  (add-new-line))

(define-vise-renderer (array form ctx) expr
  (ensure-expr-ctx form ctx)
  (display (string-append
             "["
             (string-join 
               (map (pa$ vise-render-to-string 'expr) (cdr form))
               ",")
             "]")))

(define-vise-renderer (dict form ctx) expr
  (ensure-expr-ctx form ctx)
  (display "{")
  (display (string-join 
             (map
               (lambda (item)
                 (string-append 
                   "'"
                   (x->string (vexp (car item)))
                   "' : "
                   (vise-render-to-string 'expr (cadr item))))
               (cdr form))
             ","))
  (display "}"))

(define-vise-renderer (augroup form ctx) stmt
  (ensure-stmt-or-toplevel-ctx form ctx)
  (display "augroup ")
  (let1 next (if (vsymbol? (cadr form))
               (begin
                 (display (cadr form))
                 (cddr form))
               (cdr form))
    (add-new-line)
    (add-indent
      (for-each
        (pa$ vise-render 'stmt)
        next))
    (add-new-line)
    (print "augroup END")))

(define-vise-renderer (autocmd form ctx) stmt
  (ensure-stmt-or-toplevel-ctx form ctx)
  (display "autocmd ")
  (unless (eq? 'default (vexp (cadr form)))
    (display (vexp (cadr form)))
    (display " "))
  (display
    (string-join
      (map (.$ symbol->string vexp) (caddr form))
      ","))
  (display " ")
  (display (cadddr form))
  (display " ")
  (unless (eq? :normal (vexp (car (cddddr form))))
    (display "nested "))
  (vise-render 'stmt (cadr (cddddr form)))
  (add-new-line))

(define-vise-renderer (setlocal form ctx) stmt
  (ensure-stmt-or-toplevel-ctx form ctx)
  (match form
    [(_ sym)
     (display "setlocal ")
     (vise-render-expr sym)]
    [(_ sym . val)
     (display "setlocal ")
     (vise-render-expr sym)
     (display "=")
     (display (string-join
                (map (cut vise-render-expr <> #t) val)
                " "))]))

;;------------------------------------------------------------
;; Operators
;;
;;
(define-macro (define-nary op sop)
  `(define-vise-renderer (,op form ctx) expr
     (ensure-expr-ctx form ctx)
     (match form
       [(_ a)
        (display ,sop)
        (vise-render-expr a)]
       [(_ a b)
        (vise-render-expr a)
        (display ,#`" ,sop ")
        (vise-render-expr b)]
       [(_ a b . x)
        (vise-render-expr a)
        (display ,#`" ,sop ")
        (vise-render-expr b)
        (display (string-join
                   (map (cut vise-render-expr <> #t) x)
                   ,#`" ,sop " 'prefix))])))

(define-macro (define-operation-ref op zero-argument-value)
  `(define-syntax-function-ref 
     ',op
     '(let1 len (len args)
        (cond
          [(== len 0) ,zero-argument-value]
          [(== len 1) (,op (ref args 0))]
          [else 
            (let ((acc (ref args 0)))
              (dolist [num (subseq args 1)]
                (set! acc (,op acc num))) 
              acc)]))))

(define-nary + "+")
(define-operation-ref + 0)

(define-nary - "-")
(define-operation-ref - (throw "- procedure requires at least 1 argument"))

(define-macro (define-nary-no-single op sop)
  `(define-vise-renderer (,op form ctx) expr
     (ensure-expr-ctx form ctx)
     (match form
       [(_ a)
        (vise-render-expr a)]
       [(_ a b)
        (vise-render-expr a)
        (display ,#`" ,sop ")
        (vise-render-expr b)]
       [(_ a b . x)
        (vise-render-expr a)
        (display ,#`" ,sop ")
        (vise-render-expr b)
        (display (string-join
                   (map (cut vise-render-expr <> #t) x)
                   ,#`" ,sop " 'prefix))])))
(define-nary-no-single * "*")
(define-operation-ref * 1)
(define-nary-no-single / "/")
(define-operation-ref / (throw "/ procedure requires at least 1 argument"))
(define-nary-no-single string-append ".")
(define-operation-ref string-append "")


(define-nary-no-single and "&&")
(define-operation-ref and #t)
(define-nary-no-single or  "||")
(define-operation-ref or #f)

(define-macro (define-set-nary op sop sop2)
  `(define-vise-renderer (,op form ctx) stmt
     (ensure-stmt-or-toplevel-ctx form ctx)
     (match form
       [(_ a b)
        (display "let ")
        (vise-render-expr a)
        (display ,#`" ,sop ")
        (vise-render-expr b)]
       [(_ a b . x)
        (display "let ")
        (vise-render-expr a)
        (display ,#`" ,sop ")
        (vise-render-expr b)
        (display (string-join
                   (map (cut vise-render-expr <> #t) x)
                   ,#`" ,sop2 " 'prefix))])))

(define-set-nary += "+=" "+")
(define-set-nary -= "-=" "-")
(define-set-nary .= ".=" ".")

(define-macro (define-unary op sop)
  `(define-vise-renderer (,op form ctx) expr
     (ensure-expr-ctx form ctx)
     (match form
       [(_ a)
        (display ,sop) 
        (vise-render-expr a)])))

(define-macro (define-one-arg-operation-ref op)
  `(define-syntax-function-ref 
     ',op
     '(if (== (len args) 1) 
        (,op (ref args 0))
        (throw ,(string-append (x->string op) " procedure requires at 1 argument")))))

(define-unary not    "!")
(define-one-arg-operation-ref not)
(define-unary lognot "~")
(define-one-arg-operation-ref lognot)
(define-unary &      "&")               ; only unary op
(define-one-arg-operation-ref not)

(define-macro (define-binary op sop)
  `(define-vise-renderer (,op form ctx) expr
     (ensure-expr-ctx form ctx)
     (match form
       [(_ a b)
        (vise-render-expr a)
        (display " ")
        (display ,sop)
        (display " ")
        (vise-render-expr b)])))

(define-macro (define-two-arg-operation-ref op)
  `(define-syntax-function-ref 
     ',op
     '(if (== (len args) 2) 
        (,op (ref args 0) (ref args 1))
        (throw ,(string-append (x->string op) " procedure requires at 2 argument")))))

(define-binary %       "%")
(define-two-arg-operation-ref %)

(define-macro (define-compare-operation-ref op)
  `(define-syntax-function-ref 
     ',op
     '(if (< (len args) 2) 
        (throw ,(string-append (x->string op) " procedure requires at least 1 argument"))
        (let1 org (ref args 0)
          (dolist [num (subseq args 1)]
            (if (,op org num)
              (set! org num)
              (return #f)))
          (return #t)))))

(define-macro (define-compare op sop)
  `(begin
     (define-binary ,op ,sop)
     (define-compare-operation-ref ,op)))
;;TODO multi argument
(define-compare <       "<")
(define-compare <=      "<=")
(define-compare >       ">")
(define-compare >=      ">=")
(define-compare ==      "==")
(define-compare !=      "!=")

(define-compare is "is")
(define-compare isnot "isnot")

(define-compare |==#| "==#")
(define-compare |!=#| "!=#")
(define-compare |>#| ">#")
(define-compare |>=#| ">=#")
(define-compare |<#| "<#")
(define-compare |<=#| "<=#")

(define-compare |==?| "==?")
(define-compare |!=?| "!=?")
(define-compare |>?| ">?")
(define-compare |>=?| ">=?")
(define-compare |<?| "<?")
(define-compare |<=?| "<=?")

(define-compare |=~| "=~")
(define-compare |=~#| "=~#")
(define-compare |=~?| "=~?")
(define-compare |!~| "!~")
(define-compare |!~#| "!~#")
(define-compare |!~?| "!~?")

(define-vise-renderer (ref form ctx) expr
  (:definition
    (define (render-index i) 
      (display "[")
      (let1 i (if (and (list? i) (eq? 'quote (vexp (car i))))
                (x->string (cadr i))
                i)
        (vise-render 'expr i))
      (display "]")))
  (ensure-expr-ctx form ctx)
  (match form
    [(_ a i1 . i2)
     (vise-render 'expr a)
     (render-index i1)
     (for-each render-index i2)]))
(define-syntax-function-ref 
  'ref
  '(if (< (len args) 2) 
    (throw ,(string-append (x->string op) " procedure requires at least 1 argument"))
    (let1 tmp1 (ref args 0)
      (dolist [refer (subseq args 1)]
        (let1 tmp2 (ref tmp1 refer)
          (unlet tmp1)
          (set! tmp1 tmp2)
          (unlet tmp2)))
      (return tmp1))))

(define-vise-renderer (subseq form ctx) expr
  (ensure-expr-ctx form ctx)
  (match form
    [(_ seq)
     (vise-render 'expr seq)
     (display "[:]")]
    [(_ seq s)
     (vise-render 'expr seq)
     (display "[")
     (vise-render 'expr s)
     (display ":]")]
    [(_ seq s e)
     (vise-render 'expr seq)
     (display "[")
     (vise-render 'expr s)
     (display ":")
     (vise-render 'expr e)
     (display "]")]))
(define-syntax-function-ref 
  'subseq
  '(let1 len (len args)
     (cond
       [(== len 1)
        (subseq (ref args 0))]
       [(== len 2)
        (subseq (ref args 0) (ref args 1))]
       [(== len 3)
        (subseq (ref args 0) (ref args 1) (ref args 2))]
       [else
        (throw "subseq procedure requires at 1 to 3 argument")])))

(define (**-str form delimiter converter)
  (match form
    [(_ str)
     (display delimiter)
     (unless (string? str)
       (vise-error "string literal require, bug got ~a. ~a" str form))
     (display (converter str))
     (display delimiter)]))

(define-vise-renderer (qq-str form ctx) expr
  (ensure-expr-ctx form ctx)
  (**-str form "`" (lambda (str)
                     (let1 str (write-to-string str display)
                       substring str 1 (- (string-length str) 1))) ))

(define-vise-renderer (qq= form ctx) expr
  (ensure-expr-ctx form ctx)
  (match form
    [(_ exp)
     (display "`=")
     (vise-render-expr exp)
     (display "`")]))

(define-vise-renderer (sq-str form ctx) expr
  (ensure-expr-ctx form ctx)
  (**-str form "'" identity))

(define-vise-renderer (key-str form ctx) expr
  (ensure-expr-ctx form ctx)
  (**-str form "\"" identity))

;;
;;
;;Util

(define (funcall? exp)
  (if (list? exp)
    (if (or* eq? (vexp (car exp)) 'quote 'ref 'string-append)
      #f
      #t)
    #f))

(define (vise-render-expr exp :optional (to-string? #f))
  (let ([port (make-vise-output-port (open-output-string))]
        [funcall? (funcall? exp)])
    (with-output-to-port
      port
      (lambda () 
        (when funcall? (display "("))
        (vise-render 'expr exp)
        (when funcall? (display ")"))))
    (if to-string?
      (get-output-string (@ port.raw))
      (display (get-output-string (@ port.raw))))))

(define (vise-render-identifier sym)
  (vise-safe-name-friendly (x->string sym)))

(define (vise-safe-name-friendly str)
  (receive (prefix str) (cond
                          [(or* string=? (substring* str 0 2) "@@" "g:" "s:" "v:" "b:" "w:" "l:" "a:")
                           (values (substring* str 0 2) (substring str 2 (string-length str)))]
                          [(or* eq? (string-ref str 0) #\& #\@)
                           (values (x->string (string-ref str 0)) (substring str 1 (string-length str)))]
                          [else (values "" str)])
    (string-append
      prefix
      (with-string-io str
                      (^[] (let loop ([c (read-char)])
                             (unless (eof-object? c)
                               (case c
                                 [(#\-) (let1 d (read-char)
                                          (cond [(eqv? d #\>) (display "_TO") (loop (read-char))]
                                            [else         (display #\_) (loop d)]))]
                                 [(#\?) (display #\P) (loop (read-char))]
                                 [(#\!) (display #\X) (loop (read-char))]
                                 [(#\<) (display "_LT") (loop (read-char))]
                                 [(#\>) (display "_GT") (loop (read-char))]
                                 [(#\* #\> #\@ #\$ #\% #\^ #\& #\* #\+ #\: #\= #\. #\/ #\~)
                                  (display #\_)
                                  (display (number->string (char->integer c) 16))
                                  (loop (read-char))]
                                 [else (display c) (loop (read-char))]))))))))

