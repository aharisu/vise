
(define auto-generate-exp (make-parameter '()))

(define (add-auto-generate-exp sym exp)
  (let1 l (auto-generate-exp)
    (unless (assoc sym l)
      (auto-generate-exp (acons sym exp l)))))

(define (vise-phase-render exp)
  (let loop ((exp exp)
             (acc '()))
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
    (if-let1 d (and (vsymbol? symbol)
                 (env-find-data (@ symbol.env) symbol))
      (get-vim-name d)
      (x->string symbol))))

(define (vim-unboxing vim-symbol)
  (string-append vim-symbol "[0]"))

(define (boxing? symbol)
  (let1 d (and (vsymbol? symbol) (env-find-data (@ symbol.env) symbol))
    (and d 
      (eq? (@ d.scope) 'local)
      (has-attr? d 'free)
      (not (or (has-attr? d 'ref-only) 
             (has-attr? d 'not-use))))))

(define (vim-ref-symbol symbol)
  (let1 sym (vim-symbol symbol)
    (receive (d outside?) (if (vsymbol? symbol)
                            (env-find-data-with-outside-lambda? (@ symbol.env) symbol)
                            (values #f #f))
      (let1 sym (if (and d outside? (eq? (@ d.scope) 'local))
                  (string-append "self['" sym "']")
                  sym)
        (if (boxing? symbol) 
          (vim-unboxing sym)
          sym)))))

(define (render-func-call ctx form)
  (when (or (stmt-ctx? ctx) (toplevel-ctx? ctx))
    (display "call "))
  (display 
    (let ((params #`"(,(string-join (map (pa$ vise-render-to-string 'expr) (cdr form)) \",\"))")
          (sym (vim-ref-symbol (car form)))
          (d (and (vsymbol? (car form))
               (env-find-data (slot-ref (car form) 'env) (car form)))))
      (cond
        [(or (not d) (eq? (@ d.scope) 'syntax) (has-attr? d 'function))
         (string-append sym params)]
        [(has-attr? d 'lambda) (string-append sym ".func" params)]
        [else #`"(type(,sym)==s:dict_type) ? ,|sym|.func,|params| : ,|sym|,|params|"])))
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
      (vise-error "vise: statment appears in an statment context:~a" form)
      (vise-error "vise: statment appears in a toplevel context:~a" form))))

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
(define-constant renderer-table (make-hash-table 'eq?))

(define (vise-register-renderer! name renderer)
  (hash-table-put! renderer-table name renderer))

(define (vise-lookup-renderer sym)
  (cond 
    [(vsymbol? sym)
     (if-let1 d (env-find-data (@ sym.env) sym)
       (and (eq? (@ d.scope) 'syntax) (@ d.exp))
       #f)]
    [(symbol? sym) (hash-table-get renderer-table sym #f)]
    [else #f]))

(define-syntax define-vise-renderer
  (syntax-rules ()
    [(_ (op form ctx) . body)
     (vise-register-renderer! 'op (lambda (form ctx) . body))]
    [(_ op op2); alias
                 (vise-register-renderer! 'op (or (vise-lookup-macro 'op2)
                                                (vise-error "unknown vise renderer:~a" 'op2)))]))

;;------------------------------------------------------------
;; Syntax
;;

(define (render-literal exp ctx)
  (if (and (or (stmt-ctx? ctx) (toplevel-ctx? ctx)) (string? exp))
    (print exp)
    (begin
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
          [(string? exp) 
           (string-append
             "\""
             (regexp-replace-all #/\"/ exp "\\\\\"")
             "\"")]
          [(boolean? exp) (if exp 1 0)]
          [else exp])))))

(define-vise-renderer (quote form ctx)
  (render-literal (cadr form) ctx))

(define-vise-renderer (defun form ctx)
  (define (gen-args args)
    ($ (cut string-join <> ",")
      $ map (lambda (sym) 
              (let* ([d (env-find-data (@ sym.env) sym)]
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
    (print "endfunction"))

  (ensure-toplevel-ctx form ctx)
  (match form
    [(_ name (args ...) modify . body) (gen-vfn name args modify body)]))

(define (find-symbol-recursion action arg exp)
  (define (find exp arg)
    (cond
      [(vsymbol? exp)
       (action exp arg)]
      [(list? exp)
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

(define (render-symbol-bind sym init :optional (for-rendering? #f))
  (define (find-self-recursion sym init)
    (if (and (list? init) (eq? 'lambda (vexp (car init))))
      (begin (when (not (vsymbol? sym))
               (vise-error "Not allow distribute binding:~a ~a" sym init))
        (let1 self-data (env-find-data (@ sym.env) sym)
          (find-symbol-recursion
            (lambda (exp vars)
              (receive (d outside?) (env-find-data-with-outside-lambda? (@ exp.env) exp)
                (if (and d (has-attr? d 'free) outside? (eq? self-data d))
                  (begin
                    (attr-push! exp 'self-recursion) 
                    (set-cons vars exp))
                  vars)))
            '() init)))
      '()))

  ;;boxing
  (for-each
    (lambda (sym)
      (when (and (vsymbol? sym) (boxing? sym))
        (display "let ")
        (display (vim-symbol sym))
        (print " = [0]")))
    (if (vsymbol? sym) (list sym) sym))
  (display (if for-rendering?  "for " "let "))
  (if (vsymbol? sym)
    (display (vim-symbol sym))
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
      (display "]")))
  ;;find and mark 
  (let1 self-rec (find-self-recursion sym init)
    (display (if for-rendering?  " in " " =("))
    (vise-render 'expr init)
    (display (if for-rendering?  "" ")"))
    (add-new-line)
    (unless (null? self-rec)
      (print #`"let ,(vim-symbol sym)[',(vim-symbol sym)'] = ,(vim-symbol sym)"))))

(define-vise-renderer (defvar form ctx)
  (ensure-stmt-or-toplevel-ctx form ctx)
  (render-symbol-bind (cadr form) (caddr form)))

(define-vise-renderer (lambda form ctx)
  (define (find-free exp vars)
    (find-symbol-recursion
      (lambda (exp vars)
        (receive (d outside?) (env-find-data-with-outside-lambda? (@ exp.env) exp)
          (if (and (not (has-attr? exp 'self-recursion)) 
                d (has-attr? d 'free) outside?)
            (set-cons vars exp)
            vars)))
      vars exp))

  (ensure-expr-ctx form ctx)
  (let1 func-name (symbol->string (gensym "s:display"))
    (add-auto-generate-exp 
      func-name
      `(defun ,func-name ,(cadr form) :dict ,@(cddr form)))
    (display "{'func':function('")
    (display func-name)
    (display "')")
    (display
      (string-join
        (map
          (lambda (var) #`"',(vim-symbol var)':,(vim-symbol var)")
          (fold 
            find-free
            '()
            (cddr form)))
        "," 'prefix))
    (display "}")))

(define-vise-renderer (let form ctx)
  (define (all-ref-only? vars)
    (every
      (lambda (var) (has-attr? (env-find-data (@ var.env) var) 'ref-only))
      vars))

  (if (expr-ctx? ctx)
    (let1 func-name (symbol->string (gensym "s:let_func"))
      ;;check distribute binding
      (when (any (.$ list? car) (cadr form))
        (vise-error "Expression context let, distribute binding not allow:~a" form))
      (add-auto-generate-exp
        func-name
        ;;constract auto generate function
        `(defun ,func-name 
                ,(map 
                   (lambda (vars)
                     ;;chage scope local -> arg
                     (slot-set! (env-find-data (slot-ref (car vars) 'env) (car vars)) 'scope 'arg)
                     (car vars))
                   (cadr form))
                :normal
                ,@(if (all-ref-only? (map car (cadr form)))
                    (cddr form)
                    ;;surrounded by let
                    (let ([body-env (assq-ref (slot-ref (car form) 'prop) 'body-env)]
                          [injection-env (assq-ref (slot-ref (car form) 'prop) 'injection-env)])
                      `((,(make <vsymbol> :exp 'let :env injection-env)
                          ,(map
                             (lambda (var)
                               (list
                                 (rlet1 sym (make <vsymbol> :exp var :env injection-env)
                                   (env-add-symbol injection-env sym 'local))
                                 (make <vsymbol> :exp var :env body-env)))
                             (map (.$ vexp car) (cadr form)))
                          ;;original body
                          ,@(cddr form)))))))
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


(define-vise-renderer (begin form ctx)
  (ensure-stmt-or-toplevel-ctx form ctx)
  (for-each
    (lambda (exp)
      (vise-render ctx exp)
      (add-new-line))
    (cdr form)))

(define-vise-renderer (if form ctx)
  (if (expr-ctx? ctx)
    (match form
      [(?: test then else)
       (display "((")
       (vise-render 'expr test)
       (display ")?(")
       (vise-render 'expr then)
       (display "):(")
       (vise-render 'expr else)
       (display "))")]
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

(define-vise-renderer (try form ctx)
  (ensure-stmt-or-toplevel-ctx form ctx)
  (print "try")
  (add-indent (vise-render ctx (cadr form)))
  (add-new-line)
  (for-each
    (lambda (clause)
      (if (string? (car clause))
        (begin
          (display "catch ")
          (vise-render 'expr (car clause)))
        (if (eq? (vexp (car clause)) 'else)
          (display "catch")
          (display "finally")))
      (add-new-line)
      (add-indent
        (vise-render 'stmt `(begin ,@(cdr clause)))))
    (cddr form))
  (add-new-line)
  (print "endtry"))

(define-vise-renderer (dolist form ctx)
  (ensure-stmt-or-toplevel-ctx form ctx)
  (render-symbol-bind (caadr form) (cadadr form) #t)
  (add-indent
    (vise-render 'stmt `(begin ,@(cddr form))))
  (add-new-line)
  (print "endfor"))

(define-vise-renderer (while form ctx)
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

(define-vise-renderer (list-func form ctx)
  (define (find-free found-action exp :optional (vars '()))
    (find-symbol-recursion 
      (lambda (exp vars)
        (receive (d outside?) (env-find-data-with-outside-lambda? (@ exp.env) exp)
          (if (and d (has-attr? d 'free) outside?)
            (set-cons vars (found-action exp))
            vars)))
      vars exp))

  ;(ensure-expr-ctx form ctx)
  (when (or (stmt-ctx? ctx) (toplevel-ctx? ctx))
    (display "call "))
  (display (cadr form))
  (display "(")
  (vise-render 'expr (caddr form))
  (display ",\"")
  (if (eq? 'lambda (vexp (car (cadddr form))))
    (let* ([lambda-form (cadddr form)]
           [lambda-env (slot-ref (caadr lambda-form) 'env)]
           [free (find-free
                   (lambda (exp) 
                     (rlet1 sym (make <vsymbol> :exp (@ exp.exp) :env lambda-env)
                       (env-add-symbol lambda-env sym 'arg)))
                   (cddr lambda-form))]
           [func-name (symbol->string (gensym "s:list_func"))])
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
  (display "\")")
  (add-new-line))

(define-vise-renderer (return form ctx)
  (ensure-stmt-ctx form ctx)
  (match form
    [(_ expr) 
     (display "return ")
     (vise-render 'expr expr)
     (add-new-line)]
    [(_)
     (print "return")]))

(define-vise-renderer (break form ctx)
  (ensure-stmt-ctx form ctx)
  (match form
    [(_) (print "break")]))

(define-vise-renderer (continue form ctx)
  (ensure-stmt-ctx form ctx)
  (match form
    [(_) (print "continue")]))

(define-vise-renderer (set! form ctx)
  (ensure-stmt-or-toplevel-ctx form ctx)
  (match form
    [(_ var val)
     (display "let ")
     (vise-render 'expr var)
     (display "=(")
     (vise-render 'expr val)
     (display ")")]
    [_   (vise-error "uneven args for set!:~a" form)]))

(define vim-cmd-list '())
(define-macro (define-vim-cmd op sop)
  `(begin
     (set! vim-cmd-list (cons (string->symbol ,sop) vim-cmd-list))
     (define-vise-renderer (,op form ctx)
       (ensure-stmt-or-toplevel-ctx form ctx)
       (display ,sop)
       (display (string-join
                  (map
                    (pa$ vise-render-to-string 'expr)
                    (cdr form))
                  " " 'prefix))
       (add-new-line))))

(define-vim-cmd call "call")
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
;;TODO
;(define-vim-cmd keymap "key")

(set! vim-cmd-list (cons 'vim-cmd vim-cmd-list))
(define-vise-renderer (vim-cmd form ctx)
  (ensure-stmt-or-toplevel-ctx form ctx)
  (vise-render 'expr (cadr form))
  (display (string-join
             (map
               (pa$ vise-render-to-string 'expr)
               (cddr form))
             " " 'prefix))
  (add-new-line))

(define-vise-renderer (dict form ctx)
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

(define-vise-renderer (augroup form ctx)
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

(define-vise-renderer (autocmd form ctx)
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

;;------------------------------------------------------------
;; Operators
;;
;;
(define-macro (define-nary op sop)
  `(define-vise-renderer (,op form ctx)
     (ensure-expr-ctx form ctx)
     (match form
       [(_ a)
        (display ,sop)
        (display "(")
        (vise-render 'expr a)
        (display ")")]
       [(_ a b)
        (display "(")
        (vise-render 'expr a)
        (display ")")
        (display ,sop)
        (display "(")
        (vise-render 'expr b)
        (display ")")]
       [(_ a b . x)
        (list* ',op (list ',op a b) x)])))

(define-nary + "+")
(define-nary - "-")
(define-nary * "*")
(define-nary / "/")
(define-nary string-append ".")


(define-nary and "&&")
(define-nary or  "||")

(define-macro (define-unary op sop)
  `(define-vise-renderer (,op form ctx)
     (ensure-expr-ctx form ctx)
     (match form
       [(_ a)
        (display ,sop) 
        (display "(")
        (vise-render 'expr a)
        (display ")")])))

(define-unary not    "!")
(define-unary lognot "~")
(define-unary &      "&")               ; only unary op

(define-macro (define-binary op sop)
  `(define-vise-renderer (,op form ctx)
     (ensure-expr-ctx form ctx)
     (match form
       [(_ a b)
        (display "(")
        (vise-render 'expr a)
        (display ")")
        (display ,sop)
        (display "(")
        (vise-render 'expr b)
        (display ")")])))

(define-binary %       "%")
(define-binary <       "<")
(define-binary <=      "<=")
(define-binary >       ">")
(define-binary >=      ">=")
(define-binary ==      "==")
(define-binary !=      "!=")

;;TODO
(define-binary +=      "+=")
(define-binary -=      "-=")

(define-binary is "is")
(define-binary isnot "isnot")

(define-binary |==#| "==#")
(define-binary |!=#| "!=#")
(define-binary |>#| ">#")
(define-binary |>=#| ">=#")
(define-binary |<#| "<#")
(define-binary |<=#| "<=#")

(define-binary |==?| "==?")
(define-binary |!=?| "!=?")
(define-binary |>?| ">?")
(define-binary |>=?| ">=?")
(define-binary |<?| "<?")
(define-binary |<=?| "<=?")

(define-binary |=~| "=~")
(define-binary |=~#| "=~#")
(define-binary |=~?| "=~?")
(define-binary |!~| "!~")
(define-binary |!~#| "!~#")
(define-binary |!~?| "!~?")

(define-vise-renderer (ref form ctx)
  (define (render-index i) 
    (display "[")
    (let1 i (if (and (list? i) (eq? 'quote (vexp (car i))))
              (x->string (cadr i))
              i)
      (vise-render 'expr i))
    (display "]"))
  (ensure-expr-ctx form ctx)
  (match form
    [(_ a i1 . i2)
     (vise-render 'expr a)
     (render-index i1)
     (for-each render-index i2)]))

(define-vise-renderer (subseq form ctx)
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


;;
;;
;;Util

(define (vise-render-identifier sym)
  (vise-safe-name-friendly (x->string sym)))

(define (vise-safe-name-friendly str)
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
                             [(#\* #\> #\@ #\$ #\% #\^ #\& #\* #\+ #\= #\. #\/ #\~)
                              (display #\_)
                              (display (number->string (char->integer c) 16))
                              (loop (read-char))]
                             [else (display c) (loop (read-char))]
                             ))))))

