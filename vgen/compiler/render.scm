
(define auto-generate-exp (make-parameter '()))

(define (add-auto-generate-exp sym exp)
  (let1 l (auto-generate-exp)
    (unless (assoc sym l)
      (auto-generate-exp (acons sym exp l)))))

(define (vise-phase-render out-port exp)
  (let loop ((exp exp))
    (parameterize ([auto-generate-exp '()])
      (for-each
        (cut vise-render 'toplevel <> out-port)
        exp)
      (unless (null? (auto-generate-exp))
        (loop (map cdr (auto-generate-exp)))))))

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
        [else ;litera
          (display exp)]))))

(define (vim-symbol symbol)
  (vise-render-identifier
    (if-let1 d (and (vsymbol? symbol)
                 (env-find-data (@ symbol.env) symbol))
      (get-vim-name d)
      (x->string symbol))))

(define (vim-boxing vim-symbol)
  (string-append "[" vim-symbol "]"))

(define (vim-unboxing vim-symbol)
  (string-append vim-symbol "[0]"))

(define (boxing? symbol)
  (let1 d (and (vsymbol? symbol) (env-find-data (@ symbol.env) symbol))
    (and d 
      (eq? (@ d.scope) 'local)
      (env-data-has-attr? d 'free)
      (not (or (env-data-has-attr? d 'ref-only) 
             (env-data-has-attr? d 'not-use))))))

(define (vim-ref-symbol symbol)
  (let1 sym (vim-symbol symbol)
    (if (boxing? symbol) 
      (vim-unboxing sym)
      sym)))

(define (render-func-call ctx form)
  (when (or (stmt-ctx? ctx) (toplevel-ctx? ctx))
    (display "call "))
  (display 
    (let ((params #`"(,(string-join (map (pa$ vise-render-to-string 'expr) (cdr form)) \",\"))")
          (sym (vim-symbol (car form)))
          (d (env-find-data (slot-ref (car form) 'env) (car form))))
      (cond
        ((or (not d) (env-data-has-attr? d 'function)) (string-append sym params))
        ((env-data-has-attr? d 'lambda) (string-append sym ".func" params))
        (else #`"(type(,sym)==s:dict_type) ? ,|sym|.func,|params| : ,|sym|,|params|"))))
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
      (error "vise: statment appears in an statment context:" form)
      (error "vise: statment appears in a toplevel context:" form))))

(define (ensure-stmt-ctx form ctx)
  (unless (stmt-ctx? ctx)
    (if (expr-ctx? ctx)
      (error "vise: statment appears in an expression context:" form)
      (error "vise: statment appears in a toplevel context:" form))))

(define (ensure-toplevel-ctx form ctx)
  (unless (toplevel-ctx? ctx)
    (error "vise: form can only appear in toplevel:" form)))
(define (ensure-stmt-or-toplevel-ctx form ctx)
  (unless (or (toplevel-ctx? ctx) (stmt-ctx? ctx))
    (error "vise: form can only appear in toplevel or statment context:" form)))

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
                                                (error "unknown vise renderer:" 'op2)))]))

;;------------------------------------------------------------
;; Syntax
;;

(define-vise-renderer (defun form ctx)
  (define (gen-args args)
    ($ (cut string-join <> ",")
      $ map vise-render-identifier 
      $ map (lambda (sym) 
              (remove-symbol-prefix (get-vim-name (env-find-data (@ sym.env) sym))))
      args))
  (define (gen-vfn name args modify body)
    (display "function! ")
    (display (vim-symbol name))
    (display "(")
    (display (gen-args args))
    (display ")")
    (unless (eq? modify :normal)
      (display modify))
    (newline)
    (add-indent 
      (vise-render 'stmt `(begin ,@body)))
    (add-new-line)
    (print "endfunction"))

  (ensure-toplevel-ctx form ctx)
  (match form
    [(_ name (args ...) modify . body) (gen-vfn name args modify body)]))

(define (render-symbol-bind sym init :optional (for-rendering? #f))
  (display (if for-rendering?  "for " "let "))
  (if (vsymbol? sym)
    (display (vim-symbol sym))
    (begin
      (display "[")
      (let loop ((sym sym))
        (unless (null? sym)
          (display (if (vsymbol? (car sym))
                     (vim-symbol (car sym))
                     ";"))
          (when (not (or (null? (cdr sym)) (eq? (cadr sym) :rest) (eq? (car sym) :rest)))
            (display ","))
          (loop (cdr sym))))
      (display "]")))
  (display (if for-rendering?  " in " " =("))
  (display
    (let1 init (vise-render-to-string 'expr init)
      (if (boxing? sym)
        (vim-boxing init)
        init)))
  (display (if for-rendering?  "" ")"))
  (add-new-line))

(define-vise-renderer (defvar form ctx)
  (ensure-stmt-or-toplevel-ctx form ctx)
  (render-symbol-bind (cadr form) (caddr form)))

(define-vise-renderer (ref-display-var form ctx)
  (display 
    ((if (boxing? (cadr form)) vim-unboxing identity)
     (string-append "self['" (vim-symbol (cadr form)) "']"))))

(define-vise-renderer (lambda form ctx)
  (define (find-free exp vars)
    (cond
      ((symbol? exp) vars)
      ((vsymbol? exp)
       (receive (d outside?) (env-find-data-with-outside-lambda? (@ exp.env) exp)
         (if (and d (env-data-has-attr? d 'free) outside?)
           (set-cons vars exp)
           vars)))
      (else
        (case (and (list? exp) (get-symbol (car exp)))
          ((quote) vars)
          ((lambda) ) ;;TODO
          ((if)
           (if (null? (cdddr exp))
             ($ find-free (cadr exp) ;test
               $ find-free (caddr exp)  ;then
               vars)
             ($ find-free (cadr exp) ;test
               $ find-free (caddr exp) ;then
               $ find-free (cadddr exp) ;else
               vars))) ;else
          ((set!)
           ($ find-free (cadr exp)
             $ find-free (caddr exp)
             vars))
          ((while begin and or)
           (fold find-free vars (cdr exp)))
          ((quasiquote) ) ;;TODO
          (else 
            (fold find-free vars exp))))))

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
          (lambda (var) 
            #`"',(vim-symbol var)':,(vim-symbol var)")
          (fold find-free '() (cddr form)))
        "," 'prefix))
    (display "}")
    ))

(define-vise-renderer (let* form ctx)
  (ensure-stmt-or-toplevel-ctx form ctx)
  ;;render vars declare
  (for-each
    (lambda (vars)
      (render-symbol-bind (car vars) (cadr vars)))
    (cadr form))
  ;;render body
  (vise-render 'stmt `(begin ,@(cddr form))))


(define-vise-renderer (begin form ctx)
  (ensure-stmt-or-toplevel-ctx form ctx)
  (for-each
    (lambda (exp)
      (vise-render ctx exp)
      (add-new-line))
    (cdr form)))

(define-vise-renderer (if form ctx)
  (ensure-stmt-or-toplevel-ctx form ctx)
  (let1 cctx 'expr
    (display "if ")
    (match form
      [(_ test then)
       (vise-render cctx test)
       (add-new-line)
       (add-indent (vise-render ctx then)) ]
      [(_ test then else)
       (vise-render cctx test)
       (add-new-line)
       (add-indent (vise-render ctx then))
       (add-new-line)
       (print "elseif")
       (add-indent (vise-render ctx else))])
    (add-new-line)
    (print "endif")))

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
     (display
       (let ((val (vise-render-to-string 'expr val))
             (d (env-find-data (@ var.env) var)))
         (if (and d (env-data-has-attr? d 'free))
           (vim-boxing val)
           val)))
     (display ")")]
    [_   (error "uneven args for set!:" form)]))

(define-vise-renderer (echo form ctx)
  (ensure-stmt-or-toplevel-ctx form ctx)
  (match form
    [(_ expr)
     (display "echo ")
     (vise-render 'expr expr)]))



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
        (display (vise-render 'expr a))
        (display ")")
        (display ,sop)
        (display "(")
        (display (vise-render 'expr b))
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

(define-vise-renderer (?: form ctx)
  (ensure-expr-ctx form ctx)
  (match form
    [(?: test then else)
     (display "((")
     (vise-render 'expr test)
     (display ")?(")
     (vise-render 'expr then)
     (display "):(")
     (vise-render 'expr else)
     (display "))")]))

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

