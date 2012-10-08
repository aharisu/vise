
(define (vise-phase-render out-port exp)
  (for-each
    (cut vise-render 'toplevel <> out-port)
    exp))

(use gauche.interactive)

(define (vise-render ctx exp :optional (out-port (current-output-port)))
  (with-output-to-port
    out-port
    (lambda ()
      (cond
        [(vsymbol? exp)
         (display (slot-ref (env-find-data (@ exp.env) exp) 'vim-name))]
        [(list? exp)
         (if-let1 renderer (cond
                             [(vsymbol? (car exp)) 
                              (vise-lookup-renderer (slot-ref (car exp) 'exp))]
                             [(symbol? (car exp))
                              (vise-lookup-renderer (car exp))]
                             [else #f])
           (let1 ret (renderer exp ctx)
             (unless (undefined? ret) (vise-render ctx ret out-port)))
           (render-func-call ctx exp))]
        [else ;literal
          (display "literal:")
          (display exp)]))))

(define (render-func-call ctx form)
  ;;TODO lambda
  (when (or (stmt-ctx? ctx) (toplevel-ctx? ctx))
    (display "call "))
  (display (car form))
  (display (intersperse "," (map (pa$ vise-render-to-string 'expr) (cdr form))))
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

;;
;;renderer
(define-constant renderer-table (make-hash-table 'eq?))

(define (vise-register-renderer! name renderer)
  (hash-table-put! renderer-table name renderer))

(define (vise-lookup-renderer name)
  (hash-table-get renderer-table name #f))

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
    ($ intersperse "," 
      $ map vise-render-identifier 
      $ map (lambda (sym) 
              (remove-symbol-prefix (slot-ref (env-find-data (@ sym.env) sym) 'vim-name)))
      args))
  (define (gen-vfn name args body)
    (print #`"function! ,(vise-render-identifier name) ,(gen-args args)")
    (add-indent 
      (vise-render 'stmt `(begin ,@body)))
    (add-new-line)
    (print "endfunction"))
  (ensure-toplevel-ctx form ctx)
  (match form
    [(_ name (args ...) . body) (gen-vfn name args body)]))

(define-vise-renderer (devar form ctx)
  (ensure-toplevel-ctx form ctx)
  ;;TODO
  )

(define-vise-renderer (let* form ctx)
  (ensure-stmt-or-toplevel-ctx form ctx)
  ;;render vars declare
  (for-each
    (lambda (vars)
      (let ((sym (car vars))
            (init (cadr vars)))
        (display "let ")
        (display (vise-render-identifier
                   (slot-ref (env-find-data (@ sym.env) sym) 'vim-name)))
        (display "=(")
        (vise-render 'expr init)
        (display ")")
        (add-new-line)))
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
     (vise-render 'expr val)
     (display ")")]
    [_   (error "uneven args for set!:" form)]))

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
                             [(#\* #\> #\@ #\$ #\% #\^ #\& #\* #\+ #\= #\: #\. #\/ #\~)
                              (display #\_)
                              (display (number->string (char->integer c) 16))
                              (loop (read-char))]
                             [else (display c) (loop (read-char))]
                             ))))))

