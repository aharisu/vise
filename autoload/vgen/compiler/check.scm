(define-module vgen.compiler.check
  (use srfi-1)

  (use vgen.util)
  (use vgen.common)
  (export vise-phase-check))

(select-module vgen.compiler.check)

(define (vise-phase-check global exp-list)
  (check-env global)
  (for-each
    (pa$ check-expression 0)
    exp-list)
  exp-list)

(define (check-env env)
  (let1 l (filter-map
            (lambda (sym&d) 
              (and (or* eq? (slot-ref (cdr sym&d) 'scope) 'script 'global 'window 'buffer)
                (cons (vise-gensym (car sym&d) (slot-ref (cdr sym&d) 'scope) '()) (car sym&d))))
            (@ env.symbols))
    (for-each
      (lambda (s1)
        (for-each
          (lambda (s2)
            (when (and (not (eq? s1 s2)) ;自分自身ではないか?
                    (string=? (car s1) (car s2)) ;意味が同じか?
                    (not (eq? (cdr s1) (cdr s2)))) ;表記が異なるか?
              (vise-error "Duplicate symbol of the same meaning: ~a and ~a" (cdr s1) (cdr s2))))
          l))
      l))
  (for-each check-env (@ env.children)))

(define (check-expression nest-quasiquote exp)
  (cond 
    [(vsymbol? exp)
     (check-refer-symbol exp)]
    [(list? exp)
     (case (get-symbol (car exp))
       [(quote)] ;nothing
       [(defun) (check-defun nest-quasiquote exp)]
       [(defvar) (check-defvar nest-quasiquote exp)]
       [(lambda) (check-lambda nest-quasiquote exp)]
       [(if) (check-if nest-quasiquote exp)]
       [(set!) (check-set! nest-quasiquote exp)]
       [(let) (check-let nest-quasiquote exp)]
       [(dolist) (check-dolist nest-quasiquote exp)]
       [(while begin and or raw-vimscript)
        (for-each
          (pa$ check-expression nest-quasiquote)
          (cdr exp))]
       [(augroup)
        (unless (vsymbol? (cadr exp))
          (check-expression nest-quasiquote (cadr exp)))
        (for-each
          (pa$ check-expression nest-quasiquote)
          (cddr exp))]
       [(list-func)
        (check-expression nest-quasiquote (caddr exp))
        (check-expression nest-quasiquote (cadddr exp))]
       ;;TODO
       #;[(quasiquote) (check-quasiquote nest-quasiquote exp)]
       ;;TODO
       #;[(unquote unquote-splicing)
        (if (zero? nest-quasiquote)
          (vise-error "Illegal ~a" (car exp))
          (for-each
            (pa$ check-expression (- nest-quasiquote 1))
            (cdr exp)))]
       [(array) (check-array nest-quasiquote exp)]
       [(dict) (check-dict nest-quasiquote exp)]
       [(try) (check-try nest-quasiquote exp)]
       [(autocmd) (check-autocmd nest-quasiquote exp)]
       [(vim-cmd) (check-vim-cmd nest-quasiquote exp)]
       [else (check-apply nest-quasiquote exp)])]
    [(pair? exp)
     (vise-error "Compiler: Syntax error:~a" exp)]))

(define (check-refer-symbol vsymbol)
  (unless (env-find-data vsymbol)
    (let1 symbol (symbol->string (@ vsymbol.exp))
      (unless (or 
                (string=? "@@" symbol)
                (char-upper-case? (string-ref symbol 0));global command?
                (and (< 1 (string-length symbol))
                  ;;Vim embedded symbol?
                  (or (eq? (string-ref symbol 0) #\&)
                    ;;global or window or buffer scope?
                    (or* string=? (substring symbol 0 2) "g:" "v:" "w:" "b:" "l:" "a:")))
                (cmd-symbol? symbol));refer name space?
        (vise-error "Compiler: ~a reference does not exist.~a" 
                    vsymbol (@ vsymbol.parent))))))

(define (check-defun nest-quasiquote exp)
  (unless (vsymbol? (cadr exp))
    (vise-error "Illegal argument:~a" exp))
  (let1 modify (cadddr exp)
    (unless (or (eq? modify :normal) (eq? modify :dict) (eq? modify :range))
      (vise-error "Illegal function modifier:~a" modify)))
  (check-fun nest-quasiquote (caddr exp) (cddddr exp)))

(define (check-symbol-bind sym form)
  (cond
    ((list? sym)
     (let loop ((sym sym))
       (unless (null? sym)
         (cond
           ((eq? (car sym) :rest)
            (unless (and (not (null? (cdr sym))) (null? (cddr sym)))
              (vise-error "Illegal :rest argument:~a ~a" (car sym) form)))
           ((not (vsymbol? (car sym))) 
            (vise-error "Illegal argument:~a ~a" (car sym) form)))
         (loop (cdr sym)))))
    ((not (vsymbol? sym))
     (vise-error "Illegal argument:~a ~a" sym form))))

(define (check-defvar  nest-quasiquote exp)
  (check-symbol-bind (cadr exp) exp)
  (check-expression nest-quasiquote (caddr exp)))

(define (check-lambda nest-quasiquote exp)
  (check-fun nest-quasiquote (cadr exp) (cddr exp)))

(define (check-fun nest-quasiquote args body)
  (define (err msg related-exp)
    (vise-error "~a:~a" msg related-exp))
  ;;check args
  (let loop ((arg-cell args)
             (set '()))
    (unless (null? arg-cell)
      (let1 arg (car arg-cell)
        (unless (or (vsymbol? arg) (eq? :rest arg))
          (err "Illegal argument." args))
        (when (eq? :rest arg)
          (when (or (null? (cdr arg-cell)) (not (null? (cddr arg-cell))))
            (err "Illegal :rest parameter." args))
          (when (vsymbol? (cadr arg-cell))
            (attr-push! (env-find-data (cadr arg-cell))
                        'rest)))
        (when (set-exists set arg)
          (err "Duplicate arguments." args))
        (loop (cdr arg-cell) (set-cons set arg)))))
  ;;check body
  (when (null? body)
    (err "Nothing function body." exp))
  (for-each
    (pa$ check-expression nest-quasiquote)
    body))

(define (check-if nest-quasiquote exp)
  (check-expression nest-quasiquote (cadr exp))
  (check-expression nest-quasiquote (caddr exp))
  (unless (null? (cdddr exp))
    (check-expression nest-quasiquote (cadddr exp)))
  )

(define (check-set! nest-quasiquote exp)
  (let1 var (cadr exp)
    ;(unless (vsymbol? var)
    ;  (vise-error "Illegal argument:~a" exp))
    (unless (allow-rebound? var)
      (vise-error "It is '~a that Can not rebound: ~a" var exp))
    (check-expression nest-quasiquote var))
  (check-expression nest-quasiquote (caddr exp)))

(define (check-dolist nest-quasiquote exp)
  (check-symbol-bind (caadr exp) (cadadr exp)) ;var
  (check-expression nest-quasiquote (cadadr exp)) ;expr
  (for-each ;body
    (pa$ check-expression nest-quasiquote)
    (cddr exp)))

(define (check-let nest-quasiquote exp)
  ;;check declare
  (for-each
    (lambda (spec)
      (check-symbol-bind (car spec) exp)
      (check-expression nest-quasiquote (cadr spec)))
    (cadr exp))
  ;;check body
  (when (null? (cddr exp))
    (vise-error "Nothing let body: ~a" exp))
  (for-each
    (pa$ check-expression nest-quasiquote)
    (cddr exp)))

(define (check-quasiquote nest-quasiquote exp)
  (define (unquote? exp)
    (and (pair? exp) (symbol? (car exp))
      (let1 sym (car exp)
        (or (eq? 'unquote sym) (eq? 'unquote-splicing sym)))))

  (define (can-unquote? exp nest-level)
    (cond
      [(pair? exp)
       (let loop ((c exp))
         (if (null? c)
           #f
           (or (and (pair? c)
                 (or (can-unquote? c nest-level)
                   (and (vsymbol? (car c))
                     (case (slot-ref (car c) 'exp)
                       [(quasiquote) (can-unquote? (cdr c) (+ nest-level 1))]
                       [(unquote unquote-splicing) (or (= nest-level 0) (can-unquote? (cdr c) (- nest-level 1)))]
                       [else #f]))))
             (loop (cdr c)))))]
      [else #f]))

  (define (qq-eval nest parent)
    (when (pair? (car parent))
      (let1 c (car parent)
        (when (and (not (and (vsymbol? (car c))
                          (case (slot-ref (car c) 'exp)
                            [(quasiquote) (qq-check (+ nest 1) (cdr c) )]
                            [(unquote unquote-splicing)
                             (if (zero? nest)
                               (check-expression (- nest 1) (cadr c))
                               (qq-each (cdr c) (- nest 1)))]
                            [else #f])))
                (can-unquote? c nest))
          (if (unquote? c)
            (qq-check nest c)
            (qq-each nest c))))))

  (define (qq-each nest cell)
    (for-each
      (pa$ qq-eval nest)
      cell))

  (define (qq-check nest qq)
    (when (can-unquote? qq nest)
      (if (unquote? (car qq))
        (qq-eval nest qq)
        (qq-each nest (car qq)))))

  (qq-check (cdr exp) nest-level))

(define (check-array nest-level exp)
  (for-each
    (pa$ check-expression nest-level)
    (cdr exp)))

(define (check-dict nest-level exp)
  (for-each
    (lambda (item)
      (unless (vsymbol? (car item))
        (vise-error "Illegal dictionary key:~a ~a" (car item) exp))
      (check-expression nest-level (cadr item)))
    (cdr exp)))

(define (check-try nest-level exp)
  (check-expression nest-level (cadr exp))
  (for-each
    (lambda (clause)
      (unless (list? clause)
        (vise-error "Syntax error:~a" clause))
      (unless (or (and (vsymbol? (car clause)) 
                    (or* eq? (slot-ref (car clause) 'exp) 'else 'finally))
                (string? (car clause)))
        (vise-error "Syntax error. Require catch string or 'finally, 'else:~a" clause))
      (for-each
        (pa$ check-expression nest-level)
        (cdr clause)))
    (cddr exp)))

(define (check-autocmd nest-level exp)
  (define (err)
    (vise-error "Bad syntax. autocmd format (autocmd [group] (event1 event2 ...) pat [:nested] cmd).\n~a" exp))
  (unless (vsymbol? (cadr exp)) (err)) ;group
  (unless (every vsymbol? (caddr exp)) (err)) ;events 
  (unless (or* eq? (car (cddddr exp)) :nested :normal) (err)) ;:nested
  (check-expression nest-level (cadr (cddddr exp)))) ;cmd

(define (check-vim-cmd nest-level exp)
  (when (< (length exp) 2)
    (vise-error "Bad syntax. vim-cmd has require command :~a" exp))
  (unless (and (list? (cadr exp))
            (eq? (caadr exp) 'quote)
            (symbol? (cadadr exp)))
    (vise-error "Bad syntax. vim-cmd has require command symbol :~a" exp))
  (for-each
    (pa$ check-expression nest-level)
    (cddr exp)))

(define (check-apply nest-level exp)
  (for-each
    (pa$ check-expression nest-level)
    exp)
  (when (vsymbol? (car exp))
    (let1 d (env-find-data (car exp))
      (if (and d (not (or (eq? (@ d.scope) 'syntax)
                        (has-attr? d 'function))))
        (attr-push! d 'func-call)))))

