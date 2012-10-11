
(define (vise-phase-check exp-list)
  (for-each
    (pa$ check-expression 0)
    exp-list)
  exp-list)

(define (check-expression nest-quasiquote exp)
  (define (ref-error exp)
    (errorf <vise-error> "Compiler: ~a reference does not exist" exp))

  (cond 
    [(vsymbol? exp)
     (check-refer-symbol exp)]
    [(list? exp)
     (case (get-symbol (car exp))
       [(quote)] ;nothing
       [(defun) (check-defun nest-quasiquote exp)]
       [(defvar) ] ;;TODO
       ;;TODO
       [(lambda) (check-lambda nest-quasiquote exp)]
       [(if) (check-if nest-quasiquote exp)]
       [(set!) (check-set! nest-quasiquote exp)]
       [(let*) (check-let* nest-quasiquote exp)]
       [(while begin and or)
        (for-each
          (pa$ check-expression nest-quasiquote)
          (cdr exp))]
       ;;TODO
       #;[(quasiquote) (check-quasiquote nest-quasiquote exp)]
       ;;TODO
       #;[(unquote unquote-splicing)
        (if (zero? nest-quasiquote)
          (errorf <vise-error> "Illegal ~a" (car exp))
          (for-each
            (pa$ check-expression (- nest-quasiquote 1))
            (cdr exp)))]
       [else (check-apply nest-quasiquote exp)])]
    [(pair? exp)
     (errorf <vise-error> "Compiler: Syntax error:~a" exp)]))

(define (check-refer-symbol vsymbol)
  ;;not found from environment?
  (unless (env-find-data (@ vsymbol.env) vsymbol)
    (let1 symbol (symbol->string (@ vsymbol.exp))
      (unless (or 
                (char-upper-case? (string-ref symbol 0));global command?
                (and (< 1 (string-length symbol)) ;global or window or buffer scope?
                  (let1 prefix (substring symbol 0 2)
                    (or (string=? "g:" prefix)
                      (string=? "w:" prefix)
                      (string=? "b:" prefix))))
                (string-scan symbol #\#)) ;refer name space?
        (errorf <vise-error> "Compiler: ~a reference does not exist" vsymbol)))))

(define (check-defun nest-quasiquote exp)
  (unless (vsymbol? (cadr exp))
    (errorf <vise-error> "Illegal argument:~a" exp))
  (check-fun nest-quasiquote (caddr exp) (cdddr exp)))

(define (check-lambda nest-quasiquote exp)
  (check-fun nest-quasiquote (cadr exp) (cddr exp)))

(define (check-fun nest-quasiquote args body)
  (define (err msg related-exp)
    (errorf <vise-error> "~a:~a" msg related-exp))
  ;;check args
  (let loop ((arg-cell args)
             (set '()))
    (unless (null? arg-cell)
      (let1 arg (car arg-cell)
        (unless (or (vsymbol? arg) (eq? :rest arg))
          (err "Illegal argument." args))
        (if (eq? :rest arg)
          (when (or (null? (cdr arg-cell)) (not (null? (cddr arg-cell))))
            (err "Illegal :rest parameter." args))
          (when (set-exists set arg)
            (err "Duplicate arguments." args)))
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
    (unless (vsymbol? var)
      (errorf <vise-error> "Illegal argument:~a" exp))
    (unless (allow-rebound? var)
      (errorf <vise-error> "It is '~a that Can not rebound: ~a" var exp))
    (check-expression nest-quasiquote var))
  (check-expression nest-quasiquote (caddr exp)))

(define (check-let* nest-quasiquote exp)
  ;;check declare
  (for-each
    (lambda (spec)
      (unless (vsymbol? (car spec))
        (errorf <vise-error> "Illegal variable. Must be a symbol: ~a" spec))
      (check-expression nest-quasiquote (cadr spec)))
    (cadr exp))
  ;;check body
  (when (null? (cddr exp))
    (errorf <vise-error> "Nothing let* body: ~a" exp))
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

(define (check-apply nest-level exp)
  (for-each
    (pa$ check-expression nest-level)
    exp)
  (when (vsymbol? (car exp))
    (let1 d (env-find-data (slot-ref (car exp) 'env) (car exp))
      (if (and d (not (or (eq? (@ d.scope) 'syntax)
                        (env-data-has-attr? d 'function))))
        (env-data-attr-push! d 'func-call)))))

