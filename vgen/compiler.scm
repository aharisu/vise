
(define-module vgen.compiler 
  (use gauche.vport)
  (use gauche.parameter)
  (use util.match)
  (use srfi-13)

  (use vgen.util)
  (export-all)
  )

(select-module vgen.compiler)


(define-condition-type <vise-error> <error> #f)

;;-------------
;;Util Type
;;-------------

;;
;; <vexp>
(define-class <vexp> ()
  (
   (exp :init-keyword :exp)
   )
  )

(define-method write-object ((sym <vexp>) port)
  (format port "~a" (@ sym.exp)))

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

;;
;;refer data
(define-class <env-data> ()
  (
   (symbol :init-keyword :symbol)
   (exp :init-keyword :exp)
   (scope :init-keyword :scope)
   (attr :init-value '())
   (vim-name :init-value #f)
   (ref-count :init-value 0)
   (set-count :init-value 0)
   )
  )

(define (env-data-attr-push! env-data attr)
  (@! env-data.attr (set-cons (@ env-data.attr) attr)))

(define (env-data-has-attr? env-data attr)
  (set-exists (@ env-data.attr) attr))

(define (get-vim-name env-data)
  (if (@ env-data.vim-name)
    (@ env-data.vim-name)
    (rlet1 name (vise-gensym (@ env-data.symbol) (@ env-data.scope) (@ env-data.attr))
      (@! env-data.vim-name name))))

;;;;;
;;@param scope {@ 'arg 'script 'global 'window 'buffer 'syntax}
;;@param attr {@ set-list}
(define (vise-gensym sym scope attr) 
  (let ((prefix (case scope
                  ((arg) "a:")
                  ((script) "s:")
                  ((global) "g:")
                  ((window) "w:")
                  ((buffer) "b:")
                  (else "")))
        (sym (if (set-exists attr 'func-call)
               (string-titlecase (x->string sym))
               (x->string sym))))
    (if (string-null? prefix)
      (symbol->string 
        (gensym 
          (string-append 
            sym
            "_")))
      (string-append prefix (x->string sym)))))


(define (remove-symbol-prefix symbol)
  (string-scan (x->string symbol) ":" 'after))

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
(define-method write-object ((env <env>) port)
  (with-output-to-port 
    port
    (pa$ print-env-table env)))

(define (make-env parent :optional (lambda-border? #f))
  (rlet1 env (make <env> 
                   :parent parent
                   :lambda-border? lambda-border?)
    (when parent
      (@push! env.parent.children env))))

(define env-toplevel?  (.$ not (cut slot-ref <> 'parent)))

(define (env-add-symbol env symbol scope)
  (env-add-symbol&exp env symbol scope #f))

(define (env-add-symbol&exp env symbol scope exp)
  (let1 symbol (get-symbol symbol)
    (@push! env.symbols 
            (cons symbol (make <env-data>
                               :symbol symbol
                               :exp exp
                               :scope scope)))))

(define (env-find-data-with-outside-lambda? env symbol)
  (let1 symbol (get-symbol symbol)
    (let loop ((env env)
               (outside? #f))
      (if-let1 d (assq-ref (@ env.symbols) symbol)
        (values d outside?)
        (if (@ env.parent)
          (loop (@ env.parent) (or outside? (@ env.lambda-border?)))
          (values #f #f))))))

(define (env-find-data env symbol)
  (receive (d outside?) (env-find-data-with-outside-lambda? env symbol)
    d))

(define (env-find-exp env symbol)
  (if-let1 d (env-find-data env symbol)
    (@ d.exp)
    #f))

(define (allow-rebound? vsymbol)
  (if-let1 d (env-find-data (@ vsymbol.env) vsymbol)
    (not (or (eq? (@ d.scope) 'arg) (eq? (@ d.scope) 'syntax)))
    #t))

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

(define-macro (add-indent . body)
  `(unwind-protect
     (begin 
       (inc! (car (slot-ref (current-output-port) 'indent)))
       ,@body)
     (dec! (car (slot-ref (current-output-port) 'indent)))))

;;
;;set
(define (set-exists set obj)
  (any (pa$ equal? (if (is-a? obj <vexp>) (@ obj.exp) obj))
       set))

(define (set-cons set obj)
  (if (set-exists set obj)
    set
    (cons obj set)))

;;----------
;;Compile
;;----------

;(add-load-path ".." :relative)
(include "compiler/read.scm")
(include "compiler/load.scm")
(include "compiler/expand.scm")
(include "compiler/check.scm")
(include "compiler/render.scm")

(define (vise-compile-from-string str)
  (vise-compile (open-input-string str)))

(define (vise-compile in-port)
  (let* ((global-env (rlet1 env (make-env #f)
                       (hash-table-for-each
                         renderer-table
                         (lambda (k v) (env-add-symbol&exp env k 'syntax v)))))
         (out-port (make-vise-output-port (standard-output-port)))
         (exp-list ((.$
                      (pa$ vise-phase-render out-port)
                      ;check
                      vise-phase-check
                      (pa$ vise-phase-expand global-env)
                      vise-phase-load 
                      vise-phase-read)
                    in-port)))
    (print exp-list)
    ;(print (get-output-string out-port))
    ))

