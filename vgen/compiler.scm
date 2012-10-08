
(define-module vgen.compiler 
  (use gauche.vport)
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
   (exp :init-keyword :exp)
   (type :init-keyword :type)
   (vim-name :init-keyword :vim-name)
   (ref-count :init-value 0)
   (set-count :init-value 0)
   )
  )

;;
;;environment
(define-class <env> ()
  (
   (symbols :init-value '())
   (children :init-value '())
   (parent :init-keyword :parent)
   )
  )
(define-method write-object ((env <env>) port)
  (with-output-to-port 
    port
    (pa$ print-env-table env)))

(define (make-env parent)
  (rlet1 env (make <env> :parent parent)
    (when parent
      (@push! env.parent.children env))))

(define env-toplevel?  (.$ not (cut slot-ref <> 'parent)))

(define (env-add-symbol env symbol type)
  (env-add-symbol&exp env symbol type #f))

(define (env-add-symbol&exp env symbol type exp)
  (let1 symbol (get-symbol symbol)
    (@push! env.symbols 
            (cons symbol (make <env-data>
                               :exp exp
                               :type type
                               :vim-name (vise-gensym type symbol))))))

(define (env-has-symbol? env symbol)
  (boolean (assq-ref (@ env.symbols) (get-symbol symbol))))

(define (env-find-data env symbol)
  (let1 symbol (get-symbol symbol)
    (let loop ((env env))
      (let1 d (assq-ref (@ env.symbols) symbol)
        (or d (and (@ env.parent) (loop (@ env.parent))))))))

(define (env-find-exp env symbol)
  (if-let1 d (env-find-data env symbol)
    (@ d.exp)
    #f))

(define (allow-rebound? vsymbol)
  (if-let1 d (env-find-data (@ vsymbol.env) vsymbol)
    (not (or (eq? (@ d.type) 'arg) (eq? (@ d.type) 'syntax)))
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

