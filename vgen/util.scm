
(define-module vgen.util
  (use srfi-13)
  (export-all)
  )

(select-module vgen.util)

;;
;; util

(define (slot-ref-conv tokens)
  (fold
    (lambda (token acc)
      `(slot-ref
         ,acc
         (quote ,(string->symbol token))))
    (string->symbol (car tokens))
    (cdr tokens)))

(define-macro (@ expr)
  (slot-ref-conv (string-split (symbol->string expr) ".")))

(define-macro (@! expr obj)
  (let* ([tokens (reverse (string-split (symbol->string expr) "."))]
         [last (car tokens)]
         [head (reverse (cdr tokens))])
    `(slot-set!
       ,(slot-ref-conv head)
       (quote ,(string->symbol last))
       ,obj)))

(define-macro (@inc! expr :optional delta)
  (if (undefined? delta)
    `(inc! ,(slot-ref-conv (string-split (symbol->string expr) ".")))
    `(inc! ,(slot-ref-conv (string-split (symbol->string expr) ".")) ,delta)))

(define-macro (@dec! expr :optional delta)
  (if (undefined? delta)
    `(dec! ,(slot-ref-conv (string-split (symbol->string expr) ".")))
    `(dec! ,(slot-ref-conv (string-split (symbol->string expr) ".")) ,delta)))

(define-macro (@push! expr data)
  `(push! ,(slot-ref-conv (string-split (symbol->string expr) ".")) ,data))

(define-macro (@op= expr op obj)
  `(@! ,expr (,op (@ ,expr) ,obj)))



