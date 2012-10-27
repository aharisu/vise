
(define (vise-phase-include load-path exp-list)
  (define (recursion form)
    (if (and (not (null? form)) (list? form))
      (if (eq? 'include (car form))
        (include-file form load-path)
        (cons (append-map recursion form) '())) 
      (cons form '())))
  (append-map recursion exp-list))

(define (include-file form load-path)
  (unless (and (= 2 (length form)) (string? (cadr form)))
    (vise-error "Bad Syntax: ~a" form))
  (if-let1 path (find-file-in-paths (cadr form) 
                                    :paths load-path 
                                    :pred file-is-readable?)
    (file->sexp-list path)
    (vise-error "File not found:~a" (cadr form))))

