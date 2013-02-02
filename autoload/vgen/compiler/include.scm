(define-module vgen.compiler.include
  (use srfi-1)
  (use file.util)

  (use vgen.common)
  (export vise-phase-include))

(select-module vgen.compiler.include)

(define (vise-phase-include load-path cur-file-directory exp-list)
  (define (recursion form)
    (if (and (not (null? form)) (list? form))
      (if (eq? 'include (car form))
        (include-file form load-path cur-file-directory)
        (cons (append-map recursion form) '())) 
      (cons form '())))
  (append-map recursion exp-list))

(define (include-file form load-path cur-file-directory)
  (unless (and (= 2 (length form)) (string? (cadr form)))
    (vise-error "Bad Syntax: ~a" form))
  (if-let1 path (find-file-in-paths (cadr form) 
                                    :paths (append load-path (cons cur-file-directory '()))
                                    :pred file-is-readable?)
    (vise-phase-include load-path (sys-dirname path) (file->sexp-list path))
    (vise-error "File not found:~a" (cadr form))))

