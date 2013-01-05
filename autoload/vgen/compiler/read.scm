(define-module vgen.compiler.read
  (export vise-phase-read vise-phase-read-repl
    ))

(select-module vgen.compiler.read)

;;TODO read dictionary
(define (vise-phase-read in)
  (port-map
    identity
    (pa$ read in)))

(define (vise-phase-read-repl in)
  (read in))


