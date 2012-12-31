(define-module vgen.compiler.read
  (export vise-phase-read))

(select-module vgen.compiler.read)

;;TODO read dictionary
(define (vise-phase-read in)
  (port-map
    identity
    (pa$ read in)))


