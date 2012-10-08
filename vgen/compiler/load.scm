
(define (vise-phase-load exp-list :optional (load-path-root "."))
  (reverse!
    (fold
      (lambda (e acc)
        (cons e acc))
      '()
      exp-list)))

