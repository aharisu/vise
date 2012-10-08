

;;TODO read dictionary
(define (vise-phase-read in
                    :key (environment (make-module #f)))
  (port-map
    identity
    (pa$ read in)))


