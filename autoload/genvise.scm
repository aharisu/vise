
(add-load-path "." :relative)
(use vgen.compiler)
(use vgen.common)
(use gauche.parseopt)

(define (main args)
  (let1 loadpath '()
    (let-args (cdr args)
      ((out "o|output=s")
       (#f "s|loadpath=s"
        => (lambda (opt) (set! loadpath (cons opt loadpath))))
       (else (opt . _) (error "Unkown option : " opt))
       . args)
      (when (zero? (length args))
        (error "no source file."))
      (let ([in-port (open-input-file (x->string (car args)))]
            [out-port (if out (open-output-file out) (standard-output-port))])
        (guard (e [(<vise-error> e)
                   (display "*** vise ERROR: " (standard-error-port))
                   (display (slot-ref e 'message) (standard-error-port))
                   (newline (standard-error-port))
                   1])
          (unwind-protect
            (vise-compile in-port :out-port out-port
                          :load-path (reverse! loadpath)
                          :prologue "\" Generated automatically DO NOT EDIT\n\n")
            (begin
              (close-input-port in-port)
              (unless (eq? out-port (standard-output-port))
                (close-output-port out-port))))
          0)))))

