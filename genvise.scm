
(add-load-path "." :relative)
(use vgen.compiler)
(use gauche.parseopt)

(define (main args)
  (let-args (cdr args)
    ((out "o|output=s")
     (else (opt . _) (error "Unkown option : " opt))
     . args)
    (when (zero? (length args))
      (error "no source file."))
    (let ([in-port (open-input-file (x->string (car args)))]
          [out-port (if out (open-output-file out) (standard-output-port))])
      (unwind-protect
        (vise-compile in-port out-port)
        (begin
          (close-input-port in-port)
          (close-output-port out-port))))))

