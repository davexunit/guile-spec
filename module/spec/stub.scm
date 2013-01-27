(define-module (spec stub)
  #:use-module (ice-9 pretty-print)
  #:export (stub))

(define* (module-stub module-name name thunk #:optional (return #f))
  (let* ((module   (resolve-module module-name))
         (proc-var (module-variable module name))
         (old-proc (variable-ref proc-var)))
    ;; dynamic-wind will restore the old procedure binding even if an
    ;; exception is thrown.
    (dynamic-wind
      (lambda () (variable-set! proc-var (lambda args return)))
      thunk
      (lambda () (variable-set! proc-var old-proc)))))

(define-syntax %stub
  (syntax-rules (return)
    ((%stub module-name name ret (return val) rest ...)
     (%stub module-name name val rest ...))
    ((%stub module-name name ret body ...)
     (module-stub 'module-name 'name (lambda () body ...) ret))))

;; Author: Mark Weaver
(define-syntax stub
  (syntax-rules ()
    ((stub module-name () body ...)
     (let () body ...))
    ((stub module-name ((proc opt ...) rest ...) body ...)
     (%stub module-name proc #f opt ...
            (stub module-name (rest ...) body ...)))))
