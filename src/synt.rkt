#lang racket

(require "misc.rkt" "env.rkt" "expr.rkt")

(provide synt:perform)

(define (synt:perform inputs)
  (synt-all inputs (env:make-synt)))

(define (synt-all inputs env)
  (map (lambda (input) (synt input env)) inputs))

(define (synt input env)
  (cond ((null? input) (raise "Syntax error: null input.\n"))
        ((list? input) (cond ((eq? (car input) 'lambda) (synt-abs input env))
                             ((eq? (car input) 'define) (synt-def input env))
                             ((eq? (car input) 'set!) (synt-assign input env))
                             ((eq? (car input) 'sif) (synt-sif input env))
                             ((eq? (car input) 'dif) (synt-dif input env))
                             ((eq? (car input) 'vif) (synt-vif input env))
                             ((eq? (car input) 'coerce) (synt-coerce input env))
                             (else (synt-app input env))))
        ((symbol? input) (synt-id input env))
        (else (synt-const input env))))

(define (synt-const const env)
  (expr:make-const const))

(define (synt-id id env)
  (env:lookup env id)
  (expr:make-id id))

(define (synt-abs abs env)
  (cond ((< (length abs) 3) (raise "Syntax error: body cannot be empty."))
        ((or (not (list? (car (cdr abs))))
             (not (all-satisfy? (car (cdr abs)) symbol?))) (raise "Syntax error: <params> must be a list of symbols."))
        (else (let* ((params (car (cdr abs)))
                     (ext-env (env:add-all (env:push-frame env) params (map (const null) params)))
                     (body (cdr (cdr abs))))
                (expr:make-abs params (synt-all body ext-env))))))

(define (synt-assign assign env)
  (cond ((not (equal? (length assign) 3)) (raise "Syntax error: expected (set! <name> <bind>)."))
        ((not (symbol? (car (cdr assign)))) (raise "Syntax error: <name> must be a symbol."))
        (else (expr:make-assign (car (cdr assign)) (synt (car (cdr (cdr assign))) env)))))

(define (synt-def def env)
  (cond ((not (equal? (length def) 3)) (raise "Syntax error: expected (define <name> <bind>)."))
        ((not (symbol? (car (cdr def)))) (raise "Syntax error: <name> must be a symbol."))
        (else (let* ((name (car (cdr def)))
                     (bind (car (cdr (cdr def)))))
                (env:add! env name null)
                (expr:make-def name (synt bind env))))))

(define (synt-sif sif env)
  (if (equal? (length sif) 4)
      (let ((exprs (synt-all (cdr sif) env)))
        (expr:make-sif (car exprs) (car (cdr exprs)) (car (cdr (cdr exprs)))))
      (raise "Syntax error: expected (sif <pred> <alt> <cons>).")))

(define (synt-dif dif env)
  (if (equal? (length dif) 4)
      (let ((exprs (synt-all (cdr dif) env)))
        (expr:make-dif (car exprs) (car (cdr exprs)) (car (cdr (cdr exprs)))))
      (raise "Syntax error: expected (dif <pred> <alt> <cons>).")))

(define (synt-vif vif env)
  (if (equal? (length vif) 3)
      (let ((exprs (synt-all (cdr vif) env)))
        (expr:make-vif (car exprs) (car (cdr exprs))))
      (raise "Syntax error: expected (vif <attempt> <alt>).")))

(define (synt-app app env)
  (let ((exprs (synt-all app env)))
    (expr:make-app (car exprs) (cdr exprs))))

(define (synt-coerce coerce env)
  (if (equal? (length coerce) 2)
      (expr:make-unsafe (synt (car (cdr coerce)) env))
      (raise "Syntax error: expected (coerce <expr>).")))