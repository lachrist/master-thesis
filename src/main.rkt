#lang racket

(require "synt.rkt" "infer.rkt" "expr.rkt" "inter.rkt" "value.rkt")
(define (test)
  (define tests (quote (
                        
                        (
                         (define evil (lambda (x) (dif x 1 "foo")))
                         (+ 1 (evil #t))
                         (concat "yoo" (evil #f))
                         (+ 1 (evil #f))
                         )
                        
                        (
                         (define evil (lambda (b)
                                        (dif b
                                             1
                                             "foo")))
                         (concat (evil #t) "bar")
                         )
                        
                        
                        (
                         (define test (lambda (x)
                                        (vif (+ x 1)
                                             (concat x "foo"))))
                         (test 1)
                         (test "foo")
                         )
                        
                        
                        
                        (
                         (define map (lambda (f xs) (sif (null? xs)
                                                         null
                                                         (cons (f (car xs)) (map f (cdr xs))))))
                         (map (lambda (x) (+ x 1)) (cons 1 (cons 2 (cons 3 null))))
                         (map (lambda (x) (concat "yo" x)) (cons "foo" (cons "bar" null)))
                         )
                        
                        (
                         (define evil (lambda (b)
                                        (dif b
                                            1
                                            "foo")))
                         (concat (evil #t) "bar")
                         )
                        
                        
                        (
                         (define Y
                           (lambda (f)
                             ((lambda (x) (f (lambda (v) ((x x) v))))
                              (lambda (x) (f (lambda (v) ((x x) v)))))))
                         )
                        
                        (
                         (define ident (lambda (x) x))
                         (ident 1)
                         (ident "foo")
                         )
                        
                        
                        
                        (
                         (define yo 1)
                         (set! yo (+ yo 3))
                         yo
                         )
                        
                        
                        (
                         (define read (lambda ()
                                        ((lambda (input)
                                           (dif (string-boolean? input)
                                                (string->boolean input)
                                                (dif (string-number? input)
                                                     (string->number input)
                                                     input)))
                                         (read-line))))
                         (+ 1 (read))
                         (concat "yo" (read))
                         )
                        
                        
                        
                        )))
  (define (run-test test)
    (call/cc (lambda (cont)
               (call-with-exception-handler
                (lambda (exn) (if (string? exn)
                                  (begin (display exn) (cont '()))
                                  exn))
                (lambda ()
                  (display "Input >> ")
                  (let ((exprs (synt:perform test)))
                    (for-each (lambda (expr) (display (expr:print expr 9)) (display "         ")) exprs) 
                    (display "\nInfer >> ")
                    (let ((lexprs (infer:perform exprs)))
                      (for-each (lambda (lexpr) (display (expr:print lexpr 9)) (display "         ")) lexprs)
                      (display "\nInter >> ")
                      (let ((values (inter:perform lexprs)))
                        (for-each (lambda (value) (when (not (eq? (value:get-void) value)) (display (value:print value)) (display "\n         "))) values))))))))
    (display "\n=================================================================================================================================================\n"))
  (for-each run-test tests))

(test)
