;; Venus Yu
;; Homework 4
;; 2/1/2017

(define-datatype prefix prefix?
  (const-exp
    (const integer?))
  (diff-exp
    (body1 prefix?)
    (body2 prefix?)))

;; check if it's a const
(define const?
  (lambda (x)
    (integer? x)))

;; check if it's a diff
(define diff?
  (lambda (x)
    (and
      (list? x)
      (equal? (car x) '-))))

;; Exercise 1
;; parse-prefix
(define parse-prefix
  (lambda (exp)
    (cond
      [(const? (car exp)) (const-exp (car exp))]
      [(diff? exp)
        (cond
          [(diff? (cdr exp)) (diff-exp (parse-prefix (cdr exp)) (parse-prefix (next-exp exp)))]
          [else (diff-exp (parse-prefix (cdr exp)) (parse-prefix (cddr exp)))])]
      [else (eopl:error 'lc-parse "Abstract unparse error with expression ~s" exp)])))

;; grab next exp
(define next-exp
  (lambda (exp)
    (cddddr exp)))

;; Exercise 2
;; unparse-prefix
(define unparse-prefix
  (lambda (exp)
    (cases prefix exp
        [const-exp (const) (list const)]
        [diff-exp (body1 body2) (cons '- (append (unparse-prefix body1) (unparse-prefix body2)))])))

;; Exercise 3
;; eval-prefix
(define eval-prefix
  (lambda (exp)
    (cond
      [(null? exp) (display "error!")]
      [(equal? (car exp) 'const-exp) (cadr exp)]
      [(equal? (car exp) 'diff-exp) (eval (list '- (eval-prefix (cadr exp)) (eval-prefix (caddr exp))))]
      [else (eopl:error 'lc-parse "Eval error with expression ~s" exp)])))
