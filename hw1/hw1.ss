;; Venus Yu
;; HW1
;; 1/11/17

;; Exercise 1
42

;; Exercise 2
3.14159

;; Exercise 3
"hello world"

;; Exercise 4
"let the grecian dream of his sacred stream"

;; Exercise 5
(+ 3 4)

;; Exercise 6
(* (- 6.7 13) (+ (* 4 (/ 5 9)) 17))

;; Exercise 7
(+ 1 2 3 4 5 6 7 8)

;; Exercise 8
(define x "hello world")

;; Exercise 9
(list? 6)

;; Exercise 10
((lambda (x) x) 10)

;; Exercise 11
(define (identity e) e)

;; Exercise 12
(identity x)

;; Exercise 13
(let ([a 2] [b 7] [c 18])
    (/ (+ (* b -1) (sqrt (- (expt b 2) (* 4 a c)))) (* 2 a)))

;; Exercise 14
(define (plus42 e)
    (cond [(number? e) (+ e 42)]
          [else "the answer to..."]))

;; Exercise 15
(car (list 1 1 2 3 5))

;; Exercise 16
(cadddr '(1 1 2 3 5))

;; Exercise 17
(cons '1 (cons '1 (cons '2 (cons '3 (cons '5 '())))))

;; Exercise 18
(cons 3 4)

;; Exercise 19
(cons (cons (cons (cons 1 2) (cons '3 (cons '4 '()))) 5) '())

;; Exercise 20
(and (or #t #f) #t)

;; Exercise 21
(let ([a #t] [b #f] [c #f])
    (or (or (not [boolean=? a (not b)]) (and c (not a))) b)))

;; Exercise 22
(cond [(string? x) 42]
      [else "no"]))

;; Exercise 23
(define (positive? e)
    (cond [(number? e) (> e 0)]
          [else "not a number..."])))

;; Exercise 24
(define (numMonth->strMonth n)
    (cond [(eq? n 1) "January"]
          [(eq? n 2) "February"]
          [(eq? n 3) "March"]
          [(eq? n 4) "April"]
          [(eq? n 5) "May"]
          [(eq? n 6) "June"]
          [(eq? n 7) "July"]
          [(eq? n 8) "August"]
          [(eq? n 9) "September"]
          [(eq? n 10) "October"]
          [(eq? n 11) "November"]
          [(eq? n 12) "December"]))

;; Exercise 25
(define (member e ls)
    (cond [(null? ls) #f]
          [(eq? (car ls) e) #t]
          [else (member e (cdr ls))]))

;; Exercise 26
(define (range num1 num2)
      (cond [(> num1 num2) '()]
            [else (cons num1 (range (+ num1 1) num2))]))

;; Exercise 27
(define (append ls1 ls2)
    (cond [(null? ls1) ls2]
          [else (cons (car ls1) (append (cdr ls1) ls2))]))

;; Exercise 29
(define (map fn ls)
   (cond [(null? (cdr ls)) (cons (fn (car ls)) '())]
         [else (cons (fn (car ls)) (map fn (cdr ls)))]))

;; Exercise 30
(define (filter p? ls)
    (cond [(null? (cdr ls)) (cond [(p? (car ls)) (cons (car ls) '())]
                                  [else '()])]
          [else (cond [(p? (car ls)) (cons (car ls ) (filter p? (cdr ls)))]
                      [else (filter p? (cdr ls))])]))

;; Exercise 31
(define (counts p? ls)
    (counts-help p? ls 0))

(define (counts-help p? ls total)
    (cond [(null? (cdr ls)) (if (p? (car ls)) (+ total 1))]
          [else (cond [(p? (car ls)) (counts-help p? (cdr ls) (+ total 1))]
                      [else (counts-help p? (cdr ls) total)])]))

;; Exercise 35
(define (score100-help f n)
    (if (>= (f n) 100) n
       (score100-help f (+ n 1))))

(define (score100 f)
    (score100-help f 0)))
