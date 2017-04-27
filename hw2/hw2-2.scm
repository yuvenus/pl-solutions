;; Venus Yu
;; 1/18/17
;; hw2-2

;; Exercise 1
;; GRADE
(define times10
    (lambda (nums)
      (map (lambda (x) (* x 10)) nums)))

;; Exercise 2
;; GRADE
(define pair-up
    (lambda (elt ls)
      (map (lambda (x) (cons elt x)) ls)))

;;  Exercise 3
;; GRADE
(define x-odds
    (lambda (nums)
      (map (lambda (x) (cond [(odd? x) 'x]
                             [else x])) nums)))

;; Exercise 4
;; GRADE
(define replace
    (lambda (old new syms)
      (map (lambda (x) (cond [(equal? x old) new]
                             [else x])) syms)))

;; Exercise 5
;; GRADE
(define remove
    (lambda (elt ls)
      (filter (lambda (x) (not (equal? x elt))) ls)))

;; Exercise 7
;; GRADE
(define length
  (lambda (ls)
    (fold-left (lambda (acc head) (+ acc 1)) 0 ls)))

;; Exercise 8
;; GRADE
(define average
    (lambda (nums)
      (/ (fold-right + 0 nums) (length nums))))
