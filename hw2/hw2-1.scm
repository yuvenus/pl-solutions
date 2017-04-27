;; Venus Yu
;; 1/18/2017
;; hw2-1

;; Global variable for storing a list of tests, initially empty.
(define my-tests! '())
(define total 0)
(define earned 0)

(define clear-tests! (lambda () (set! my-tests! '())))

;; (add-my-test! test-name-str ex-name-str ptval qe1 qe2)
(define add-my-test!
  (lambda (test-name-str ex-name-str ptval qe1 qe2)
    (set! my-tests! (cons (list test-name-str ex-name-str ptval qe1 qe2) my-tests!))))

;; (add-batch tests! ex-name-str-q-tests)
;; add multiple tests at once
(define add-batch-tests!
  (lambda (ex-name-str q-tests)
    (if (not (null? q-tests))
      (let ([qe1 (car q-tests)]
          [qe2 (caddr q-tests)])
      (add-my-test! "" ex-name-str 1 qe1 qe2)
      (add-batch-tests! ex-name-str (cdddr q-tests)))
      )))

;; (display-result! val1 val2)
;; Takes two values and displays them.
(define display-result!
  (lambda (val1 val2)
    (display val1)
    (display " => ")
    (display val2)))

;; (display-test-success! name-str qe1 qe2 val1 val2)
;; Displays text to indicate test success.
(define display-test-success!
  (lambda (name-str qe1 qe2 val1 val2)
    (display name-str)
    (display " -- Success -- ")
    (display-result! qe1 qe2)
    (display "\n")))

;; (display-test-failure! name-str qe1 qe2 val1 val2)
;; Displays text to indicate failure.
(define display-test-failure!
  (lambda (name-str qe1 qe2 val1 val2)
    (display name-str)
    (display " -- Failure\n")
    (display "  Expected: ")
    (display-result! qe1 qe2)
    (display "\n    Actual: ")
    (display-result! qe1 val1)
    (display "\n            ")
    (display-result! qe2 val2)
    (display "\n")))

;; (run-one-test! name-str qe1 qe2)
;; Runs a test with the given name two quoted expressions
(define run-one-test!
  (lambda (name-str qe1 qe2)
    (let
	([val1 (eval qe1)]  ;; This is why the quote are necessary.
	 [val2 (eval qe2)])
      (cond
       [(equal? val1 val2) (display-test-success! name-str qe1 qe2 val1 val2)]
       [else (display-test-failure! name-str qe1 qe2 val1 val2)]))))

;; (run-one-exercise! ex-name-str test-ls)
;; find all tests with exer name associated
;; then runs run-one-test! on each one that applies
(define run-one-exercise!
  (lambda (ex-name-str test-ls)
      (cond [(null? (cdr test-ls)) (display "no tests")]
	    [else (fold-left
		    (lambda (acc head)
		      (cond [(equal? ex-name-str (cadr head)) (set! total (+ total (caddr head)))
			 (cond [(equal? (eval (car (qe-help head))) (eval (cadr (qe-help head))))
		                (set! earned (+ earned (caddr head)))]
			       [else earned])]
		        [else total]))
		     0 test-ls)
		(display earned)
		(display " out of ")
		(display total)
		(display " points!")
    (set! total 0)
    (set! earned 0)])))

;; helper to get qe1 and qe2
(define qe-help
   (lambda (ls)
      (let ([qes (cdddr ls)])
         qes)))

;; (run-all-tests!)
;; Runs all tests.  Note this is a 0-ary function, i.e., it takes no
;; arguments.
(define run-all-tests!
  (lambda ()
    (run-all-tests!* my-tests!)))

;; (run-all-tests!* ls)
;; Recursive function to recurse through tests running each one
(define run-all-tests!*
  (lambda (ls)
    (if (not (null? ls))
      (fold-left
        (lambda (acc head)
          (set! total (+ total (caddr head)))
          (cond [(equal? (eval (car (qe-help head))) (eval (cadr (qe-help head))))
     		                (set! earned (+ earned (caddr head)))]
                [else earned]))
       0 ls))

       (display earned)
       (display " out of ")
       (display total)
       (display " points!")
       (set! total 0)
       (set! earned 0)))

