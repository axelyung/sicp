(define (print x) (display x) (newline))

(print "=== ex 1.2 ===")
(print 10)
(print (+ 5 3 4))
(print (- 9 1))
(print (/ 6 2))
(print (+ (* 2 4) (- 4 6)))
(define a 3)
(define b (+ a 1))
(print (+ a b (* a b)))
(print (= a b))
(print (if (and (> b a) (< b (* a b)))
	b
	a))

(print (cond ((= a 4) 6)
	((= b 4) (+ 6 7 a))
	(else 25)))

(print (+ 2 (if (> b a) b a)))

(print (* (cond ((> a b) a)
	((< a b) b)
	(else -1))
	(+ a 1)))

(print "=== ex 1.2 ===")
(print 
	(/
		(+ 5 4 (- 2 (- 3 (+ 6 (/ 4 5)))))
		(* 3 (- 6 2) (- 2 7))
	)
)

(print "=== ex 1.3 ===")
(define (square x) (* x x))
(define (sumOfSquares x y) (+ (square x) (square y)))
(define (maxSumOfSquares a b c)
	(define a-lowest (and (<= a b) (<= a c)))
	(define b-lowest (and (<= b a) (<= b c)))
	(cond 
	  (a-lowest (sumOfSquares b c))
	  (b-lowest (sumOfSquares a c))
	  (else (sumOfSquares a b))
	)
)
(print (maxSumOfSquares 1 2 3))
(print (maxSumOfSquares 1 1 1))
(print (maxSumOfSquares 1 2 2))
(print (maxSumOfSquares 1 1 2))
(print (maxSumOfSquares 1 4 3))

(print "=== ex 1.4 ===")
; a + |b|
(define (a-plus-abs-b a b)
	((if (> b 0) + -) a b)
)
(print (a-plus-abs-b 1 2)) ; 3
(print (a-plus-abs-b 1 -2)) ; 3

(print "=== ex 1.5 ===")
; applicative-order => does not terminate
; normal-order => terminates to 0

(print "=== ex 1.6 ===")
(define (average a b)
	(/ (+ a b) 2)
)
(define (improve guess x)
	(average guess (/ x guess))
)
(define tolerance 0.00001)
(define (good-enough? guess x)
	(< (abs (- (square guess) x)) tolerance)
)
(define (new-if predicate then-clause else-clause)
	(cond (predicate then-clause)
		(else else-clause)
	)
)
(define (new-sqrt-iter guess x)
	(new-if (good-enough? guess x)
		guess
		(sqrt-iter (improve guess x) x)
	)
)

(print "=== ex 1.7 ===")
(define (good-enough-2? guess x)
	(define tolerance (* x 0.00001))
	(< (abs (- (square guess) x)) tolerance)
)
(define (sqrt-iter-2 guess x)
	(if (good-enough? guess x)
		guess
		(sqrt-iter-2 (improve guess x) x)
	)
)
(print (sqrt-iter-2 0.01 0.0004))
(print (sqrt-iter-2 0.1 0.04))
(print (sqrt-iter-2 1.0 4.0))
(print (sqrt-iter-2 10.0 400.0))

(print "=== ex 1.8 ===")
(define (cube x) (* x x x))
(define (improve-cube-root target guess)
	(/ (+ (/ target (* guess guess)) (* 2 guess)) 3)
)
(define (good-enough-cube-root? target guess)
	(define tolerance (/ target 1000000.0))
	(define delta (abs (- target (cube guess))))
	(display "tolerance: ") (display tolerance) (newline)
	(display "delta: ") (display delta) (newline)
	(< delta tolerance)
)
(define (cube-root target guess)
	(define good-enough (good-enough-cube-root? target guess))
	(define improvement (improve-cube-root target guess))
	(display "target: ") (display target) (newline)
	(display "guess: ") (display guess) (newline)
	(display "good-enough: ") (display good-enough) (newline)
	(display "improvement: ") (display improvement) (newline)
	(if good-enough
	  guess
	  (cube-root target improvement)
	)
)
(print (cube-root 9.0 1.1))

(print "=== ex 1.9 ===")
; a = 4, b = 5
(if (= a 0) b (inc (+ (dec a) b)))
(if (= a 0) b (+ (+ (- a 1) b) 1))
(if (= 4 0) 5 (+ (+ (- 4 1) 5) 1))
(if #f 5 (+ (+ 3 5) 1))
(if #f 5 (+ 8 1))
(if #f 5 9)
9

