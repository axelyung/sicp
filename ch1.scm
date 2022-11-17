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


