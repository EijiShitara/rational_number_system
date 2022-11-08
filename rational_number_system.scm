(define (make-rat x y) 
	(let ((g (gcd x y))) 
	(cond
	((< (* x y) 0)
		(cons (/ (- 0 (abs x)) g) (/ (abs y) g)))
	(else
		(cons (/ (abs x) g) (/ (abs y) g)))
)))



(define (denom x) (cdr x))
(define (numer x) (car x))

(define (add-rat x y)
	(make-rat 
		(+ (* (denom y) (numer x)) (* (denom x) (numer y)))
		(* (denom x) (denom y)) ))

(define (sub-rat x y)
	(make-rat 
		(- (* (denom y) (numer x)) (* (denom x) (numer y)))
		(* (denom x) (denom y)) ))

(define (mul-rat x y)
	(make-rat 
		(* (numer x) (numer y))
		(* (denom x) (denom y)) ))

(define (div-rat x y)
	(make-rat 
		(* (numer x) (denom y))
		(* (denom x) (numer y)) ))

(define (equal-rat? x y)
	(= (* (numer x) (denom y)) (* (denom x) (numer y))))

(define (print-rat x) 
	(newline)
	(display (numer x))
	(display "/")
	(display (denom x)))




(define (average-damp f)
  (lambda (x) (/ (+ x (f x)))))



(define (list-ref items n)
  (if (= n 0)
      (car items)
      (list-ref (cdr items) (- n 1))))

(define (length items)
  (if (null? items)
      0
      (+ 1 (length (cdr items)))))

(map sqrt '(1 2 3 4 5))

