#lang sicp
;; SICP Exercise 2.37
;; Matrix operations

(define dot-product
  (lambda (v w)
    (accumulate + 0 (map * v w))))

(define matrix-*-vector
  (lambda (m v)
    (map (lambda (mat) (accumulate + 0 (map * mat v))) m)))

(define transpose
  (lambda (mat)
    (accumulate-n cons '() mat)))

(define matrix-*-matrix
  (lambda (m n)
    (let ((cols (transpose n)))
      (map (lambda (mat) (matrix-*-vector cols mat)) m))))

(define accumulate
  (lambda (op initial sequence)
    (if (null? sequence)
	initial
	(op (car sequence)
	    (accumulate op initial (cdr sequence))))))

(define accumulate-n
  (lambda (op init seqs)
    (if (null? (car seqs))
	'()
	(cons (accumulate op init (map car seqs))
	      (accumulate-n op init (map cdr seqs))))))