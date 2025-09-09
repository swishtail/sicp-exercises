#lang sicp
;; SICP Exercise 2.37
;; Matrix operations

(define dot-product
  (lambda (v w)
    (accumulate + 0 (map * v w))))

(define matrix-*-vector
  (lambda (m v)
    (map (lambda (row)
           (dot-product row v))
         m)))

(define transpose
  (lambda (m)
    (accumulate-n cons '() m)))

(define matrix-*-matrix
  (lambda (m n)
    (let ((cols (transpose n)))
      (map (lambda (row)
             (matrix-*-vector cols row))
           m))))

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
