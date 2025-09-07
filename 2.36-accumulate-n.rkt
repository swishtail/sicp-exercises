#lang sicp
;; SICP Exercise 2.36
;; accumulate-n

(define accumulate-n
  (lambda (op init seqs)
    (if (null? (car seqs))
	'()
	(cons (accumulate op init (map car seqs))
	      (accumulate-n op init (map cdr seqs))))))

(define accumulate
  (lambda (op initial sequence)
    (if (null? sequence)
	initial
	(op (car sequence)
	    (accumulate op initial (cdr sequence))))))
