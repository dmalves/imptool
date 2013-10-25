;;; $Id: filter.scm,v 1.2 2013/03/02 15:54:41 daniel Exp daniel $
;;; $Name:  $

(define-structure filter factor matrix)

(define (filter-cols filter)
  (matrix-cols (filter-matrix filter)))

(define (filter-rows filter)
  (matrix-rows (filter-matrix filter)))
