;;; $Id: matrix-io.scm,v 1.3 2013/03/02 15:54:41 daniel Exp daniel $
;;; $Name:  $

(declare
 (block)
 (standard-bindings)
 (extended-bindings)
 (proper-tail-calls)
 (safe))

;;;+-------------------------------------------------------------------------------------------------------+
;;;| matrix-io                                                                                             |
;;;| Procedures to help printing matrizes                                                                  |
;;;+-------------------------------------------------------------------------------------------------------+
;;;| Exported procedures:                                                                                  |
;;;|                                                                                                       |
;;;|* print-matrix:                                                                                        |
;;;|  usage: (print-matrix matrix)                                                                         |
;;;|                                                                                                       |
;;;|                                                                                                       |
;;;|                                                                                                       |
;;;|                                                                                                       |
;;;|                                                                                                       |
;;;|                                                                                                       |
;;;+-------------------------------------------------------------------------------------------------------+



(define (matrix-print matrix)
  (let ((rows (matrix-rows matrix)) (cols (matrix-cols matrix)))
    (begin
      (let loop1 ((j 0))
        (begin
          (if (< j rows)
              (begin
                (display "| ")
                (let loop2 ((i 0))
                  (if (< i cols)
                      (begin
                        (display (matrix-ref matrix i j))
                        (display " ")
                        (loop2 (+ i 1)))))
                (display "|\n")
                (loop1 (+ j 1)))))))))
