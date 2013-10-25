;;; $Id: edge.scm,v 1.2 2013/03/02 15:54:41 daniel Exp daniel $
;;; $Name:  $

(define (edge-gray img-gray)
  (let* ((filter (make-f32matrix 3 3))
         (mat (image-get-gray-matrix img-gray)))
    (begin
      (f32matrix-set! filter 0 0 0.)
      (f32matrix-set! filter 1 0 1.)
      (f32matrix-set! filter 2 0 0.)
      (f32matrix-set! filter 0 1 1.)
      (f32matrix-set! filter 1 1 -4.)
      (f32matrix-set! filter 2 1 1.)
      (f32matrix-set! filter 0 2 0.)
      (f32matrix-set! filter 1 2 1.)
      (f32matrix-set! filter 2 2 0.)
      (let loop-x ((x 0))
        (if (< x ))))))
