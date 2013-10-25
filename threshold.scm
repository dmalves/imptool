;;; $Id: threshold.scm,v 1.3 2013/03/02 15:54:41 daniel Exp daniel $
;;; $Name:  $

(declare
 (block)
 (inline)
 (inline-primitives)
 (inlining-limit 10000)
 (lambda-lift)
 (constant-fold)
 (standard-bindings)
 (extended-bindings)
 (proper-tail-calls)
 (safe))

;;;+-------------------------------------------------------------------------------------------------------------+
;;;|Thresholding                                                                                                 |
;;;+-------------------------------------------------------------------------------------------------------------+
;;;| Here we set a couple of function to threshold image channels                                                |
;;;+-------------------------------------------------------------------------------------------------------------+

(define (threshold! image condition channel-num)
  (let ((vec '()) (matrix (image-get-channel-matrix image channel-num)))
    (cond
     ((eq? (type-tag matrix) 'u8matrix)
      (set! vec (u8matrix-get-raw-vector matrix))
      (u8vector-map! vec condition))
     ((eq? (type-tag matrix) 'f32matrix)
      (set! vec (f32matrix-get-raw-vector matrix))
      (f32vector-map! vec condition ))
     (else (error "unknown matrix type")))))

(define (threshold-u8binary! image channel-num)
  (let ((condition (lambda (x)
                     (cond ((>= x 128) 255)
                           (else       0)))))
    (threshold! image condition channel-num)))

(define (threshold-u8binary-inverse! image channel-num)
  (let ((condition (lambda (x)
                     (cond ((< x 128) 255)
                           (else        0)))))
    (threshold! image condition channel-num)))


(define (threshold-u8binary-4levels! image channel-num)
  (let ((condition (lambda (x)
                     (cond ((< x 64)   0)
                           ((< x 128) 64)
                           ((< x 192) 128)
                           (else      255)))))
    (threshold! image condition channel-num)))


