;;; $Id: sc-filter.scm,v 1.3 2013/03/02 15:54:41 daniel Exp daniel $
;;; $Name:  $


;; symmetrical and convolution filter
(define (make-scfilter s8vector rank scale)
  (attach-tag 'filter (list rank scale s8vector)))




(define (scfilter? filter)
  (eq? (type-tag filter) 'filter))

(define (scfilter-get-rank filter)
  (car (contents filter)))

(define (scfilter-get-scale filter)
  (car (cdr (contents filter))))

(define (scfilter-get-vector filter)
  (car (cdr (cdr (contents filter)))))



(define (scfilter-u8vector-convolution-sum
         u8vec rank x y scfilter-vec cols2 rows2)
  (let ((sum 0) (i0 (quotient cols2 2)) (j0 (quotient rows2 2)))
    (let loop-i ((i (- i0)))
      (if (<= i i0)
          (begin
            (let loop-j ((j (- j0)))
              (if (<= j j0)
                  (begin
                    (set! sum (+ sum (* (u8vector/2d-ref u8vec rank
                                                         (+ x i) (+ y j))
                                        (s8vector/2d-ref scfilter-vec rank
                                                         (+ i0 i) (+ j0 j)))))
                    (loop-j (+ j 1)))))
            (loop-i (+ i 1)))))
    sum))


; 
(define (scfilter-apply! image channel-num scfilter)
  (let ((vec '()) (matrix (image-get-channel-matrix image channel-num))
        (cols '()) (rows '())
        (rank '(scfilter-get-rank scfilter)) (scale (scfilter-get-scale scfilter))
        (s8vec (scfilter-get-vector)))
    (cond
     ((eq? (type-tag matrix) 'u8matrix)
      (set! vec  (u8matrix-get-raw-vector matrix))
      (set! cols (u8matrix-get-num-cols   matrix))
      (set! rows (u8matrix-get-num-rows   matrix))
      (let loop-x ((x 0))
        (cond
         ((< x cols)
          (let loop-y ((y 0))
            (cond
             ((< x rows)
              (cond
               ((not (hit-border? x y cols1 rows1 cols2 rows2))
                (scfilter-u8vector-convolution-sum u8vec rank x y scfilter-vec cols2 rows2)))))))))))))
