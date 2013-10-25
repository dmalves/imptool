;;; $Id: convolve.scm,v 1.2 2013/03/02 15:54:41 daniel Exp daniel $
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
 (fixnum)
 (safe))

;; convolve filters with odd number of rows
;;
(define (convolve-symmetric channel filter edge-action)
  (declare (standard-bindings)       ; we let the compiler know
           (extended-bindings)       ; that we want it to inline
           (inline)                  ; our local lambda functions
           (inline-primitives)       ; and all calculations are
           (inlining-limit 10000)    ; fixed number
           (fixnum)
           (not safe)
           (lambda-lift)
           (constant-fold))
  (let* ((v1 (matrix-vector (channel-matrix channel))) ; input channel vector
         (v2 (matrix-vector (filter-matrix filter)))   ; input filter vector
         (ch (channel-copy channel))
         (m3 (channel-matrix ch))
         (v3 (matrix-vector m3))                       ; output channel vector
         (cols1 (matrix-cols m3))                      ; cols - input channel
         (rows1 (matrix-rows m3))                      ; rows - input channel
         (cols2 (filter-cols filter))                  ; cols/rows - filter
         (rows2 (filter-rows filter))
         (cols2-over2 (fxquotient cols2 2))             ; half size of filter
         (rows2-over2 (fxquotient rows2 2))
         (sum 0) (factor (filter-factor filter))
         (irow 0) (icol 0) (frow 0) (fcol 0)
         (hit-border?
          (lambda (c r) ;; given c col and r row,
            ;;it checks if we hit a border of the image channel
            (or (##fx<  c cols2-over2)
                (##fx>= c (##fx- cols1 cols2-over2))
                (##fx<  r rows2-over2)
                (##fx>= r (##fx- rows1 rows2-over2)))))
         (convolution-loop
          (lambda (c r) ;; apply convolution and return the sum
            (begin
              (set! sum 0) ; we start the loop on the left top corner of the filter
              (let loop-i ((i (##fx- rows2-over2)))
                (cond
                 ((##fx<= i rows2-over2)
                  (set! irow (##fx* (##fx+ r i) cols1)) ; row on input channel
                  (set! frow (##fx* (##fx+ i rows2-over2) cols2)) ; row on filter
                  (let loop-j ((j (##fx- cols2-over2)))
                    (cond
                     ((##fx<= j cols2-over2)
                      (set! icol (##fx+ c j)) ; col on input channel
                      (set! fcol (##fx+ cols2-over2 j)) ; col on filter
                      (set! sum
                            (##fx+
                             sum (##fx*
                                  (##u8vector-ref  v1 (##fx+ irow icol))
                                  (##s16vector-ref v2 (##fx+ frow fcol)))))
                      (loop-j (##fx+ 1 j)))))
                  (loop-i (##fx+ i 1)))))
              sum))))
    (let loop-row ((row 0)) ;; each row and col point to a pixel on the input image
      (cond ((##fx< row rows1)
             (let loop-col ((col 0))
               (cond ((##fx< col cols1)
                      (cond ((hit-border? col row)          ;; if we hit the border
                             (cond ((eq? edge-action 'HIGH) ;; set border to high value
                                    (##u8vector-set! v3 (##fx+ (##fx* cols1 row) col) 255))
                                   ((eq? edge-action 'ZERO) ;; set border to zero
                                    (##u8vector-set! v3 (##fx+ (##fx* cols1 row) col) 0))))
                            (else (##u8vector-set!
                                   v3
                                   (##fx+ (##fx* cols1 row) col)
                                   (u8fit (##fxquotient (convolution-loop col row) factor)))))
                      (loop-col (##fx+ col 1)))))
             (loop-row (##fx+ 1 row)))
            (else
             ch)))))

(define (test-convolve)
  (let* ((img  (read-image "images/lena.png"))
         (gray (rgb-image-to-gray-image img))
         (f    (make-filter 25 (make-s16matrix 5 5)))
         (img2 0)
         (ch 0))
    (begin
      (display "convolve test- image lena.png\n")
      (display "image info:\n")
      (display "file: lena.png\n")
      (display "cols: ") (display (image-cols img))
      (display "\nrows: ") (display (image-rows img))
      (display "\nfilter 5x5 smothing\n")
      (display "time to process:\n")
      (matrix-map! (lambda (x) 1) (filter-matrix f))
      (time (begin (set! ch (convolve-symmetric (image-channel gray 0) f 'ZERO)) 'done))
      (set! img2 (image-copy gray))
      (image-channels-set! img2 (list ch))
      (save-raw-image img2 "test-results/test-convolve-256-256-gray.raw")
      (display "\nimage saved on: test-results/test-convolve-256-256-gray.raw\n"))))

;; (define (convolve4 channel filter edge-action)
;;   (declare (standard-bindings)
;;            (extended-bindings)
;;            (inline)
;;            (inline-primitives)
;;            (inlining-limit 10000)
;;            (fixnum)
;;            (not safe))
;;   (let* ((v1 (matrix-vector (channel-matrix channel))) ; input channel vector
;;          (v2 (matrix-vector (filter-matrix filter)))   ; input filter vector
;;          (ch (channel-copy channel))
;;          (m3 (channel-matrix ch))
;;          (v3 (matrix-vector m3))                       ; output channel vector
;;          (cols1 (matrix-cols m3))
;;          (rows1 (matrix-rows m3))
;;          (cols2 (filter-cols filter))
;;          (rows2 (filter-rows filter))
;;          (cols-over2 (fxquotient cols2 2))
;;          (rows-over2 (fxquotient cols2 2))
;;          (sum 0) (offset-col 0) (offset-row 0) (scale (filter-scale filter))
;;          (irow 0) (i2 0))
;;     (let loop-row ((row 0)) ;; each row and col point to a pixel on the input image
;;       (cond ((##fx< row rows1)
;;              (let loop-col ((col 0))
;;                (cond ((##fx< col cols1)
;;                       (set! sum 0) ;; sum is the result of each matrix convolution
;;                       (cond ((or (##fx< col cols-over2) ;; check border hit
;;                                  (##fx< row rows-over2)
;;                                  (##fx>= col (##fx- cols1 cols-over2))
;;                                  (##fx>= row (##fx- rows1 rows-over2)))
;;                              (cond ((eq? edge-action 'HIGH) ;; set border to high value
;;                                     (##u8vector-set! v3 (##fx+ (##fx* cols1 row) col) 255))
;;                                    ((eq? edge-action 'ZERO) ;; set border to zero
;;                                     (##u8vector-set! v3 (##fx+ (##fx* cols1 row) col) 0))))
;;                             (else (begin (let loop-i ((i (##fx- rows-over2)))
;;                                            (cond ((##fx<= i rows-over2)
;;                                                   (set! irow (##fx+ row i))
;;                                                   (set! offset-row  (##fx* irow cols1))
;;                                                   (set! i2 (##fx* cols2 (##fx+ i rows-over2)))
;;                                                   (let loop-j ((j (##fx- cols-over2)))
;;                                                     (cond ((##fx<= j cols-over2)
;;                                                            (set! offset-col (##fx+ col j))
;;                                                            (set! sum
;;                                                                  (##fx+ sum
;;                                                                         (##fx*
;;                                                                          (##u8vector-ref  v1 (##fx+ offset-col offset-row))
;;                                                                          (##s32vector-ref v2 (##fx+ i2 (##fx+ j cols-over2))))))
;;                                                            (loop-j (##fx+ 1 j)))))
;;                                                   (loop-i (##fx+ i 1)))))
;;                                          (##u8vector-set! v3 (##fx+ (##fx* cols1 row) col) (u8fit (##fxquotient sum scale))))))
;;                       (loop-col (##fx+ col 1)))))
;;              (loop-row (##fx+ 1 row)))
;;             (else
;;              ch)))))

;; (define (convolve3 channel filter edge-action)
;;   (declare (standard-bindings)
;;            (extended-bindings)
;;            (inline)
;;            (inline-primitives)
;;            (inlining-limit 10000)
;;            (fixnum)
;;            (not safe))
;;   (let* ((v1 (matrix-vector (channel-matrix channel))) ; input channel vector
;;          (v2 (matrix-vector (filter-matrix filter)))   ; input filter vector
;;          (ch (channel-copy channel))
;;          (m3 (channel-matrix ch))
;;          (v3 (matrix-vector m3))                       ; output channel vector
;;          (cols1 (matrix-cols m3))
;;          (rows1 (matrix-rows m3))
;;          (cols2 (filter-cols filter))
;;          (rows2 (filter-rows filter))
;;          (cols-over2 (fxquotient cols2 2))
;;          (rows-over2 (fxquotient cols2 2))
;;          (sum 0) (offset-col 0) (offset-row 0) (scale (filter-scale filter))
;;          (irow 0) (bool1 #f) (bool2 #f)
;;          (i2 0))
;;     (let loop-row ((row 0)) ;; each row and col point to a pixel on the input image
;;       (cond ((##fx< row rows1)
;;              (let loop-col ((col 0))
;;                (cond ((##fx< col cols1)
;;                       (set! sum 0) ;; sum is the result of each matrix convolution
;;                       (let loop-i ((i (##fx- rows-over2)))
;;                         (cond ((##fx<= i rows-over2)
;;                                (set! irow (##fx+ row i))
;;                                (set! offset-row  (##fx* irow cols1))               ;; row offset for image
;;                                (set! i2 (##fx* cols2 (##fx+ i rows-over2)))        ;; row offset for filter
;;                                         ;(set! bool1 (##fx>= irow 0))
;;                                         ;(set! bool2 (##fx<  irow rows1))
;;                                (let loop-j ((j (##fx- cols-over2)))
;;                                  (cond ((##fx<= j cols-over2)
;;                                         (set! offset-col (##fx+ col j)) ;; col offset for image
;;                                         (cond ((and ;bool1                      ;; dont hit left of image
;;                                         ;bool2                      ;; dont hit right of image
;;                                                 (##fx>   irow rows-over2)
;;                                                 (##fx<=  irow (##fx- rows1 rows-over2))
;;                                                 (##fx>  offset-col cols-over2)      ;; dont hit bottom of image
;;                                                 (##fx<=  offset-col (##fx- cols1 cols-over2))) ;; dont hit top of image
;;                                                ;; process a no border pixel
;;                                                (set! sum
;;                                                      (##fx+ sum
;;                                                             (##fx* (##u8vector-ref  v1 (##fx+ offset-col offset-row))
;;                                                                    (##s32vector-ref v2 (##fx+ i2 (##fx+ j cols-over2)))))))
;;                                               (else ; border hit - we do the action set by edge-action
;;                                                (cond ((eq? edge-action 'ZERO)
;;                                                       (set! sum 255))
;;                                                      ((eq? edge-action 'CLAMP)
;;                                                       (;TODO: CLAMP
;;                                                        (set! sum 0)))
;;                                                      ((eq? edge-action 'WRAP)
;;                                                       (;TODO: WRAP
;;                                                        (set! sum 0)))
;;                                                      (else
;;                                                       (error "convolve3: edge-action unknown.")))))
;;                                         (loop-j (##fx+ 1 j)))))
;;                                (loop-i (##fx+ i 1)))))
;;                       (##u8vector-set! v3 (##fx+ (##fx* cols1 row) col) (u8fit (##fxquotient sum scale)))
;;                       (loop-col (##fx+ col 1)))))
;;              (loop-row (##fx+ 1 row)))
;;             (else
;;              ch)))))

;; (define (convolve2 channel filter edge-action)
;;   (declare (standard-bindings)
;;            (extended-bindings)
;;            (inline)
;;            (inline-primitives)
;;            (inlining-limit 10000)
;;            (fixnum)
;;            (not safe))
;;   (let* ((m1 (channel-matrix channel))
;;          (v1 (matrix-vector m1))
;;          (m2 (filter-matrix filter))
;;          (v2 (matrix-vector m2))
;;          (ch (channel-copy channel))
;;          (m3 (channel-matrix ch))
;;          (cols1 (matrix-cols m1))
;;          (rows1 (matrix-rows m1))
;;          (cols2 (filter-cols filter))
;;          (rows2 (filter-rows filter))
;;          (cols-over2 (fxquotient cols2 2))
;;          (rows-over2 (fxquotient cols2 2))
;;          (color 0) (sum 0) (iy 0) (jx 0) (scale (filter-scale filter)))
;;     (let loop-y ((y 0))
;;       (cond ((##fx< y rows1)
;;              (let loop-x ((x 0))
;;                (cond ((##fx< x cols1)
;;                       (set! sum 0)
;;                       (let loop-i ((i (##fx- rows-over2)))
;;                         (cond ((##fx<= i rows-over2)
;;                                (set! iy (##fx+ y i))
;;                                (let loop-j ((j (##fx- cols-over2)))
;;                                  (cond ((##fx<= j cols-over2)
;;                                         (set! jx (##fx+ x j))
;;                                         (cond ((and (##fx>= iy 0)
;;                                                     (##fx< iy rows1)
;;                                                     (##fx>= jx 0)
;;                                                     (##fx< jx cols1))
;;                                                (set! sum (##fx+ sum
;;                                                                 (##fx* (##u8vector-ref  v1 (##fx+ (##fx* cols1 iy) jx))
;;                                                                        (##s32vector-ref v2 (##fx+ (##fx* cols2 (##fx+ i rows-over2)) (##fx+ j cols-over2))))))))
;;                                         (loop-j (##fx+ 1 j)))))
;;                                (loop-i (##fx+ i 1)))))
;;                       (matrix-set! m3 x y (u8fit (##fxquotient sum scale)))
;;                       (loop-x (##fx+ x 1)))))
;;              (loop-y (##fx+ 1 y)))
;;             (else
;;              ch)))))

;; (define (convolve channel filter edge-action)
;;   (let* ((m1 (channel-matrix channel))
;;          (m2 (filter-matrix filter))
;;          (ch (channel-copy channel))
;;          (m3 (channel-matrix ch))
;;          (cols1 (matrix-cols m1))
;;          (rows1 (matrix-rows m1))
;;          (cols2 (filter-cols filter))
;;          (rows2 (filter-rows filter))
;;          (cols-over2 (fxquotient cols2 2))
;;          (rows-over2 (fxquotient cols2 2))
;;          (color 0) (sum 0) (iy 0) (jx 0))
;;     (let loop-y ((y 0))
;;       (cond ((< y rows1)
;;              (let loop-x ((x 0))
;;                (cond ((< x cols1)
;;                       (set! sum 0)
;;                       (let loop-i ((i (- rows-over2)))
;;                         (cond ((<= i rows-over2)
;;                                (set! iy (+ y i))
;;                                (let loop-j ((j (- cols-over2)))
;;                                  (cond ((<= j cols-over2)
;;                                         (set! jx (+ x j))
;;                                         (cond ((and (>= iy 0)
;;                                                     (< iy rows1)
;;                                                     (>= jx 0)
;;                                                     (< jx cols1))
;;                                                (set! sum (+ sum
;;                                                             (* (matrix-ref m1 jx iy)
;;                                                                (matrix-ref m2 (+ j cols-over2) (+ i rows-over2)))))))
;;                                         (loop-j (+ 1 j)))))
;;                                (loop-i (+ i 1)))))
;;                       (matrix-set! m3 x y (u8fit (fxquotient sum (filter-scale filter))))
;;                       (loop-x (+ x 1)))))
;;              (loop-y (+ 1 y)))
;;             (else
;;              ch)))))
