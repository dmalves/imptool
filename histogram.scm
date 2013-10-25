;;; $Id: histogram.scm,v 1.4 2013/03/02 15:54:41 daniel Exp daniel $
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


;;; +-----------------------------------------------------------------+
;;; |histogram - routines to manipulate image histograms              |
;;; |-----------------------------------------------------------------+
;;; |the histogram object can have the histogram of multiple channels |
;;; +-----------------------------------------------------------------+

(define (histogram-channel img channel)
  (let* ((m (channel-matrix (image-channel img channel)))
         (vec (matrix-vector m)) (size (matrix-size m)) (hist (make-vector 256)) (color 0) (old 0)
         (vec-ref (matrix-func-vector-ref m)))
    (begin
      (vector-fill! hist 0)
      (let loop-i ((i 0))
        (cond ((< i size)
               (set! color (vec-ref vec i))
               (set! old (vector-ref hist color))
               (vector-set! hist color (+ old 1))
               (loop-i (+ 1 i)))
              (#t hist))))))

(define (histogram img)
  (let ((n (image-n-channels img))
        (hist '()))
    (let loop-i ((i 0))
      (cond ((< i n)
             (set! hist (append (list (histogram-channel img i)) hist))
             (loop-i (+ 1 i)))
            (#t (reverse hist))))))

(define (save-histogram-to-file hist file)
  (let ((colors 256) (size (length hist)))
    (with-output-to-file file
      (lambda () (let loop-i ((i 0))
              (cond ((< i colors)
                     (display (format "~a" i))
                     (for-each (lambda (x) (display (format " ~a " (vector-ref x i)))) hist)
                     (display "\n")
                     (loop-i (+ 1 i)))))))))

;; (define (histogram-equalization img)
;;   (let* ((size (image-get-size img)) (hist (histogram-channel img 0)) (m (image-get-channel-matrix img 0))
;;          (vec (u8matrix-get-raw-vector m)) (sum 0.0) (sum_hist (make-vector 256)) (scale (fl/ 255.0 (fixnum->flonum size))))
;;     (begin
;;       (vector-fill! sum_hist 0)
;;       (let loop-i ((i 0))
;;         (cond ((< i 256)
;;                (set! sum (+ sum (vector-ref hist i)))
;;                (vector-set! sum_hist i (flround (* sum scale)))
;;                (loop-i (+ i 1)))))
;;       (let loop-j ((j 0))
;;         (cond ((< j size)
;;                (u8vector-set! vec j (inexact->exact (u8fit (flround (vector-ref sum_hist (u8vector-ref vec j))))))
;;                (loop-j (+ j 1))))))))
