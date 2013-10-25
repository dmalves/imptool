;;; $Id: matrix.scm,v 1.7 2013/03/02 15:54:41 daniel Exp daniel $
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



;;;+----------------------------------------------------------------------------------------------------------+
;;;| matrix                                                                                                   |
;;;| Procedures to access and change 2D matrixes.                                                             |
;;;+----------------------------------------------------------------------------------------------------------+
;;;| Exported procedures:                                                                                     |
;;;|                                                                                                          |
;;;|* make-u8matrix,..., make-f64matrix:                                                                      |
;;;|  usage: (make-u8matrix cols rows)                                                                        |
;;;|                                                                                                          |
;;;|* matrix-ref:                                                                                             |
;;;|  usage: (matrix-ref col row)                                                                             |
;;;|                                                                                                          |
;;;|* matrix-set!:                                                                                            |
;;;|  usage: (matrix-set! col row value)                                                                      |
;;;|                                                                                                          |
;;;|* matrix-copy:                                                                                            |
;;;|  usage: (matrix-copy matrix)                                                                             |
;;;|                                                                                                          |
;;;|* matrix-shape:                                                                                           |
;;;|  usage: (matrix-shape matrix)                                                                            |
;;;|  returns: a pair (cols rows)                                                                             |
;;;|                                                                                                          |
;;;|* matrix-same-shape?:                                                                                     |
;;;|  usage: (matrix-same-shape? matrix)                                                                      |
;;;|                                                                                                          |
;;;|* matrix-map:                                                                                             |
;;;|  usage: (matrix-map lambda-expr matrix)                                                                  |
;;;|  where: lambda-expr is a function                                                                        |
;;;|                                                                                                          |
;;;|* matrix-map!:                                                                                            |
;;;|  usage: (matrix-map! lambda-expr matrix)                                                                 |
;;;|                                                                                                          |
;;;|* matrix-add:                                                                                             |
;;;|  usage: (matrix-add matrix1 matrix2)                                                                     |
;;;|                                                                                                          |
;;;|* matrix-add!:                                                                                            |
;;;|  usage: (matrix-add! matrix1 matrix2)                                                                    |
;;;|  result: matrix1 = matrix1 + matrix2                                                                     |
;;;|                                                                                                          |
;;;|* matrix-sub:                                                                                             |
;;;|  usage: (matrix-sub matrix1 matrix2)                                                                     |
;;;|                                                                                                          |
;;;|* matrix-sub!:                                                                                            |
;;;|  usage: (matrix-sub! matrix1 matrix2)                                                                    |
;;;|  result: matrix1 = matrix1 - matrix2                                                                     |
;;;|                                                                                                          |
;;;|* matrix-mul:                                                                                             |
;;;|  usage: (matrix-mul matrix1 matrix2)                                                                     |
;;;|  result: a matrix that is the product of each corresponding element os matrix1 and matrix2               |
;;;|                                                                                                          |
;;;|* matrix-mul!:                                                                                            |
;;;|  usage: (matrix-mul! matrix1 matrix2)                                                                    |
;;;|                                                                                                          |
;;;|* matrix-add-scalar:                                                                                      |
;;;|  usage: (matrix-add-scalar matrix scalar)                                                                |
;;;|                                                                                                          |
;;;|* matrix-add-scalar!:                                                                                     |
;;;|  usage: (matrix-add-scalar! matrix scalar)                                                               |
;;;|                                                                                                          |
;;;|* matrix-mul-scalar:                                                                                      |
;;;|  usage: (matrix-mul-scalar matrix scalar)                                                                |
;;;|                                                                                                          |
;;;|* matrix-mul-scalar!:                                                                                     |
;;;|  usage: (matrix-mul-scalar! matrix scalar)                                                               |
;;;|                                                                                                          |
;;;+----------------------------------------------------------------------------------------------------------+





(define-structure matrix
  type cols rows size vector
  func-vector-ref func-vector-set! func-vector-map!
  func-vector-map func-vector/2d-ref func-vector/2d-set!
  func-fit)

(define (make-matrix-by-type type cols rows)
  (cond
   ((eq? type 'u8matrix)  (make-u8matrix cols rows))
   ((eq? type 's8matrix)  (make-s8matrix cols rows))
   ((eq? type 'u16matrix) (make-u16matrix cols rows))
   ((eq? type 's16matrix) (make-s16matrix cols rows))
   ((eq? type 'u32matrix) (make-u32matrix cols rows))
   ((eq? type 's32matrix) (make-s32matrix cols rows))
   ((eq? type 'u64matrix) (make-u64matrix cols rows))
   ((eq? type 's64matrix) (make-s64matrix cols rows))
   ((eq? type 'f32matrix) (make-f32matrix cols rows))
   ((eq? type 'f64matrix) (make-f64matrix cols rows))))

(define (make-u8matrix cols rows)
  (make-matrix 'u8matrix cols rows (fx* cols rows)
               (make-u8vector (fx* cols rows))
               u8vector-ref u8vector-set! u8vector-map!
               u8vector-map u8vector/2d-ref u8vector/2d-set! u8fit))

(define (make-s8matrix cols rows)
  (make-matrix 's8matrix cols rows (fx* cols rows)
               (make-s8vector (fx* cols rows))
               s8vector-ref s8vector-set! s8vector-map!
               s8vector-map s8vector/2d-ref s8vector/2d-set! (lambda (x) x)))

(define (make-u16matrix cols rows)
  (make-matrix 'u16matrix cols rows (fx* cols rows)
               (make-u16vector (fx* cols rows))
               u16vector-ref u16vector-set! u16vector-map!
               u16vector-map u16vector/2d-ref u16vector/2d-set! (lambda (x) x)))

(define (make-s16matrix cols rows)
  (make-matrix 's16matrix cols rows (fx* cols rows)
               (make-s16vector (fx* cols rows))
               s16vector-ref s16vector-set! s16vector-map!
               s16vector-map s16vector/2d-ref s16vector/2d-set! (lambda (x) x)))

(define (make-u32matrix cols rows)
  (make-matrix 'u32matrix cols rows (fx* cols rows)
               (make-u32vector (fx* cols rows))
               u32vector-ref u32vector-set! u32vector-map!
               u32vector-map u32vector/2d-ref u32vector/2d-set! (lambda (x) x)))

(define (make-s32matrix cols rows)
  (make-matrix 's32matrix cols rows (fx* cols rows)
               (make-s32vector (fx* cols rows))
               s32vector-ref s32vector-set! s32vector-map!
               s32vector-map s32vector/2d-ref s32vector/2d-set! (lambda (x) x)))

(define (make-u64matrix cols rows)
  (make-matrix 'u64matrix cols rows (fx* cols rows)
               (make-u64vector (fx* cols rows))
               u64vector-ref u64vector-set! u64vector-map!
               u64vector-map u64vector/2d-ref u64vector/2d-set! (lambda (x) x)))

(define (make-s64matrix cols rows)
  (make-matrix 's64matrix cols rows (fx* cols rows)
               (make-s64vector (fx* cols rows))
               s64vector-ref s64vector-set! s64vector-map!
               s64vector-map s64vector/2d-ref s64vector/2d-set! (lambda (x) x)))

(define (make-f32matrix cols rows)
  (make-matrix 'f32matrix cols rows (fx* cols rows)
               (make-f32vector (fx* cols rows))
               f32vector-ref f32vector-set! f32vector-map!
               f32vector-map f32vector/2d-ref f32vector/2d-set! (lambda (x) x)))

(define (make-f64matrix cols rows)
  (make-matrix 'f64matrix cols rows (fx* cols rows)
               (make-f64vector (fx* cols rows))
               f64vector-ref f64vector-set! f64vector-map!
               f64vector-map f64vector/2d-ref f64vector/2d-set! (lambda (x) x)))

(define (matrix-ref matrix col row)
  ((matrix-func-vector/2d-ref matrix)
   (matrix-vector matrix) (matrix-cols matrix) col row))

(define (matrix-set! matrix col row value)
  ((matrix-func-vector/2d-set! matrix)
   (matrix-vector matrix) (matrix-cols matrix) col row value))

(define (matrix-copy matrix)
  (let* ((copy '()) (type (matrix-type matrix)) (cols (matrix-cols matrix))
         (rows (matrix-rows matrix)) (size (* cols rows))
         (v1 (matrix-vector matrix)) (v2 '())
         (func-set! (matrix-func-vector-set! matrix))
         (func-ref  (matrix-func-vector-ref matrix)))
    (begin
      (set! copy (make-matrix-by-type type cols rows))
      (set! v2   (matrix-vector copy))
      (let loop-i ((i 0))
        (cond ((fx< i size)
               (func-set! v2 i (func-ref v1 i))
               (loop-i (+ 1 i)))))
      copy)))

(define (matrix-shape matrix)
  (cons (matrix-cols matrix) (matrix-rows matrix)))

(define (matrix-same-shape? matrix1 matrix2)
  (and (= (matrix-cols matrix1) (matrix-cols matrix2))
       (= (matrix-rows matrix1) (matrix-rows matrix2))))

(define (matrix-map func m1)
  (let ((vec-map (matrix-func-vector-map m1))
        (m2 (matrix-copy m1)))
    (begin
      (matrix-vector-set! m2 (vec-map func (matrix-vector m1)))
      m2)))

(define (matrix-map! func m1)
  (let ((vec-map! (matrix-func-vector-map! m1)))
    (vec-map! func (matrix-vector m1))))

(define (matrix-map2 func m1)
  (let ((copy (matrix-copy m1))
        (cols (matrix-cols m1))
        (rows (matrix-rows m1))
        (vec/2d-ref  (matrix-func-vector/2d-ref  m1))
        (vec/2d-set! (matrix-func-vector/2d-set! m1)))
    (begin
      (let loop-x ((x 0))
        (cond ((< x cols)
               (let loop-y ((y 0))
                 (cond ((< y rows)
                        (func x y copy)
                        (loop-y (+ 1 y)))))
               (loop-x (+ 1 x)))))
      copy)))

;; It makes some operation op (procedure)
;; where the args of this procedure are
;; matrixes matrix1 and matrix2.
;; *** matrix1 is changed, holding the result
;;     of the procedure
(define (matrix-op! op matrix1 matrix2)
  (if (matrix-same-shape? matrix1 matrix2)
      (let* ((cols (matrix-cols matrix1))
             (rows (matrix-rows matrix1))
             (vec1 (matrix-vector matrix1))
             (vec2 (matrix-vector matrix2))
             (size  (matrix-size   matrix1))
             (vec1-set! (matrix-func-vector-set! matrix1))
             (vec1-ref (matrix-func-vector-ref  matrix1))
             (vec2-ref (matrix-func-vector-ref  matrix2)))
        (let loop-i ((i 0))
          (if (fx< i size)
              (begin
                (vec1-set! vec1 i (op (vec1-ref vec1 i) (vec2-ref vec2 i)))
                (loop-i (fx+ i 1))))))))

;; (define (matrix-map m1 func)
;;   (let* ((size (matrix-size m1))
;;          (cols (matrix-cols m1))
;;          (rows (matrix-rows m1))
;;          (m2   (make-matrix-by-type (matrix-type m1) cols rows))
;;          (vec2 (matrix-vector m2))
;;          (vec1 (matrix-vector m1))
;;          (vec-ref  (matrix-func-vector-ref  m1))
;;          (vec-set! (matrix-func-vector-set! m1)))
;;     (begin
;;       (let  loop-i ((i 0))
;;         (if (fx< i size)
;;             (begin
;;               (vec-set! vec2 i (func (vec-ref vec1 i)))
;;               (loop-i (+ 1 i)))))
;;       m2)))

;; It makes some operation op (procedure)
;; where the args of this procedure
;; are matrixes matrix1 and matrix2. It returns
;; a new matrix as result of this operation.
(define (matrix-op op m1 m2)
  (if (matrix-same-shape? m1 m2)
      (let* ((cols (matrix-cols m1))
             (rows (matrix-rows m1))
             (vec1 (matrix-vector m1))
             (vec2 (matrix-vector m2))
             (size (* cols rows))
             (m3   (make-matrix-by-type (matrix-type m1) cols rows))
             (vec3 (matrix-vector m3))
             (vec1-set! (matrix-func-vector-set! m1))
             (vec1-ref  (matrix-func-vector-ref  m1))
             (vec2-ref  (matrix-func-vector-ref  m2)))
        (begin
          (let loop-i ((i 0))
            (if (fx< i size)
                (begin
                  (vec1-set! vec3 i (op (vec1-ref vec1 i) (vec2-ref vec2 i)))
                  (loop-i (fx+ i 1)))))
          m3))))

;; m3(i,j) = m1(i,j) + m2(i,j)
(define (matrix-add m1 m2)
  (matrix-op (lambda (x y) ((matrix-func-fit m1) (+ x y))) m1 m2))

;; m1(i,j) += m2(i,j)
(define (matrix-add! m1 m2)
  (matrix-op! (lambda (x y) ((matrix-func-fit m1) (+ x y))) m1 m2))

;; m3(i,j) = m1(i,j) * m2(i,j)
(define (matrix-mul m1 m2)
  (matrix-op (lambda (x y) ((matrix-func-fit m1) (* x y))) m1 m2))

;;  m1(i,j) *=  m2(i,j)
(define (matrix-mul! m1 m2)
  (matrix-op! (lambda (x y) ((matrix-func-fit m1) (* x y))) m1 m2))

;; m3(i,j) = m1(i,j) - m2(i,j)
(define (matrix-sub m1 m2)
  (matrix-op (lambda (x y) ((matrix-func-fit m1) (- x y))) m1 m2))

;;  m1(i,j) -=  m2(i,j)
(define (matrix-sub! m1 m2)
  (matrix-op! (lambda (x y) ((matrix-func-fit m1) (- x y))) m1 m2))

;;  m1(i,j) += scalar
(define (matrix-add-scalar! m1 scalar)
  (matrix-map! m1 (lambda (x) ((matrix-func-fit m1) (+ x scalar)))))

;;  m3(i,j) = m1(i,j) + scalar
(define (matrix-add-scalar m1 scalar)
  (matrix-map m1 (lambda (x) ((matrix-func-fit m1) (+ x scalar)))))

;;  m(i,j) = m1(i,j) * scalar
(define (matrix-mul-scalar m1 scalar)
  (matrix-map m1 (lambda (x) ((matrix-func-fit m1) (* x scalar)))))

;;  m1(i,j) *= scalar
(define (matrix-mul-scalar! m1 scalar)
  (matrix-map! m1 (lambda (x) ((matrix-func-fit m1) (* x scalar)))))
