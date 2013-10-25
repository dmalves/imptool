;;; $Id: srfi-4-util.scm,v 1.2 2013/03/02 15:54:41 daniel Exp daniel $
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

;;;+------------------------------------------------------------------------------------------------+
;;;| srfi-u-util                                                                                    |
;;;| Procedures for acess or change vectors of specific type.                                       |
;;;|------------------------------------------------------------------------------------------------+
;;;| Exported procedures:                                                                           |
;;;|                                                                                                |
;;;|* vector/2d-ref, u8vector/2d-ref, ...,f64vector/2d-ref:                                         |
;;;|  usage: (vector/2d-ref  vector rank col row)                                                   |
;;;|  where: rank = number of cols                                                                  |
;;;|                                                                                                |
;;;|* vector/2d-set!, u8vector/2d-set!, ..., f64vector/2d-set!:                                     |
;;;|  usage: (vector/2d-set! vector rank col row value)                                             |
;;;|                                                                                                |
;;;|* vector-map, u8vector-map, ..., f64vector-map:                                                 |
;;;|  usage: (vector-map lambda-expr vector)                                                        |
;;;|  where: lambda-expr is a procedure                                                             |
;;;|                                                                                                |
;;;|* vector-map!, u8vector-map!, ..., f64vector-map!:                                              |
;;;|  usage: (vector-map! lambda-expr vector)                                                       |
;;;|                                                                                                |
;;;+------------------------------------------------------------------------------------------------+



(define (make-func-vector/2d-ref vector-ref)
  (lambda (vec rank col row)
    (vector-ref vec (+ (* rank row) col))))

;; usage:
;; (vector/2d-ref vec rank col row)
(define vector/2d-ref    (make-func-vector/2d-ref vector-ref))
(define u8vector/2d-ref  (make-func-vector/2d-ref u8vector-ref))
(define s8vector/2d-ref  (make-func-vector/2d-ref s8vector-ref))
(define u16vector/2d-ref (make-func-vector/2d-ref u16vector-ref))
(define s16vector/2d-ref (make-func-vector/2d-ref s16vector-ref))
(define u32vector/2d-ref (make-func-vector/2d-ref u32vector-ref))
(define s32vector/2d-ref (make-func-vector/2d-ref s32vector-ref))
(define u64vector/2d-ref (make-func-vector/2d-ref s64vector-ref))
(define s64vector/2d-ref (make-func-vector/2d-ref s64vector-ref))
(define f32vector/2d-ref (make-func-vector/2d-ref f32vector-ref))
(define f64vector/2d-ref (make-func-vector/2d-ref f64vector-ref))

(define (make-func-vector/2d-set! vector-set!)
  (lambda (vec rank col row value)
    (vector-set! vec (fx+ (* rank row) col) value)))

;; usage:
;; (vector/2d-set! vec rank col row value)
(define vector/2d-set!    (make-func-vector/2d-set! vector-set!))
(define u8vector/2d-set!  (make-func-vector/2d-set! u8vector-set!))
(define s8vector/2d-set!  (make-func-vector/2d-set! s8vector-set!))
(define u16vector/2d-set! (make-func-vector/2d-set! u16vector-set!))
(define s16vector/2d-set! (make-func-vector/2d-set! s16vector-set!))
(define u32vector/2d-set! (make-func-vector/2d-set! u32vector-set!))
(define s32vector/2d-set! (make-func-vector/2d-set! s32vector-set!))
(define u64vector/2d-set! (make-func-vector/2d-set! s64vector-set!))
(define s64vector/2d-set! (make-func-vector/2d-set! s64vector-set!))
(define f32vector/2d-set! (make-func-vector/2d-set! f32vector-set!))
(define f64vector/2d-set! (make-func-vector/2d-set! f64vector-set!))

(define (make-func-vector-map! vector-length vector-ref vector-set!)
  (lambda (func vec)
    (let  ((len (vector-length vec)))
      (let loop-i ((i 0))
        (if (fx< i len)
            (begin
              (vector-set! vec i (func (vector-ref vec i)))
              (loop-i (fx+ i 1))))))))

;; vector-map!: it maps a function to all elements of a vector
;;              it modifies the vector!
;;                with func (function)
;;                     vec  (vector)
;; usage:
;;(vector-map! (lambda (x) (+ x 1)) vec)
(define vector-map!    (make-func-vector-map! vector-length    vector-ref    vector-set!))
(define u8vector-map!  (make-func-vector-map! u8vector-length  u8vector-ref  u8vector-set!))
(define s8vector-map!  (make-func-vector-map! s8vector-length  s8vector-ref  s8vector-set!))
(define u16vector-map! (make-func-vector-map! u16vector-length u16vector-ref u16vector-set!))
(define s16vector-map! (make-func-vector-map! s16vector-length s16vector-ref s16vector-set!))
(define u32vector-map! (make-func-vector-map! u32vector-length u32vector-ref u32vector-set!))
(define s32vector-map! (make-func-vector-map! s32vector-length s32vector-ref s32vector-set!))
(define u64vector-map! (make-func-vector-map! u64vector-length u64vector-ref u64vector-set!))
(define s64vector-map! (make-func-vector-map! s64vector-length s64vector-ref s64vector-set!))
(define f32vector-map! (make-func-vector-map! f32vector-length f32vector-ref f32vector-set!))
(define f64vector-map! (make-func-vector-map! f64vector-length f64vector-ref f64vector-set!))

(define (make-func-vector-map vector-length vector-ref vector-set! make-vector)
  (lambda (func vec)
    (let* ((len (vector-length vec)) (vec2 (make-vector len)))
      (let loop-i ((i 0))
        (if (fx< i len)
            (begin
              (vector-set! vec2 i (func (vector-ref vec i)))
              (loop-i (fx+ i 1)))
            vec2)))))


;; usage:
;; (vector-map (lambda (x) (+ x 1)) vec)
(define vector-map    (make-func-vector-map vector-length    vector-ref    vector-set!    make-vector))
(define u8vector-map  (make-func-vector-map u8vector-length  u8vector-ref  u8vector-set!  make-u8vector))
(define s8vector-map  (make-func-vector-map s8vector-length  s8vector-ref  s8vector-set!  make-s8vector))
(define u16vector-map (make-func-vector-map u16vector-length u16vector-ref u16vector-set! make-u16vector))
(define s16vector-map (make-func-vector-map s16vector-length s16vector-ref s16vector-set! make-s16vector))
(define u32vector-map (make-func-vector-map u32vector-length u32vector-ref u32vector-set! make-u32vector))
(define s32vector-map (make-func-vector-map s32vector-length s32vector-ref s32vector-set! make-s32vector))
(define u64vector-map (make-func-vector-map u64vector-length u64vector-ref u64vector-set! make-u64vector))
(define s64vector-map (make-func-vector-map s64vector-length s64vector-ref s64vector-set! make-s64vector))
(define f32vector-map (make-func-vector-map f32vector-length f32vector-ref f32vector-set! make-f32vector))
(define f64vector-map (make-func-vector-map f64vector-length f64vector-ref f64vector-set! make-f64vector))
