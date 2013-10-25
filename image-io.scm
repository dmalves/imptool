;;; $Id: image-io.scm,v 1.5 2013/03/02 15:54:41 daniel Exp daniel $
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
 (not interrupts-enabled)
 (safe))



(c-declare #<<c-declare-end
#include "stb_image.h"
#include "stb_image_write.h"
c-declare-end
)

;;;+-----------------------------------------------/---------------------------------------------------+
;;;|  TODO         /-------------------------------                                                    |
;;;+---------------                                                                                    |
;;;| add code on read-image and write-image to check file permitions, or any other file IO error       |
;;;|                                                                                                   |
;;;+---------------------------------------------------------------------------------------------------+


;;;+---------------------------------------------------------------------------------------------------+
;;;|image-io                                                                                           |
;;;|Read and write image files: it supports png and jpeg for reading and jpeg for writing              |
;;;+---------------------------------------------------------------------------------------------------+
;;;|Exported Procedures:                                                                               |
;;;|                                                                                                   |
;;;|* read-image:                                                                                      |
;;;|  usage: (read-image file-path)                                                                    |
;;;|  return: an image record                                                                          |
;;;|                                                                                                   |
;;;|                                                                                                   |
;;;|* image-write:                                                                                     |
;;;|  usage: (image-write file-path image)                                                             |
;;;|                                                                                                   |
;;;|                                                                                                   |
;;;+---------------------------------------------------------------------------------------------------+


(define (read-image image-file-path) 
  (let* ((proc (c-lambda
                   (nonnull-char-string (pointer int) (pointer int) (pointer int) int)
                   (pointer unsigned-char)
                 "stbi_load"))
        (w (make-int*)) (h (make-int*)) (n (make-int*)) (img '()) (mats '()) (vec '())
        (data (proc (path-expand image-file-path) w h n 0))
        (data-size (* (int*-read w) (int*-read h) (int*-read n))))
    (begin
      (cond ((= (int*-read n) 3) (set! img (make-rgb-image  (int*-read w) (int*-read h))))
            ((= (int*-read n) 4) (set! img (make-rgba-image (int*-read w) (int*-read h)))))
      (set! mats (image-matrixes img))
      (let loop ((i 0))
        (cond ((< i (int*-read n))
               (set! vec (u8-buffer->u8vector data data-size (int*-read n) i))
               (matrix-vector-set! (list-ref mats i) vec)
               (loop (+ i 1)))
              (else (free w)
                    (free h)
                    (free n)
                    (free data)
                    img))))))




(define (write-image image-path img)
  (let* ((proc (c-lambda
                  (nonnull-char-string int int int (pointer unsigned-char) int) bool
                "stbi_write_png"))
        (vec (image->u8vector img))
        (data (u8vector->u8-buffer vec))
        (boolean 0)
        (image-path (path-expand image-path)))
    (begin (set! boolean (proc image-path (image-cols img) (image-rows img) 3 data 0))
           (free data)
           boolean)))
