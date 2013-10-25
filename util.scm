;;; $Id: util.scm,v 1.5 2013/03/02 15:54:41 daniel Exp daniel $
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




;; some utility procedures


;; (define-meroon-macro (FLOAT . rest)
;;   `(let ()
;;      (declare (flonum))
;;      ,@rest))

;; (define-meroon-macro (FIX . rest)
;;   `(let ()
;;      (declare (fixnum))
;;      ,@rest))



(define (list-set! l k obj)
  (cond
   ((or (< k 0) (null? l)) #f)
   ((= k 0) (set-car! l obj))
   (else (list-set! (cdr l) (- k 1) obj))))

(define (foldr func end lst)
  (if (null? lst)
      end
      (func (car lst) (foldr func end (cdr lst)))))

(define (foldl func accum lst)
  (if (null? lst)
      accum
      (foldl func (func accum (car lst)) (cdr lst))))

(define (filter pred lst)   (foldr (lambda (x y) (if (pred x) (cons x y) y)) '() lst))


(define (symbol-append . symbols)
    (string->symbol
     (apply
      string-append
      (map symbol->string symbols))))


(define (attach-tag type-tag contents)
  (cons type-tag contents))

(define (type-tag datum)
  (if (pair? datum)
      (car datum)
      (error "Bad tagged datum -- TYPE-TAG" datum)))

(define (contents datum)
  (if (pair? datum)
      (cdr datum)
      (error "Bad tagged datum -- CONTENTS" datum)))

(define (time-n-times n-times proc . args)
  (time (let loop ((n 0))
          (if (< n n-times)
              (begin
                (apply  proc args)
                (set! n (+ n 1)) (loop n))))))

;;; some numerical operations
(define (rad-to-angle angle)
  (* ( fl/ 180.0 3.14) angle))

(define (angle-to-rad angle)
  (* (fl/ 3.14 180.0) angle))

(define (u8fit n)
  (declare (standard-bindings)
           (extended-bindings)
           (fixnum)
           (not safe))
  (cond ((##fx> n 255) 255)
        ((##fx< n 0) 0)
        (else n)))

(define (exact-round n)
  (inexact->exact (round n)))


(define (make-u8lut size proc)
  (let ((vec (make-u8vector size)))
    (let loop-i ((i 0))
      (cond ((fx< i size)
             (u8vector-set! vec i (proc i))
             (loop-i (fx+ i 1)))
            (#t (lambda (x)
                  (u8vector-ref vec x)))))))

(define (make-u8lut2 cols rows proc)
  (let ((vec (make-u8vector (* cols rows))))
    (let loop-i ((i 0))
      (cond ((fx< i rows)
             (let loop-j ((j 0))
               (cond ((fx< j cols)
                      (u8vector-set! vec (fx+ (fx* i cols) j) (proc j i))
                      (loop-j (fx+ j 1)))
                     (else (loop-i (fx+ i 1))))))
            (else
             (lambda (x y) (##u8vector-ref vec (##fx+ (##fx* y cols) x))))))))


(define (make-lut size proc)
  (let ((vec (make-vector size)))
    (let loop-i ((i 0))
      (cond ((< i size)
             (vector-set! vec i (proc i))
             (loop-i (+ i 1)))
            (#t (lambda (x)
                  (vector-ref vec x)))))))

;; Copyright (C) Scott G. Miller (2002). All Rights Reserved.


(define format
  (lambda (format-string . objects)
    (let ((buffer (open-output-string)))
      (let loop ((format-list (string->list format-string))
                 (objects objects))
        (cond ((null? format-list) (get-output-string buffer))
              ((char=? (car format-list) #\~)
               (if (null? (cdr format-list))
                   (error 'format "Incomplete escape sequence")
                   (case (cadr format-list)
                     ((#\a)
                      (if (null? objects)
                          (error 'format "No value for escape sequence")
                          (begin
                            (display (car objects) buffer)
                            (loop (cddr format-list) (cdr objects)))))
	             ((#\s)
                      (if (null? objects)
                          (error 'format "No value for escape sequence")
                          (begin
                            (write (car objects) buffer)
                            (loop (cddr format-list) (cdr objects)))))
                     ((#\%)
                      (newline buffer)
                      (loop (cddr format-list) objects))
                     ((#\~)
                      (write-char #\~ buffer)
                      (loop (cddr format-list) objects))
                     (else
                      (error 'format "Unrecognized escape sequence")))))
              (else (write-char (car format-list) buffer)
                    (loop (cdr format-list) objects)))))))



