;;; $Id: ffi.scm,v 1.2 2013/03/02 15:54:41 daniel Exp daniel $
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


(c-declare #<<c-declare-end

#include <stdlib.h>
#include <stdint.h>

c-declare-end
)


(define make-int*
  (c-lambda () (pointer int)
    "___result_voidstar = malloc(sizeof(int));"))

(define int*-write
  (c-lambda ((pointer int) int) void
    "*(int*)___arg1_voidstar = ___arg2;"))

(define int*-read
  (c-lambda ((pointer int)) int
    "___result = *(int*)___arg1_voidstar;"))

(define free-int*
  (c-lambda ((pointer int)) void
    "free(___arg1);"))

(define free
  (c-lambda ((pointer void #f)) void "free((void*)___arg1);"))

(define (free-uchar-buffer buffer)
  (let ((proc (c-lambda ((pointer unsigned-char)) void
                  "free(___arg1);")))
    (proc buffer)))


(define (u8-buffer->u8vector data data-size increment offset)
  (let ((proc (c-lambda ((pointer unsigned-char) int int int scheme-object) void
               " ___U8 *v = ___CAST(___U8*, ___BODY_AS(___arg5, ___tSUBTYPED));
                  int offset = ___arg4;
                  int  inc = ___arg3;
                  int size = ___arg2;
                  unsigned char *data = ___arg1;
                  int i, j;

                  for(i = offset, j = 0; i < size; i = i + inc, j++) {
                     v[j] = data[i];
                  }"))
        (u8vec (make-u8vector (/ data-size increment))))
    (begin
      (proc data data-size increment offset u8vec)
      u8vec)))

(define (u8vector->u8-buffer u8vec)
  (let ((proc (c-lambda (scheme-object int) (pointer unsigned-char)
                " ___U8 *v = ___CAST(___U8*, ___BODY_AS(___arg1, ___tSUBTYPED));
                  int size = ___arg2;
                  unsigned char *data = (unsigned char*)malloc(size*sizeof(unsigned char));
                  int i;

                  for(i = 0; i < size; i++) {
                     data[i] = v[i];
                  }
                  ___result_voidstar = data;")))
    (proc u8vec (u8vector-length u8vec) )))


(define (make-u8-buffer size)
  (let ((proc (c-lambda (int) (pointer unsigned-int8)
                "
                int size = ___arg1;
                unsigned char* buffer;

                buffer = (uint8_t*) (malloc(size*sizeof(uint8_t)));
                ___result_voidstar = buffer;
                ")))
    (proc size)))

(define (free-u8-buffer buffer)
  (let ((proc (c-lambda ((pointer unsigned-int8)) void
                  "free(___arg1);")))
    (proc buffer)))

(define (u8-buffer-get buffer index)
  (let ((proc (c-lambda ((pointer unsigned-int8) int) unsigned-int8 
                "___result =(uint8_t) ___arg1[___arg2];")))
    (proc buffer index)))

(define (u8-buffer-set! buffer index value)
  (let ((proc (c-lambda ((pointer unsigned-int8) int int) void
                "___arg1[___arg2] =(uint8_t) ___arg3;")))
    (proc buffer index value)))



