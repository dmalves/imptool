;;; $Id: image.scm,v 1.9 2013/03/02 15:54:41 daniel Exp daniel $
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
 (mostly-fixnum-flonum)
 (safe))

;;;+---------------------------------------------/----------------------------------------------------------+
;;;|  TODO         /-----------------------------                                                           |
;;;+---------------                                                                                         |
;;;|* code better set-pixel and get-pixel procedure                                                         |
;;;|* need to check if image is rgb or rgba on procedure image->u8vector                                    |
;;;+--------------------------------------------------------------------------------------------------------+


;;;+--------------------------------------------------------------------------------------------------------+
;;;| image                                                                                                  |
;;;| Procedures do create, access and modify image records                                                  |
;;;+--------------------------------------------------------------------------------------------------------+
;;;| image types                                                                                            |
;;;| * gray - 1 channel  / 8 bits per pixel                                                                 |
;;;| * rgb  - 3 channels / each channel 8 bits per pixel / total of 24 bits per pixel                       |
;;;| * rgba - 4 channels / each channel 8 bits per pixel / total of 24 bits per pixel                       |
;;;| * hsi  - 3 channels / each channel 32 bits per pixel / total 96 bits per pixel                         |
;;;+--------------------------------------------------------------------------------------------------------+
;;;| Exported procedures:                                                                                   |
;;;|                                                                                                        |
;;;|* make-gray-image:                                                                                      |
;;;|  usage: (make-gray-image cols rows)                                                                    |
;;;|                                                                                                        |
;;;|* make-rgb-image:                                                                                       |
;;;|  usage: (make-rgb-image cols rows)                                                                     |
;;;|                                                                                                        |
;;;|* make-rgba-image:                                                                                      |
;;;|  usage: (make-rgba-image cols rows)                                                                    |
;;;|                                                                                                        |
;;;|* make-hsi-image:                                                                                       |
;;;|  usage: (make-hsi-image cols rows)                                                                     |
;;;|                                                                                                        |
;;;|* image-channel                                                                                         |
;;;|  usage: (image-channel image number)                                                                   |
;;;|       | number must be 0 to n-1, where n is the number of channels                                     |
;;;|  return: image channel record                                                                          |
;;;|                                                                                                        |
;;;|* image-channel-matrix:                                                                                 |
;;;|  usage: (image-channel-matrix image number)                                                            |
;;;|  return: the matrix of a given channel number                                                          |
;;;|                                                                                                        |
;;;|* image-matrixes:                                                                                       |
;;;|  usage: (image-matrixes image)                                                                         |
;;;|  return: a list of all channel's matrixes                                                              |
;;;|                                                                                                        |
;;;|* image-channel-vector:                                                                                 |
;;;|  usage: (image-channel-vector image number)                                                            |
;;;|  return: return matrix of a channel as a vector                                                        |
;;;|                                                                                                        |
;;;|* image-vectors:                                                                                        |
;;;|  usage: (image-vectors image)                                                                          |
;;;|  return: a list of all channels vectors                                                                |
;;;|                                                                                                        |
;;;|* image-matrix-by-color                                                                                 |
;;;|  usage: (image-matrix-by-color image color)                                                            |
;;;|       | color is the name of a channel: 'red, 'green, 'blue, 'gray, 'alpha, hue,                           |
;;;|       | saturation, intensity                                                                          |
;;;|  return: a channel                                                                                     |
;;;|                                                                                                        |
;;;|* image-rows and image-cols:                                                                            |
;;;|  usage: (image-rows image) or (image-cols image)                                                       |
;;;|                                                                                                        |
;;;|* image-size:                                                                                           |
;;;|  usage: (image-size image)                                                                             |
;;;|  return: image area (cols x rows)                                                                      |
;;;|                                                                                                        |
;;;|* image-copy:                                                                                           |
;;;|  usage: (image-copy image)                                                                             |
;;;|  return: a copy of image                                                                               |
;;;|                                                                                                        |
;;;|* channel-copy                                                                                          |
;;;|  usage: (channel-copy channel)                                                                         |
;;;|  return: a copy of a channel                                                                           |
;;;|                                                                                                        |
;;;|* channel-map:                                                                                          |
;;;|  usage: (channel-map lambda-exp channel)                                                               |
;;;|       | apply lambda-exp to each element of a channel                                                  |
;;;|  return: a new channel                                                                                 |
;;;|                                                                                                        |
;;;|* channel-map!:                                                                                         |
;;;|  usage: (channel-map! lambda-exp channel)                                                              |
;;;|       | apply lambda-exp to each element of a channel                                                  |
;;;|                                                                                                        |
;;;|* image-map:                                                                                            |
;;;|  usage: (image-map lambda-exp image)                                                                   |
;;;|       | apply lambda-exp to each element of every channel of a image                                   |
;;;|  return: a new image                                                                                   |
;;;|                                                                                                        |
;;;|* image-map!:                                                                                           |
;;;|  usage: (image-map! lambda-exp image)                                                                  |
;;;|       | apply lambda-exp to each element of every channel of a image                                   |
;;;|                                                                                                        |
;;;|* image->u8vector                                                                                       |
;;;|  usage: (image->u8vector image)                                                                        |
;;;|       | convert each image pixel to a sequence of rgb or rgba bytes                                    |
;;;|                                                                                                        |
;;;|                                                                                                        |
;;;+--------------------------------------------------------------------------------------------------------+



(define-structure channel name matrix)

(define-structure image name n-channels channels)

(define (make-gray-image cols rows)
  (make-image 'gray-image 1 (list (make-channel 'gray (make-u8matrix cols rows)))))

(define (make-rgb-image cols rows)
  (make-image 'rgb-image 3 (list (make-channel 'red   (make-u8matrix cols rows))
                                 (make-channel 'green (make-u8matrix cols rows))
                                 (make-channel 'blue  (make-u8matrix cols rows)))))

(define (make-rgba-image cols rows)
  (make-image 'rgba-image 4 (list (make-channel 'red   (make-u8matrix cols rows))
                                  (make-channel 'green (make-u8matrix cols rows))
                                  (make-channel 'blue  (make-u8matrix cols rows))
                                  (make-channel 'alpha     (make-u8matrix cols rows)))))

(define (make-hsi-image cols rows)
  (make-image 'hsi-image 3  (list (make-channel 'hue        (make-f32matrix cols rows))
                                  (make-channel 'saturation (make-f32matrix cols rows))
                                  (make-channel 'intensity  (make-f32matrix cols rows)))))

(define (image-channel image num)
  (list-ref (image-channels image) num))

(define (image-channel-matrix image num)
  (channel-matrix (image-channel image num)))

(define (image-matrixes image)
  (map (lambda (ch) (channel-matrix ch)) (image-channels image)))

(define (image-channel-vector image num)
  (matrix-vector (image-channel-matrix image num)))

(define (image-vectors image)
  (map (lambda (ch)
         (matrix-vector (channel-matrix ch)))
       (image-channels image)))

(define (image-matrix-by-color image color)
  (car (filter (lambda (x) (eq? x color)) (image-channels image))))

(define (image-rows image)
  (matrix-rows (channel-matrix (image-channel image 0))))

(define (image-cols image)
  (matrix-cols (channel-matrix (image-channel image 0))))

(define (image-size image)
  (* (image-cols image) (image-rows image)))

(define (channel-copy channel)
  (make-channel (channel-name channel) (matrix-copy (channel-matrix channel))))

(define (image-copy image)
  (let ((copy '()) (name (image-name image))
        (cols (image-cols image))
        (rows (image-rows image))
        (ch1 '()) (ch2 '()) (size (image-n-channels image)))
    (begin
      (cond ((eq? name 'gray-image)
             (set! copy (make-gray-image cols rows)))
            ((eq? name 'rgb-image)
             (set! copy (make-rgb-image cols rows)))
            ((eq? name 'rgba-image)
             (set! copy (make-rgba-image cols rows)))
            ((eq? name 'hsi-image)
             (set! copy (make-hsi-image cols rows))))
      (image-channels-set! copy '())
      (map (lambda (ch) (image-channels-set!
                    copy (cons (channel-copy ch) (image-channels copy))))
           (reverse (image-channels image)))
      copy)))




(define (channel-map func channel)
  (let ((ch (channel-copy channel))
        (m  (channel-matrix channel)))
    (begin
      (channel-matrix-set! ch (matrix-map func m))
      ch)))

(define (channel-map! func channel)
  (matrix-map! func (channel-matrix channel)))

(define (image-map func image)
  (let* ((img2     (image-copy image))
         (channels (image-channels img2)))
    (begin
      (map
       (lambda (ch) (channel-map! func ch))
       channels)
      img2)))

(define (image-map! func image)
  (map (lambda (ch) (channel-map! func ch)) (image-channels image)))


(define (image->u8vector img)
  (let ((size (* (matrix-cols (channel-matrix (image-channel img 0)))
                 (matrix-rows (channel-matrix (image-channel img 0)))))
        (v-list '())
        (n (image-n-channels img)))
    (begin
      (let loop-i ((i 0))
        (cond ((< i n)
               (set! v-list (append v-list (list (matrix-vector (channel-matrix (image-channel img i))))))
               (loop-i (+ i 1)))))
                        (with-output-to-u8vector
                         '() (lambda () (let loop-j ((j 0))
                                     (cond ((< j size)
                                            (for-each (lambda (v) (write-u8 (u8vector-ref v j))) v-list)
                                            (loop-j (+ j 1))))))))))


;TODO
; pixel is a vector
; the size of the vector depends on the number of channels you are dealing with!!!
(define (image-pixel-set! image col row pixel)
  (let loop-i ((i 0) (n (image-n-channels image)) (matrixes (image-matrixes image)))
    (cond ((< i n)
           (matrix-set! (car matrixes) col row (vector-ref pixel i))
           (loop-i (+ 1 i) n  (cdr matrixes))))))

(define (image-pixel image col row)
  (list->vector ( map (lambda (m) (matrix-ref m col row)) (image-matrixes image))))
