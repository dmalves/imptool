;;; $Id: gnuplot.scm,v 1.5 2013/03/02 15:54:41 daniel Exp daniel $
;;; $Name:  $

(declare
 (block)
 (standard-bindings)
 (extended-bindings)
 (proper-tail-calls)
 (safe))


(define  (gnuplot-open)
  (open-process
   (list path: "/usr/bin/gnuplot" arguments: '("-p") stdin-redirection: #t)))

(define (gnuplot-send port cmds)
  (begin  (for-each (lambda (x)
                      (display x port)
                      (display "\n" port))
                    cmds)
          (force-output port)))

(define (create-temp-file)
  (with-input-from-process "mktemp" read-line))

(define (gnuplot-format-show-image img tempfile)
  (let* ((m (channel-matrix (image-channel img 0)))
         (w (matrix-cols m)) (h (matrix-rows m)) (nchannels (image-n-channels img))
                                        ; (str1 (format "set title 'Image rgb - W=~a H=~a'\n" w h))
         (str1 (format "set size ratio ~a\n" (fl/ (fixnum->flonum h) (fixnum->flonum w))))
         (gray (format "plot '~a' binary array=(~a,~a) flipy format='%uchar' with image\n"    tempfile w h))
         (rgb  (format "plot '~a' binary array=(~a,~a) flipy format='%uchar' with rgbimage\n" tempfile w h))
         (rgba (format "plot '~a' binary array=(~a,~a) flipy format='%uchar' with rgbalpha\n" tempfile w h)))
    (cond
     ((= nchannels 1)
      (string-append str1 "set palette gray\n" gray))
     ((= nchannels 3)
      (string-append str1 rgb))
     ((= nchannels 4)
      (string-append str1 rgba)))))

(define (gnuplot-show-image port img)
  (let* ((tempfile (create-temp-file)))
    (begin
      (save-raw-image img tempfile)
      (gnuplot-send port (list "reset\n"
                               (gnuplot-format-show-image img tempfile))))))

(define (gnuplot-format-histogram-plot-cmd hist file)
  (let ((cmd "") (size (length hist)))
    (let loop-i2 ((i2 0))
      (cond
       ((= i2 0)
        (set! cmd (string-append cmd (format  "plot '~a' u 1:~a w l lw 3 title 'channel ~a' "  file (+ i2 2) i2)))
        (loop-i2 (+ 1 i2)))
       ((< i2 size )
        (set! cmd (string-append cmd (format ", '~a' u 1:~a w l lw 3 title 'channel ~a'" file (+ i2 2) i2)))
        (loop-i2 (+ 1 i2)))
       (#t
        (set! cmd (string-append cmd "\n"))
        cmd)))))

(define (gnuplot-show-histogram port hist)
  (let ((colors 256) (f (create-temp-file)) (cmd ""))
    (begin
      (save-histogram-to-file hist f)
      (set! cmd (string-append cmd
                               "reset\n"
                               "set style function lines\n"))
      (set! cmd (string-append cmd (gnuplot-format-histogram-plot-cmd hist f)))
      (gnuplot-send port (list cmd)))))

(define (gnuplot-show-image-histogram port img hist)
  (let ((cmd "") (f-hist (create-temp-file)) (f-img (create-temp-file)))
    (begin
      (save-histogram-to-file hist f-hist)
      (save-raw-image img  f-img)
      (set! cmd (string-append cmd
                               "reset \n"
                               "unset xtic\n"
                               "unset ytic\n"
                                        ; "unset key\n"
                               "set rmargin 0\n"
                               "set lmargin 0\n"
                               "set tmargin 0\n"
                               "set bmargin 0\n"
                               "set size 1,1\n"
                               "set origin 0,0\n"
                               "set multiplot\n"
                               "set size 1,0.2\n"
                               "set origin 0.0,0.0\n"
                                        ; "set size ratio 0.75\n"
                               ))
      (set! cmd (string-append cmd (gnuplot-format-histogram-plot-cmd hist f-hist)))
      (set! cmd (string-append cmd "set size 1,0.8\n"
                               "set origin 0,0.2\n"
                                        ; "unset multiplot\n"
                                        ; "unset xtics\n"
                               ))
      (set! cmd (string-append cmd (gnuplot-format-show-image img f-img)))
      (gnuplot-send port (list cmd))
      (gnuplot-send port (list "unset multiplot\n")))))

(define (gnuplot-show-2-images-histogram port img1 img2 hist1 hist2)
  (let ((cmd "") (f-hist1 (create-temp-file)) (f-hist2 (create-temp-file))
        (f-img1 (create-temp-file)) (f-img2 (create-temp-file)))
    (begin
      (save-histogram-to-file hist1 f-hist1)
      (save-histogram-to-file hist2 f-hist2)
      (save-raw-image img1  f-img1)
      (save-raw-image img2  f-img2)
      (set! cmd (string-append cmd
                               "reset\n"
                               "unset xtic\n"
                               "unset ytic\n"
                               "set lmargin 0.1\n"
                               "set rmargin 0.1\n"
                               "set tmargin 0.1\n"
                               "set bmargin 0.1\n"
                               "set size 1,1\n"
                               "set origin 0,0\n"
                               "set multiplot\n"
                               "set size 0.5,0.2\n"
                               "set origin 0,0\n"))
      (set! cmd (string-append cmd (gnuplot-format-histogram-plot-cmd hist1 f-hist1)))
      (set! cmd (string-append cmd
                               "set size 0.5,0.2\n"
                               "set origin 0.5,0\n"))
      (set! cmd (string-append cmd (gnuplot-format-histogram-plot-cmd hist2 f-hist2)))
      (set! cmd (string-append cmd
                               "set size 0.5,0.8\n"
                               "set origin 0,0.2\n"))
      (set! cmd (string-append cmd (gnuplot-format-show-image img1 f-img1)))
      (set! cmd (string-append cmd
                               "set origin 0.5,0.2\n"))
      (set! cmd (string-append cmd (gnuplot-format-show-image img2 f-img2)))
      (gnuplot-send port (list cmd))
      (gnuplot-send port (list "unset multiplot\n")))))
