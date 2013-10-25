;;; $Id: simple-tests.scm,v 1.2 2013/03/02 15:54:41 daniel Exp daniel $
;;; $Name:  $

;; (display "loading image...\n")
;; (define rgbImage (make-image-from-png-file "test-images/cute.png"))
;; (display "converting rgb image to hsi ...\n")
;; (define hsiImage (rgb-image-to-hsi-image rgbImage))
;; (display "converting hsi image back to rgb ...\n")
;; (define rgb2Image (hsi-image-to-rgb-image hsiImage))
;; (display "all done.\n")

(define cute (make-image-from-png-file "test-images/cute.png"))
(define cute-gray (rgb-image-to-gray-image cute))
(define cute-dither (dithering-gray 'error-difusion cute-gray))
(save-image-as-raw-file cute-dither "~/teste.raw")
(define plot (gnuplot-open))
(gnuplot-show-image plot cute-dither)
