#!/usr/bin/env gsi-script
;;; $Id: profile-test.scm,v 1.2 2013/03/02 15:54:41 daniel Exp daniel $
;;; $Name:  $

(define (main)

  (load "imptool.scm")
  (load-all-files)
  (test-convolve))
