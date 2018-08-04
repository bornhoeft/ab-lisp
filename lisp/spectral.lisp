;;; Copyright (c) 2009-2015, Achim Bornhoeft. All rights reserved.

;;; Redistribution and use in source and binary forms, with or without
;;; modification, are permitted provided that the following conditions
;;; are met:

;;;   * Redistributions of source code must retain the above copyright
;;;     notice, this list of conditions and the following disclaimer.

;;;   * Redistributions in binary form must reproduce the above
;;;     copyright notice, this list of conditions and the following
;;;     disclaimer in the documentation and/or other materials
;;;     provided with the distribution.

;;;; THIS SOFTWARE IS PROVIDED BY THE AUTHOR 'AS IS' AND ANY EXPRESSED
;;; OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
;;; WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
;;; ARE DISCLAIMED.  IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY
;;; DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
;;; DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE
;;; GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
;;; INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
;;; WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
;;; NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
;;; SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

(in-package :ab-lisp)

;;; Spectral functions

;;; code from: http://subsynth.sourceforge.net/midinote2freq.html
(defun midi-to-hertz (midi)
  (* (/ 440 32) (expt 2 (/ (- midi 9) 12))))

;; (midi-to-hertz 60) => 261.62555

;;; code from: http://www.musicdsp.org/showone.php?id=125
(defun hertz-to-midi (hertz &optional (precision 1))
  "Convert hertz to midi, precision 1 = semtiones, 0.5 = quartertones."
  (* 1.0 precision (round (+ 69.0 (* 12 (log (/ hertz 440) 2))) precision )))

;; (hertz-to-midi 220) => 57
;; (hertz-to-midi 454.536 4) => 70
;; (hertz-to-midi 454.536 0.5) => 69.5
;; (hertz-to-midi 454.536 1/3) => 69.666664

#|
To convert hertz to milliseconds, first determine the duration or period of one vibration by dividing one second by the frequency in hertz. For example, for a 500 Hz signal, the duration of one cycle is 1/500 or .002 seconds. To convert this figure to milliseconds, multiply it by 1,000. From the example, .002 seconds * 1,000 = 2 milliseconds. Each cycle in a 500 Hz signal takes 2 milliseconds to complete. Read more : http://www.ehow.com/how_6608111_convert-hertz-milliseconds.html
|#

(defun hz2msec (freq)
  "Convert hertz to milliseconds"
  (* 1000 (/ 1 freq)))

;; (hz2msec 500) => 2

(defun harmonic-delay (fundamental partials &optional (scale 1.0))
  (loop for i from 1 to partials
    collect (* 1000.0 (/ scale (* fundamental i)))))

;; (harmonic-delay 100 8 ) => (10.0 5.0 3.3333335 2.5 2.0 1.6666667 1.4285715 1.25)
;; (harmonic-delay 100 8 10) => (100.0 50.0 33.333336 25.0 20.0 16.666668 14.285714 12.5)

(defun subharmonic-delay (fundamental partials &optional (scale 10))
  (loop for i from 1 to partials
    collect (/ fundamental (/ scale i))))

;; (subharmonic-delay 100 8) => (10 20 30 40 50 60 70 80)
;; (subharmonic-delay 220 8 1) => (220 440 660 880 1100 1320 1540 1760)
