;;; Copyright (c) 2009, Achim Bornhoeft. All rights reserved.

;;; Redistribution and use in source and binary forms, with or without
;;; modification, are permitted provided that the following conditions
;;; are met:

;;;   * Redistributions of source code must retain the above copyright
;;;     notice, this list of conditions and the following disclaimer.

;;;   * Redistributions in binary form must reproduce the above
;;;     copyright notice, this list of conditions and the following
;;;     disclaimer in the documentation and/or other materials
;;;     provided with the distribution.

;;; THIS SOFTWARE IS PROVIDED BY THE AUTHOR 'AS IS' AND ANY EXPRESSED
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

;;; Quantisation

(defun index-no (val lis)
    "Returns the list index of key in row."
  (if (not (member val lis))
    (format t "Value is not in list!")
    (loop for i in lis
       for y from 0 do
       (when (= i val) (return y)))))
;; (index-no 4 '(3 24 5 76 1 99 49)) => NIL
;; (index-no 99 '(3 24 5 76 1 99 49)) => 5

(defun quant (num steps)
"Quantize a number to a defined step value (number or list of numbers)."
  (labels ((index-no (val lis)
    "Returns the list index of key in row."
  (if (not (member val lis))
    (format t "Value is not in list!")
    (loop for i in lis
       for y from 0 do
       (when (= i val) (return y))))))
  
  (let* ((stp (if (numberp steps) (list steps) steps))
	 (round-lis (loop for i in stp collect (* (round (/ num i)) i)))
	 (diff-lis (loop for i in round-lis collect (abs (- i num))))
	 (smallest (apply #'min diff-lis)))
    (nth (index-no smallest diff-lis) round-lis))))

;; (quant 5.231 '(.5 .2 .25)) => 5.25
;; (quant 5.342 .6) => 5.4
;; (loop for i in '(1 1.2 3 5.5 6.4 7.6 7.8 5.4 6.1 9.5) collect (quant i 0.4)) 
;; => (0.8 1.2 3.2 5.6 6.4 7.6 8.0 5.6 6.0 9.6)
;; (loop for i in '(1 1.2 3 5.5 6.4 7.1 7.8 5.4) collect (* 1.0 (quant i 1/7))) 
;; => (1.0 1.1428572 3.0 5.428571 6.428571 7.142857 7.857143 5.428571)
;; (quant 1 0.4) => 0.8 because (round (/ 1.0 0.4)) => 2, 0.5

(defun quant-list (lis steps)
"Quantisation of a list to a defined step value (number or list of numbers."
  (loop for i in lis collect (quant i steps)))
;; (quant-list '(1.25346 5.43765) 0.5)

(defun raster (lis quant-lis)
  (loop for i in lis collect
    (loop for j in quant-lis
      collect (abs (- i j)) into reslis
      finally (return (nth (position (apply #'min reslis) reslis) quant-lis)))))

#|
(raster '(1/12 5/27 31/108 7/18 53/108 16/27 25/36 43/54 97/108 1)
        '(1/12 1/10 1/9 1/8 1/7 1/6 1/5 1/4 1/3 1/2 1 2 3 4))
 => (1/12 1/5 1/4 1/3 1/2 1/2 1/2 1 1 1)
|#  


(defun quant-up (num step)
    (let ((q (* (round (/ num step)) step)))
	  (if (< q num) (+ q step) q)))
;; (quant-up 2.2 .5) => 2.5

(defun quant-down (num step)
    (let ((q (* (round (/ num step)) step)))
	  (if (> q num) (- q step) q)))
;; (quant-down 2.2 .5) => 2.0
;; (quant-down 2.5 .6) => 2.0

(defun which-quant (num steps)
  "returns the closest quantization from a list of steps"
  (let* ((stp (if (number? steps) (list steps) steps))
	 (round-lis (loop for i in stp collect (quant num i)))
	 (diff-lis (loop for i in round-lis collect (abs (- i num))))
	 (smallest (apply #'min diff-lis)))
    (list-ref stp (index-no diff-lis smallest))))
;; (which-quant 5.456 '(.5 .2 .25)) => 0.5

(defun round-up (val)
    "round up value"
    (let ((r (round val)))
      (if (< r val) (+ r 1) r)))
;; (round-up 10.125) => 11

(defun round-down (val)
  "round down value"
  (let ((r (round val)))
    (if (> r val) (- r 1) r)))
;; (round-down 7.99) => 7

(defun round-to (val nextval)
  ""
  (let ((r (round val))
	(v (- nextval 1)))
    (if (< r val) (- r v) (+ r v))))
;; (round-to 2.3 2) => 1
;; (round-to 2.7 2) => 4

(defun next-measure (time quarters)
  "calculates from the current time the time of the next measure start. (quarters = quarters per measure)"
  (let* ((b (round-up (/ time quarters)))
	 (c (/ b quarters))
	 (d (* quarters c)))
    (* d quarters)))
;; (next-measure 19.2 4) => 20
;; (next-measure 19.2 3) => 21

(defun prev-measure (time quarters)
    "calculates from the current time the time of the previous measure start. (quarters = quarters per measure)"
    (let* ((b (round-down (/ time quarters)))
	   (c (/ b quarters))
	   (d (* quarters c)))
      (* d quarters)))
;; (prev-measure 19.2 4) => 16
;; (prev-measure 14.76 3) => 12

(defun closest-measure (time quarters)
    "calculates from the current time the time of the closest measure start. (quarters = quarters per measure)"
    (let* ((dif-next (diff time (next-measure time quarters)))
	  (dif-prev (diff time (prev-measure time quarters)))
	  (closest (min dif-next dif-prev)))
      (if (= closest dif-next) (+ time dif-next) (- time dif-prev))))
;; (closest-measure 14.1 4) => 16.0
;; (closest-measure 13.9 4) => 12.0
;; (closest-measure 14.156 3) => 15.0
