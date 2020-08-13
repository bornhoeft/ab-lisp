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

;;; Random functions

(defun rand (low high &optional (digits 6))
 "Return alues chosen at random between low and high including the limits.
  If the numbers given are integers the result will be an integer."
  (let* ((dig (expt 10 digits))
         (hi (* high dig))
         (lo (* low dig)))
    (if (and (integerp low) (integerp high))
      (+ (min low high) (random (abs (+ 1 (- high low)))))
      (/ (+ (min lo hi) (random (abs (+ 1 (- hi lo))))) dig))))

;; (loop repeat 10 collect (rand 0 10)) ; => (3 0 3 1 10 0 2 6 4 9)
;; (loop repeat 10 collect (rand 0 1.0)) ; => (0.19683109 0.9814336 0.6428565 0.666238 0.8315703 0.059967414 0.9817517 0.20357414 0.09638849 0.7393965)
;; (loop repeat 10 collect (rand 1 5)) ; => (4 4 3 5 4 5 5 3 1 2)
;; (loop repeat 10 collect (rand -2 3)) ; => (3 3 0 1 3 0 -2 -1 1 0)
;; (loop repeat 10 collect (rand 0.0 1.0)) ; => (0.84254044 0.49642378 0.96591794 0.48625877 0.09820962 0.012784736 0.83146054 0.758958 0.21964629 0.3659834)
;; (loop repeat 10 collect (rand -1 1.0)) ; => (0.43671826 -0.099772654 0.91709256 -0.31474653 0.91441834 -0.75535315 -0.20307846 -0.4239697 0.7960043 0.8241785)

;;; Funktioniert nicht mit Floats
(defun center-random (num)
  "Random between positive and negative <num> including the limits. 
  If the number given is an integer the result will be an integer."
  (if (= num 0) 0
      (- (random (+ 1 (* 2 num))) num)))
;; (loop repeat 10 collect (center-random 5)) => (-2 0 0 3 2 5 4 -5 1 -2)
;; (loop repeat 5 collect (center-random 5.0)) =>(-3.4431937 0.76110363 1.2877498 2.243022 -0.25518608)

(defun negative-random (num)
  "Negative signed numbers between 0 and num."
  (if (= num 0) 0
      (- (random num) num)))
;; (loop repeat 20 collect (negative-random 10)) => (-4 -1 -5 -1 -9 -4 -1 -6 -1 -8 -4 -10 -4 -6 -1 -6 -6 -10 -8 -6)

(defun sign-random (num)
  "Randomly signed num."
  (let ((ran (random 2)))
    (if (= ran 0) num (- num))))
;; (loop repeat 20 collect (sign-random 1)) => (-1 -1 1 1 1 1 1 1 -1 -1 -1 -1 -1 1 -1 1 1 -1 1 -1)

(defun split-random (num sort)
  "split a value randomly in two numbers."
  (let* ((rval (random (if (equal num 0) 1 num)))
	 (vrval (- num rval))
	 (mi (min rval vrval))
	 (ma (max rval vrval)))      
    (if (= sort 0) (list mi ma) (list ma mi))
    ;; when sort = 0 the min value is first, with sort = 1 its the other way round
    ))
;;  (split-random 10 0) => (1 9)
;;  (split-random 10 1) => (7 3)
;; (loop repeat 10 collect (split-random (random 10) (random 2))) => ((8 2) (5 5) (5 5) (4 6) (1 9) (5 5) (7 3) (7 3) (6 4) (4 6))

(defun lowpass-random (n)
  "Linear random low-pass distribution (original code by Heinrich Taube)."
  (min (random n) (random n)))
; (loop repeat 100 collect (lowpass-random 50))

#|
=> (42 18 20 43 23 9 3 8 0 27 10 15 18 1 44 7 8 5 21 12 0 6 1 13 12 12 8 14 2 9 42 14 14 28 11 2 19 4 25 1 2 9 34 23 10 3 10 22 11 13 27 25 15 26 21 0 29 0 12 13 31 9 7 28 9 3 3 8 2 6 25 45 2 14 19 14 47 21 4 40 26 34 1 2 0 22 18 25 24 17 16 12 12 5 15 2 27 11 21 6)
|#

(defun lowpass-random-q (n q)
"random-lowpass with variable filter-q."
    (if (= q 0) (random n)
  (loop repeat q collect (random n) into numberlist
       finally (return (apply #'min numberlist)))))
; (loop repeat 50 collect (lowpass-random-q 50 0))

#|
=> (5 0 3 0 1 8 0 2 1 0 6 0 0 2 5 3 2 2 1 0 2 2 3 1 0 4 10 5 3 0 4 7 2 0 0 1 0 2 4 0 1 1 2 2 0 0 4 5 1 0)
|#

(defun highpass-random (n)
 "Linear random high-pass distribution (original code by Heinrich Taube)."
  (max (random n) (random n)))

; (loop repeat 100 collect (highpass-random 50))

#|
=> (30 36 32 11 34 23 48 47 21 44 46 22 24 7 7 43 23 43 17 33 10 40 47 31 36 22 17 49 18 46 23 30 43 33 47 27 18 24 41 48 37 6 34 40 26 39 25 44 16 38 44 43 30 43 18 32 35 44 38 28 36 32 5 29 45 5 46 27 37 35 23 17 42 46 49 47 28 41 38 35 30 23 29 6 44 39 48 23 6 5 45 42 40 21 23 37 28 39 7 20)
|#

(defun highpass-random-q (n q)
  "Random-highpass with variable filter-q."
  (if (= q 0) (random n)
      (loop repeat q collect (random n) into numberlist
	 finally (return (apply #'max numberlist)))))

; (loop repeat 50 collect (highpass-random-q 50 20)) 

#|
=> (48 48 49 47 47 44 49 49 48 49 44 49 43 46 45 49 47 43 48 48 48 48 49 49 49
    46 49 49 46 48 47 49 48 47 48 48 45 44 48 48 49 41 48 48 39 49 42 44 47 47)
|#

(defun choice (val1 val2 perc)
    "Choose between val1 an val2 with perc = random percentage of val1."
    (let ((r (random 100.0)))
      (if (< r perc) val1 val2)))
;; (loop repeat 30 collect (choice 0 1 10)) => (0 1 1 1 1 1 1 1 0 1 1 1 1 1 0 1 1 1 1 1 1 1 1 1 1 0 0 0 0 1)

(defun norep-random (rep low high)
  "Random without without equal consecutive numbers."
  (loop until (= reslength rep)
     with value and last-value
     with reslength = 0 do
     (setf last-value value
	   value (+ low (random (+ 1 (- high low)))))	
     when (not (equal value last-value))
     collect value into resultlist and do
     (setf reslength (length resultlist))
     finally (return resultlist)))

;; (norep-random 10 0 10) => (2 6 1 9 2 3 9 2 6 3)

(defun norep-nth-random (rep lis)
  "Random in list without equal consecutive numbers."
  (loop until (= reslength rep)
     with value and last-value
     with reslength = 0 do
     (setf last-value value
	   value (nth (random (length lis)) lis))	
     when (not (equal value last-value))
     collect value into resultlist and do
     (setf reslength (length resultlist))
     finally (return resultlist)))

;; (norep-nth-random 10 '(1 2 3 4)) => (3 4 2 3 4 3 4 1 2 4)

;;; norep nth random mit mod12
;;; NOT WORKING WITH REPEATED ELEMENTS IN LIST E.G. '(4 4 4 5 5 5 6 6 6)
;; shuffle list
(defun shuffle (lis &optional (rep 1) &key (total NIL))
  "Returns a list of the same with the elements randomly reordered"
  (let* ((llis (length lis))
	 (end (if total rep (* rep llis))))
    (loop with cdrlis and n
       for i from 0 below end do  
       (when (zerop (mod i llis)) (setf cdrlis lis))	    
       ;; when modulo i from the length of lis is equal to 0
       ;; reset cdrlis to lis (see print)	         
       (setf n (nth (random (length cdrlis)) cdrlis))
       ;; takes a random element from cdrlis and
       (setf cdrlis (remove n cdrlis :count 1))
       ;; remove it
       collect n)))

;; (shuffle '(1 2 3 4 5 6 7 8)) => (5 4 8 1 3 7 6 2)
;; (shuffle '(1 2 3 4 5 6 7 8) 2) => (2 1 5 4 6 3 7 8 6 5 3 8 4 7 2 1)
;; (shuffle '(1 2 3 4 5 6 7 8) 13 :total T) => (1 6 2 7 3 4 8 5 7 5 8 6 4)

(defun nshuffle (sequence)
  "The   Knuth shuffle   (a.k.a. the Fisher-Yates shuffle) is an algorithm 
  for randomly shuffling the elements of an array: 
  https://www.rosettacode.org/wiki/Knuth_shuffle#Common_Lisp"
  (loop for i from (length sequence) downto 2
    do (rotatef (elt sequence (random i))
                (elt sequence (1- i))))
  sequence)

;; (nshuffle '(0 1 2 3 4 5)) => (3 0 2 1 5 4)

;; unique elements
(defun unique (lis &optional n)
  "Returns n unique elements from lis"
  (let* ((ls (remove-duplicates lis))
         (llis (length ls))
	 (end (if n (min n llis) llis)))
    (loop with n
      with cdrlis = ls
      for i from 0 below end do     	         
      (setf n (nth (random (length cdrlis)) cdrlis))
      (setf cdrlis (remove n cdrlis))
      collect n)))

;; (unique '(1 2 3 4 2 5 6 7 8)) => (5 7 8 2 1 6 4 3)
;; (unique '(1 2 3 4 5 3 6 7 8) 5) => (8 4 1 5 6)


(defun weighted-random (n weight-list)
  "Returns a number of weighted random numbers. Weights are taken from 
  a list indicating <element> and <random percentage> in pairs."
  (let ((wlis (loop for j from 0 below (- (length weight-list) 1) by 2
                append 
                (make-list (nth (+ 1 j) weight-list) 
                           :initial-element (nth j weight-list)) 
                into reslis
                finally (return reslis))))
    (loop repeat n 
      collect (nth (random (- (length wlis) 1)) wlis))))

;; (weighted-random 10 '(1 10 2 20 3 70)) => (1 2 3 3 3 3 3 3 2 3)

(defun random-element (list)
  "Return some element of the list, chosen at random."
  (nth (random (length list)) list))

;; (random-element '(3 9 6 5 7 1 3 4 6 4 7 8)) => 9
  
(defun sample-with-replacement (n population)
  (let ((result nil))
    (dotimes (i n) 
      (push (nth (random (- (length population) 1)) population) result))
    result))

;; (sample-with-replacement 10 '(1 2 3 4 5 6 7 8)) => (1 5 2 2 2 6 6 5 6 5)

(defun sample-without-replacement 
       (n population &optional (m (length population)))
  ; Assumes that m = (length population)
  (cond ((<= n 0) nil)
	((>= n m) population)
	((>= (/ n m) (random 1.0))
	 (cons (first population) (sample-without-replacement
				   (- n 1) (rest population) (- m 1))))
	(t (sample-without-replacement n (rest population) (- m 1)))))

;; (sample-without-replacement 10 '(1 2 3 4 5 6 7 8 9 10 11 12)) => (1 2 4 6 7 8 9 10 11 12)

(defun brownian-single-step (n stp weight start)
  (let ((wlis (append 
                (make-list weight :initial-element stp)
                (make-list (- 100 weight) :initial-element (* -1 stp)))))
    (loop repeat n
      with s = start
      collect s into reslis
      do (setf s (+ s (nth (random (- (length wlis) 1)) wlis)))
      finally (return reslis))))

;; (brownian-single-step 10 1 40 10)

(defun brownian (n weight-list start)
  (let ((wlis (loop for j from 0 below (- (length weight-list) 1) by 2
                append 
                (make-list (nth (+ 1 j) weight-list) 
                           :initial-element (nth j weight-list)) 
                into reslis
                finally (return reslis))))
    (loop repeat n
      with s = start
      collect s into reslis
      do (setf s (+ s (nth (random (- (length wlis) 1)) wlis)))
      finally (return reslis))))

;; (brownian 40 '(1 30 -1 30 2 20 -2 20) 10)

(defun brownian-borders (n weight-list start low high)
  (let ((wlis (loop for j from 0 below (- (length weight-list) 1) by 2
                append 
                (make-list (nth (+ 1 j) weight-list) 
                           :initial-element (nth j weight-list)) 
                into reslis
                finally (return reslis))))
    (loop repeat n
      with s = start
      do (setf s (if (< s low) 
                     (+ low (abs (- s low)))
                     (if (> s high) 
                         (- high (abs (- s high)))
                         s)))
      collect s into reslis
      do (setf s (+ s (nth (random (length wlis)) wlis)))
      finally (return reslis))))

;; (brownian-borders 50 '(1 40 -1 60 2 20) 10 5 15)

(defun random-sum1 (lst-sum low high)
  "Collects random numbers between low and high until lst-sum. If the result is > lst sum
  the last number is truncated to fit lst-sum."
  (loop
    with s
    collect (+ low (random (- high low))) into reslis
    do (setf s (reduce #'+ reslis))
    until (>= s lst-sum)
    finally (return
             (if (> s lst-sum)
               (append (butlast reslis) 
                       (list (- lst-sum (reduce #'+ (butlast reslis)))))
                      reslis))))

;; (random-sum1 20 2 5)

(defun random-sum2 (lst-sum lis)
  "Try to build a list containing the numbers in lis with the sum of lst-sum."
  (loop
    with sum
    for x = (loop
               with s
               collect (nth (random (length lis)) lis) into reslis
               do (setf s (reduce #'+ reslis))
               until (>= s lst-sum)
               finally (return reslis))
    do (setf sum (reduce #'+ x))
    do (print x)
    until (= sum lst-sum)
    finally (return x)))

;; (random-sum2 20 '(7 3 8))