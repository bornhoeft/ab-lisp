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

;;; arithmetic mean
;;; The measure of central tendency of a set of values computed by dividing the 
;;; sum of the values by their number; commonly called the mean or the average.
;;; http://en.wikipedia.org/wiki/Arithmetic_mean

(defun average (numbers)
  "Numerical average (mean) of a list of numbers."
  (* 1.0 (/ (reduce #'+ numbers) (length numbers))))

;; (average '(1 2 3 4 5)) => 3
;; (average '(1 2 3 4 5 6 7 8 9 10)) => 5.5

;;; The average is carried out by dividing by N - 1 instead of N

(defun arithmetic-mean (lis)
  (loop for x in lis
        for n from 0
        summing x into xx-sum
        finally (return (* 1.0 (/ xx-sum n)))))

;; (arithmetic-mean '(1 2 3 4 5)) => 3.75
;; (arithmetic-mean '(1 2 3 4 5 6 7 8 9 10)) => 6.111111

#|
The mean deviation describes how far the i-th sample deviates (differs) from the mean. The average deviation of a signal is found by summing the deviations of all the individual samples, and then dividing by the number of samples, N. Notice that we take the absolute value of each deviation before the summation; otherwise the positive and negative terms would average to zero. The average deviation provides a single number representing the typical distance that the samples are from the mean. While convenient and straightforward, the average deviation is almost never used in statistics. This is because it doesn't fit well with the physics of how signals operate. In most cases, the important parameter is not the deviation from the mean, but the power represented by the deviation from the mean. For example, when random noise signals combine in an electronic circuit, the resultant noise is equal to the combined power of the individual signals, not their combined amplitude.
The standard deviation is similar to the average deviation, except the averaging is done with power instead of amplitude. This is achieved by squaring each of the deviations before taking the average (remember, power <221D> voltage2). To finish, the square root is taken to compensate for the initial squaring. 
In the alternative notation: sigma = sqrt((x0 -<03BC>)2 + (x1 -<03BC>)2 + ... + (xN-1 -<03BC>)2 / (N-1)). Notice that the average is carried out by dividing by N - 1 instead of N. This is a subtle feature of the equation that will be discussed in the next section. The term, <03C3>2, occurs frequently in statistics and is given the name variance. The standard deviation is a measure of how far the signal fluctuates from the mean. The variance represents the power of this fluctuation.
|#

(defun deviation (number lis)
  "Deviation of number from the mean of lis."
  (sqrt (* 1.0 (expt (- number (/ (reduce #'+ lis) (length lis))) 2))))

;; (average '(1 2 3 4 5 6)) => 3.5
;; (deviation 2 '(1 2 3 4 5 6)) => 1.5

(defun mean-deviation (lis)
 (loop for x in lis
        for n from 1
        for xx = (expt (- x (arithmetic-mean lis)) 2)
        summing xx into xx-sum
        finally (return (sqrt (/ xx-sum n))))) 

;; (mean-deviation '(1 2 3 4 5 6 7 8 9 10)) => 2.9365723

;;; quadratic mean (root mean square)
;;; http://en.wikipedia.org/wiki/Root_mean_square
;;; A type of average, calculated as the square root of the mean of the squares. In contrary to the geometric mean higher values are favored.

(defun quadratic-mean (lis)
  (loop for x in lis
        for xx = (* x x)
        for n from 1
        summing xx into xx-sum
        finally (return (sqrt (/ xx-sum n)))))

; (quadratic-mean '(1 2 3 4 5 6 7 8 9 10)) => 6.204837

;;; harmonic-mean
;;; http://en.wikipedia.org/wiki/Harmonic_mean

(defun harmonic-mean (lis)
  (loop for x in lis
        for n from 1
        summing (/ 1.0 x) into xx-sum
        finally (return (/ n xx-sum))))

; (harmonic-mean '(80 120))
; (harmonic-mean '(1 2 3 4 5 6 7 8 9 10)) => 3.4141712

;;; geometric mean
;;; (never higher than the arithmetic mean)
;;; http://en.wikipedia.org/wiki/Geometric_mean

(defun geometric-mean (lis)
  (loop for x in lis
        for n from 1
        collect x into reslis
        finally (return (expt (reduce #'* reslis) (/ 1 n)))
        ;; (sqrt 2) = (expt 2 (/ 1 2))
        ))
  
; (geometric-mean '(1.03 1.07))
; (geometric-mean '(1 2 3 4 5 6 7 8 9 10)) => 4.5287285

;;; hoelder mean
;;; p = 1 arithmetic, p = 2 quadric, p = -1 harmonic, p = 3 kubic

(defun hoelder-mean (lis &optional (p 1)) 
  (loop for x in lis
        for xx = (expt x p)
        for n from 1
        summing xx into xx-sum
        finally (return (expt (/ xx-sum n) (/ 1.0 p)))))

; (hoelder-mean '(1 2 3 4 5 6 7 8 9 10)) => 5.5
; (hoelder-mean '(1 2 3 4 5 6 7 8 9 10) 2) => 6.204837

;;; power mean
;;; types: 'arithmetic, 'quadric, 'harmonic, 'kubic, default = 'arithmetic

(defun power-mean (lis &optional type)
  (let ((p (case type
             (:arithmetic 1)
             (:quadric 2)
             (:harmonic -1)
             (:kubic 3)
             (otherwise 1))))
    (loop for x in lis
          for xx = (expt x p)
          for n from 1
          summing xx into xx-sum
          finally (return (expt (/ xx-sum n) (/ 1.0 p))))))

; (power-mean '(1 2 3 4 5 6 7 8 9 10)) => 5.5
; (power-mean '(1 2 3 4 5 6 7 8 9 10) :arithmetic) => 5.5
; (power-mean '(1 2 3 4 5 6 7 8 9 10) :quadric) => 6.204837

(defun diff (val1 val2)
  "absolute difference between val1 and val2: (diff -1 5) => 6"
  (abs (- val1 val2)))

;; (diff -1 5) => 6 

(defun square (x) (* x x))

(defun round-off (number precision)
  "Round off the number to specified precision. E.g. (round-off 1.23 .1) = 1.2"
  (* precision (round number precision)))

;; (round-off 2.134253 .12)

(defun round-even (val)
  "Round to the next even number."
  (let ((rv (round val))
        (dec (nth-value 1 (floor val))))
    (if (and (oddp rv) (< rv val))
      (+ val (- 1 dec))
      (if (and (oddp rv) (> rv val))
      (- val dec)
        (if (oddp rv)
          (* 1.0 (+ val 1))
          rv)))))

;; (round-even 1.4) => 2
;; (round-even 0.9) => 0
;; (round-even 1) => 2
;; (round-even 0.8) => 0
;; (round-even 11.7) => 12
;; (round-even 5.5) => 6
;; (round-even 4.99) => 4


(defun root (n x)
  "(root 12 2) => 1.0594632 (semitone)"
  (expt x (/ 1 n)))

;; (root 12 2) => 1.0594632 (semitone)

;;; Golden Section

(defun *golden-number* ()
  (/ (+ 1 (sqrt 5)) 2))

;; (*golden-number*) -> 1.618034

(defun gs (s &optional part)
(let ((gn (/ (+ 1 (sqrt 5)) 2))) 
    (case part
      ('major (/ s gn))
      ('minor (- s (/ s gn)))
      (otherwise (* s gn)))))

;; (gs 1) => 1.618034
;; (gs 1 'major) => 0.618034
;; (gs 1 'minor) => 0.381966

;;; KEYS FUNKTIONIEREN NICHT IN BOX !???

(defun golden-section (s &optional p)
  (if (numberp s) (gs s p)
    (loop for i in s
          collect (gs i p))))

;; (golden-section 1) => 1.618034
;; (golden-section '(1 2 3 4))
;; (golden-section '(1 2 3 4) 'major)

(defun percentage (p h)
  "How much percent is p from h?"
  (* h (/ p 100)))

;; (percentage 10 80) => 8

(defun percentage? (n h)
  "How much is n in percent from h?"
  (* 100 (/ n h)))

;; (percentage? 20 80) = 25

(defun hundred-percent? (n p)
  "What is hundred percent when n is p percent ?"
  (* 100 (/ n p)))

;; (hundred-percent? 20 10) => 200

;;; Greatest common divisor
;;; in lisp: (gcd 2345 5432) => 7
;;; http://rosettacode.org/wiki/Greatest_common_divisor#Common_Lisp

(defun gcd2 (a b)
  "Greatest common divisor of 2 numbers."
  (if (zerop b) a (gcd2 b (mod a b))))

;; (gcd2 2345 5432) => 7

(defun gcd-lst (lst)
  "Greatest common divisor of numbers in lst."
  (apply #'gcd lst))

;; (gcd-lst '(30 40 50)) => 10

(defun lcm2 (&rest args)
  "Least common multiple of numbers."
	   (reduce (lambda (m n)
		     (cond ((or (= m 0) (= n 0)) 0)
			   (t (abs (/ (* m n) (gcd m n))))))
		   args :initial-value 1))

(defun lcm-lst (lst)
  "Least common multiple of numbers in lst."
  (apply #'lcm lst))

;; (lcm-lst '(1 2 3 4)) => 12

(defun cartesian-product (lists)
  "Cartesian product of n equally long lists."
  (labels ((distl (m N)
             (cond
              ((null N) nil)
              (t (cons (list m (car N))
                       (distl m (cdr N))))))
           (cartesian (M N)
             (cond
              ((null M) nil)
              (t (append (distl (car M) N)
                         (cartesian (cdr M) N))))))
    (reduce #'cartesian lists)))

;; (cartesian-product '((A B C) (1 2 3) (x y z)))

(defun decimals (number)
  "Decimal numbers of a float."
  (nth-value 1 (floor number)))

;; (decimals 5.432) -> 0.43200016

(defun residual (number divisor)
  "Rest of an integer division."
  (nth-value 1 (floor number divisor)))

;; (residual 17 5) -> 2

(defun g+ (val1 val2)
    "Version of +. Both arguments can be numbers or lists"
(let* ((va1 (if (numberp val1) (list val1) val1))
       (va2 (if (numberp val2) (list val2) val2))
       (v1 (if (numberp val1) (loop repeat (length va2) collect val1) va1))
       (v2 (if (numberp val2) (loop repeat (length va1) collect val2) va2)))
       (loop for i in v1
         for j in v2
         collect (+ i j) into reslis
         finally (return (if (equal (length reslis) 1)
                           (first reslis)
                           reslis)))))
;; (g+ 4 2) => 6
;; (g+ 2 4) => 6
;; (g+ 4 '(1 2 3)) => (5 6 7)
;; (g+ '(1 2 3) 4) => (5 6 7)
;; (g+ '(1 2 3) '(2 3 4)) => (3 5 7)
;; (g+ '(1 2 3) '(2 3)) => (3 5)

(defun g- (val1 val2)
    "Version of -. Both arguments can be numbers or lists"
(let* ((va1 (if (numberp val1) (list val1) val1))
       (va2 (if (numberp val2) (list val2) val2))
       (v1 (if (numberp val1) (loop repeat (length va2) collect val1) va1))
       (v2 (if (numberp val2) (loop repeat (length va1) collect val2) va2)))
       (loop for i in v1
         for j in v2
         collect (- i j) into reslis
         finally (return (if (equal (length reslis) 1)
                           (first reslis)
                           reslis)))))
;; (g- 4 2) => 2
;; (g- 2 4) => -2
;; (g- 4 '(1 2 3)) => (3 2 1)
;; (g- '(1 2 3) 4) => (-3 -2 -1)
;; (g- '(1 2 3) '(2 3 4)) => (-1 -1 -1)
;; (g- '(1 2 3) '(2 3)) => (-1 -1)

(defun g* (val1 val2)
    "Version of *. Both arguments can be numbers or lists"
(let* ((va1 (if (numberp val1) (list val1) val1))
       (va2 (if (numberp val2) (list val2) val2))
       (v1 (if (numberp val1) (loop repeat (length va2) collect val1) va1))
       (v2 (if (numberp val2) (loop repeat (length va1) collect val2) va2)))
       (loop for i in v1
         for j in v2
         collect (* i j) into reslis
         finally (return (if (equal (length reslis) 1)
                           (first reslis)
                           reslis)))))
;; (g* 4 2) => 8
;; (g* 2 4) => 8
;; (g* 4 '(1 2 3)) => (4 8 12)
;; (g* '(1 2 3) 4) => (4 8 12)
;; (g* '(1 2 3) '(2 3 4)) => (2 6 12)
;; (g* '(1 2 3) '(2 3)) => (2 6)

(defun g/ (val1 val2)
    "Version of /. Both arguments can be numbers or lists"
(let* ((va1 (if (numberp val1) (list val1) val1))
       (va2 (if (numberp val2) (list val2) val2))
       (v1 (if (numberp val1) (loop repeat (length va2) collect val1) va1))
       (v2 (if (numberp val2) (loop repeat (length va1) collect val2) va2)))
       (loop for i in v1
         for j in v2
         collect (/ i j) into reslis
         finally (return (if (equal (length reslis) 1)
                           (first reslis)
                           reslis)))))
;; (g/ 4 2) => 2
;; (g/ 2 4) => 1/2
;; (g/ 4.0 '(1 2 3)) => (4.0 2.0 1.3333334)
;; (g/ '(1 2 3) 4) => (1/4 1/2 3/4)
;; (g/ '(1 2 3) '(2 3 4)) => (1/2 2/3 3/4)
;; (g/ '(1 2 3) '(2 3)) => (1/2 2/3)
