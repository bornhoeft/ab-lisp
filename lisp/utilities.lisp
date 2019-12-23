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

;;; Utilities

(defun switch-pos-neg (rep &optional (start 1))
  (loop repeat rep
    for i from start
    for j = (mod i 2)
    collect (if (= j 0) -1 1)))

;; (switch-pos-neg 10) => (1 -1 1 -1 1 -1 1 -1 1 -1)
;; (switch-pos-neg 10 0) => (-1 1 -1 1 -1 1 -1 1 -1 1)

(defun rev-switch (val)
  "switch 0 and 1: (rev-switch 0) => 1"
  (* (- val 1) -1))
;; (rev-switch 0) => 1
;; (rev-switch 1) => 0

(defun between? (x y z)
  "Predicate; return t iff number x is between numbers y and z."
  (or (<= y x z) (>= y x z)))
  
(defun iota (n &optional (start-at 0))
  "Return a list of n consecutive integers, by default starting at 0."
  (if (<= n 0) nil (cons start-at (iota (- n 1) (+ start-at 1)))))

(defun half (val1 val2)
"half way between two values: (half -10 5) => -2.5" 
    (* (+ val1 val2) .5))
;; (half -10 5) => -2.5

(defun half-list (lis &key (type))
  (let* ((lenlis (length lis))
         (listest (oddp lenlis))
         (half (round (* 0.5 lenlis)))
         (half-elt-right (nth half lis))
         (half-elt-left (nth (- half 1) lis)))
    (if listest
      half-elt-right
      (case type
        (left half-elt-left)
        (right half-elt-right)
        (otherwise (list half-elt-left half-elt-right))))))
;; (half-list '(3 5 4 6 8))
;; (half-list '(3 5 4 6 8 1))
;; (half-list '(3 5 4 6 8 1) :type 'left)
;; (half-list '(3 5 4 6 8 1) :type 'right)
;; (half-list '(3 5 4 6 8) :type 'right)

(defun semi-to-srate (semitones)
  "semitones just returns the right src from the given 
  semitone transposition. e.g. (semitones 12) will return 2."
  (if (numberp semitones) 
    (expt 2 (/ semitones 12.0))
    (loop for i in semitones collect (expt 2 (/ i 12.0)))))

;; (semi-to-srate -7) => 0.6674199
;; (semi-to-srate 12) => 2.0

(defun sec-to-min (seconds)
  (let ((m (floor (/ seconds 60)))
        (s (rem seconds 60)))
    (format nil "~D min ~D sec" m s)))

;; (sec-to-min 999) => "16 min 39 sec"

(defun midi-to-integer (midis)
  "Convert midi notes to the base c4 = 0."
  (if (numberp midis) 
    (- midis 60)
    (loop for i in midis collect (- i 60))))

;; (midi-to-integer 62) => 2
;; (midi-to-integer '(60 61 57 45 67 78 56 89)) => (0 1 -3 -15 7 18 -4 29)

(defun integer-to-midi (integers)
  "Convert c4 = 0 based numbers to midi notes."
  (if (numberp integers) 
    (+ integers 60)
    (loop for i in integers collect (+ i 60))))

;; (integer-to-midi 3) => 63
;; (integer-to-midi '(0 1 -3 -15 7 18 -4 29)) => (60 61 57 45 67 78 56 89)

(defun measure-to-quarter (durs)
  "Convert 4/4 measure based durations to 1/4 quarter based durations."
(if (numberp durs) 
    (* durs 4)
    (loop for i in durs collect (* 4 i))))

;; (measure-to-quarter 1/4) => 1
;; (measure-to-quarter 0.25) => 1.0
;; (measure-to-quarter '(1/16 1/16 1/16 1/16 1/12 1/12 1/12 1/8 1/8)) => (1/4 1/4 1/4 1/4 1/3 1/3 1/3 1/2 1/2)

(defun quarter-to-measure (durs)
  "Convert 1/4 quarter based durations to 4/4 measure based durations."
(if (numberp durs) 
    (/ durs 4)
    (loop for i in durs collect (/ i 4))))

;; (quarter-to-measure 1) => 1/4
;; (quarter-to-measure 1/4) => 1/16
;; (quarter-to-measure '(1/4 1/4 1/4 1/4 1/3 1/3 1/3 1/2 1/2)) => (1/16 1/16 1/16 1/16 1/12 1/12 1/12 1/8 1/8)

(defun index-no (val lis)
             "Returns the list index of key in row."
             (if (not (member val lis))
               (format t "Value is not in list!")
               (loop for i in lis
                 for y from 0 do
                 (when (= i val) (return y)))))

;; (index-no 3 '(2 5 4 6 3 7 8)) => 4
;; (index-no 3 '(2 5 4 6 3 7 8 3)) => 4
;; same as:
;; (position 3 '(2 5 4 6 3 7 8 3)) => 4 - only the first occurence is returned

(defun indexes (val lis &key (type :all))
             "Returns the list index of key in row."
  (let* ((results (loop for i in lis
                 for y from 0
                 when (= i val) collect y))
         (lres (length results)))
    (if (equal results nil)
      (format t "Value ~d is not in list!" val)
      (case type
        (:start (first results))
        (:end (first (last results)))
        (:all (if (= lres 1) (first results) results))))))
                     
;; (indexes 3 '(2 5 4 6 3 7 8 3)) => (4 7)
;; (indexes 9 '(2 5 4 6 3 7 8 3)) => ? Value 9 is not in list!
;; (indexes 3 '(2 5 4 6 3 7 8 3) :type :end) => 7
;; (indexes 3 '(2 5 4 6 3 7 8 3) :type :start) => 4

(defun wavelength (freq)
  (let* ((period (/ 1000 freq))
         (fr (/ 1 period))
         (soundspeed 343)) ; m/sec
    (/ (/ soundspeed fr) 10.0)))

;; (wavelength 800)     

(defun ulam-spiral (n)
  (loop for a in (spiral n n (* n n)) do
        (format t "~{~d~}~%" a)))
 
(defun spiral
    (n m b &aux (row (loop for a below n
                           collect (if (primep (- b a))
                                       '* '#\space))))
  (if (= m 1) (list row)
      (cons row (mapcar #'reverse
                        (apply #'mapcar #'list
                               (spiral (1- m) n
                                       (- b n)))))))
(defun primep (n)
  (when (> n 1) (loop for a from 2 to (isqrt n)
                      never (zerop (mod n a)))))

;; (ulam-spiral 60)

