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

;; Pitchclass

(defun pc? (keys)
  "Pitch class from keys."
  (let ((k (if (listp keys) keys (list keys))))
    (loop for i in k collect (mod i 12))))
;; (pc? '(60 61 62 63 64 65 66 67 68 69 70 71 72)) => (0 1 2 3 4 5 6 7 8 9 10 11 0)
;; (pc? '(45 78 67 89 34 56 90 60)) => (9 6 7 5 10 8 6 0)


(defun oct? (keys)
  "Pitch class from keys."
  (let ((k (if (listp keys) keys (list keys))))
    (loop for i in k collect (floor (/ i 12)))))
;; (oct? '(5 17 29 41 53 65 77 89 101 113)) => (0 1 2 3 4 5 6 7 8 9)
;; (oct? '(45 78 67 89 34 56 90 22 11 112)) => (3 6 5 7 2 4 7 1 0 9)


(defun same-pitchclass-chord (keys pc)
  "Collect and sort all keys of a given pitchclass (or list of pitchclasses) in a row of keynumbers."
  (let ((class (if (listp pc) pc (list pc))))
    (sort (loop for y in class append
	       (loop for i in keys
		  for p = (mod i 12)	
		  when (= p y) collect i)) '<)))
;; (same-pitchclass-chord '(45 78 67 89 34 56 90 60 101) 5) => (89 101)
;; (same-pitchclass-chord '(45 78 67 89 34 56 90 60 101) '(5 6)) => (78 89 90 101)


;;; PWGL Box [same-pc]
(defun same-pitchclass (keys pc)
  "Collect and sort all keys of a given pitchclass (or list of pitchclasses) in a row of keynumbers."
  (let ((class (if (listp pc) pc (list pc))))
    (loop for y in class collect
	        (sort (loop for i in keys
		  for p = (mod i 12)	
		  when (= p y) collect i) '<))))
;; (same-pitchclass '(45 78 67 89 34 56 90 60 101) 5) => ((89 101))
;; (same-pitchclass '(45 78 67 89 34 56 90 60 101) '(5 6)) => ((89 101) (78 90))

(defun pc-instances (lis)
  ""
  (let ((chord (sort (loop for i in lis collect (+ 60 (mod i 12))) #'<)))
    (loop repeat (length chord)
       with ls = chord
       collect ls into reslis
       do (setq ls (append (cdr ls) (list (+ 12 (car ls)))))	
       finally (return reslis))))


(defun pc-simple-mult (lis1 lis2)
  (let ((l1 (loop for i in lis1
	       collect (loop for x in i 
			  collect (- x (first i)))))
	(l2 (loop for i in lis2
	       collect (loop for x in i
			  collect (- x (first i))))))
    (append
     (loop for i in l1
	collect (loop for x in i
		   append (mapcar #'(lambda (y)
				      (+ 60 (mod (+ x y) 12))) (first lis2)) into outlis
		   finally (return (remove-duplicates outlis))))
     (loop for i in l2
	collect (loop for x in i
		   append (mapcar #'(lambda (y)
				      (+ 60 (mod (+ x y) 12))) (first lis1)) into outlis
		   finally (return (remove-duplicates outlis)))))))

	
