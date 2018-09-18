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

;;; Envelope functions

(defun env-scale (dur xylist &key (scale 1))
    "envelope from list of x/y values in specified duration"
  (let* ((llis (length xylist)))
    (loop for y from 0 to (- llis 1) by 2
       collect (* dur (nth y xylist))
       collect (* scale (nth (+ 1 y) xylist)))))
;; (env-scale 100 '(0 0.2 0.5 0.8 0.8 0.2 1 0.1) :scale 10) => (0 2.0 50.0 8.0 80.0 2.0 100 1.0)

(defun sample-values (start end samples)
  "Any number of equidistant values between start an end."
  (let ((qnt (/ 1 (- samples 1)))
        (dif (abs (- end start))))
  (loop repeat samples
    for i from 0 by qnt
    collect (* 1.0 (+ start (* i dif))))))

;; (sample-values 20 60 9) => (20.0 25.0 30.0 35.0 40.0 45.0 50.0 55.0 60.0)
;; (sample-values 21 60.5 8) => (21.0 26.642857 32.285713 37.928574 43.57143 49.214287 54.857143 60.5)

(defun resample-x (lst samples)
  "Resample a list of x-values to any number of equidistant x-values in the same range."
  (let* ((start (first lst))
         (end (first (reverse lst)))
         (dist (- end start))
         (stp (/ dist (- samples 1))))
    (loop repeat samples
      for i from start by stp collect i)))

;; (loop for i in (resample-x '(0 1 2 3) 5) collect (* 1.0 i)) => (0.0 0.75 1.5 2.25 3.0)
;; (loop for i in (resample-x '(0 1 2 3) 6) collect (* 1.0 i)) => (0.0 0.6 1.2 1.8 2.4 3.0)
;; (loop for i in (resample-x '(0 1 2 4) 6) collect (* 1.0 i)) => (0.0 0.8 1.6 2.4 3.2 4.0)
;; (loop for i in (resample-x '(0 1 2 4 5 8) 6) collect (* 1.0 i)) => (0.0 1.6 3.2 4.8 6.4 8.0)
;; (loop for i in (resample-x '(1 2.5 4.5 5 7.5) 5) collect (* 1.0 i)) => (1.0 2.625 4.25 5.875 7.5)

(defun map-range (a1 a2 b1 b2 s)
  "Calculates the value s according to a range between 
  a1 and a2 into a new range between b1 and b2."
               (* 1.0 (+ b1
                         (/ (* (- s a1)
                               (- b2 b1))
                            (- a2 a1)))))

;; Envelope with xy-values: '(1 23 3 30)
;; Which value at x  = 2.5 ? => 28,243

;; (map-range 1 3 0 1 2.5) => 0.75
;; (map-range 0 1 23 30 0.75) => 28,25 !!!

;; (map-range 1 3 0 1 1) => 0.00
;; (map-range 0 1 23 30 0) => 23.0

;; (map-range 1 4 0 1 2) => 0.33333334
;; (map-range 0 1 5 3 0.3333334) => 4.333333


(defun split-xy (lst)
  "Split a list of x/y-values into a list of x-values and a list of y-values."
  (loop for i from 0 to (- (length lst) 1) by 2
    for j from 1 by 2
    collect (nth i lst) into xlis
    collect (nth j lst) into ylis
    finally (return (list xlis ylis))))

;; (split-xy '(1 23 3 30 4 40)) => ((1 3 4) (23 30 40))

#|
(defun sample-env-x (env samples)
  (labels ((resample-x (lst samples)
             "Resample a list of x-values to any number of 
             equidistant x-values in the same range."
             (let* ((start (first lst))
                    (end (first (reverse lst)))
                    (dist (- end start))
                    (stp (/ dist (- samples 1))))
               (loop repeat samples
                 for i from start by stp collect i)))
           (map-range (a1 a2 b1 b2 s)
             "Calculates the value s according to a range 
             between a1 and a2 into a new range between b1 and b2."
             (* 1.0 (+ b1
                       (/ (* (- s a1)
                             (- b2 b1))
                          (- a2 a1))))))
    (let* ((xlst (first env))
           (ylst (second env))
           (rs-x (resample-x xlst samples)))
      (loop for i in rs-x
        collect
        (loop 
          for x in xlst
          for xc in (cdr xlst)
          for y in ylst
          for yc in (cdr ylst)
          for r1 = (map-range x xc 0 1 i)
          for r2 = (map-range 0 1 y yc r1)
          when (>= xc i x) do (return r2))
        into results
        finally (return (list rs-x results))))))
  
;; (sample-env-x '((0 1 2 3) (1 3 2 4)) 5) => ((0 3/4 3/2 9/4 3) (1.0 2.5 2.5 2.5 4.0))
;; (sample-env-x '((0 2 3 5) (1 3 2 4)) 5) => ((0 5/4 5/2 15/4 5) (1.0 2.25 2.5 2.75 4.0))

(defun sample-env-xy (env samples)
  (labels ((split-xy (lst)
             "Split a list of x/y-values into a list 
             of x-values and a list of y-values."
             (loop for i from 0 to (- (length lst) 1) by 2
               for j from 1 by 2
               collect (nth i lst) into xlis
               collect (nth j lst) into ylis
               finally (return (list xlis ylis))))
           (resample-x (lst samples)
             "Resample a list of x-values to any number of 
             equidistant x-values in the same range."
             (let* ((start (first lst))
                    (end (first (reverse lst)))
                    (dist (- end start))
                    (stp (/ dist (- samples 1))))
               (loop repeat samples
                 for i from start by stp collect i)))
           (map-range (a1 a2 b1 b2 s)
             "Calculates the value s according to a range 
             between a1 and a2 into a new range between b1 and b2."
               (* 1.0 (+ b1
                         (/ (* (- s a1)
                               (- b2 b1))
                            (- a2 a1))))))
    (let* ((xy (split-xy env))
           (xlst (first xy))
           (ylst (second xy))
           (rs-x (resample-x xlst samples)))
      (loop for i in rs-x
        collect i into results
        collect
        (loop 
          for x in xlst
          for xc in (cdr xlst)
          for y in ylst
          for yc in (cdr ylst)
          for r1 = (map-range x xc 0 1 i)
          for r2 = (map-range 0 1 y yc r1)
          when (>= xc i x) do (return r2))
        into results
        finally (return results)))))
  
;; (sample-env-xy '(0 1 1 3 2 2 3 4) 5) => (0 1.0 3/4 2.5 3/2 2.5 9/4 2.5 3 4.0)
|#

(defun sample-env (env samples)
  "Taking any number of samples from an envelope of x/y-values.
  Envelope can be a list of xy-value pairs or a list containing 
  a list of x-values and a list of y-values."
  (labels ((split-xy (lst)
             "Split a list of x/y-values into a list of x-values 
              and a list of y-values."
             (loop for i from 0 to (- (length lst) 1) by 2
               for j from 1 by 2
               collect (nth i lst) into xlis
               collect (nth j lst) into ylis
               finally (return (list xlis ylis))))
           (resample-x (lst samples)
             "Resample a list of x-values to any number of 
              equidistant x-values in the same range."
             (let* ((start (first lst))
                    (end (first (reverse lst)))
                    (dist (- end start))
                    (stp (/ dist (- samples 1))))
               (loop repeat samples
                 for i from start by stp collect i)))
           (map-range (a1 a2 b1 b2 s)
             "Calculates the value s according to a range between 
              a1 and a2 into a new range between b1 and b2."
             (* 1.0 (+ b1
                       (/ (* (- s a1)
                             (- b2 b1))
                          (- a2 a1))))))
    (let* ((type? (numberp (first env)))
           (xy (if type? (split-xy env)))
           (xlst (if type? (first xy) (first env)))
           (ylst (if type? (second xy) (second env)))
           (rs-x (resample-x xlst samples)))
      (loop for i in rs-x
        when type? collect i into results
        collect
        (loop 
          for x in xlst
          for xc in (cdr xlst)
          for y in ylst
          for yc in (cdr ylst)
          for r1 = (map-range x xc 0 1 i)
          for r2 = (map-range 0 1 y yc r1)
          when (>= xc i x) do (return r2))
        into results
        finally (if type? (return results) (return (list rs-x results)))))))
  
;; (sample-env '(0 1 1 3 2 2 3 4) 5) => ((0 3/4 3/2 9/4 3) (1.0 2.5 2.5 2.5 4.0))
;; (sample-env '((0 1 2 3) (1 3 2 4)) 5) => ((0 3/4 3/2 9/4 3) (1.0 2.5 2.5 2.5 4.0))
;; (sample-env '((0 2 3 6) (60 54 83 69)) 5) => ((0 3/2 3 9/2 6) (60.0 55.5 83.0 76.0 69.0))    