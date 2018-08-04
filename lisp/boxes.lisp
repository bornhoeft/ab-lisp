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

;;; EXAMPLES

;; some box examples with value input-boxes, button input-boxes and menu input-boxes

;; 1 argument name = 'a', default value = 1
(PWGLdef fn1 ((a 1))
    "fn1"
    ()
  (list a))

;; 2 arguments 'a' and 'b'
(PWGLdef fn2 ((a 1) (b '(1 2)))
    "fn2"
    ()
  (list a b))

;;  3 arguments 'a', 'b' and 'c' grouped as three 1 input-box rows
;; c's default value is a list '(1 2 3)
(PWGLdef fn3 ((a 1) (b '(1 2)) (c '(1 2 3))) 
    "fn3"
    (:groupings '(1 1 1))
  (list a b c))

;; 3 required arguments 'a', 'b' and 'c' grouped as three 1 input-box rows
;; and an optional argument of type &rest
(PWGLdef fn4 ((a 1) (b '(1 2)) (c '(1 2 3)) &rest (ds 0)) 
    "fn4"
    (:groupings '(1 1 1))
  (list a b c ds))

;; this box has three arguments, where the second input is
;; a button subview that changes its title when clicked
;; the third input is a simple menu input-box with three options
(defun change-this (b)  
  (setf (ccl::box-string b) (if (equal (ccl::box-string b) "OK") "stop" "OK")))

(PWGLdef fn5 ((a 1)  
              (press () (ccl::mk-button-subview :box-string  "OK"
                                                :r 0.7 :g 0.75 :b 0.7
                                                :pwgl-action-function 'change-this))
              (c () (ccl::mk-menu-subview :menu-list '("test" "key" "skip")))) 
    "fn5"
    (:w 0.5 :x-proportions '((1 (:fix 0.2)) (1)))
  (list a  c))

;; a box with 2 menu input-boxes
(PWGLdef fn6 ((a 1)  (b () (ccl::mk-menu-subview :menu-list '("yes" "no" "maybe") :value 1))
              (c () (ccl::mk-menu-subview :menu-list '("test" "key" "skip") :value 2))) 
    "fn6"
    (:w 0.5 :groupings '(2 1) :x-proportions '((1 (:fix 0.2)) (1)))
  (list a b c))

;; a more advanced example with a hierarchical menu input-box
;; both menu-titles and menu-items are cached
(ccl::add-menu-list-keyword  :my-hierarchical-menu-titles '("First" "Second" "Third"))
(ccl::add-menu-list-keyword :my-hierarchical-menu-items
                            '(("a" "b" "c")
                              ("d" "e" "f" "g")
                              ("h" "i" )))


(PWGLdef fn7 ((a () (ccl::mk-hierarchical-menu-subview
                     :value 0 :value2 2 
                     :menu-titles :my-hierarchical-menu-titles
                     :menu-list :my-hierarchical-menu-items)))
    "fn7"
    ()
  a)


;;; ----------------------------------------------------------

;;; Box for quant function
(system::PWGLdef quantize ((num) (steps))
   "rounds value to the nearest multiple of steps. Steps can be a list of values"
    ()
    (quant num steps))

;;; Box for quant-list function
(system::PWGLdef quantize-list ((lis) (steps))
   "rounds a list of values to the nearest multiple of steps. Steps can be a list of values"
    ()
  (quant-list lis steps))

(system::PWGLdef same-pc ((keys) (pc))
		 "Collect and sort all keys of a given pitchclass (or list of pitchclasses) in a row of keynumbers."
		 ()
		 (same-pitchclass keys pc))

(system::PWGLdef list-intervals ((lis) (interv 0) &optional (start 0))
		 "Collects all values in a specific interval from a given position in a list" 
		 ()
		 (interval-in-list lis interv))

(system::PWGLdef flat-last ((lis))
		 "remove additional parens until the last list"
		 ()
		 (flat-last-list lis))

#|
(system::PWGLdef flat-lasti ((lis))
		 "remove additional parens until the last list"
		 ()
		 (let ((l lis))
		   (loop while (= (length l) 1) do
			(setq l (first l))
			finally (return l))))
|#



