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

;;; List functions

(defun first-length (lis)
  (length (first lis)))
;; (first-length '((1 2 3 4 3 5 6) (3 6 4 5 7) (3 6 4 7 3))) => 7

(defun flatten (obj)
  "http://rosettacode.org/wiki/Flatten_a_list#C ommon_Lisp"
  (do* ((result (list obj))
        (node result))
       ((null node) (delete nil result))
    (cond ((consp (car node))
           (when (cdar node) (push (cdar node) (cdr node)))
           (setf (car node) (caar node)))
          (t (setf node (cdr node))))))
;; (flatten '((1 2 (3 4) 3 5 6) (3 6 ((4 5)) 7))) => (1 2 3 4 3 5 6 3 6 4 5 7)

(defun flat-length (lis)
  (labels ((flatten (obj)
             "http://rosettacode.org/wiki/Flatten_a_list#Common_Lisp"
             (do* ((result (list obj))
                   (node result))
                  ((null node) (delete nil result))
               (cond ((consp (car node))
                      (when (cdar node) (push (cdar node) (cdr node)))
                      (setf (car node) (caar node)))
                     (t (setf node (cdr node)))))))
    (length (flatten lis))))
;; (flat-length '((1 2 (3 4) 3 5 6) (3 6 ((4 5)) 7) (3 6 4 7 3))) => 17

(defun flat-last-list (lis)
"remove additional parens until the last list"
(let ((l lis))
(loop while (= (length l) 1) do
  (setq l (first l))
  finally (return l))))
;; (flat-last-list '(((1 2) (3 4) (5 6)))) => ((1 2) (3 4) (5 6)) 

(defun flat-once (lis)
(append (car lis) (cdr lis)))
;; (flat-once '(((c4 cs4 d4) (ds4 e4 f4 fs4) (g4 gs4) (a4 as4 b4))));;

(defun sum-zerop (lis)
    (if (= (reduce '+ lis) 0) t nil))
;; (sum-zerop '( -2 3 5 -1 -3 -2)) => T
;; (sum-zerop '( -2 3 5 -3 -2)) => NIL

(defun range (lis min max)
  "range a list between min and max."
  (let ((minlis (reduce #'min lis))
        (maxlis (reduce #'max lis)))
    (labels ((map-range (a1 a2 b1 b2 s)
               (* 1.0 (+ b1
                         (/ (* (- s a1)
                               (- b2 b1))
                            (- a2 a1))))))
      (loop for i in lis
        collect (map-range minlis maxlis min max i)))))

;; (range '(1 2 3 4 5) 1 2) => (1.0 1.25 1.5 1.75 2.0)
;; (range '(1 3 6 7) 1 2) => (1.0 1.3333334 1.8333334 2.0)
;; (range '(2 3 6 4 5 8 6 7) 2 3) => (2.0 2.1666667 2.6666667 2.3333333 2.5 3.0 2.6666667 2.8333333)

(defun scaling-max (lis new-max)
  (let* ((max-value (apply #'max lis))
        (mult-factor (/ new-max max-value)))
    (loop for i in lis
      collect (* mult-factor i))))

;; (scaling-max '(1 2 3 4 5 6) 10.0) => (1.6666666 3.3333333 5.0 6.6666665 8.333333 10.0)

(defun scaling-sum (lis new-sum)
  (let* ((sum (reduce #'+ lis))
    (sum-factor (/ new-sum sum)))
    (loop for i in lis
      collect (* sum-factor i))))

;; (scaling-sum '(1 2 3 4 5) 30.0) => (2.0 4.0 6.0 8.0 10.0)
;; (reduce #'+ (scaling-sum '(1 2 3 4 5) 30.0)) => 30.0
;; (reduce #'+ (scaling-sum '(4 6 5 8 7 11 2 3 4.5 11.2) 100.0)) => (6.482982 9.724473 8.103727 12.965964 11.345219 17.828201 3.241491 4.8622365 7.293355 18.15235)

(defun lengths (ls)
  "Returns the length of <ls>. <ls> can be a list of lists."
  (if (numberp (first ls))
      (length ls)
        (mapcar #'length ls)))  

;; (lengths '((1 2 3 4) (2 3 4 5 6 2))) => (4 6)
;; (lengths '(1 2 3 4)) => 4

(defun length-rest (lis id)
    "calculate the rest length of a list from a given index (including the index)"
    (let* ((max-id (length lis)))
      (if (> id max-id)
	   (format t "List is shorter than the given index!")
	   (if (< id 0)
	     (format t "Negative index!")  
      (- max-id id)))))
;; (length-rest '(1 2 3 4 5 6 7 8 9) 5) => 4
;; (length-rest '(1 2 3 4 5 6 7 8 9) -5) => NIL (Negative index!)
;; (length-rest '(1 2 3 4 5 6 7 8 9) 10) => NIL (list is shorter than the given index!)

(defun sublist (lis index int)
    "calculates the rest list of lis from a given id with length int (maximal the length-rest)"
    (let ((reps (min int (length-rest lis index))))
    (loop repeat reps
	 for i from index
	 collect (list-ref lis i))))
;; (sublist '(2 3 4 5 6 7 8 9) 3 12) => (5 6 7 8 9) 
;; (sublist '(3 2 4 5 6 9 8 0 7 1) 3 6) => (5 6 9 8 0 7)

(defun nth? (e ls)
  (if (numberp (member e ls))
           (error "Element is not in list.")
    (loop for i in ls
      for j from 0
      when (equal i e) return j)))

;; (nth? 7 '(3 5 2 6 4 7 8)) => 5
;; Lisp: (position 7 '(3 5 2 6 4 7 8)) => 5
           
(defun intervals (lis interv start)
  "returns the indexes in a list for with a specific interval (interv).
  Example: (intervals '(50 45 56 62 67) 5 1)" 
  (labels (( nth? (e ls)
             (if (numberp (member e ls))
               (error "Element is not in list.")
               (loop for i in ls
                 for j from 0
                 when (equal i e) return j))))
    (loop for y in lis 
      for z from 0
      for stval = (nth start lis)
      for i = (+ y interv)   
      when (>= y stval) 
      when (member i lis) collect (list z (nth? i lis)))))

;; (intervals '(50 45 56 62 67 68) 5 1) => ((1 0) (3 4))

(defun interval-in-list (lis interv &optional (start 0))
  "collects all values in a specific interval from a given position in a list" 
  (loop for y in lis
     for stval = (nth start lis)
     when (=  (mod (- y stval) interv) 0)
     collect y))
;; (interval-in-list '(45 78 67 89 34 56 90 22 11 112) 5) => (45 90)
;; (interval-in-list '(45 78 67 89 34 56 90 22 11 112) 5 2) => (67 22 112)

(defun remove-sequent (lis)
  "remove same sequent numbers."
(let ((last nil))
(loop for i in lis   
      when (not (equal last i)) collect i into reslis
      do (setf last i)
      finally (return reslis))))

;; (remove-sequent '(2 3 4 5 5 6 7 5 8)) => (2 3 4 5 6 7 5 8)

(defun modulo-reflect (lis x)
  ""
  (let ((plis (append lis (reverse (cdr (butlast lis))))))
    (nth (mod (abs x) (length plis)) plis)))

;; (loop for i from -5 to 9 collect (modulo-reflect '(1 2 3 4 5) i)) 
;; => (4 5 4 3 2 1 2 3 4 5 4 3 2 1 2)

(defun make-element-list (lis)
  (loop for j from 0 below (- (length lis) 1) by 2
        append (make-list (nth (+ 1 j) lis) :initial-element (nth j lis)) into reslis
        finally (return reslis)))

; (make-element-list '(1 4 2 5 3 6)) => (1 1 1 1 2 2 2 2 2 3 3 3 3 3 3)

(defun last-elt (list)
  "Return the last element of a list."
  (first (last list)))
  
(defun left-rotate (list)
  "Move the first element to the end of the list."
  (append (rest list) (list (first list))))

(defun right-rotate (list)
  "Move the last element to the front of the list."
  (append (last list) (butlast list)))

(defun transpose-matrix (list-of-lists)
  "Transpose a matrix represented as a list of lists.
  Example: (transpose '((a b c) (d e f))) => ((a d) (b e) (c f))."
  (apply #'mapcar #'list list-of-lists))
  
(defun insert-between (item list)
  "Insert item between every element of list."
  (if (or (null list) (length=1 list))
      list
    (list* (first list) item (insert-between item (rest list)))))
    
(defun reflect (hi lo values)
  (loop for i in values
        with x = (abs (- hi lo))
        for y = (mod i x)
        collect
        (if (evenp (floor (/ i x))) y (- x y)) into reslis
        finally (return (mapcar #'(lambda (x) (+ x  lo)) reslis))))
;; (reflect 5 -1 '(-5 9 6 2 -4 -5 0 6 9 -5 -3 -5 9 -10 0 2 7 4 5 -2 2))
;; => (4 2 5 1 3 4 -1 5 2 4 2 4 2 1 -1 1 4 3 4 1 1)

(defun fibonacci-sum (n)
  "Tail-recursive Fibonacci number function"
  (labels ((calc-fib (n a b)
	     (if (= n 0)
		 a
		 (calc-fib (- n 1) b (+ a b)))))
    (calc-fib n 0 1)))
;; (fibonacci-sum 40) => 102334155

(defun count-all (lst)
  (let ((rd-lst (remove-duplicates lst :test #'equalp)))
    (mapcar #'(lambda (x) (count x lst)) rd-lst)))

;;; ???
;; (count-all '(3 4 2 5 5 7 6 8 3 4 2 5 1 6 4 7)) => (1 2 2 3 1 2 3 2)

(defun identical-lists? (lis1 lis2)
  "compares if two lists are identical"
(loop for i in lis1
  for j in lis2
  collect (equal i j) into reslis
  finally (return (if (member nil reslis) nil t)))) 

;; (identical-lists? '(1 2 3 4) '(1 2 3 4)) => t
;; (identical-lists? '(1 2 3 4) '(1 2 5 4)) => nil

(defun different-lists? (lis1 lis2)
  "compares if two lists are different"
(loop for i in lis1
  for j in lis2
  collect (equal i j) into reslis
  finally (return (if (member nil reslis) t nil)))) 

;; (different-lists? '(1 2 3 4) '(1 2 3 4)) => nil
;; (different-lists? '(1 2 3 4) '(1 2 5 4)) => t
;; (different-lists? '(1 2 3 4) '(2 1 3 4)) => t

(defun opposite-lists? (lis1 lis2)
  "Compares if two lists are opposite. 
  (Each element of lis1 with the same index in lis2 is different.)."
  (if (numberp (first lis2))
      (loop for i in lis1
            for j in lis2
            collect (equal i j) into reslis
            finally (return (if (member t reslis) nil t)))
    (loop for i in lis2
          append
          (loop for j in i
                for k in lis1
                collect (equal i j)) into reslis
          finally (return (if (member t reslis) nil t)))))

;; (opposite-lists? '(1 2 3 4) '(3 1 4 2)) => t
;; (opposite-lists? '(1 2 3 4) '(4 2 1 3)) => nil
;; (opposite-lists? '(1 2 3 4) '((3 1 4 2) (4 3 2 1) (2 4 1 3))) => t

(defun x-dx (ls)
  "Returns the list of the intervals between the contiguous values of a list <ls>. <ls> can also be a list of lists of intervals. Used convert notes to intervals."
  (labels ((x-dx-fun (lst)
             (loop for i in lst
               for j in (cdr lst)
               collect (- j i))))
    (if (numberp (first ls))
      (x-dx-fun ls)
      (mapcar #'x-dx-fun ls))))

;; (x-dx '(0.5 0.5 0.25 0.25 0.25 0.3)) => 0.0 -0.25 0.0 0.0 0.050000012)
;; (x-dx '((0.5 0.5 0.25) (0.25 0.25 0.3))) => ((0.0 -0.25) (0.0 0.050000012))

(defun dx-x (ls &optional (st 0))
  "Constructs a list of numbers from <start> with the consecutives intervals of <ls>.
  <ls> can also be a list of lists of intervals. Used convert rhythms to entry points."
  (labels ((dx-x-fun (ls &optional (st 0))
             (let ((r st))     
               (loop for i in ls
                 for n = (+ r i)
                 collect n into reslis
                 do (setf r n)
                 finally (return (cons st reslis))))))
    (if (numberp (first ls))
      (dx-x-fun ls st)
      (loop for i in ls collect (dx-x-fun i st)))))

;; (dx-x '(0.5 0.5 0.25 0.25 0.25 0.3)) => (0 0.5 1.0 1.25 1.5 1.75 2.05)
;; (dx-x '(0.5 0.5 0.25 0.25 0.25 0.3) 3) => (3 3.5 4.0 4.25 4.5 4.75 5.05)
;; (dx-x '((0.5 0.5) (0.25 0.25 0.25 0.3)) 3) => ((3 3.5 4.0) (3 3.25 3.5 3.75 4.05))

(defmacro sort-lists (lists &key (sort-id 'first))
  `(sort (copy-seq ,lists) #'< :key #',sort-id))

;; (setq testlis '((0) (1) (2) (0 1 5) (0 1 3) (0 1 5) (0 3 0) (0) (1) (2 7 19) (0 0 3 0)))
;; (sort-lists testlis) => ((0) (0 1 5) (0 1 3) (0 1 5) (0 3 0) (0) (0 0 3 0) (1) (1) (2) (2 7 19))
;; (sort-lists '((3 4) (2 7) (6 3) (1 7))) => ((1 7) (2 7) (3 4) (6 3))
;; (sort-lists '((0 1 5)(0 1 3)(0 1 5)(0 3 0)(2 7 19) (0 0 3 0)) :sort-id second) => ((0 0 3 0) (0 1 5) (0 1 3) (0 1 5) (0 3 0) (2 7 19))

(defun list-permute (lis permutation)
  "Calculates a permutation (positive or negative) of a list."
  (let ((ln (length lis)))
    (loop for i from 0 to (- ln 1)
      for k from permutation
      collect (nth (mod k ln) lis))))

;; (list-permute '(1 2 3 4 5 6) 0) => (1 2 3 4 5 6)
;; (list-permute '(1 2 3 4 5 6) 2) => (3 4 5 6 1 2)
;; (list-permute '(1 2 3 4 5 6) -2) => (5 6 1 2 3 4)

(defun permute-list (lis &optional (start-permutation 0) (permutations 1) (dir :fw))
  "Calculates permutations of a list given a start-permutation, the number of permutations and the directions of this permustions: forward (:fw) or backward (:bw)."  
  (labels ((list-permute (lis permutation)
             (let ((ln (length lis)))
               (loop for i from 0 to (- ln 1)
                 for k from permutation
                 collect (nth (mod k ln) lis)))))
    (case dir
      (:fw
       (loop repeat permutations
         for i from start-permutation
         if (= permutations 1)
         append (list-permute lis i)
         else
         collect (list-permute lis i)))
      (:bw
       (loop repeat permutations
         for i downfrom start-permutation
         if (= permutations 1)
         append (list-permute lis i)
         else
         collect (list-permute lis i)))
      (otherwise (error "Wrong direction specifier!")))))

;; (permute-list '(1 2 3 4 5 6)) => (1 2 3 4 5 6)
;; (permute-list '(1 2 3 4 5 6) 2) => (3 4 5 6 1 2)
;; (permute-list '(1 2 3 4 5 6) 2 4) => ((3 4 5 6 1 2) (4 5 6 1 2 3) (5 6 1 2 3 4) (6 1 2 3 4 5))
;; (permute-list '(1 2 3 4 5 6) 2 3 :bw) => ((3 4 5 6 1 2) (2 3 4 5 6 1) (1 2 3 4 5 6))
;; (permute-list '(1 2 3 4 5 6) 2 4 :back) => > Error: Wrong direction specifier!

(defun number-of-elements (lis)
  "Compute the number of elements in a list and returns a list of lists containing 
  the element and the number of occurrences."
  (let ((l (sort (remove-duplicates lis) #'<)))
    (loop for i in l
      collect (list i (count i lis)))))

;; (number-of-elements (print (loop for i from 1 to 20 collect (random 10)))) => 
;; (7 2 7 6 5 2 0 6 0 9 9 6 4 1 3 1 7 5 6 2) 
;; ((0 2) (1 2) (2 3) (3 1) (4 1) (5 2) (6 4) (7 3) (9 2))

;;; Median
;;; The Median is the 'middle number' (in a sorted list of numbers).

(defun median (lis &key type)
  "Middle number in a list."
  (let* ((len (length lis))
         (slis (sort lis '< ))
         (half (/ len 2)))  
    (if (oddp len)
        (nth (/ (- len 1) 2) slis)
      (case type
        ('virtual (/ (+ (nth (- half 1) slis) (nth half slis)) 2.0))
        (otherwise (list (nth (- half 1) slis) (nth half slis)))))))

;; (median '(5 2 4 1 3)) => 3
;; (median '(5 2 4 1 3 6)) => 3 4
;; (median '(5 2 4 1 3 6) :type 'virtual) => 3.5

(defun half-minmax (lis)
  "The middle value of min and max in a list."
  (let ((mi (apply #'min lis))
        (ma (apply #'max lis)))
    (/ (+ mi ma) 2.0)))

;; (half-minmax '(4 3 6 5 7 8 2 9 1 10)) => 5.5
;; (half-minmax '(0 4 3 6 5 7 8 2 9 1 10)) => 5.0

(defun normalise-list (lis)
  "Normalize the sum of a list to 1."
  (let ((sum-lis (reduce #'+ lis)))
    (loop for i in lis collect (* 1.0 (/ i sum-lis)))))

;; (normalise-list '(3 4 2 5 6)) => (0.15 0.2 0.1 0.25 0.3) 
;; (reduce #'+ (normalise-list '(3 4 2 5 6))) => 1.0

(defun palindrome (lis &key double)  
  (if double (append lis (reverse lis))
    (append lis (cdr (reverse lis)))))

;; (palindrome '(1 2 3 4 5)) => (1 2 3 4 5 4 3 2 1)
;; (palindrome '(1 4 2 5 3 6)) => 1 4 2 5 3 6 3 5 2 4 1)
;; (palindrome '(1 4 2 5) :double t) => (1 4 2 5 5 2 4 1)
;; (palindrome (loop for i from 1 to 4 collect i)) => (1 2 3 4 3 2 1)

(defun trans-mat (list-of-lists)
  "transpose a matrix represented as a list of lists.
  Example: (trans-mat '((a b c) (d e f))) => ((a d) (b e) (c f))." 
  (apply #'mapcar #'list list-of-lists))

(defun shift-cut-lst (lst shift numb)
  (loop repeat shift
    with reslis = lst 
    do (setf reslis (butlast (append (list numb) reslis)))
    finally (return reslis)))

;; (shift-cut-lst '(1 2 3 4 5) 1 1) ; => (1 1 2 3 4)
;; (shift-cut-lst '(1 2 3) 1 0) => (0 1 2)

(defun shift-ext-lst (lst shift numb)
  (loop repeat shift
    with reslis = lst 
    do (setf reslis (append (list numb) reslis))
    finally (return reslis)))

;; (shift-ext-lst '(1 2 3 4 5) 2 1) ; => (1 1 1 2 3 4 5)
;; (shift-ext-lst '(5 4 3) 1 0) => (0 5 4 3)

(defun shift-replace-lst (lst shift numb)
  (let ((ones (loop repeat shift collect numb)))
    (append ones (subseq lst shift))))

;; (shift-replace-lst '(1 2 3) 2 0) => (0 0 3)

(defun replace-nth (n val lis)
         "Replace position <n> with <val> in <lis>."
         (loop for i from 0 
           for j in lis 
           collect (if (= i n) val j)))

;; (replace-nth 1 10 '(1 2 3 4 5 6 7)) => (1 10 3 4 5 6 7)

(defun sequencep (x)
  (or (listp x) (vectorp x)))

(defun listsp (l)
  (and (sequencep l) (every #'sequencep l)))

(defun maybe-map (function sequence &rest more-sequences)
  "Determines if the sequence is a nested sequence, and if so, 
  maps the function across the subsequences."
  (if (listsp sequence)
    (apply #'map (class-of sequence) function sequence more-sequences)
    (apply function sequence more-sequences)))

(defun signum-reduce (length)
  (maybe-map (lambda (pattern)
               (prog (out element store previous)
                  (setf previous (signum (car pattern)))
                  (setf store 0)
                loop
                  (cond ((null pattern)
                         (push store out)
                         (return (nreverse out))))
                  (setf element (car pattern))
                  (cond ((not (= previous (signum element)))
                         (push store out)
                         (setf store 0)
                         (setf previous (signum element))))
                  (setf store (+ store element))
                  (pop pattern)
                  (go loop)))
             length))

;; ;; (signum-reduce '(-1 -1 1 1 1 1 -1 -1))

(defun divide-range (start end samples &key rounded)
  "n equally spaced samples between (including) start and end."
  (loop repeat samples
    with >stnd = (> start end)
    with st = (if >stnd end start)
    with nd = (if >stnd start end)
    with step = (/ (- nd st) (- samples 1))
    with counter = st
    collect (if rounded (round counter) counter) into reslis
    do (setf counter (+ counter step))
    finally (return (if >stnd (reverse reslis) reslis))))

;; (divide-range 59.0 83.5 8)
;; => (59.0 62.5 66.0 69.5 73.0 76.5 80.0 83.5)
;; (divide-range 83.5 59.0 8)
;; => (83.5 80.0 76.5 73.0 69.5 66.0 62.5 59.0)
;; (divide-range 59.0 83.5 8 :rounded t) ; => (59 62 66 70 73 76 80 84)
;; (divide-range 83.5 59.0 8 :rounded t) ; => (84 80 76 73 70 66 62 59)

#|
(loop 
  with lst = (divide-range 59.0 83.5 8)
  for i in lst
  for j in (cdr lst)
  collect (- j i))
=> (3.5 3.5 3.5 3.5 3.5 3.5 3.5)
|#

(defun group-number (num grp)
  "Make a list with grp numbers fitting in num. If grp does not fit
  in num the last entry in the list is respectively reduced."
  (let* ((rest (rem num grp))
         (div (/ (- num rest) grp)))
    (loop repeat div
      collect grp into reslis
      finally (return 
               (if (zerop rest) 
                 reslis
                 (reverse (cons rest reslis)))))))

;; (group-number 12 3) => (3 3 3 3)
;; (group-number 12 4) => (4 4 4);; 
;; (group-number 10 4) => (4 4 2)
;; (group-number 11 3) => (3 3 3 2)

(defun group-list (inlis grps)
  "Groups list into subsequences, where grps indicates the length of each sublist. 
  Grps can be a number or a list of numbers. 
  If inlis is not exhausted by grps, the remaining values are ignored. 
  If inlis is exceeded by grps, the last group is respectively shortened."
(labels 
    ((dx-x (ls &optional (st 0))
       "Constructs a list of numbers from <start> with the consecutives intervals of <ls>."
       (let ((r st))     
         (loop for i in ls
           for n = (+ r i)
           collect n into reslis
           do (setf r n)
           finally (return (cons st reslis)))))
     (group-number (num grp)
        "Make a list with grp numbers fitting in num. 
       If grp does not fit in num the last entry in the list is respectively reduced."
       (let* ((rest (rem num grp))
              (div (/ (- num rest) grp)))
         (loop repeat div
           collect grp into reslis
           finally (return 
                    (if (zerop rest) 
                      reslis
                      (reverse (cons rest reslis))))))))
(let* ((ll (length inlis))
      (pl (if (numberp grps) 
            (dx-x (group-number ll grps))
            (dx-x grps))))
  (loop 
    for i in pl
    for j in (cdr pl)
    collect (subseq inlis i (if (> j ll) ll j))))))

;; (group-list '(1 2 3 4 5 6 7 8 9) 3) => ((1 2 3) (4 5 6) (7 8 9))
;; (group-list '(1 2 3 4 5 6 7 8 9) 2) => ((1 2) (3 4) (5 6) (7 8) (9))
;; (group-list '(1 2 3 4 5 6 7 8 9) 4) => ((1 2 3 4) (5 6 7 8) (9))
;; (group-list '(1 2 3 4 5 6 7 8 9) 10) => ((1 2 3 4 5 6 7 8 9))
;; (group-list '(1 2 3 4 5 6 7 8 9) '(3 3 3)) => ((1 2 3) (4 5 6) (7 8 9))
;; (group-list '(1 2 3 4 5 6 7 8 9) '(2 3 4)) => ((1 2) (3 4 5) (6 7 8 9))
;; (group-list '(1 2 3 4 5 6 7 8 9) '(2 3 6)) => ((1 2) (3 4 5) (6 7 8 9))
;; (group-list '(1 2 3 4 5 6 7 8 9) '(2 3 2)) => ((1 2) (3 4 5) (6 7))

(defun replace-in-list (lis rep)
  (labels
      ((replace-nth (n val lis)
         "Replace position <n> with <val> in <lis>."
         (loop for i from 0 
           for j in lis 
           collect (if (= i n) val j))))
(loop repeat rep
  for ll = (length lis)
  with ls = lis
  collect ls into reslis
  do (setf ls (replace-nth (random ll) (random ll) ls))
  finally (return reslis))))

;; (replace-in-list '(1 2 3 4 5 6 7) 10)

(defun list-transform (rep lis count)
  (labels
      ((replace-nth (n val lis)
         "Replace position <n> with <val> in <lis>."
         (loop for i from 0 
           for j in lis 
           collect (if (= i n) val j)))
       (shuffle (lis &optional (rep 1) &key (total NIL))
         "Returns a list of the same with the elements randomly reordered"
         (let* ((llis (length lis))
                (end (if total rep (* rep llis))))
           (loop with cdrlis and n
             for i from 0 below end do  
             (when (zerop (mod i llis)) (setf cdrlis lis))	            
             (setf n (nth (random (length cdrlis)) cdrlis))   
             (setf cdrlis (remove n cdrlis))      
             collect n))))
(let* ((ll (length lis))
       (cnt (if (numberp count) (list count) count))
       (idlis (loop for i from 0 below ll collect i)))
(loop repeat rep
  with ls = lis
  for cnt-ran = (nth (random (length cnt)) cnt)
  for shuffle-ids = (shuffle idlis)
  collect ls into reslis
  do (loop for i from 0 below cnt-ran
       do (setf ls (replace-nth (nth i shuffle-ids) (random ll) ls)))
  finally (return reslis)))))

;; (list-transform 3 '(1 2 3 4 5 6 7) 2) 
;; => ((1 2 3 4 5 6 7) (1 4 3 4 5 6 0) (1 4 3 4 0 6 3))


(defun nestedp (lists)
  (if (listp (second lists)) t
      nil))

;; (nestedp '(1 2 3 4 5)) => nil
;; (nestedp '((1 2 3 4 5) nil)) => t
;; (nestedp '((1 2 3 4 5) (6 7 8 9))) => t

(defun cycle (lis rep &key (abs nil))
  ""
  (loop repeat rep 
    for i from 0 
    if abs
    collect  (nth (mod i (length lis)) lis)
    else
    append lis
    end))

;; (cycle '(1 2 3 4) 3) => (1 2 3 4 1 2 3 4 1 2 3 4)
;; (cycle '(1 2 3 4) 11 :abs t) => (1 2 3 4 1 2 3 4 1 2 3)

(defun member? (m lst)
  (if (member m lst) t nil))

;; (member? 'a '(b 3 45 a c)) => t
;; (member? '10 '(b 3 45 a c)) => nil
;; (member? '10 '(b 3 45 a 10)) => t

;;; TODO ;;;
;;; loop deterministisch dann heuristisch


