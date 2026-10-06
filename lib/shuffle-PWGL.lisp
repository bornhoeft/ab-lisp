(in-package :cl-user)

(defun shuffle (lis &key (rep 1) (total NIL) (seed 123))
  "Returns a list of the same with the elements randomly reordered"
  (let* ((llis (length lis))
	 (end (if total rep (* rep llis))))
    (loop with cdrlis and n
          with generator = (random-state::make-generator :mersenne-twister-32 seed)
       for i from 0 below end do  
       (when (zerop (mod i llis)) (setf cdrlis lis))	    
       ;; when modulo i from the length of lis is equal to 0
       ;; reset cdrlis to lis (see print)	         
       (setf n (nth (random-state::random-int generator 0 (- (length cdrlis) 1)) cdrlis))
       ;; takes a random element from cdrlis and
       (setf cdrlis (remove n cdrlis))
       ;; remove it
       collect n)))

(defun shuffle-search (fund pitch lst) 
(loop for x = (print (pw::nth-overtones fund (pw::dx->x 1 (cl-user::shuffle lst :seed nil))))
      until (numberp (find pitch x))
      finally (return x)))

;; (shuffle-search 48 60 '(1 2 3 4 5))


;;; connect invertals randomly with one similar note
(loop repeat rep
      with s = NIL ; seed
      with y = NIL ; last interval
      with w = 0
      initially (setf s start) ; first seed number
      initially (setf y (nth 1 intervals)) ; first interval
      for x =  (cl-user::shuffle intervals :seed s) ; shuffled list of intervals
      collect (loop for j in x
                    when (and
                          (if (= w 0)
                              (numberp (find (first y) j)) ; look in interval list if the first or 
                              (numberp (find (second y) j))) ; the second pitch is the same 
                          (not (equal j y))) ; and if its not the same interval as the last
                    return j) into reslis
      do (setf y (first (last reslis))) ; set the result to the new interval
      do (setf s (+ 1 s)) ; increment the seed value
      do (setf w (if (= w 0) 1 0))
      finally (return reslis))
;; (cl-user::shuffle '(1 2 3 4 5) :seed 234)



