(in-package :OPENMUSIC)

(cl:defun consecutive-open-intervals-2-internal-fn (voice2x)
  (labels
      ((lists=v (xs ys) (apply #'andv (mapcar #'(lambda (x y) (=v x y)) xs ys)))
       (interval%12 (x y) (?% (-v (?max x y) (?min x y)) 12)))
    (maplist
     #'(lambda (chord-list)
         (cond 
          ((null chord-list) T)
          ((not (cdr chord-list)) T)     
          (T
           (let ((chord1 (car chord-list)) (chord2 (cadr chord-list)))
             (cond
              ((has-null-values chord1) T)
              ((has-null-values chord2) T)
              ((equal chord1 chord2) T)
              ((equal (car chord1) (car chord2)) T)
              ((equal (cadr chord1) (cadr chord2)) T)
              (T
               (let ((interval1 (apply #'interval%12 chord1))
                     (interval2 (apply #'interval%12 chord2)))
                 ; (orv (lists=v chord1 chord2)
                      (andv (notv (=v 7 interval1 interval2))
                            (notv (=v 0 interval1 interval2))))))))))
     (mat-trans voice2x))))

(defmethod! consecutive-open-intervals!!-3 (sequence)
  :icon 1100
  (if (null sequence)
      T
    (progn
      (format om-lisp::*om-stream* "consecutive-open-intervals!!-3 (~A*2x)" (if sequence (length (list2comb (flatten-seqc sequence)))))
      (dolist (voice2x (list2comb (flatten-seqc sequence)) )
        (consecutive-open-intervals-3-internal-fn voice2x)                       
        (format om-lisp::*om-stream* ","))
      (format om-lisp::*om-stream* "~%")
      sequence)))

(cl:defun consecutive-open-intervals-3-internal-fn (voice2x)
  (labels
      ((lists=v (xs ys) (apply #'andv (mapcar #'(lambda (x y) (=v x y)) xs ys))))
    (maplist
     #'(lambda (chord-list)
         (s:assert!
          (cond 
           ((null chord-list) T)
           ((not (cdr chord-list)) T)     
           (T
            (let ((chord1 (car chord-list)) (chord2 (cadr chord-list)))
              (cond
               ((has-null-values chord1) T)
               ((has-null-values chord2) T)
               ((equal chord1 chord2) T)
               ((equal (car chord1) (car chord2)) T)
               ((equal (cadr chord1) (cadr chord2)) T)
               (T
                (labels
                    ((interval%12 (x y) (%v (-v (maxv x y) (minv x y)) 12)))
                  (let ((interval1 (interval%12 (car chord1) (cadr chord1)))
                        (interval2 (interval%12 (car chord2) (cadr chord2))))
                    (format om-lisp::*om-stream* ".")
                    (orv (lists=v chord1 chord2)
                         (notv (orv (andv (=v interval1 7) (=v interval2 7))
                                    (andv (=v interval1 0) (=v interval2 0))))))))))))))
     (mat-trans voice2x))))

