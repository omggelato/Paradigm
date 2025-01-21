

(in-package :OPENMUSIC)

(cl:defun consecutive-open-intervals-2-internal-fn (voice2x)
  (labels
      ((lists=v (xs ys) (apply #'andv (mapcar #'(lambda (x y) (=v x y)) xs ys))))
    (maplist?and
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
               (labels
                   ((interval%12 (x y) (%v (-v (maxv x y) (minv x y)) 12)))
                 (let ((chords-repeat (andv (lists=v chord1 chord2)))
                       (interval1 (interval%12 (car chord1) (cadr chord1)))
                       (interval2 (interval%12 (car chord2) (cadr chord2))))
                   (orv 
                    (andv (notv (andv (=v interval1 7) (=v interval2 7)))
                          (notv (andv (=v interval1 0) (=v interval2 0))))
                    chords-repeat)))))))))
     (mat-trans voice2x))))

(defmethod! consecutive-open-intervals!!-3 (sequence)
  :icon 1100
  (dolist (voice2x (list2comb (flatten-seqc sequence)) )
    (consecutive-open-intervals-3-internal-fn voice2x))
  sequence)

(cl:defun consecutive-open-intervals-3-internal-fn (voice2x)
  (labels
      ((lists=v (xs ys) (apply #'andv (mapcar #'(lambda (x y) (=v x y)) xs ys))))
    (maplist
     #'(lambda (chord-list)
         (print (format nil "chord-list: ~A" chord-list))
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
                 (let ((chords-repeat (andv (lists=v chord1 chord2)))
                       (interval1 (interval%12 (car chord1) (cadr chord1)))
                       (interval2 (interval%12 (car chord2) (cadr chord2))))
                   (print (format nil "interval1: ~A interval2: ~A" interval1 interval2))
                   (s:assert!
                    (orv 
                     (andv (notv (andv (=v interval1 7) (=v interval2 7)))
                           (notv (andv (=v interval1 0) (=v interval2 0))))
                     chords-repeat))))))))))
     (mat-trans voice2x))))

