(in-package :OPENMUSIC)

(defmethod! remove-consecutive-duplicates (list &key test)
  :icon 235
  (let ((test (or test #'equalp)))
    (if (cdr list)
        (if (funcall test (car list) (cadr list)) ; 1st+2nd duplicates
            (if (cddr list) ; 
                (remove-consecutive-duplicates (append (list (car list)) (cddr list)) :test test)
              (butlast list))
          (append (list (car list)) (remove-consecutive-duplicates (cdr list) :test test))) ; not duplicates
      list)))

(defmethod! write-textfile (input label ext &optional timezone)
  :indoc '("input" "label" "ext" "timezone")
  :icon 908
  :doc ""
  (labels
      ((format-filename (label) 
         (multiple-value-bind 
             (second minute hour date month year day) 
             (decode-universal-time (get-universal-time))
           (format nil "~A_~A-~A-~A-~A_~A_~A.~A" label month day year hour minute second ext))))
    (let ((filename (format-filename label)))
      (with-open-file (str (om::outfile filename)
                           :direction :output
                           :if-exists :supersede
                           :if-does-not-exist :create)
        (format str (write-to-string input)))
      filename)))



(in-package :SCREAMER-USER)
(defun an-expanded-list (templates rules &key randomize-choices) ; delete
  (let ((r (solution (an-expanded-list-rec 1 templates :randomize-choices randomize-choices)
                     (static-ordering #'linear-force))))
    (unless (every #'(lambda (rule) (funcall rule r)) (if (consp rules) rules (list rules))) (fail))
    r))
(defun an-expanded-list-rec (len templates &key randomize-choices)
  (either    
    (mapcar #'(lambda (x) (if randomize-choices (a-random-member-ofv templates) (a-member-ofv templates))) (make-sequence 'list len))
    (an-expanded-list-rec (1+ len) templates :randomize-choices randomize-choices)))
(defun evaluate-an-expanded-list-function (input r)
  (cond ((null r) t)
        ((consp r)
         (and (evaluate input (car r))
              (evaluate input (cdr r))))
        ((functionp r)
         (funcall r input))
        (t nil)))

(in-package :SCREAMER)
(defun grouplist-nondeterministic (list min-groupsize max-groupsize wpartitioncount wmeangroupsize wdeviation wdistancefrommedian )
  (let ((points (a-realv)))
    (map-func
         (let (items)
           (dolist (x list)
             (push x items))
           (setf items (reverse items))
           (print 'mapfunc)
           #'(lambda (x) (pop items)))
         (mapcar #'make-list (car 
     (best-value     
      (let ((min-groupsize (or (and min-groupsize (min min-groupsize (1- (length list)))) 1))
            (max-groupsize (or (and max-groupsize (min max-groupsize (1- (length list)))) (1- (length list))))
            (wpartitioncount (or wpartitioncount 0))
            (wmeangroupsize (or wmeangroupsize 0.5))
            (wdeviation (or wdeviation 0.5))
            (wdistancefrommedian (or wdistancefrommedian 1)))
       ;(print "<<<<<<<>>>>>>>>>>>>")
       ;(print (list 'grouplist-nondeterministic 'min-groupsize min-groupsize 'max-groupsize max-groupsize ))
        (let ((min2maxseries (let ((w min-groupsize)
                                   (stack nil))
                               (dotimes (- max-groupsize min-groupsize) 
                                 (push (incf w) stack))
                               (reverse stack)))
              (min-count (max 1 (floor (/ (length list) max-groupsize))))
              (max-count (min (length list) (floor (/ (length list) min-groupsize)))))
          (let ((median-groupsize (nth (floor (/ (length min2maxseries) 2)) min2maxseries)))
        ;(print (format nil " grouplist-nondeterministic min-count ~A, max-count ~A for min-groupsize ~A, max-groupsize ~A; median-groupsize: ~A" min-count max-count min-groupsize max-groupsize median-groupsize))
           ;(print (list 'median-groupsize median-groupsize))
            (let ((partitions (an-integer-between 1 (length list))))
              (when (< partitions min-count) (fail))
              (when (> partitions max-count) (fail))
             ;(print (list 'partitions partitions))
              (let ((group-counts (mapcar #'(lambda (x) (an-integer-betweenv min-groupsize max-groupsize)) 
                                          (make-list partitions))))
                  (let ((total (apply #'+v group-counts))
                        (mean-groupsize (a-realv)))
                    (assert! (=v total (length list)))
                    (assert! (equalv mean-groupsize (/v total partitions)))
                 ;(print (list 'group-counts group-counts 'total total))
                 ;(print (list 'mean-groupsize mean-groupsize))
                (let ((rs (solution group-counts (static-ordering #'linear-force))))
                    (let ((deviation (/v (apply #'+v (mapcar #'(lambda (xs) (-v mean-groupsize xs)) group-counts))
                                         partitions)))
                      (let ((points-for-partitioncount
                             (*v wpartitioncount (/ (length list) partitions)))
                            (points-for-meangroupsize 
                             (*v wmeangroupsize mean-groupsize))
                            (points-for-deviation
                             (*v wdeviation deviation))
                            (points-for-distancefrommedian 
                             (*v wdistancefrommedian (-v median-groupsize mean-groupsize))))
                        (assert! (equalv points (+v points-for-partitioncount 
                                                    points-for-meangroupsize
                                                    points-for-deviation
                                                    points-for-distancefrommedian))))
                      (print (list partitions :points points :list rs))
                      rs))))))))
      points))))))

(in-package :OPENMUSIC)
(defmethod get-boxcallclass-fun ((self (eql 'grouplist-nondeterministic))) 'screamerboxes)
(defmethod! grouplist-nondeterministic (list min-groupsize max-groupsize wpartitioncount wmeangroupsize wdeviation wdistancefrommedian)
  :icon 235 
  :indoc '("list" "min-groupsize" "max-groupsize" "wpartitioncount" "wmeangroupsize" "wdeviation" "wdistancefrommedian")
  :initvals '(nil 1 5 0 1 1 -1)
  (s::grouplist-nondeterministic min-groupsize max-groupsize wpartitioncount wmeangroupsize wdeviation wdistancefrommedian))

(in-package :OPENMUSIC)
(defmethod! group-seqc-by-motion-type (sequence)
  :icon 261
  (let (fragments)
    (labels
        ((group-sequence (sequence)
           (mapcar
            #'unflatten
            (mapcar 
             #'mat-trans
             (group-list-on
                 (let ((i -1))
                   #'(lambda (xs ys)
                       (block test
                         ; (print (format nil ">>> xs: ~A ys: ~A" xs ys))
                         (mapcar #'(lambda (x y) 
                                     (cond 
                                      ((and (null x) (null y))
                                       (return-from test T))
                                      ((or (null x) (null y))
                                       (return-from test nil))))
                                 xs ys)
                         (or (equal xs ys)
                             (let ((count 0)) 
                               (mapcar #'(lambda (x y) (if (not (equal x y)) (incf count))) xs ys)
                               (if (= count 1)
                                   (loop for j from 0 while (< j (length xs)) do
                                           (if (not (equal (elt xs j) (elt ys j)))
                                               (cond 
                                                ((or (= i -1) (= i j))
                                                 (setf i j)
                                                 (return-from test T))
                                                (T 
                                                 (setf i -1)
                                                 (return-from test nil)))))
                                 (progn
                                   (setf i -1)
                                   nil)))))))
           (mat-trans (flatten-seqc sequence))))))
         
         (firsts (seqc) (mapcar #'list (mapcar #'car (mapcar #'flatt (mapcar #'list seqc)))))
         (flatten-with-continuation (seqc next)
           (flatten-seqc (mat-trans (list seqc (firsts next)))))
         (unflatten (seqc)
           (mapcar #'(lambda (voice) 
                       (cond 
                        ((= 1 (length voice)) (car voice))
                        (T voice)))
                   (mapcar #'remove-consecutive-duplicates seqc)))
         (register (xs)
           (unless (find xs fragments :test #'equal)
             (push xs fragments)))
         (register-fragments (sequence-list)
           (maplist #'(lambda (sequences)
                        (let ((seqc (car sequences))
                              (next (if (cdr sequences)
                                        (cadr sequences)
                                      (make-list (length (car sequences)) :initial-element '(nil)))))
                          (register (list (flatten-with-continuation seqc next) 
                                          seqc
                                          next))))
                    sequence-list)))
      
      (mapcar #'(lambda (voices2x) 
                  (register-fragments (group-sequence voices2x)))
              (list2comb sequence))

      (maplist #'(lambda (sequences)
               (let ((seqc (car sequences))
                     (next (if (cdr sequences)
                               (cadr sequences)
                             (make-list (length (car sequences)) :initial-element '(nil)))))
                 (mapcar #'(lambda (seqc next)
                             (register (list (flatten-with-continuation seqc next) 
                                          seqc
                                          next)))
                         (list2comb seqc)
                         (list2comb next))))
               (group-sequence sequence))

      (reverse fragments))))


(defmethod! group-and-apply-definitions (sequence selector-fn definition &rest defns)
  :icon 908
  (let (fragments)
    (labels
        ((group-sequence (sequence)
           (mapcar
            #'unflatten
            (mapcar 
             #'mat-trans
             (group-list-on
                 (let ((i -1))
                   #'(lambda (xs ys)
                       (block test
                         ; (print (format nil ">>> xs: ~A ys: ~A" xs ys))
                         (mapcar #'(lambda (x y) 
                                     (cond 
                                      ((and (null x) (null y))
                                       (return-from test T))
                                      ((or (null x) (null y))
                                       (return-from test nil))))
                                 xs ys)
                         (or (equal xs ys)
                             (let ((count 0)) 
                               (mapcar #'(lambda (x y) (if (not (equal x y)) (incf count))) xs ys)
                               (if (= count 1)
                                   (loop for j from 0 while (< j (length xs)) do
                                           (if (not (equal (elt xs j) (elt ys j)))
                                               (cond 
                                                ((or (= i -1) (= i j))
                                                 (setf i j)
                                                 (return-from test T))
                                                (T 
                                                 (setf i -1)
                                                 (return-from test nil)))))
                                 (progn
                                   (setf i -1)
                                   nil)))))))
           (mat-trans (flatten-seqc sequence))))))
         
         (firsts (seqc) (mapcar #'list (mapcar #'car (mapcar #'flatt (mapcar #'list seqc)))))
         (flatten-with-continuation (seqc next)
           (flatten-seqc (mat-trans (list seqc (firsts next)))))
         (unflatten (seqc)
           (mapcar #'(lambda (voice) 
                       (cond 
                        ((= 1 (length voice)) (car voice))
                        (T voice)))
                   (mapcar #'remove-consecutive-duplicates seqc)))
         (register (xs)
           (unless (find xs fragments :test #'equal)
             (push xs fragments)))
         (register-fragments (sequence-list)
           (maplist #'(lambda (sequences)
                        (let ((seqc (car sequences))
                              (next (if (cdr sequences)
                                        (cadr sequences)
                                      (make-list (length (car sequences)) :initial-element '(nil)))))
                          (register (list (flatten-with-continuation seqc next) 
                                          seqc
                                          next))))
                    sequence-list)))
      
      (mapcar #'(lambda (voices2x) 
                  (register-fragments (group-sequence voices2x)))
              (list2comb sequence))

      (maplist #'(lambda (sequences)
               (let ((seqc (car sequences))
                     (next (if (cdr sequences)
                               (cadr sequences)
                             (make-list (length (car sequences)) :initial-element '(nil)))))
                 (mapcar #'(lambda (seqc next)
                             (register (list (flatten-with-continuation seqc next) 
                                          seqc
                                          next)))
                         (list2comb seqc)
                         (list2comb next))))
               (group-sequence sequence))

      (setf fragments (nreverse fragments))

      (let ((fragments-to-process (remove-if-not #'(lambda (entry) 
                                                     (let ((selected? (funcall selector-fn (cadr entry))))
                                                       ; (print (format nil "entry: ~A selected? ~A" entry selected?))
                                                       selected?)) fragments)))
        (if (null fragments-to-process)
            T
          (progn
            (fecho "~A out ~A fragments" (length fragments-to-process) (length fragments))
            (apply #'andv (mapcar #'(lambda (entry) 
                         (apply #'andv (mapcar #'(lambda (defn)
                                      (funcall defn (first entry) (second entry) (third entry)))
                                  (append (list definition) defns))))
                     fragments-to-process))))))))