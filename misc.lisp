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
                    sequence-list))
         (group-seqc-by-motion-type-internal (sequence)
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
           (reverse fragments))

         (group-seqc-by-motion-type-2 (sequence)
           (remove-duplicates            
            (remove
             nil
             (mapcar
              #'(lambda (xs)
                  (let ((trans (mat-trans xs)))
                    (if (and (has-null-values (car (reverse trans)))
                             (notany #'has-null-values (butlast trans)))
                        (mat-trans (butlast trans))
                      xs)))
              (mapcar
               #'(lambda (xs)
                   (let ((trans (mat-trans xs)))
                     (if (and (has-null-values (car trans))
                              (notany #'has-null-values (cdr trans)))
                         (mat-trans (cdr trans))
                       xs)))
               (remove-if
                #'(lambda (xs)
                    (or (= (length (mat-trans xs)) 1)
                        (and (<= (length (mat-trans xs)) 2)
                             (or (has-null-values (car xs))
                                 (has-null-values (cadr xs))))))
                (remove nil 
                        (flat1
                         (mapcar #'(lambda (xs) (group-two-voices (car xs) (cadr xs))) 
                                 (mapcar #'flatten-seqc (list2comb sequence)))))))))
            :from-end T
            :test #'(lambda (xs ys) (or (equal xs ys) (equal xs (reverse ys)) (equal (reverse xs) ys)))))
         
         (group-two-voices (voice1 voice2)
           (mapcar
            #'mat-trans
            (maplist
             #'(lambda (xs)
                 (let ((group (car xs))
                       (continuation (if (and (cdr xs) (not (every #'null (caadr xs))))
                                         (list (caadr xs))
                                       nil)))
                   (if (and (= 1 (length group))
                            (every #'null (car group)))
                       nil
                     (append group continuation))))
             (labels
                 ((empty? (xs) (every #'null xs)))
               (group-list-on 
                (let ((i NIL))
                  #'(lambda (xs ys)
                   ; (fecho NIL "xs: ~A ys: ~A" xs ys)
                      (cond 
                       ((every #'null xs)
                        (setf i NIL)
                        NIL)
                       ((every #'null ys)
                        (setf i NIL)
                        NIL)
                       ;; ??
                       ((or (and (not (every #'null (list (car xs) (car ys))))
                                 (some #'null (list (car xs) (car ys))))
                            (and (not (every #'null (list (cadr xs) (cadr ys))))
                                 (some #'null (list (cadr xs) (cadr ys)))))
                        (setf i NIL)
                        NIL)
                       ((and (equal (car xs) (car ys))
                             (or (null i) (= i 0)))
                        (setf i 0)
                        T)
                       ((and (equal (cadr xs) (cadr ys))
                             (or (null i) (= i 1)))
                        (setf i 1)
                        T)
                       (T
                        (setf i NIL)
                        NIL))))
                (remove-consecutive-duplicates (mat-trans (list voice1 voice2)))))))))
      (group-seqc-by-motion-type-2 sequence))))


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



(cl:defun consecutive-open-intervals! (sequence)
  (print (format nil "CONSECUTIVE-OPEN-INTERVALS"))
  (labels
      ((interval%12 (x y) (%v (om-v (ommaxv x y) (omminv x y)) 12)))
    (mapcar 
     #'(lambda (voice1-2)
         (maplist
          #'(lambda (chord-list)
              (if (or (not (cdr chord-list))
                      (has-null-values (flatt
                                        (list (car chord-list)
                                              (cadr chord-list)))))
                  T
                (let* ((chord1 (car chord-list))
                       (chord2 (cadr chord-list))
                       (interval1 (interval%12 (car chord1)
                                               (cadr chord1)))
                       (interval2 (interval%12 (car chord2)
                                               (cadr chord2)))
                       (assert! (orv
                                 (andv
                                  (notv (andv (=v interval1 7)
                                              (=v interval2 7)))
                                  (notv (and (=v interval1 0)
                                             (=v interval2 0))))
                                 (andv (=v (car chord1) (car chord2))
                                       (=v (cadr chord1) (cadr chord2)))))))))
          (mat-trans (flatten-seqc voice1-2))))
     (combinations-of2 sequence)))
  sequence)



(in-package :SCREAMER)

(cl:defun interleaf--sequence-counter (start end)
  (loop for i from start while (<= i end) collect i))

(defun interleaf-internal (list1 list2)
  (cond
   ((and (null list1) (null list2)) nil)
   ((null list1) (Fail))
   ((null list2) (Fail))
   (T
  (let* ((seq1 (interleaf--sequence-counter 1 (min (length list1) 2)))
           (seq2 (interleaf--sequence-counter 1 (min (length list2) 2)))
           (m (a-random-member-of seq1))
          (n  (a-random-member-of seq2)))
      (append (subseq list1 0 m) (subseq list2 0 n) (interleaf (subseq list1 m) (subseq list2 n)))))))

(defun interleaf (list1 list2)
  (cond
   ((null list1) list2)
   ((null list2) list1)
   (T (either (interleaf-internal list1 list2) (interleaf-internal list2 list1)))))

(in-package :OPENMUSIC)
(defmethod get-boxcallclass-fun ((self (eql 'interleaf))) 'screamerboxes)
(defmethod! interleaf (list1 list2) (s::interleaf list1 list2))


(in-package :OPENMUSIC)
(defmethod! fuse-sequence-lists (left right &optional (alignment 1))
  :icon 230
  :initvals '(((60 62) (64 (65 67)) (67 70)) ((65 60) (69 67)))
  :indoc '("a sequential list of midi-values" "a sequential list of midi-values" "alignment option for place-holder symbols added for sequences that do not match")
  :menuins '((2 (("match-from-first" 1) ("match-from-last" 2))))
  :doc "Joins two lists containing sequential lists of midi-values"
  (labels
      ((fill-in-sequence (seqc count)
         (let ((placeholder 
                (cond
                 ((= 0 (length seqc)) NIL)
                 ((some #'atom seqc) NIL)
                 (T
                  (make-list (length (car seqc)) :INITIAL-ELEMENT NIL)))))
           (cond
            ((eq 2 alignment)
             (append (mapcar #'(lambda (x) placeholder) (make-list count)) seqc))
            (T
             (append seqc (mapcar #'(lambda (x) placeholder) (make-list count)))))))
       (init (input)
         (cond
          ((null input) '(NIL))
          ((atom input) (list input))
          (T input)))
       (append-sequences (x y) (append (init x) (init y))))
  (cond 
   ((null left) right)
   ((null right) left)
   ((< (length left) (length right))
    (fuse-sequence-lists (fill-in-sequence left (- (length right) (length left))) right NIL))
   ((> (length left) (length right))
    (fuse-sequence-lists left (fill-in-sequence right (- (length left) (length right))) NIL))
   (T
    (mapcar #'append-sequences
            left
            right)))))
   
    
