(unless (find-package :t2l)
  (if (find-package :OPENMUSIC)
      (screamer:define-screamer-package :t2l 
        (:use :om :system))
    (screamer:define-screamer-package :t2l 
      (:use :system))))

(in-package :t2l)

(cl:defun map-func (fn list &key with-levels level-min level-max ignore-null-input)

  (unless (or (and (null level-min)
                   (null level-max))
              (and (null level-min)
                   (>= level-max 0))
              (and (>= level-min 0)
                   (null level-max))
              (and (>= level-min 0) 
                   (>= level-max 0)
                   (<= level-min level-max)))
    (error (format nil "MAP-FUNC given invalid :LEVEL-MIN (~A) and/or :LEVEL-MAX (~A)" level-min level-max)))

  (let ((fn (if ignore-null-input
                #'(lambda (x) (if x (funcall fn x) nil))
              fn))
        (level-min (if level-min (1- level-min) nil))
        (level-max (if level-max (1- level-max) nil)))
    (let ((callfn (if with-levels
                      #'(lambda (x level)
                          (funcall fn x level))
                    #'(lambda (x level)
                        (funcall fn x)))))
      (labels
          ((map-func-internal (list level)
             (cond 
              ((null list) (transform list level))
              ((and level-max (> level level-max)) list) ; abort              
              ((not (consp list)) (transform list level))
              (T
               (cons (if (consp (car list))
                         (map-func-internal (car list) (1+ level))
                       (map-func-internal (car list) (1+ level)))
                     (if (cdr list) (map-func-internal (cdr list) level))))))

           (transform (x level)
             (if (and (or (null level-min)
                          (>= level level-min))
                      (or (null level-max)
                          (<= level level-max)))
                 (funcall callfn x level)
               x)))

        (map-func-internal list 0)))))

(cl:defun map2func (fn list1 list2)
    (labels
        ((map-func-internal (list1 list2)
         (cond ((and (null list1)
                     (null list2)) 
                (funcall fn list1 list2))

               ((or (null list1)
                    (null list2))
                (return-from map2func nil))

               ((and (not (consp list1))
                     (not (consp list2)))
                (funcall fn list1 list2))
               
               
               ((or (not (consp list1))
                    (not (consp list2)))
                (return-from map2func nil))

               ((and (consp (car list1))
                     (consp (car list2)))
                (cons (map-func-internal (car list1) (car list2))
                      (if (cdr list1) (map-func-internal (cdr list1) (cdr list2)))))

               ((or (consp (car list1))
                    (consp (car list2)))
                (return-from map2func nil))

               (T
                (cons (funcall fn (car list1) (car list2))
                      (if (cdr list1) (map-func-internal (cdr list1) (cdr list2))))))))
      (map-func-internal list1 list2)))

(cl:defun map3func (fn list1 list2 list3)
    (labels
        ((map-func-internal (list1 list2 list3)
         (cond ((and (null list1)
                     (null list2)
                     (null list3)) 
                (funcall fn list1 list2 list3))

               ((or (null list1)
                    (null list2)
                    (null list3))
                (return-from map3func nil))

               ((and (not (consp list1))
                     (not (consp list2))
                     (not (consp list3)))
                (funcall fn list1 list2 list3))               
               
               ((or (not (consp list1))
                    (not (consp list2))
                    (not (consp list3)))
                (return-from map3func nil))

               ((and (consp (car list1))
                     (consp (car list2))
                     (consp (car list3)))
                (cons (map-func-internal (car list1) (car list2) (car list3))
                      (map-func-internal (cdr list1) (cdr list2) (cdr list3))))

               ((or (consp (car list1))
                    (consp (car list2))
                    (consp (car list3)))
                (return-from map3func nil))

               (T
                (cons (funcall fn (car list1) (car list2) (car list3))
                      (map-func-internal (cdr list1) (cdr list2) (cdr list3)))))))
      (map-func-internal list1 list2 list3)))

(cl:defun map-andv (fn list)
  (cond ((null list) T)
        (T (apply #'andv (mapcar fn list)))))

(cl:defun map2andv (fn list1 list2)
  (cond ((null list1) T)
        ((null list2) T)
        (T (apply #'andv (mapcar fn list1 list2)))))

(cl:defun map3andv (fn list1 list2 list3)
  (cond ((null list1) T)
        ((null list2) T)
        ((null list3) T)
        (T (apply #'andv (mapcar fn list1 list2 list3)))))

(cl:defun map4andv (fn list1 list2 list3 list4)
  (cond ((null list1) T)
        ((null list2) T)
        ((null list3) T)
        ((null list4) T)
        (T (apply #'andv (mapcar fn list1 list2 list3 list4)))))

(cl:defun map5andv (fn list1 list2 list3 list4 list5)
  (cond ((null list1) T)
        ((null list2) T)
        ((null list3) T)
        ((null list4) T)
        ((null list5) T)
        (T (apply #'andv (mapcar fn list1 list2 list3 list4 list5)))))

(cl:defun map-orv (fn list)
  (cond ((null list) T)
        (T (apply #'orv (mapcar fn list)))))

(cl:defun map2orv (fn list1 list2)
  (cond ((null list1) T)
        ((null list2) T)
        (T (apply #'orv (mapcar fn list1 list2)))))

(cl:defun map3orv (fn list1 list2 list3)
  (cond ((null list1) T)
        ((null list2) T)
        ((null list3) T)
        (T (reduce #'orv (mapcar fn list1 list2 list3)))))

(cl:defun map4orv (fn list1 list2 list3 list4)
  (cond ((null list1) T)
        ((null list2) T)
        ((null list3) T)
        ((null list4) T)
        (T (reduce #'orv (mapcar fn list1 list2 list3 list4)))))

(cl:defun map5orv (fn list1 list2 list3 list4 list5)
  (cond ((null list1) T)
        ((null list2) T)
        ((null list3) T)
        ((null list4) T)
        ((null list5) T)
        (T (apply #'orv (mapcar fn list1 list2 list3 list4 list5)))))

(cl:defun maplist-andv (fn list)
  (cond ((null list) T)
        (T (reduce #'andv (maplist fn list)))))

(cl:defun maplist2andv (fn list1 list2)
  (cond ((null list1) T)
        ((null list2) T)
        (T (reduce #'andv (maplist fn list1 list2)))))

(cl:defun maplist3andv (fn list1 list2 list3)
  :icon 147
  (cond ((null list1) T)
        ((null list2) T)
        ((null list3) T)
        (T (reduce #'andv (maplist fn list1 list2 list3)))))

(cl:defun maplist4andv (fn list1 list2 list3 list4)
  (cond ((null list1) T)
        ((null list2) T)
        ((null list3) T)
        ((null list4) T)
        (T (reduce #'andv (maplist fn list1 list2 list3 list4)))))

(cl:defun maplist-orv (fn list)
  (cond ((null list) T)
        (T (reduce #'orv (maplist fn list)))))

(cl:defun maplist2orv (fn list1 list2)
  (cond ((null list1) T)
        ((null list2) T)
        (T (reduce #'orv (maplist fn list1 list2)))))

(cl:defun maplist3orv (fn list1 list2 list3)
  (cond ((null list1) T)
        ((null list2) T)
        ((null list3) T)
        (T (reduce #'orv (maplist fn list1 list2 list3)))))

(cl:defun maplist4orv (fn list1 list2 list3 list4)
   (cond ((null list1) T)
        ((null list2) T)
        ((null list3) T)
        ((null list4) T)
        (T (reduce #'orv (maplist fn list1 list2 list3 list4)))))

(cl:defun list-depth (list)
  (let ((depth 0))
    (labels
        ((list-depth-internal (list level)
           (cond 
            ((not (consp list)) nil)
            (T
             (if (> level depth)
                 (setf depth level))
             (list-depth-internal (car list) (if (consp (car list)) (1+ level) level))
             (list-depth-internal (cdr list) level)))))
      (list-depth-internal list 1)
      depth)))

(cl:defun list-structure-equal (list1 list2 &key test)
  (let ((test (or test #'(lambda (x y) (or (and (null x) (null y))
                                           (and x y))))))
    (labels
        ((list-structure-equal-internal (list1 list2)
         (cond ((and (null list1)
                     (null list2)) T)

               ((or (null list1)
                    (null list2)) (return-from list-structure-equal nil))

               ((and (not (consp list1))
                     (not (consp list2))) T)               
               
               ((or (not (consp list1))
                    (not (consp list2))) (return-from list-structure-equal nil))

               ((and (consp (car list1))
                     (consp (car list2)))
                (and (list-structure-equal-internal (car list1) (car list2))
                     (list-structure-equal-internal (cdr list1) (cdr list2))))

               ((or (consp (car list1))
                    (consp (car list2))) (return-from list-structure-equal nil))

               (T (and (funcall test (car list1) (car list2))
                       (list-structure-equal-internal (cdr list1) (cdr list2)))))))
      (list-structure-equal-internal list1 list2))))

(cl:defun map-by-level (fn tree &key level-max)  ;; iffy - delete
  "Recursively applies fn to list elements (see 'map-func'). map-by-level accepts a function with two inputs - the first input is the list-element from the 'tree' input being processed. The second input indicates the 'depth' of that element within the list. The function counts from 1; elements of a simple list that does not contain any other list will be labelled level 1."
  (labels
      ((map-func-internal (fn tree level level-max)
         (cond ((null tree) nil)
               ((not (consp tree)) (funcall fn tree))
               ((consp (car tree))
                (cons (if (or (null level-max)
                              (<= level level-max))
                          (map-func-internal fn (car tree) (1+ level) level-max)
                        (funcall fn (car tree) level))
                      (map-func-internal fn (cdr tree) level level-max)))
               (t
                (cons (if (or (null level-max)
                              (<= level level-max))
                          (funcall fn level (car tree))
                        (car tree))
                      (map-func-internal fn (cdr tree) level level-max))))))
    (map-func-internal fn tree 0 level-max)))




(cl:defun fuseseqc (seqc1 seqc2) (mat-trans (list seqc1 seqc2)))
(cl:defun fuseseqc (seqc1 seqc2)
  (labels
      ((atom? (x) (or (null x) (not (listp x))))
       (promote-atom (x) (if (atom? x) (list x)))
       (promote-atoms (xs) (if (atom? xs) (list xs) xs))
       (demote-list (x) (cond ((atom? x) x) ((= 1 (length x)) (car x)) (T x)))
       (as-sequence-list (xs) 
         (cond ((some #'(lambda (x) 
                          (or (atom? x) 
                              (and (listp x) 
                                   (= 1 (length x))))) xs) 
                (list xs))
               (T (mat-trans xs)))))
    (cond ((null seqc1) seqc2) 
          ((null seqc2) seqc1)
          (T 
           (assert (= (length seqc1) (length seqc2)))
           (let ((list1 (as-sequence-list seqc1))
                 (list2 (as-sequence-list seqc2)))
             (mat-trans (append list1 list2)))))))

(cl:defun flat1 (list)
  (cond ((null list) nil)
        ((and (listp list)
              (every #'listp list))
         (apply #'append list))
        ((listp list)
         (flat1 (mapcar #'(lambda (x) (cond ((listp x) x)
                                            (t (list x))))
                        list)))
        (t list)))

(cl:defun indexof (x list &key test)
  (unless test
    (setf test #'equal))
  (cond ((null list) nil)
        ((atom list) nil)
        (T
         (loop for j from 0 while (< j (length list)) do 
                 (if (funcall test (elt list j) x) (return j))))))


;;;; SCREAMER:ASSERT!
(cl:defun assert!! (x &rest xs)
  (labels
      ((assert!!-functionmode (ys)
         (unless (null ys)
           (format om-lisp::*om-stream* " > assert!!: ~A of ~A... " (1+ (- (length (remove nil xs)) (length ys))) (length (remove nil xs)))
           
           (cond ((null (car ys)) T)
                 ((functionp (car ys))
                  (screamer:assert! (funcall (car ys) x)))
                 (T (screamer:assert! (car ys))))

           (format om-lisp::*om-stream* "OK~%")

           (assert!!-functionmode (cdr ys))))
       
       (assert!!-variables (ys)
         (unless (null ys)
           (screamer:assert! (car ys))
           (assert!!-variables (cdr ys)))))
         
  (cond 
   ((null xs)
    (screamer:assert! x))
   
   ((some #'functionp xs)  ;; the first input is passed through and function-mode is initiated
    (assert!!-functionmode (remove nil xs))
    x)

   (T 
    (assert!!-variables (butlast (append (list x) xs)))
    (car (last xs))))))








(cl:defun listdx (list)
  (butlast (maplist #'(lambda (x) 
                        (if (cdr x) 
                            (if (and (cadr x) (car x))
                                (- (cadr x) (car x))
                              nil))) 
                    list)))

(cl:defun listdxv (list)
  (cond ((null list) nil)
        ((cdr list)
         (append (if (or (null (car list))
                         (null (cadr list)))
                     (list nil)
                   (list (om-v (cadr list) (car list))))
                 (listdxv (cdr list))))
        (T nil)))

(cl:defun absv (k) (ommaxv k (om*v k -1)))

(cl:defun omabsv (x)
  (vregister #'(lambda () (absv x))
             #'omabsv
             x))

(cl:defun omavgv (&rest xs)
  (apply #'vregister
         (append (list
                  #'(lambda ()
                      (om/v (apply #'om+v xs) (length xs)))
                  #'omavgv)
                 xs)))

;(cl:defun lists=v (list1 list2 &optional symbol-mode)
;  (apply #'andv
;         (mapcar #'(lambda (a b) (=v a b))
;                 list1
;                 list2)))

;;;; ?ANY
(cl:defun any?fn (fn xs value) (any?internal fn (flatt xs) value))
(cl:defun any?internal (fn xs value)
  (cond 
   ((null xs)
    (funcall fn nil value)) ;; 
   ((not (listp xs))
    (funcall fn xs value))
   ((cdr xs)
    (orv 
     (any?internal fn (car xs) value)
     (any?internal fn (cdr xs) value)))
   (T ;;  list has one item
      (any?internal fn (car xs) value))))
(cl:defun any=v (xs value) (any?fn #'om=v xs value))
(cl:defun any/=v (xs value) (any?fn #'om/=v xs value))
(cl:defun anyequalv (xs value) (any?fn #'omequalv xs value))
(cl:defun anynotequalv (xs value) (any?fn #'(lambda (x y) (omnotv (omequalv x y))) xs value))
(cl:defun any<v (xs value) (any?fn #'om<v xs value))
(cl:defun any>v (xs value) (any?fn #'om>v xs value))
(cl:defun any<=v (xs value) (any?fn #'om<=v xs value))
(cl:defun any>=v (xs value) (any?fn #'om>=v xs value))

;;;; ?LIST+-*/%
(cl:defun listXv (fn xs value)
  (mapcar #'(lambda (x) (if x (funcall fn x value) nil)) xs))
(cl:defun list+v (xs value) (listXv #'om+v xs value))
(cl:defun list-v (xs value) (listXv #'om-v xs value))
(cl:defun list*v (xs value) (listXv #'om*v xs value))
(cl:defun list/v (xs value) (listXv #'om/v xs value))
(cl:defun list%v (xs value) (listXv #'om%v xs value))

;;;; ?LIST MAX, MIN
(cl:defun list-maxv (xs) (apply #'ommaxv xs))
(cl:defun list-minv (xs) (apply #'omminv xs))

(cl:defun ?any2comb (fn list) (reduce #'orv (map2comb fn list)))

(cl:defun ?all2comb (fn list) (reduce #'andv (map2comb fn list)))

(cl:defun map2comb (fn list)
  (let ((cart (mat-trans (list2comb list))))
    (mapcar fn (car cart) (cadr cart))))

(unless (fboundp 'om::mat-trans)
  (cl:defun mat-trans (matrix &key use-mingroupsize fill-symbol)
    (labels ((list-min (list)
               (let ((min (car list)))
                 (loop for i from 1 while (< i (length list)) do
                         (let ((item (elt list i)))
                           (when (< item min)
                             (setf min item))))
                 min))
             (list-max (list)
               (let ((max (car list)))
                 (loop for i from 1 while (< i (length list)) do
                         (let ((item (elt list i)))
                           (when (> item max)
                             (setf max item))))
                 max))
             (use-mingroupsize (matrix)
               (let ((mingroupsize (list-min (mapcar #'length matrix))))
                 (loop for col from 0 while (< col mingroupsize) collect
                         (loop for row from 0 while (< row (length matrix)) collect
                                 (elt (elt matrix row) col)))))
             (use-maxgroupsize (matrix)
               (let ((groups (mapcar #'length matrix)))
                 (let ((mingroupsize (list-min groups))
                       (maxgroupsize (list-max groups)))
                   (let ((trans (mapcar #'(lambda (x) (make-list (length matrix)))
                                        (make-list maxgroupsize :initial-element fill-symbol))))
                     (loop for row while (< row (length matrix)) do
                             (loop for col while (< col (length (elt matrix row))) do
                                     (setf (elt (elt trans col) row) (elt (elt matrix row) col))))
                     trans)))))
      (use-maxgroupsize matrix))))
                               

  


;;; ?TEMPLATE functions (see visual.lisp)
(defvar *symincr* nil) ; counter
(cl:defun symincr (xs &key reset-counter)
  (if reset-counter (setf *symincr* nil))
  (labels
      ((last-charof (str)
         (cond
          ((null str) nil)
          ((= 1 (length str)) str)
          (T (subseq str (1- (length str)) (length str)))))
       (extract-label (str)
         (cond
          ((null str) nil)
          ((= 1 (length str)) str)
          ((or (string-equal "+" (last-charof str))
               (string-equal "-" (last-charof str))
               (string-equal "#" (last-charof str)))
           (subseq str 0 (1- (length str))))
          (T str)))

       (increment? (str)
         (and (> (length str) 1)
              (string-equal "+" (last-charof str))))
       (decrement? (str)
         (and (> (length str) 1)
              (string-equal "-" (last-charof str))))
       (same-number? (str)
         (and (> (length str) 1)
              (string-equal "#" (last-charof str))))

       (lookup (str)         
         (let ((entry (assoc (extract-label str) *symincr* :test #'string-equal)))
           (if entry (cdr entry) nil)))

       (increment-key (input)
         (let ((key (extract-label input)))
           (if (lookup key)
               (rplacd (assoc key *symincr* :test #'string-equal) (1+ (lookup key)))
             (push (cons key 0) *symincr*))))
       (decrement-key (input)
         (let ((key (extract-label input)))         
           (if (lookup key)
               (rplacd (assoc key *symincr* :test #'string-equal) (1- (lookup key)))
             (push (cons key 0) *symincr*))))
       (register-key (input)         
         (let ((key (extract-label input)))
           (if (lookup key)
               key
             (push (cons key 0) *symincr*))))

       (with-sequence-no (str)
         (cond ((null str) nil)

               ((numberp str) str)
               ((screamer::variable? str) str)

               ((= 0 (length str)) str)
               ((= 1 (length str)) str)
               ((not (string-equal "?" (subseq str 0 1))) str)

               ((increment? str)
                (increment-key str)
                (concatenate 'string (extract-label str) (number-to-string (lookup str))))
               ((decrement? str)
                (decrement-key str)
                (concatenate 'string (extract-label str) (number-to-string (lookup str))))
               ((same-number? str)
                (register-key str)
                (concatenate 'string (extract-label str) (number-to-string (lookup str))))

               (T str))))
    (let ((input-symbol-names (map-func #'(lambda (x)
                                            (cond
                                             ((null x) nil)
                                             ((numberp x) x)
                                             ((screamer::variable? x) x)
                                             (T (symbol-name x)))) xs)))
      (map-func #'(lambda (x) ; (if x (intern x) nil)) 
                    (cond
                     ((null x) nil)
                     ((numberp x) x)
                     ((screamer::variable? x) x)
                     (T (intern x))))
                (map-func #'with-sequence-no input-symbol-names)))))

(cl:defun make-screamer-vars (list &key min max integers-mode floats-mode symbol-mode retain-null-values)
  (make-screamer-variables list 
                           :min min
                           :max max
                           :integers-mode integers-mode
                           :floats-mode floats-mode
                           :symbol-mode symbol-mode
                           :retain-null-values retain-null-values))

(defun generate-ordering-force-function (&key force-function
                                              cost-fun
                                              terminate-test
                                              order)
  (let ((force-function-param 
         (cond ((null force-function) #'linear-force)
               ((functionp force-function) 
                (print-warning " >>> FORCE-FUNCTION: ~A" force-function)
                force-function)
               ((or (equal force-function 'om::lf)
                    (equal force-function 'om::linear-force)
                    (equal force-function 't2l::lf)
                    (equal force-function 't2l::linear-force))
                (print-warning " >>> FORCE-FUNCTION: LINEAR-FORCE-FUNCTION")
                #'linear-force)
               ((or (equal force-function 'om::dacf)
                    (equal force-function 'om::divide-and-conquer-force)
                    (equal force-function 't2l::dacf)
                    (equal force-function 't2l::divide-and-conquer-force))
                (print-warning " >>> FORCE-FUNCTION: SCREAMER:DACF")
                #'divide-and-conquer-force)
               (T 
                (print-warning " >>> FORCE-FUNCTION: SCREAMER:LINEAR-FORCE-FUNCTION")
                #'linear-force))))
    (cond ((or (null order)
               (equal order 'om::static)
               (equal order 't2l::static))  
           (print-warning " >>> SCREAMER:STATIC-ORDERING")
           (static-ordering force-function-param))
          (T
           (let ((cost-fun-param (cond ((null cost-fun) #'domain-size)
                                       ((functionp cost-fun) cost-fun)
                                       ((or (equal cost-fun 'om::domain-size)
                                            (equal cost-fun 't2l::domain-size))
                                        #'domain-size)
                                       ((or (equal cost-fun 'om::range-size)
                                            (equal cost-fun 't2l::range-size))
                                        #'domain-size)
                                       (T 
                                        #'domain-size)))
                 (terminate-test-param (cond ((and terminate-test
                                                   (functionp terminate-test))
                                              terminate-test)
                                             (T
                                              #'(lambda (x) (declare (ignore x)) nil))))
                 (order-param (cond ((null order) #'<)
                                    ((functionp order) order)
                                    ((or (equal order 'om::<)
                                         (equal order 't2l::<)) #'<) 
                                    ((or (equal order 'om::>)
                                         (equal order 't2l::>)) #'>))))
             (print-warning " >>> SCREAMER:REORDER ~A ~A ~A ~A"
                            cost-fun-param
                            terminate-test-param
                            order-param
                            force-function-param)
             (reorder cost-fun-param
                      terminate-test-param
                      order-param
                      force-function-param))))))



;; FIND-ANY and FIND-ALL to be renamed

(defun find-any2 (x &key force-function cost-fun terminate-test order) ; renamed 'find-any'
  (one-value 
   (solution x 
             (generate-ordering-force-function :force-function force-function
                                               :cost-fun cost-fun
                                               :terminate-test terminate-test
                                               :order order))))

(cl:defun format-timestamp (timestamp)
  (multiple-value-bind
        (second minute hour date month year day-of-week dst-p tz)
        (decode-universal-time timestamp)
      (concatenate 'string
                   (format nil "~2,'0d:~2,'0d:~2,'0d ~d/~2,'0d "
                           hour minute second month date))))

(cl:defun format-with-timestamp (message &rest arguments)
  (let ((message (or message "")))
    (multiple-value-bind
        (second minute hour date month year day-of-week dst-p tz)
        (get-decoded-time)
      (concatenate 'string
                   (format nil "~2,'0d:~2,'0d:~2,'0d ~d/~2,'0d: "
                           hour minute second month date)
                   (apply #'format (append (list nil message) arguments))))))
(defvar *solver-status-message-timestamp* (get-universal-time))
(cl:defun print-solver-status-message (message &rest arguments)
  (let ((internal-real-time (get-universal-time)))
    (if (> (- internal-real-time
              *solver-status-message-timestamp*) 1000)
        (progn
          (format om-lisp:*om-stream* (concatenate 'string (apply #'format-with-timestamp 
                        (append
                         (list
                          (concatenate 'string
                                       (format nil "(d: ~As) " (float (/ (- internal-real-time
                                                                        *solver-status-message-timestamp*)
                                                                     1000)))
                                       message))
                         arguments)) "~%"))
          (setf *solver-status-message-timestamp* internal-real-time)
          ))))
(defvar *findall2-values* '())
(defvar *findall2-last-value-cons* nil)
(defun find-all2 (i input &key points-system catalog force-function cost-fun terminate-test order)

  ; (global (solver-input input catalog))

  (cond
   ((and i (>= i 1))
    (global
      (setf *findall2-values* '())
      (setf *findall-last-value-cons* nil)
      (print-solver-status-message " > FIND-ALL started"))
    (setf 
     *findall2-values*
     (all-values   
       (let ((n (an-integer-between 0 (1- i))))
         (ith-value 
          (progn (print-warning " >>> ITH-VALUE: #~A" (1+ n)) n)
          (solution input (generate-ordering-force-function
                           :force-function force-function
                           :cost-fun cost-fun
                           :terminate-test terminate-test
                           :order order)))))))
          
   ((functionp points-system)    
    (global
      (setf *findall2-values* '())
      (setf *findall-last-value-cons* nil)
      (print-solver-status-message " > FIND-ALL started"))
    (let ((points-total (a-realv)))
      (best-value
       (let ((value 
              (solution input (generate-ordering-force-function 
                               :force-function force-function
                               :cost-fun cost-fun
                               :terminate-test terminate-test
                               :order order))))
           (assert! (equalv points-total (funcall points-system value)))
           (global
             (if (null *findall2-values*)
                 (setf *findall-last-value-cons* (list value)
                       *findall2-values* *findall-last-value-cons*)
               (setf (rest *findall-last-value-cons*) (list value)
                     *findall-last-value-cons* (rest *findall-last-value-cons*)))
             (print-solver-status-message " (~Apts): ~A" points-total value))
           value)
       points-total)))

   (points-system ; rename    
    (global
      (setf *findall2-values* '())
      (setf *findall-last-value-cons* nil)
      (print-solver-status-message " > FIND-ALL started"))
    (best-value
     (let ((value 
            (solution input (generate-ordering-force-function 
                             :force-function force-function
                             :cost-fun cost-fun
                             :terminate-test terminate-test
                             :order order))))
       (global
         (if (null *findall2-values*)
             (setf *findall-last-value-cons* (list value)
                   *findall2-values* *findall-last-value-cons*)
           (setf (rest *findall-last-value-cons*) (list value)
                 *findall-last-value-cons* (rest *findall-last-value-cons*)))
         (print-solver-status-message (format-with-timestamp " (~Apts): ~A" points-system value)))
       value)
     points-system))
   
   (T
    (find-all2 nil input 
               :points-system (let ((counter 0))
                                 #'(lambda (x)
                                     (global
                                       (setf counter (1+ counter)))
                                     counter))
               :catalog catalog 
               :force-function force-function
               :cost-fun cost-fun 
               :terminate-test terminate-test 
               :order order)))               
  
  *findall2-values*)

(cl:defun find-any (x &key force-function cost-fun terminate-test order) ; renamed 'find-any'
  (find-any2 x 
             :force-function force-function
             :cost-fun cost-fun
             :terminate-test terminate-test
             :order order))

(cl:defun next-find-all-in-phases-parameters (list)
  (cond
   ((cdr list) T)))
(defun find-all-in-phases2 (x &rest phases)
  (print (format nil "FIND-ALL-IN-PHASES started at ~A..." (format-with-timestamp "")))

  (global
    (setf *findall2-values* '())
    (setf *findall-last-value-cons* nil))

  (let ((counter 0) (points-total (a-realv)))
    (best-value
     (let ((value (apply-nondeterministic #'solution-in-phases (append (list x) phases))))
       (assert! (equalv points-total counter))
       (global
         (setf counter (1+ counter))
         (if (null *findall2-values*)
             (setf *findall-last-value-cons* (list value)
                   *findall2-values* *findall-last-value-cons*)
           (setf (rest *findall-last-value-cons*) (list value)
                 *findall-last-value-cons* (rest *findall-last-value-cons*)))
         (print-solver-status-message (format-with-timestamp " (~Apts): ~A" points-total value)))       
       value)
       points-total)
    *findall2-values*))

(defun solution-in-phases (x &rest phases)
  (let ((value 
         (solution x 
                   (generate-ordering-force-function
                    :cost-fun #'domain-size
                    :order #'<))))
    (cond ((null phases) value)
          (T
           (apply-nondeterministic
            #'solution-in-phases 
            (append (list (funcall-nondeterministic (car phases) value))
                    (cdr phases)))))))

(defvar *om-template-unlabelled-variables-symbol* 'om::_)
(cl:defun om-template (template &key map min max make-integer make-real)
  :doc "Copies an aggregate object, replacing any symbol beginning with a question mark with a newly created variable. 

If the same symbol appears more than once in x, only one variable is created for that symbol, the same variable replacing any occurrences of that symbol. Thus (template '(a b (?c d ?e) ?e)) has the same effect as: 
            (LET ((?C (MAKE-VARIABLE))
                  (?E (MAKE-VARIABLE)))
              (LIST 'A 'B (LIST C 'D E) E)).

This is useful for creating patterns to be unified with other structures. "
  (labels
      ((generate-variable ()
         (add-constraints-from-arguments (make-variable)))
       (add-constraints-from-arguments (x)
         (if min
             (assert! (>=v x min)))
         (if max
             (assert! (<=v x max)))
         (cond ((and (or min max) make-real)
                (assert! (realpv x)))
               ((and (or min max)
                     (necessarily? 
                       (solution (orv
                                  (andv (realpv min)
                                        (notv (integerpv min)))
                                  (andv (realpv max)
                                        (notv (integerpv max))))
                                 (static-ordering #'linear-force))))
                (assert! (realpv x)))
               ((or min max make-integer)
                (assert! (integerpv x))))
         x)
       (process-input-sym (x)
         (cond ((null x) nil)
               ((equal x *om-template-unlabelled-variables-symbol*)
                (generate-variable))
               (t x)))
       (lookup (x)
         (cond
          ((find x (mapcar #'car map))
           (cdr (assoc x map)))
          (t x))))
    (let ((template (cond ((null template) nil)
                          ((listp template)
                           (map-func #'process-input-sym template))
                          (t (process-input-sym template)))))
      (cond
       (map
        (let ((template (cond ((null template) nil)
                              ((listp template)
                               (map-func #'lookup template))
                              (t (lookup template)))))
          (multiple-value-bind (template2 map2) (screamer:template template)
            (values template2
                    (append map map2)))))
       (t
        (multiple-value-bind (variables map) (screamer:template (symincr template :reset-counter T))
          (mapcar #'add-constraints-from-arguments (mapcar #'cdr map))
          (values variables map)))))))

(cl:defun make-screamer-variables (list &key min max integers-mode floats-mode symbol-mode)
  (labels ((integers-mode-fn (x) (cond ((null x) nil)
                                       ((integerp x) x)
                                       ((screamer::variable? x)
                                        (assert! (integerpv x))
                                        (assert! (>=v x min))
                                        (assert! (<=v x max))
                                        x)
                                       (t (let ((var (an-integerv)))
                                            (if min (assert! (>=v var min)))
                                            (if max (assert! (<=v var max)))
                                            var))))
           (floats-mode-fn (x) (cond ((null x) nil)
                                     ((or (integerp x) 
                                          (floatp x)) x)                                      
                                     ((screamer::variable? x)
                                      (assert! (realpv x))
                                      (assert! (>=v x min))
                                      (assert! (<=v x max))
                                      x)
                                     (t (let ((var (a-realv)))
                                          (if min (assert! (>=v var min)))
                                          (if max (assert! (<=v var max)))
                                          var))))
           (symbol-mode-fn (x) (cond ((null x) nil)
                                     ((screamer::variable? x) x)
                                     ((or (equal x *om-template-unlabelled-variables-symbol*)
                                          (equal x '_)) ; fix
                                      (make-variable))
                                     (T
                                      (let ((variable (make-variable)))
                                        (assert! (equalv variable x))
                                        variable)))))
    
    (let ((fn (cond ((and (null integers-mode) 
                          (null floats-mode) 
                          (null symbol-mode)) #'integers-mode-fn)
                    (integers-mode #'integers-mode-fn)
                    (floats-mode #'floats-mode-fn)
                    (t #'symbol-mode-fn))))
      (map-func fn 
                (cond ((null list) nil)
                      ((listp list) list)
                      (t (make-sequence 'list list :initial-element '_)))))))

(cl:defun om-solver (i input &key points-system catalog force-function) (find-all i input :points-system points-system :force-function force-function))

(cl:defun mp-solver (i input &key points-system catalog sync-mode force-function)
  (mp:mailbox-send 
   *mp-solver-inbox*
   #'(lambda () 
       (find-all2 i input 
                  :points-system points-system 
                  :catalog catalog 
                  :force-function force-function)
       (multiple-value-bind
           (second minute hour date month year day-of-week dst-p tz)
           (decode-universal-time *mp-solver-starttime*)
         (progn
           (alert2 (format nil "MP-SOLVER > started at ~2,'0d:~2,'0d:~2,'0d ~d/~2,'0d "
                           hour
                           minute
                           second
                           month
                           date))))
       (multiple-value-bind
           (second minute hour date month year day-of-week dst-p tz)
           (decode-universal-time (get-universal-time))
         (print-warning "MP-SOLVER > item completed at (~2,'0d:~2,'0d:~2,'0d ~d/~2,'0d) "
                        hour
                        minute
                        second
                        month
                        date))))
  (cond ((and (mp:find-process-from-name "mp-solver process")
              (not (mp:process-poke 
                    (mp:find-process-from-name "mp-solver process")))) 1)
        ((and (mp:find-process-from-name "mp-solver process")) t)
        (t 
         (print (format nil "MP-SOLVER > starting mp-solver process..."))
         (start-mp-solver-process)))
  (cond ((not (mp:mailbox-empty-p *mp-solver-outbox*))
         (mp:mailbox-read *mp-solver-outbox*))
        (sync-mode    
         (loop for i while (mp:mailbox-empty-p *mp-solver-outbox*) do
               (progn
                 (mp:mailbox-wait *mp-solver-outbox*)                        
                 (return (mp:mailbox-read *mp-solver-outbox*)))))
        (t 1)))

(defparameter *mp-solver-inbox* (mp:make-mailbox :name "mp-solver inbox"))
(defparameter *mp-solver-outbox* (mp:make-mailbox :name "mp-solver outbox"))

(defparameter *mp-solver-process-defn* 
  #'(lambda ()    
      (print-warning "MP-SOLVER > ~A" (format-current-time-for-mp-solver))
      (loop for i while t do
            (cond ((not (mp:mailbox-empty-p *mp-solver-inbox*))                   
                   (print-warning "MP-SOLVER > ~A " *mp-solver-inbox*)
                   (print-warning "MP-SOLVER > mp-solver process ACTIVE ...")
                   (funcall (mp:mailbox-read *mp-solver-inbox*)))
                  (t
                   (print-warning "MP-SOLVER > in SLEEP mode (process active ~A milliseconds)" (mp:process-run-time (mp:find-process-from-name "mp-solver process")))
                   (mp:mailbox-wait *mp-solver-inbox*))))))
(defparameter *mp-solver-starttime* 0)

(cl:defun start-mp-solver-process ()
  (cond ((mp:find-process-from-name "mp-solver process")
         (print (format nil "mp-solver process is ACTIVE: ~A" (mp:find-process-from-name "mp-solver process")))
         1)
        (t
         (print (format nil "starting mp-solver process..."))
         (mp:process-run-function 
          "mp-solver process"
          '(:priority 0)
          *mp-solver-process-defn*)
         (setf *mp-solver-starttime* (get-universal-time))
)))
                
(cl:defun mp-solver-process-status ()
 (cond ((mp:find-process-from-name "mp-solver process")
        (multiple-value-bind
            (second minute hour date month year day-of-week dst-p tz)
            (decode-universal-time *mp-solver-starttime*)
          (print
           (format nil "mp-solver process started at [~2,'0d:~2,'0d:~2,'0d ~d/~2,'0d] ACTIVE: ~A possibilities found for ~A variables, ~A functions in screamer::*trail*, online ~A, ~A entries in variables registry"
                                            hour
                                            minute
                                            second
                                            month
                                            date
                                            (length (solver-output))
                                            (length
                                             (remove-duplicates (remove nil (flat (solver-input)))))
                                            (length screamer::*trail*)
                                            (mp:process-run-time (mp:find-process-from-name "mp-solver process"))
                                            (length *solver-keys*)))
          T))
       (t
        (print (format nil "mp-solver process is INACTIVE"))
        ; (print (format nil "mp-solver process is INACTIVE, with ~A items in the inbox" (mp:mailbox-count (mp:process-mailbox (mp:find-process-from-name "mp-solver process")))))
        'Ok)))

(cl:defun suspend-mp-solver-process () (if (mp:find-process-from-name "mp-solver process") (mp:process-break (mp:find-process-from-name "mp-solver process")) t))
(cl:defun do-next-mp-solver-process () (mp:process-reset (mp:find-process-from-name "mp-solver process")))

(cl:defun terminate-mp-solver-process ()
  :icon 642   
  (unless (null (mp:find-process-from-name "mp-solver process"))
    (loop for i while (not (mp:mailbox-empty-p *mp-solver-inbox*)) do
          (progn
            (print (format nil "MP-SOLVER > deleting waiting process from ~A ..." *mp-solver-inbox*))
            (mp:mailbox-read *mp-solver-inbox* "reset" 8)))
    (mp:process-reset (mp:find-process-from-name "mp-solver process"))
    (mp:find-process-from-name "mp-solver process")))

(cl:defun kill-mp-solver-process ()  
  (let ((solver-process (mp:find-process-from-name "mp-solver process")))
    (if solver-process ; is active
        (progn
          (mp:process-kill solver-process)
          (print (format nil "mp-solver process ~A terminated" solver-process))
          solver-process)
      t)))

(cl:defun format-current-time-for-mp-solver ()
  (multiple-value-bind
      (second minute hour date month year day-of-week dst-p tz)
      (decode-universal-time (get-universal-time))
     (format nil "[~2,'0d:~2,'0d:~2,'0d ~d/~2,'0d, ~A solutions found for ~A variables, ~A functions in screamer::*trail*, online ~A]"
             hour
             minute
             second
             month
             date
             (length *backup-solver-output*)
             (length
              (remove-duplicates (remove nil (map-func #'(lambda (x) (if (screamer::variable? x) x nil)) *backup-solver-input*))))
             (length screamer::*trail*)
             (mp:process-run-time (mp:find-process-from-name "mp-solver process")))))


;;;; 
(cl:defun setf-solver-input (xs &optional catalog)
  (setf catalog (or catalog :backup))
  (cond ((null (assoc catalog *backup-solver-input*))
         (push (cons catalog xs) *backup-solver-input*))
        (t (rplacd (assoc catalog *backup-solver-input*) xs)))
  xs)

(cl:defun setf-solver-output (xs &optional catalog)
  (setf catalog (or catalog :backup))
  (alert2 (format nil "setf-solver-output xs: ~A catalog: ~A" xs catalog))
  (cond ((null (assoc catalog *backup-solver-output*))
         (push (cons catalog xs) *backup-solver-output*))
        (t
         (rplacd (assoc catalog *backup-solver-output*) xs)))
  nil)

(cl:defun push-to-solver-output (xs &optional catalog)
  (setf catalog (or catalog :backup))
  (if (not (find catalog (mapcar #'car *backup-solver-output*)))
      (push (cons catalog nil) *backup-solver-output*)
    (rplacd (assoc catalog *backup-solver-output*)
            (append (cdr (assoc catalog *backup-solver-output*)) (list xs)))))


(cl:defun suspend-om-eval-process () :icon 642 (if (mp:find-process-from-name "OM EVAL PROCESS") (mp:process-break (mp:find-process-from-name "OM EVAL PROCESS")) t))

(cl:defun reset-solver-output (&optional catalog)
  (global
   (progn
     (setf t2l::*findall2-values* '())
     (setf t2l::*findall-last-value-cons* nil))))

(cl:defun solver-input (&optional input catalog) 
  :icon 215
  (setf catalog (or catalog :backup))
  (if input
      (cond ((null (assoc catalog *backup-solver-input*))
             (push (cons catalog input) *backup-solver-input*))
            (t
             (rplacd (assoc catalog *backup-solver-input*) input))))
  
  (cdr (assoc catalog *backup-solver-input*)))

;(cl:defun solver-output (&optional catalog) :icon 215 
;  (cdr (assoc (or catalog :backup) *backup-solver-output*)))
(cl:defun solver-output (&optional value &key label) :icon 215 
  (cond
   (value
    (global
      (if (null t2l::*findall2-values*)
          (setf t2l::*findall-last-value-cons* (list value)
                t2l::*findall2-values* t2l::*findall-last-value-cons*)
        (setf (rest t2l::*findall-last-value-cons*) (list value)
              t2l::*findall-last-value-cons* (rest t2l::*findall-last-value-cons*))))
    (format om-lisp:*om-stream* "~A ~A~%" (length t2l::*findall2-values*) value)
    value)
   (T t2l::*findall2-values*)))

(defvar *next-solver-output-cursor* -1)

(defun next-solver-output (&optional catalog)
  (global
   (cond
    ((null *findall2-values*) 
     (setf *next-solver-output-cursor* -1)
     nil)
    (T
     (setf *next-solver-output-cursor* (min (1+ *next-solver-output-cursor*) (1- (length *findall2-values*))))
     (elt *findall2-values* *next-solver-output-cursor*)))))

(defun prev-solver-output (&optional catalog)
  (global
   (cond
    ((null *findall2-values*) 
     (setf *next-solver-output-cursor* -1)
     nil)
    (T
     (setf *next-solver-output-cursor* (max (1- *next-solver-output-cursor*) 0))
     (elt *findall2-values* *next-solver-output-cursor*)))))

(cl:defun reset-solver-input (&optional xs) 
  (cond (xs 
         (print (format nil "*backup-solver-input* (~A)" (length *backup-solver-input*))) 
         (setf *backup-solver-input* xs))
        (t
         (setf *backup-solver-input* nil))))







;;;; midinote lists









(cl:defstruct timee value flag)
(cl:defun setf-timee-value (x d)
  (cond ((listp (timee-value x))
         (setf (car (timee-value x)) d))
        (t 
         (setf (timee-value x) d))))
(cl:defun process-duration-groups-internal (l segs)
  ;(print (format nil "ENTER process-duration-groups-internal l: ~A segments: ~A" l segs))
  (labels
      ((strip-zeros (xs) (mapcar (lambda (x) (remove 0 x :test 'equalp)) xs))
       (duration (group)
         (cond ((null group) 0)
               ((and (listp group)
                     (> (length group) 1)
                     (atom (car group))
                     (listp (cadr group)))
                (abs (car group)))
               ((listp group) 
                ;(print "process-duration-groups-internal#duration is being called for a list that is not a (x (. . .)) type pulse marker")
                0)
               (t (abs group))))
       (group? (input)
         (and (not (null input))
              (listp input)
              (listp (car input))
              (not (listp (cadr input)))))
       (group-value (group)
         (cond ((null group) 0)
               ((and (listp group)
                     (> (length group) 1)
                     (atom (car group))
                     (listp (cadr group)))
                (car group))
               ((listp group)
                ;(print "process-duration-groups-internal#group-value is being called for a list that is not a (x (. . .)) type pulse marker")
                0)
               (t group)))
       (process-durations (ll segs-l segs-r)
         ;(print (format nil "process-duration-groups-internal#process-durations ll: ~A segs-l: ~A segs-r: ~A" ll segs-l segs-r))
         (cond ((and (null ll) (not (null segs-l)) (null segs-r)) (list segs-l))
               ((null ll) nil)
               ((and (null segs-l) (null segs-r)) (process-durations ll (list (* (float -1) (float (car ll)))) nil))
               ((null segs-l) (process-durations ll (list (car segs-r)) (cdr segs-r)))
               ((and (null segs-r) (< (reduce #'+ (mapcar #'duration segs-l)) (car ll)))
                (let* ((segs-l-duration (reduce #'+ (mapcar #'duration segs-l)))
                       (diff (float (* -1 (- (car ll) segs-l-duration)))))
                  (process-durations ll (append segs-l (list diff)) nil)))
               (t 
                (let ((diff (- (duration (car ll)) (reduce #'+ (mapcar #'duration segs-l)))))
                  (cond ((> diff 0)
                         (process-durations ll 
                                 (append segs-l (list (car segs-r)))
                                 (cdr segs-r)))
                        ((< diff 0)
                         (let ((next-segment (car (reverse segs-l))))
                           (cond
                          ((listp next-segment)
                           (let* ((a (duration next-segment))
                                  (c (/ (* -1 diff) a))
                                  (b (/ (+ (duration next-segment) diff) a))
                                  (s (reduce #'+ (mapcar #'duration (cadar (reverse segs-l))))) ; sum of the internal subdivisions of the group
                                  (d (* b s))
                                  (e (* c s))
                                  (groups (process-duration-groups-internal (list d e) (cadar (reverse segs-l)))))
                               (let* ((stak (copy-seq (cadar (reverse segs-l)))))
                                 (let ((groups2
                                        (map-func
                                         #'(lambda (x)
                                             (let ((top (pop stak)))
                                               (cond ((floatp top) (float x))
                                                     (t x))))
                                         groups)))
                                  ; (print (format nil "-> process-durations-groups-internal#process-durations a: ~A c: ~A b: ~A s: ~A groups: ~A groups2: ~A next-segment: ~A segments: ~A" a c b s groups groups2 (car (reverse segs-l)) segs-l))
                               (process-durations ll 
                                       (append (butlast segs-l) 
                                               (list (list (+ (duration next-segment) diff)
                                                           (car groups2))))
                                       (append (list (list (* -1 diff) (cadr groups2)))
                                             segs-r))))))
                          (t (process-durations ll
                                     (append (butlast segs-l)
                                             (list (+ (duration (car (reverse segs-l))) diff)))
                                     (append (list (* -1 diff))
                                             segs-r))))))
                        (t
                         (append (list segs-l)
                                 (process-durations (cdr ll)
                                                    nil
                                                    segs-r))))))))
       (process-durations->timees (ps)
         (mapcar ; ((1 2) (1 1))
          #'(lambda (m)
              (mapcar
               #'(lambda (n)
                   (make-timee :value n :flag t))
               m))
          ps))
       (process-timee-flags (re)
         (let ((init-s (map-func #'abs segs)) ; 
               (init-p (flat re)))
           (labels ((prcs (s p &optional (cont nil))
                      (cond ((and (null s) (null p)) nil)
                            ((null p) nil)
                            ((null s) (mapcar #'(lambda (x) 
                                                  (setf (timee-flag x) nil))
                                              p))
                            (t (let ((diff (- (duration (car s))
                                              (duration (timee-value (car p))))))
                                 (cond ((> diff 0)
                                        (progn 
                                          (setf (timee-flag (car p)) (not cont))
                                          (prcs (append (list diff) (cdr s))
                                                (cdr p)
                                                t)))
                                       ((< diff 0)
                                        (progn 
                                          (setf (timee-flag (car p)) (not cont))
                                          (prcs (cdr s)
                                                (cdr p))))
                                       (t
                                        (progn 
                                          (setf (timee-flag (car p)) (not cont))
                                          (prcs (cdr s) (cdr p))))))))))
             (prcs init-s init-p))))
       (expand-timee-groups (re)
         ; (print (format nil "expand-timee-groups re: ~A" re))
         (mapcar #'(lambda (xs)
                     ; (print (format nil "expand-timee-groups calling scale-to-int with ~A" (mapcar #'group-value (mapcar #'timee-value xs))))
                     (let ((x-values (scale-to-int (mapcar #'group-value (mapcar #'timee-value xs)))))
                       (mapcar #'(lambda (struct value)                                 
                                 (progn 
                                   ; (print (format nil "calling setf-timee-value struct: ~A value: ~A" struct value))
                                   (setf-timee-value struct value))
                                   struct)
                               xs
                               x-values)))
                 re))
       (convert-timee-values (re) 
         (mapcar #'(lambda (x)
                     (setf-timee-value x (if (timee-flag x)
                                               (floor (group-value (timee-value x)))
                                             (float (group-value (timee-value x))))))
                 (flat re)))
       (process-timee-signs (re segs)
         (let ((stak (copy-seq segs)))
           (push nil stak)
           (mapcar #'(lambda (x)
                     (if (timee-flag x) (pop stak))
                     (cond ((null stak) x)
                           ((and (< (group-value (car stak)) 0)
                                 (> (timee-value x) 0))
                            (setf (timee-value x) (* -1 (timee-value x)))
                            x)
                           (t x)))
                   (flat re)))))
    (let ((durations (strip-zeros (process-durations l nil segs))))
      (let ((duration-obj-list (process-durations->timees durations)))
        (process-timee-flags duration-obj-list)
        (expand-timee-groups duration-obj-list)
        (process-timee-signs duration-obj-list segs)
        (convert-timee-values duration-obj-list)
        (map-func #'timee-value duration-obj-list)))))

(cl:defun scale-to-int (ll)
  (cond ((numberp ll)
         (cond ((< ll 0) -1)
               ((= ll 0) 0)
               (t 1)))
        (t
         (cond ((= (length ll) 0) (list (scale-to-int 0)))
               ((= (length ll) 1) (list (scale-to-int (car ll))))
               (t 
                (let* ((gcm (gcm (mapcar (lambda (x)
                                             (if (rationalp x)
                                                 (denominator x)
                                               1))
                                           (mapcar #'rationalize ll))))
                       (r (om* ll gcm))
                       (gcd1 (gcd1 r)))
                  (values (mapcar
                           #'(lambda (x y)
                               (cond ((floatp x) (float y))
                                     (t y)))
                           ll
                           (mapcar #'floor (om/ r gcd1)))
                          gcm
                          gcd1)))))))

(cl:defun gcm (&rest n)
  (cond ((listp (car n))
         (apply #'gcm (car n)))
        (t (apply #'*
                  (mapcar (lambda (x) 
                            (apply #'expt x))
                          (remove-duplicates
                           (sort (apply #'append (mapcar #'prime-facts n))
                                 (lambda (x y)
                                   (cond ((= (car x) (car y))
                                          (> (cadr x) (cadr y)))
                                         (t (< (car x) (car y))))))
                           :test #'= 
                           :key #'car
                           :from-end t))))))

(cl:defun prime-facts (x) ; copied from om::prime-facts 
  (let ((ip 1) (r) (n 0))
    (loop while (and (> x 1) 
                     (<= (* (aref *prime-numbers* ip) 
                            (aref *prime-numbers* ip))
                         x)) do
      (when (= 0 (mod x (aref *prime-numbers* ip)))
        (setq n 1)
        (loop while (= 0 
                       (progn (setq x (/ x (aref *prime-numbers* ip))) 
                              (mod x (aref *prime-numbers* ip)))) do
              (incf n))
        (push  (list (aref *prime-numbers* ip) n) r))
      (incf ip))
    (when (/= x 1)   (push  (list x 1) r))
    (or (reverse r) (list (list 1 1)))))

(cl:defun gcd1 (&rest n)
  (cond ((listp (car n))
         (apply #'gcd1 (car n)))
        (t (let* ((factors (mapcar #'prime-facts n))
                  (shared-terms (reduce #'intersection 
                                        (mapcar (lambda (x)
                                                  (mapcar #'car x))
                                                factors)))
                  (shared-factors (mapcan (lambda (x)
                                            (if (position (car x) shared-terms)
                                                (list x)))
                                          (apply #'append factors))))  
             (apply #'*
                    (mapcar (lambda (x) 
                              (apply #'expt x))
                            (remove-duplicates
                             (sort shared-factors
                                   (lambda (x y)
                                     (cond ((= (car x) (car y))
                                            (< (cadr x) (cadr y)))
                                           (t (< (car x) (car y))))))
                             :test #'= 
                             :key #'car
                             :from-end t)))))))

(cl:defun treelen (ll)
  (cond ((null ll) 0)
	((null (car ll)) (+ 0 (treelen (cdr ll))))
	((listp (car ll)) (+ (treelen (car ll)) (treelen (cdr ll))))
	(t (+ 1 (treelen (cdr ll))))))

(cl:defun list2int (ll)
  (cond ((null ll) nil)	
	((listp ll) (cond ((cdr ll) (append (list2int (car ll)) (list2int (cdr ll))))
			  (t (list2int (car ll)))))
	(t (let ((flll (car (multiple-value-list (floor ll)))))
	     (cond ((listp flll) (list (car flll)))
		   (t (list flll)))))))

(cl:defun to-fractn (n)
  (reduce-fractn n 1))		

(cl:defun reduce-fractn (n d)
  (if (not (= (mod n 1) 0))
      (reduce-fractn (* n 10) (* d 10))
    (let ((fs (remove 1 (intersection (ftor n) (ftor d)))))
      (if fs
          (let ((gcf (list-max fs)))
            (reduce-fractn (/ n gcf) (/ d gcf)))
        (list n d)))))
		    
(cl:defun to-om-ms-den-list (ms)
  (to-ms-den-list ms))

(cl:defun to-ms-den-list (ms)
  (if (car ms) (append (list (car (cdr (car ms)))) (to-ms-den-list (cdr ms))) nil))

(cl:defun test-merge-ms-partns (partns groups)
  (merge-ms-partns partns groups))

(cl:defun merge-ms-partns (partns groups)
  (setq grouprz (if (> (treelen partns) (treelen groups))
		    (append groups (make-sequence 'list
                                                  (- (treelen partns) (treelen groups)) 
                                                  :initial-element (list -1)))
		  groups))
  (if (car partns)
      (append (list (merge-partn-group (car partns) (subseq grouprz 0 (length (car partns)))))
	      (merge-ms-partns (cdr partns) (nthcdr (length (car partns)) grouprz)))
    nil))
(cl:defun merge-partn-group (partn group) 
  (if (car partn)
      (append (list (list (car partn) (car group))) (merge-partn-group (cdr partn) (cdr group)))
    nil))
;(cl:defun to-ms-numr-list (ms den)
;		(to-ms-numr-list (print ms) (car den)))		
(cl:defun to-ms-numr-list (ms den)
  (if (car ms)
      (append (list (to-ms-numr (car ms) den)) (to-ms-numr-list (cdr ms) den))
    nil))
;(cl:defun to-ms-numr (ms den)
;  (to-ms-numr ms (car den)))
(cl:defun to-ms-numr (ms den)
  (* (car ms) (/ den (car (cdr ms)))))
(cl:defun list-fnappl (fn ll)
  (cond ((cdr ll) (funcall fn (car ll) (list-fnappl fn (cdr ll))))
	(t (car ll))))
(cl:defun prcs-ms-timepoint-signatures (ms partns &key list-mode)
  (if (car ms)
      (append (list (append (list (cond (list-mode ms)
                                        (t (format-mssign (car ms)))))
			    (list (car partns)))) 
	      (prcs-ms-timepoint-signatures (cdr ms) (cdr partns)))
    nil))
(cl:defun format-mssign (ms)
  (read-from-string (concatenate 'string (write-to-string (car ms)) "//" (write-to-string (cadr ms)))))

(cl:defun contains-list (ll)
  (cond ((null ll) nil)
        ((listp ll) (or (listp (car ll))
                        (contains-list (cdr ll))))
        (t nil)))

(cl:defun contains-atom (ll)
  (cond ((null ll) nil)
        ((listp ll) (or (atom (car ll))
                        (contains-atom (cdr ll))))
        (t nil)))


; Graph representation of context-free grammars
; Alex Shkotin arXiv:cs/0703015 http://arxiv.org/abs/cs/0703015
; 
; jan 2013 

(defvar *mapprules-default-input-process-increment* 4)

(cl:defun mapprules (input
                       prules 
                       &key symbol-mode
                            get-symbol-list
                            process-chunk-size
                            input-process-increment
                            continue
                            init
                            listdxx
                            max
                            min
                            ordered-partitions-nondeterministic-values-cap
                            superset
                            params
                            print-graph-info)
  :outputs 3
  (assert (not (and symbol-mode listdxx)))
  (if (and (null process-chunk-size) input-process-increment)
      (setf process-chunk-size input-process-increment))
  (setf process-chunk-size
        (cond ((and (null process-chunk-size) input-process-increment) input-process-increment)
              (t process-chunk-size)))
  (setf process-chunk-size
        (cond ((null process-chunk-size) nil)
              ((numberp process-chunk-size) process-chunk-size)
              (t *mapprules-default-input-process-increment*)))
  (labels
      ((init-label (string)
         (cond ((not (stringp string)) (init-label (write-to-string string)))
               ((= 0 (length string)) string)
               ((string= ":" (subseq string 0 1)) string)
               (t (concatenate 'string ":" string))))
       (symeq (x y)
         (let ((xS (init-label x))
               (yS (init-label y)))
           (string= xS yS)))
       (underscore? (x) (or (symeq x "_") (symeq x "t2l::_")))
       (atom->var (x)
         (cond ((null x) nil) 
               ((screamer::variable? x) x)
               ((underscore? x) 
                (cond (symbol-mode (make-variable))
                      (t (an-integerv))))
               ((integerp x) (make-intv= x))
               ((floatp x) (make-realv= x))
               ((numberp x) (make-numberv= x))
               (t
                (let ((v (make-variable)))                  
                  (assert! (equalv v x))
                  v))))
       (process-increment-adjusted-length (length) 
         (if process-chunk-size
             (* (ceiling (/ length process-chunk-size)) process-chunk-size)
           length))
       (make-input-sequence (length)
         (make-sequence 'list (process-increment-adjusted-length length) :initial-element '_)))
    (cond
     (get-symbol-list
      (mapprules-internal nil prules :get-symbol-list get-symbol-list))
     ((or (null input) (and (numberp input) (= 0 input)) (and (listp input) (every #'null (flat input)))) input)
     (t
      (let* ((list (cond ((null input) nil)
                         ((listp input) input)
                         ((numberp input) (make-input-sequence input))
                         (t input)))
             (list-vars (map-func #'atom->var list))
             (list-vars-flat-subseq (if init
                                        (append (list init) (remove nil (flat list-vars)))
                                      (remove nil (flat list-vars))))
             (list-vars-flat (if (and process-chunk-size
                                      (< (length list-vars-flat-subseq) (process-increment-adjusted-length (length list-vars-flat-subseq))))
                                 (append list-vars-flat-subseq 
                                         (mapcar #'atom->var (make-sequence 'list (- (process-increment-adjusted-length (length list-vars-flat-subseq)) (length list-vars-flat-subseq)) :initial-element '_)))
                               list-vars-flat-subseq))
             (map-fn-input (if listdxx (listdxv list-vars-flat) list-vars-flat)))
        (if (not symbol-mode)
            (progn 
              (if min (push (reduce #'andv (mapcar #'(lambda (x) (>=v x min)) list-vars-flat)) c))
              (if max (push (reduce #'andv (mapcar #'(lambda (x) (<=v x max)) list-vars-flat)) c))))
        (if superset (push (reduce #'andv (mapcar #'(lambda (x) (memberv x superset)) list-vars-flat)) c))
        (cond
         (process-chunk-size
          (let ((map-fn-input-chunks (let ((nsucc (nsucc map-fn-input process-chunk-size :step (1- process-chunk-size))))
                                       (cond ((and (> (length nsucc) 1)
                                                   (= (length (car (reverse nsucc))) 1)) (butlast nsucc))
                                             (t nsucc)))))
            (let ((var-input-dmg-list 
                   (maplist
                    #'(lambda (chunks)
                        (multiple-value-list
                         (mapprules-internal (car chunks)
                                             prules
                                             :continuation-mode t
                                             :symbol-mode symbol-mode
                                             :ordered-partitions-nondeterministic-values-cap ordered-partitions-nondeterministic-values-cap
                                             :params params
                                             :print-graph-info print-graph-info)))
                    map-fn-input-chunks)))
              (values (apply #'andv (mapcar #'car var-input-dmg-list))
                      (cadar var-input-dmg-list)
                      (caddar var-input-dmg-list)))))
         (t 
          (multiple-value-bind 
              (var list dmg)
            (mapprules-internal map-fn-input
                                prules
                                :get-symbol-list get-symbol-list
                                :continuation-mode continue
                                :symbol-mode symbol-mode
                                :ordered-partitions-nondeterministic-values-cap ordered-partitions-nondeterministic-values-cap
                                :params params
                                :listdxx listdxx
                                :print-graph-info print-graph-info) 
            (values var list dmg)))))))))

(defstruct (dmg-cursor (:conc-name nil) (:print-function print-dmg-cursor)) label sym stack rules)

(defclass node () 
  ((name :accessor name
         :initarg :name
         :initform nil)
   (data :accessor data
         :initarg :data
         :initform nil)
   (next-nodes :accessor next-nodes
               :initarg :next-nodes
               :initform nil)))

(cl:defun print-node (node stream print-level)
  (declare (ignore print-level))
  (princ (concatenate 'string 
                      "["
                      (write-to-string (name node))
                      (cond ((parent node)
                             (concatenate 'string
                                          "("
                                          (cond ((name (parent node)) (write-to-string (name (parent node))))
                                                (t "null"))
                                          ")"))
                            (t ""))
                                    
                      ":"
                      (write-to-string (length (next-nodes node)))
                      "]")
         stream))

(cl:defun mapprules-internal (list 
                              prules-intern
                              &key continuation-mode
                                   ordered-partitions-nondeterministic-values-cap
                                   symbol-mode
                                   get-symbol-list
                                   params
                                   print-graph-info
                                   listdxx)
  (let ((prules
         (if listdxx
             prules-intern
           (mapcar #'(lambda (xs)
                       (if (not (cdr xs))
                           xs
                         (append (list (car xs))
                                 (reverse (cdr xs)))))
                   prules-intern))))
    (labels
        ((rule-label (sym) 
           (cond ((null sym) (gensym))
                 ((not (stringp sym)) (rule-label (write-to-string sym)))
                 ((and (> (length sym) 1)
                       (string= (subseq sym 0 1) ":"))
                  (gensym (subseq sym 1 (length sym))))
                 (t (gensym sym))))
         (znd-rule? (rule) 
           (> (length (remove-if-not #'(lambda (x) (eq x (car rule))) (mapcar #'car prules))) 1))
         (zd-rule? (rule)
           (= (length (remove-if-not #'(lambda (x) (eq x (car rule))) (mapcar #'car prules))) 1))
         (one-to-one-transform (rule)
           (cond ((and (znd-rule? rule)
                       (> (length (cdr rule)) 1))
                  (let* ((sym (rule-label (car rule))))
                    (list (list (car rule) sym)
                          (append (list sym) (cdr rule)))))
                 (t (list rule))))
         (init-label (string)
           (cond ((not (stringp string)) (init-label (write-to-string string)))
                 ((= 0 (length string)) string)
                 ((string= ":" (subseq string 0 1)) string)
                 (t (concatenate 'string ":" string))))
         (symeq (x y) (eql x y))
         (underscore? (x) (symeq x '_))

         (last-atom (l)
           (cond ((null l) nil)
                 ((atom l) l)
                 (t (last-atom (car (reverse l))))))
         (all-memberv (list sequence) (apply #'andv (mapcar #'(lambda (x) (memberv x sequence)) (flat list)))))
      (let* ((dnd (flat1 (mapcar #'one-to-one-transform prules)))
             (syms (remove-duplicates (flat dnd) :test #'symeq :from-end t))
             (nonterminals (mapcar #'car dnd))
             (terminals (remove-duplicates
                         (remove-if-not
                          #'(lambda (sym)
                              (null (intersection (list sym) nonterminals :test #'symeq)))
                          syms)
                         :test #'equalp
                         :from-end t)))
        (cond
         (get-symbol-list terminals)
         (T
          (let ((and-syms (remove-if-not
                           #'(lambda (sym)
                               (or (and (not (position sym terminals :test #'symeq))
                                        (= (length (remove-if-not #'(lambda (x) (symeq sym x)) nonterminals)) 1))))
                           nonterminals))
                (or-syms (remove-if-not
                          #'(lambda (sym)                         
                              (and (not (position sym terminals :test #'symeq))
                                   (> (length (remove-if-not #'(lambda (x) (symeq sym x)) nonterminals)) 1)))
                          nonterminals))
                (dmgassoc (mapcar #'(lambda (sym) 
                                      (cons sym (make-instance 't2l::node :name sym)))
                                  syms)))
            (labels 
                ((terminal-sym? (sym) (position sym terminals))             
                 (or-sym? (sym) (position sym or-syms))
                 (and-sym? (sym) (position sym and-syms)))
              (let ((dmg (cdar dmgassoc))
                    (dmg-sym-assoc nil)
                    (dmg-wordsize-assoc nil))
                (if print-graph-info
                    (dolist (x (mapcar #'car dmgassoc))
                      (print (format nil 
                                     "~A: terminal? ~A or-sym? ~A and-sym? ~A syms: ~A" 
                                     x
                                     (terminal-sym? x)
                                     (or-sym? x)
                                     (and-sym? x)
                                     (mapcar #'name (next-nodes (cdr-assoc x dmgassoc)))))))
                (labels 
                    ((make-edges (node)
                       (cond ((or-sym? (name node))
                              (mapcar #'(lambda (sym) 
                                          (push (cdr-assoc sym dmgassoc) (next-nodes node)))
                                      (remove-duplicates
                                       (flat1
                                        (mapcar 
                                         #'cdr
                                         (remove-if-not
                                          #'(lambda (x) (symeq (car x) (name node)))
                                          dnd)))
                                       :test #'symeq
                                       :from-end t)))
                             ((and-sym? (name node))
                              (mapcar #'(lambda (sym)
                                          (om::push-end (cdr-assoc sym dmgassoc) (next-nodes node)))
                                      (flat1
                                       (mapcar 
                                        #'cdr
                                        (remove-if-not
                                         #'(lambda (x) (symeq (car x) (name node)))
                                         dnd)))))
                             (t nil)))
                     (make-list-input-variable () 
                       (cond (symbol-mode (make-variable))
                             (t (make-variable)))))
                  (mapcar #'make-edges (mapcar #'cdr dmgassoc))
                  (dolist (n (mapcar #'car dmgassoc))
                    (setf (cdr-assoc n dmg-sym-assoc) 
                          (mapcar #'name (next-nodes (cdr-assoc n dmgassoc)))))
                  (labels
                      ((dmg-max-wordsize (sym)
                         (let ((syms nil))
                           (labels
                               ((findmax (s)
                                  (cond
                                   ((terminal-sym? s) 1)
                                   ((position s syms) nil)
                                   (t
                                    (push s syms)
                                    (let ((rs (mapcar #'findmax (cdr-assoc s dmg-sym-assoc))))
                                      (cond
                                       ((position nil rs) nil)
                                       (t
                                        (cond
                                         ((and-sym? s) (reduce #'+ rs))
                                         (t (reduce #'max rs))))))))))
                             (findmax sym))))
                       (dmg-min-wordsize (sym)
                         (let ((syms nil))
                           (labels
                               ((findmin (s)
                                  (cond
                                   ((terminal-sym? s) 1)
                                   ((position s syms) nil)
                                   (t
                                    (push s syms)
                                    (let ((rs (mapcar #'findmin (cdr-assoc s dmg-sym-assoc))))
                                      (cond
                                       ((and-sym? s) (reduce #'+ (mapcar #'(lambda (x) (if (null x) 1 x)) rs)))
                                       ((null (remove nil rs)) 1)
                                       (t (reduce #'min (mapcar #'(lambda (x) (if (null x) 1 x)) rs)))))))))
                             (findmin sym))))
                       (dmg-and-syms (or-sym)                     
                         (let ((syms nil))
                           (labels
                               ((findsyms (s)
                                  (cond
                                   ((position s syms) (list s))
                                   (t
                                    (push s syms)
                                    (append
                                     (remove-if-not #'and-sym? (cdr-assoc s dmg-sym-assoc))
                                     (mapcar #'findsyms (remove-if-not #'or-sym? (cdr-assoc s dmg-sym-assoc))))
                                    ))))
                             (let ((list (remove-duplicates (flat (findsyms or-sym)) :test #'symeq :from-end t)))
                               (if (and-sym? nil) list (remove nil list))))))
                       (dmg-sym-domain (or-sym)
                         (let ((syms nil))
                           (labels
                               ((findsyms (s)
                                  (cond
                                   ((position s syms) (list s))
                                   (t
                                    (push s syms)
                                    (append
                                     (remove-if-not #'terminal-sym? (cdr-assoc s dmg-sym-assoc))
                                     (mapcar #'findsyms (remove-if-not #'or-sym? (cdr-assoc s dmg-sym-assoc))))))))
                             (remove-duplicates (flat (findsyms or-sym)) :test #'symeq :from-end t)))))
                    (dolist (n (mapcar #'car dmgassoc))
                      (setf (cdr-assoc n dmg-wordsize-assoc)
                            (cons (dmg-min-wordsize n)
                                  (dmg-max-wordsize n))))
                    (labels
                        ((min-wordsize (sym) (car (cdr-assoc sym dmg-wordsize-assoc)))
                         (max-wordsize (sym) (cdr (cdr-assoc sym dmg-wordsize-assoc))))
                      (if print-graph-info
                          (progn
                            (print (format nil "~A graph symbols" (length dmgassoc)))
                            (dolist (x (mapcar #'car dmgassoc))
                              (print (format nil
                                             "~A min-wordsize: ~A max-wordsize: ~A"
                                             x
                                             (min-wordsize x)
                                             (max-wordsize x))))))
                      (let ((vars (map-func
                                   #'(lambda (x) 
                                       (cond ((null x) nil)
                                             ((screamer::variable? x) x)
                                             ((underscore? x) (make-list-input-variable))
                                             ((integerp x) (make-intv= x))
                                             (t (let ((var (make-list-input-variable)))
                                                  (assert! (omequalv var x))
                                                  var))))
                                   list))
                            (rule-card-assoc
                             (mapcar
                              #'(lambda (sym)
                                  (append (list sym)
                                          (mapcar #'(lambda (x) (cdr-assoc x dmg-wordsize-assoc))
                                                  (cdr-assoc sym dmg-sym-assoc))))
                              (remove-if-not #'and-sym? (mapcar #'car dmgassoc))))
                            (or-sym-xs-assoc nil)
                            (term-sym-xs-assoc nil)
                            (or-and-sym-assoc nil)
                            (or-sym-domain-assoc nil))
                        (dolist (x (remove-if-not #'terminal-sym? (mapcar #'car dmg-sym-assoc)))
                          (setf (cdr-assoc x term-sym-xs-assoc) nil))
                        (dolist (x (remove-if-not #'or-sym? (mapcar #'car dmg-sym-assoc)))
                          (let ((and-syms (dmg-and-syms x)))
                            (let ((cards (remove-duplicates
                                          (mapcar #'(lambda (y) (cdr-assoc y rule-card-assoc)) 
                                                  (dmg-and-syms x))
                                          :test #'equalp
                                          :from-end t)))
                              (setf (cdr-assoc x or-and-sym-assoc)
                                    (mapcar
                                     #'(lambda (c)
                                         (remove-if-not 
                                          #'(lambda (y) 
                                              (equalp c (cdr-assoc y rule-card-assoc))) 
                                          and-syms))
                                     cards)))))
                        (dolist (x (remove-if-not #'or-sym? (mapcar #'car dmg-sym-assoc)))
                          (setf (cdr-assoc x or-sym-domain-assoc) (dmg-sym-domain x)))
                        (if print-graph-info
                            (progn
                              (print (format nil "___________~%~A" rule-card-assoc))
                              (mapcar #'(lambda (s) (print (format nil "sym: ~A  ~A" s (cdr-assoc s rule-card-assoc))))
                                      (mapcar #'car rule-card-assoc))))
                        (let ((first-var (car vars))
                              (last-var (last-atom vars)))
                          (labels
                              ((csp-variable-name (x) 
                                 (cond ((null x) nil)
                                       ((screamer::variable? x) (screamer::variable-name x))
                                       (t x)))
                             
                               (maprule (xs r)
                                 (cond
                                  ((null xs) nil)
                                  ((listp r)
                                   (reduce
                                    #'orv
                                    (mapcar
                                     #'(lambda (p)
                                         (reduce
                                          #'orv
                                          (mapcar
                                           #'(lambda (r1)
                                               (reduce
                                                #'andv
                                                (mapcar
                                                 #'(lambda (x y) (maprule x y))
                                                 p
                                                 (cdr-assoc r1 dmg-sym-assoc))))
                                           r)))
                                     xs)))
                                  ((and (terminal-sym? r)
                                        (not (cdr xs)))
                                   (cond
                                    ((assoc (car xs) (cdr-assoc r term-sym-xs-assoc))
                                     (cdr-assoc (car xs) (cdr-assoc r term-sym-xs-assoc)))
                                    (t
                                     (setf (cdr-assoc (car xs) (cdr-assoc r term-sym-xs-assoc))
                                           (cond
                                            ((or symbol-mode
                                                 (not (numberp r)))
                                             (equalv (car xs) r))
                                            (t 
                                             (=v (car xs) r))))
                                     (cdr-assoc (car xs) (cdr-assoc r term-sym-xs-assoc)))))
                                  ((terminal-sym? r) nil)
                                  ((and-sym? r) 
                                   (let* ((xs-length (length xs))
                                          (rcard (cond
                                                  ((and continuation-mode
                                                        (< xs-length (length (cdr-assoc r rule-card-assoc)))
                                                        (position last-var xs))
                                                   (subseq (cdr-assoc r rule-card-assoc) 0 xs-length))
                                                  (t
                                                   (cdr-assoc r rule-card-assoc))))
                                          (rcard-length (length rcard)))
                                     (cond
                                      ((< xs-length rcard-length)
                                       nil)
                                      (t 
                                       (let* ((xs-partitions 
                                               (all-values (n-partitions-of2 rcard list)))
                                              (next-syms
                                               (cond
                                                ((and (= (length xs) 1)
                                                      continuation-mode
                                                      (eq (car xs) last-var))
                                                 (remove-if #'or-sym? (cdr-assoc r dmg-sym-assoc)))
                                                (t (cdr-assoc r dmg-sym-assoc)))))
                                         (unless (null xs-partitions)
                                           (let ((vs (mapcar
                                                      #'(lambda (p) ; ordered partition of xs
                                                          (apply 
                                                           #'andv
                                                           (mapcar
                                                            #'(lambda (x y) (maprule x y))
                                                            p
                                                            next-syms)))
                                                      xs-partitions)))
                                             (if (> (length xs-partitions) 1)
                                                 (reduce #'orv vs)
                                               (car vs)))))))))
                                  ((or-sym? r)
                                   (cond 
                                    ((cdr xs)
                                     (apply
                                      #'orv
                                      (mapcar
                                       ;#'(lambda (x) (maprule (ordered-partitions-of xs (cdr-assoc (car x) rule-card-assoc)) x))
                                       #'(lambda (x) (maprule (all-values (n-partitions-of2 (cdr-assoc (car x) rule-card-assoc) xs)) x))
                                       (cdr-assoc r or-and-sym-assoc))))
                                    (t 
                                     (let ((existing-var (find (cons r xs) or-sym-xs-assoc :key #'car :test #'equalp)))
                                       (cond (existing-var (cadr existing-var))
                                             (t 
                                              (let ((var (apply 
                                                          #'orv
                                                          (mapcar
                                                           #'(lambda (term) (maprule xs term))
                                                           (cdr-assoc r or-sym-domain-assoc)))))
                                                (push (list (cons r xs) var) or-sym-xs-assoc)
                                                var)))))))
                                  (t nil))))
                            (values (andv (all-memberv vars terminals)
                                          (maprule vars (name dmg))) 
                                    vars
                                    dmg))))))))))))))))

(cl:defun mapprules-bt (prules
                          &key check-fn
                          apply-objective-form-constraints-fn ; 1 parameter fn that receives the current variable list and returns a number (see the screamer docs  objective-form parameter)
                          min-list-length
                          max-list-length
                          process-chunk-size
                          input-process-increment
                          continue
                          init
                          listdxx
                          max
                          min
                          ordered-partitions-nondeterministic-values-cap
                          superset
                          symbol-mode
                          params
                          print-graph-info)
                :outputs 4
                :indoc '("input template list, screamer variable list or number" 
                         "production rules"
                         "process-chunk-size"
                         "input-process-increment (use process-chunk-size)"
                         "continue"
                         "init"
                         "listdxx"
                         "max"
                         "min"
                         "ordered-partitions-nondeterministic-values-cap"
                         "superset"
                         "symbol-mode"
                         "params"
                         "print-graph-info")
                (one-value
                 (let ((variables 
                        (mapprules-bt-internal (list (if symbol-mode (make-variable) (an-integerv)))
                                               prules
                                               :check-fn (make-chained-boolean-function 
                                                          (print (append (if check-fn
                                                                             (list check-fn))
                                                                         (if min-list-length
                                                                             (list #'(lambda (xs) (>= (length xs) min-list-length))))
                                                                         (if max-list-length
                                                                             (list #'(lambda (xs) (<= (length xs) max-list-length)))))))
                                               :apply-objective-form-constraints-fn (if apply-objective-form-constraints-fn
                                                                                        apply-objective-form-constraints-fn
                                                                                      #'(lambda (x) 100))
                                               :process-chunk-size process-chunk-size
                                               :input-process-increment input-process-increment
                                               :continue continue
                                               :init init
                                               :listdxx listdxx
                                               :max max
                                               :min min
                                               :ordered-partitions-nondeterministic-values-cap ordered-partitions-nondeterministic-values-cap
                                               :superset superset
                                               :symbol-mode symbol-mode
                                               :params params
                                               :print-graph-info print-graph-info)))
     
                   (values (car variables) nil nil (cadr variables)))))

(cl:defun mapprules-bt-internal (variables
                              prules
                              &key check-fn
                              apply-objective-form-constraints-fn
                              process-chunk-size
                              input-process-increment
                              continue
                              init
                              listdxx
                              max
                              min
                              ordered-partitions-nondeterministic-values-cap
                              superset
                              symbol-mode
                              params
                              print-graph-info)
  (either 
    (progn
      (unless (funcall check-fn variables) 
        (fail))
      (let* ((solution-score (an-integerv)))
        (assert! (mapprules variables
                            prules
                            :process-chunk-size process-chunk-size
                            :input-process-increment input-process-increment
                            :continue continue
                            :init init
                            :listdxx listdxx
                            :max max
                            :min min
                            :ordered-partitions-nondeterministic-values-cap ordered-partitions-nondeterministic-values-cap
                            :superset superset
                            :symbol-mode symbol-mode
                            :params params
                            :print-graph-info print-graph-info))
        (best-value (solution (progn 
                                (assert! (=v solution-score (funcall apply-objective-form-constraints-fn variables)))
                                variables)
                              (static-ordering #'linear-force)) solution-score) ))

    (mapprules-bt-internal (append variables (list (if symbol-mode (make-variable) (an-integerv))))
                           prules
                           :check-fn check-fn
                           :apply-objective-form-constraints-fn apply-objective-form-constraints-fn
                           :process-chunk-size process-chunk-size
                           :input-process-increment input-process-increment
                           :continue continue
                           :init init
                           :listdxx listdxx
                           :max max
                           :min min
                           :ordered-partitions-nondeterministic-values-cap ordered-partitions-nondeterministic-values-cap
                           :superset superset
                           :symbol-mode symbol-mode
                           :params params
                           :print-graph-info print-graph-info)))


(defvar *infinity* 1d38)
(defvar *-infinity* -1d38)
(defvar *prime-numbers*)
(setf *prime-numbers*
#(1 2 3 5 7 11 13 17 19 23 29 31 37 41 43 47 53 59 61 67 71 73 79 83 89 97 101 103 107 109 113 127 131 137 139
  149 151 157 163 167 173 179 181 191 193 197 199 211 223 227 229 233 239 241 251 257 263 269 271 277 281 283
  293 307 311 313 317 331 337 347 349 353 359 367 373 379 383 389 397 401 409 419 421 431 433 439 443 449 457
  461 463 467 479 487 491 499 503 509 521 523 541 547 557 563 569 571 577 587 593 599 601 607 613 617 619 631
  641 643 647 653 659 661 673 677 683 691 701 709 719 727 733 739 743 751 757 761 769 773 787 797 809 811 821
  823 827 829 839 853 857 859 863 877 881 883 887 907 911 919 929 937 941 947 953 967 971 977 983 991 997 1009
  1013 1019 1021 1031 1033 1039 1049 1051 1061 1063 1069 1087 1091 1093 1097 1103 1109 1117 1123 1129 1151 1153
  1163 1171 1181 1187 1193 1201 1213 1217 1223 1229 1231 1237 1249 1259 1277 1279 1283 1289 1291 1297 1301 1303
  1307 1319 1321 1327 1361 1367 1373 1381 1399 1409 1423 1427 1429 1433 1439 1447 1451 1453 1459 1471 1481 1483
  1487 1489 1493 1499 1511 1523 1531 1543 1549 1553 1559 1567 1571 1579 1583 1597 1601 1607 1609 1613 1619 1621
  1627 1637 1657 1663 1667 1669 1693 1697 1699 1709 1721 1723 1733 1741 1747 1753 1759 1777 1783 1787 1789 1801
  1811 1823 1831 1847 1861 1867 1871 1873 1877 1879 1889 1901 1907 1913 1931 1933 1949 1951 1973 1979 1987 1993
  1997 1999 2003 2011 2017 2027 2029 2039 2053 2063 2069 2081 2083 2087 2089 2099 2111 2113 2129 2131 2137 2141
  2143 2153 2161 2179 2203 2207 2213 2221 2237 2239 2243 2251 2267 2269 2273 2281 2287 2293 2297 2309 2311 2333
  2339 2341 2347 2351 2357 2371 2377 2381 2383 2389 2393 2399 2411 2417 2423 2437 2441 2447 2459 2467 2473 2477
  2503 2521 2531 2539 2543 2549 2551 2557 2579 2591 2593 2609 2617 2621 2633 2647 2657 2659 2663 2671 2677 2683
  2687 2689 2693 2699 2707 2711 2713 2719 2729 2731 2741 2749 2753 2767 2777 2789 2791 2797 2801 2803 2819 2833
  2837 2843 2851 2857 2861 2879 2887 2897 2903 2909 2917 2927 2939 2953 2957 2963 2969 2971 2999 3001 3011 3019
  3023 3037 3041 3049 3061 3067 3079 3083 3089 3109 3119 3121 3137 3163 3167 3169 3181 3187 3191 3203 3209 3217
  3221 3229 3251 3253 3257 3259 3271 3299 3301 3307 3313 3319 3323 3329 3331 3343 3347 3359 3361 3371 3373 3389
  3391 3407 3413 3433 3449 3457 3461 3463 3467 3469 3491 3499 3511 3517 3527 3529 3533 3539 3541 3547 3557 3559
  3571 3581 3583 3593 3607 3613 3617 3623 3631 3637 3643 3659 3671 3673 3677 3691 3697 3701 3709 3719 3727 3733
  3739 3761 3767 3769 3779 3793 3797 3803 3821 3823 3833 3847 3851 3853 3863 3877 3881 3889 3907 3911 3917 3919
  3923 3929 3931 3943 3947 3967 3989 4001 4003 4007 4013 4019 4021 4027 4049 4051 4057 4073 4079 4091 4093 4099
  4111 4127 4129 4133 4139 4153 4157 4159 4177 4201 4211 4217 4219 4229 4231 4241 4243 4253 4259 4261 4271 4273
  4283 4289 4297 4327 4337 4339 4349 4357 4363 4373 4391 4397 4409 4421 4423 4441 4447 4451 4457 4463 4481 4483
  4493 4507 4513 4517 4519 4523 4547 4549 4561 4567 4583 4591 4597 4603 4621 4637 4639 4643 4649 4651 4657 4663
  4673 4679 4691 4703 4721 4723 4729 4733 4751 4759 4783 4787 4789 4793 4799 4801 4813 4817 4831 4861 4871 4877
  4889 4903 4909 4919 4931 4933 4937 4943 4951 4957 4967 4969 4973 4987 4993 4999 5003 5009 5011 5021 5023 5039
  5051 5059 5077 5081 5087 5099 5101 5107 5113 5119 5147 5153 5167 5171 5179 5189 5197 5209 5227 5231 5233 5237
  5261 5273 5279 5281 5297 5303 5309 5323 5333 5347 5351 5381 5387 5393 5399 5407 5413 5417 5419 5431 5437 5441
  5443 5449 5471 5477 5479 5483 5501 5503 5507 5519 5521 5527 5531 5557 5563 5569 5573 5581 5591 5623 5639 5641
  5647 5651 5653 5657 5659 5669 5683 5689 5693 5701 5711 5717 5737 5741 5743 5749 5779 5783 5791 5801 5807 5813
  5821 5827 5839 5843 5849 5851 5857 5861 5867 5869 5879 5881 5897 5903 5923 5927 5939 5953 5981 5987 6007 6011
  6029 6037 6043 6047 6053 6067 6073 6079 6089 6091 6101 6113 6121 6131 6133 6143 6151 6163 6173 6197 6199 6203
  6211 6217 6221 6229 6247 6257 6263 6269 6271 6277 6287 6299 6301 6311 6317 6323 6329 6337 6343 6353 6359 6361
  6367 6373 6379 6389 6397 6421 6427 6449 6451 6469 6473 6481 6491 6521 6529 6547 6551 6553 6563 6569 6571 6577
  6581 6599 6607 6619 6637 6653 6659 6661 6673 6679 6689 6691 6701 6703 6709 6719 6733 6737 6761 6763 6779 6781
  6791 6793 6803 6823 6827 6829 6833 6841 6857 6863 6869 6871 6883 6899 6907 6911 6917 6947 6949 6959 6961 6967
  6971 6977 6983 6991 6997 7001 7013 7019 7027 7039 7043 7057 7069 7079 7103 7109 7121 7127 7129 7151 7159 7177
  7187 7193 7207 7211 7213 7219 7229 7237 7243 7247 7253 7283 7297 7307 7309 7321 7331 7333 7349 7351 7369 7393
  7411 7417 7433 7451 7457 7459 7477 7481 7487 7489 7499 7507 7517 7523 7529 7537 7541 7547 7549 7559 7561 7573
  7577 7583 7589 7591 7603 7607 7621 7639 7643 7649 7669 7673 7681 7687 7691 7699 7703 7717 7723 7727 7741 7753
  7757 7759 7789 7793 7817 7823 7829 7841 7853 7867 7873 7877 7879 7883 7901 7907 7919 7927 7933 7937 7949 7951
  7963 7993 8009 8011 8017 8039 8053 8059 8069 8081 8087 8089 8093 8101 8111 8117 8123 8147 8161 8167 8171 8179
  8191 8209 8219 8221 8231 8233 8237 8243 8263 8269 8273 8287 8291 8293 8297 8311 8317 8329 8353 8363 8369 8377
  8387 8389 8419 8423 8429 8431 8443 8447 8461 8467 8501 8513 8521 8527 8537 8539 8543 8563 8573 8581 8597 8599
  8609 8623 8627 8629 8641 8647 8663 8669 8677 8681 8689 8693 8699 8707 8713 8719 8731 8737 8741 8747 8753 8761
  8779 8783 8803 8807 8819 8821 8831 8837 8839 8849 8861 8863 8867 8887 8893 8923 8929 8933 8941 8951 8963 8969
  8971 8999 9001 9007 9011 9013 9029 9041 9043 9049 9059 9067 9091 9103 9109 9127 9133 9137 9151 9157 9161 9173
  9181 9187 9199 9203 9209 9221 9227 9239 9241 9257 9277 9281 9283 9293 9311 9319 9323 9337 9341 9343 9349 9371
  9377 9391 9397 9403 9413 9419 9421 9431 9433 9437 9439 9461 9463 9467 9473 9479 9491 9497 9511 9521 9533 9539
  9547 9551 9587 9601 9613 9619 9623 9629 9631 9643 9649 9661 9677 9679 9689 9697 9719 9721 9733 9739 9743 9749
  9767 9769 9781 9787 9791 9803 9811 9817 9829 9833 9839 9851 9857 9859 9871 9883 9887 9901 9907 9923 9929 9931
  9941 9949 9967 9973))








(cl:defun count-trues-in-listv (&rest xs)
  (apply #'vregister 
         (append (list #'(lambda () (apply #'count-truesv (flatt xs)))
                       #'count-trues-in-listv)
                 xs)))

(cl:defun flatt (list &optional level)
  (cond
   ((null list) nil)
   ((atom list) list)
   (T
    (let ((flat nil)) 
      (map-func #'(lambda (x) (push x flat)) list :level-max level) 
      (nreverse flat)))))





(cl:defun exptv (a b)
  (cond ((= b 0) 1)
        ((= b 1) a)
        ((< b 0) (funcallv #'expt a b))
        (t (*v a (expt a (1- b))))))

(cl:defun floorv (x)
  (funcallv #'floor x))

(cl:defun ceilingv (x)
  (funcallv #'ceiling x))



(cl:defun reduce-chunks (fn input &key default)
  (cond
   ((null input) default)
   ((not (listp input)) (reduce-chunks fn (list input) :default default))
   ((>= (length input) call-arguments-limit) 
    (alert2 (format nil "call to ~A exceeds the call arguments limit: ~A, ~A arguments" fn (length input)))
    (reduce fn (mapcar #'(lambda (chunk) (apply fn chunk)) 
                       (nsucc input call-arguments-limit :step call-arguments-limit))))
   (t (apply fn input))))




(cl:defun make-numberv= (n)
  (let ((v (make-variable)))
    (assert! (numberpv v))
    (assert! (=v v n))
    v))

(cl:defun make-intv= (n)
  (let ((v (make-variable)))
    (assert! (integerpv v))
    (cond ((null n) v)
          ((numberp n)
           (assert! (=v v n)))
          ((listp n)
           (cond ((and (every #'listp n)
                       (every #'cdr n))
                  (assert! (memberv v (remove-duplicates (flat (mapcar #'(lambda (x) 
                                                                   (let ((s (sort x #'<)))
                                                                     (all-values (an-integer-between (car s) (cadr s))))) n))))))
                 (t (assert! (andv (>=v v (car n))
                                   (<=v v (cadr n)))))))
          (t nil))
    v))

(cl:defun make-realv= (n)
  (let ((v (make-variable)))
    (assert! (realpv v))
    (assert! (=v v n))
    v))

(cl:defun make-var-equalv (x)
  (let ((v (make-variable)))
    (assert! (equalv v x))
    v))


(cl:defun split (ll) 
  (cond ((> (length ll) 1)
         (append (list (reverse (cdr (reverse ll))))
                 (list (list (car (reverse ll))))))
        ((> (length ll) 0)
         (list ll))
        (t ll)))

(defun evaluate-an-expanded-list-function (input r)
  (cond ((null r) t)
        ((consp r)
         (and (evaluate input (car r))
              (evaluate input (cdr r))))
        ((functionp r)
         (funcall r input))
        (t nil)))
(defun an-expanded-list (templates rules &key randomize-choices) ; delete
  (let ((r (solution (an-expanded-list-rec 1 templates :randomize-choices randomize-choices)
                     (static-ordering #'linear-force))))
    (unless (evaluate-an-expanded-list-function r rules) (fail))
    r))

(cl:defun lsubs1-members-of (lists)
  (if (null lists)
      nil
    (append (list (a-member-of (car lists)))
            (lsubs1-members-of (cdr lists)))))           

(defun lsubs1 (lists predicate &optional (r 0.5))
  (let ((is (mapcar
             #'(lambda (x) (arithm-ser x (floor (* r x)) -1))
             (mapcar #'length lists))))
    (let* ((cards (permut-random 
                    (all-values (lsubs1-members-of is))))
           (c (a-member-of cards))
           (l (mapcar #'(lambda (i x) (subseq x 0 i)) c lists)))
      (if predicate (funcall predicate l))
      l)))

(cl:defun chord-seq->list (obj)
  (mapcar #'(lambda (x) (/ x 100)) (mapcar #'car (om::lmidic obj))))

(cl:defun nsucc (input n &key step list-padding pad-character)
  (cond
   ((null input) nil)
   ((null n) (nsucc input (length input) :step step :list-padding list-padding :pad-character pad-character))
   ((null step) (nsucc input n :step 1 :list-padding list-padding :pad-character pad-character))
   ((and (listp input)
         (< (length input) n))
    (list input))
   (t
    (let* ((list (if list-padding 
                     (append input
                             (make-sequence 'list (* -1 (- (length input) 
                                                           (* n (ceiling (/ (length input) n))))) :initial-element pad-character))
                   input))
           (stack nil))
      (block gathering-segments
        (loop for i from 0
              for j = (* i step)
              for k = (+ j n)
              while (< j (length input)) do
                (progn
                  (cond
                   ((< j (length list))                   
                    (push (subseq list j (min k (length list))) stack)
                    (if (and (>= k (length list))
                             (> k (1+ (* (1- i) step))))
                        (return-from gathering-segments)))
                   (T T)))))
      (reverse stack)))))

(cl:defun write-textfile (input label ext &optional timezone)
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

(cl:defun remove-successive-duplicates (list &key test)
  :icon 235
  (let ((stack nil)
        (test-fn (or test #'equalp)))
    (labels ((push-if-test-fails (x)
               (cond ((null stack) (push x stack))
                     ((not (funcall test-fn (car stack) x))
                      (push x stack))
                     (t t))))
      (mapcar #'push-if-test-fails list)
      (reverse stack))))

(cl:defun last-item (list)
  (cond ((null list) nil)
        ((not (listp list)) list)
        (t
         (car (reverse list)))))

(cl:defun omandv (&rest xs) ; deleted
  (apply #'vregister
         (append
          (list
           #'(lambda () (apply #'andv (flatt xs)))
           #'omandv)
          xs)))

(cl:defun omorv (&rest xs) ; deleted
  (or (apply #'lookup-solver-key 
             (append (list #'omorv) xs))
      (apply #'register-solver-key 
             (append (list #'omorv (apply #'orv xs)) xs))))

(cl:defun ommemberv (x sequence)
  (or (apply #'lookup-solver-key
             (append (list #'ommemberv) (list x) sequence))
      (apply #'register-solver-key
             (append (list #'ommemberv (memberv x sequence)) (list x) sequence))))

(cl:defun ommembersofv (list sequence)
  (map-andv #'(lambda (x) (memberv x sequence)) (flatt list)))

(cl:defun om!membersofv (list sequence)
  (map-andv #'(lambda (x) (notv (memberv x sequence))) (flatt list)))



(cl:defun items-inv (list sequence &key numeric fast-crosscheck)
  (let ((sequence (reverse sequence)))
    (labels
        ((numeric? (x) (and x (or (numberp x) (s::variable? x))))
         (number-or-symbol? (x) (or x (s::variable? x)))
         (member? (x) (memberv x sequence))
         (each=any (xs)
           (cond 
            ((null xs) T)
            (T (andv (item-in (car xs))
                     (each=any (cdr xs))))))
         (item-in (x) (apply #'orv (mapcar #'(lambda (y) (=v x y)) sequence))))
      (cond
       ((null list) (memberv nil sequence))
       (numeric
        (each=any (remove-duplicates (remove-if-not #'numeric? (flatt (list list))))))
       (T
        (apply #'andv (mapcar #'member? (remove-duplicates (remove-if-not #'number-or-symbol? (flatt (list list)))))))))))

(cl:defun items!inv (list sequence &key numeric fast-crosscheck)
  :icon 235
  (labels
      ((all!members-of (xs)
         (cond ((null xs) nil)
               ((not (listp xs)) (all!members-of (list xs)))
               (numeric (map-andv #'/=any xs))
               (T (ommembersofv xs sequence))))
       (/=any (x) (map-andv #'(lambda (y) (/=v x y)) sequence)))
    (all!members-of list)))

(cl:defun an-integer-member-ofv (xs)
  (let ((x (an-integerv)))
    (assert! (memberv x xs))
    x))
(cl:defun omnotv (x) 
  (or (lookup-solver-key #'omnotv x)
      (register-solver-key #'omnotv        
                           (notv x)
                           x)))

;(cl:defun %v (n d) 
;  (let ((x (an-integer-betweenv 0 (1- d)))
;        (d*m (*v d (an-integerv))))
;    (assert! (<=v d*m n))
;    (assert! (=v x (-v n d*m)))
;    x))
(defvar *paradigm--modulo-function* nil)
(defvar *paradigm--modulo-calls-native-function* 
  #'(lambda (n d)
      (let ((var (an-integer-betweenv 0 (1- d))))
        (assert! (equalv var (funcallv #'mod n d)))
        var)))
(defvar *paradigm--modulo-restricts-bounds*
  #'(lambda (n d) 
      (let ((x (-v n (*v d (an-integerv)))))
        (assert! (integerpv x))
        (assert! (<v x d))
        (assert! (>=v x 0))
        x)))
(setf *paradigm--modulo-function* *paradigm--modulo-restricts-bounds*)
(cl:defun %v (n d) (funcall *paradigm--modulo-function* n d))
(cl:defun modv (n d) (%v n d))
(cl:defun om%v (n d)
  (or (lookup-solver-key #'om%v n d)
      (register-solver-key #'om%v (t2l::%v n d) n d)))

(cl:defun ommodv (n d) (om%v n d))
(cl:defun om+v (&rest xs)
  :icon 193  
  (or (apply #'lookup-solver-key 
             (append (list #'om+v) xs))
      (apply #'register-solver-key 
             (append (list #'om+v
                           (apply #'+v xs))
                     xs))))
(cl:defun om-v (x &rest xs) :icon 194 
  (or (apply #'lookup-solver-key 
             (append (list #'om-v) (list x) xs))
      (apply #'register-solver-key 
             (append (list #'om-v
                           (apply #'-v (append (list x) xs)))
                     (list x)
                     xs))))

(cl:defun om*v (&rest xs)
 :icon 195 
 (or (apply #'lookup-solver-key 
             (append (list #'om*v) xs))
      (apply #'register-solver-key 
             (append (list #'om*v
                           (apply #'*v xs))
                     xs))))
(cl:defun om/v (&rest xs)
 :icon 196 
  (or (apply #'lookup-solver-key 
             (append (list #'om/v) xs))
      (apply #'register-solver-key 
             (append (list #'om/v
                           (apply #'/v xs))
                     xs))))

(cl:defun om1+v (x) :icon 193 (om+v 1 x))
(cl:defun om-1v (x) :icon 194 (om-v x 1))
(cl:defun om<v (&rest xs)
 :icon 255 
 (or (apply #'lookup-solver-key 
            (append (list #'om<v) xs))
     (apply #'register-solver-key 
            (append (list #'om<v
                          (apply #'<v xs))
                    xs))))
(cl:defun om>v (&rest xs) 
  :icon 256
  (or (apply #'lookup-solver-key 
             (append (list #'om>v) xs))
      (apply #'register-solver-key 
             (append (list #'om>v
                           (apply #'>v xs))
                     xs))))
(cl:defun om<=v (&rest xs)
  :icon 257
  (or (apply #'lookup-solver-key 
             (append (list #'om<=v) xs))
      (apply #'register-solver-key 
             (append (list #'om<=v
                           (apply #'<=v xs))
                     xs))))
(cl:defun om>=v (&rest xs)
  (or (apply #'lookup-solver-key 
             (append (list #'om>=v) xs))
      (apply #'register-solver-key 
             (append (list #'om>=v
                           (apply #'>=v xs))
                     xs))))
(cl:defun om=v (&rest xs)
  (or (apply #'lookup-solver-key 
             (append (list #'om=v) xs))
      (apply #'register-solver-key 
             (append (list #'om=v
                           (apply #'=v xs))
                     xs))))
(cl:defun om/=v (&rest xs)
  (or (apply #'lookup-solver-key 
             (append (list #'om/=v) xs))
      (apply #'register-solver-key 
             (append (list #'om/=v
                           (apply #'/=v xs))
                     xs))))
(cl:defun omequalv (x y)
  (or (lookup-solver-key #'omequalv x y)
      (register-solver-key #'omequalv
                           (equalv x y)
                           x
                           y)))

(cl:defun omeqlv (xs ys)
  (apply #'andv (map2func #'(lambda (x y) (omequalv x y)) xs ys)))

(cl:defun om!eqlv (xs ys)
  (apply #'andv (map2func #'(lambda (x y) (omnotv (omequalv x y))) xs ys)))

(cl:defun lists=v (xs ys)
  (apply #'andv (map2func #'(lambda (x y)
                                (cond
                                 ((and (null x) (null y)) T)
                                 ((null x) nil)
                                 ((null y) nil)
                                 (T (om=v x y))))
                            xs
                            ys)))


(cl:defun lists/=v (xs ys)
  (apply #'andv (map2func #'(lambda (x y)
                                (cond
                                 ((and (null x) (null y)) nil)
                                 ((null x) T)
                                 ((null y) nil)
                                 (T (om/=v x y))))
                            xs
                            ys)))

(cl:defun ommaxv (&rest xs)
  :icon 209
  (or (apply #'lookup-solver-key 
             (append (list #'ommaxv) xs))
      (apply #'register-solver-key 
             (append (list #'ommaxv
                           (apply #'screamer:maxv xs))
                     xs))))

(cl:defun omminv (&rest xs)
  :icon 209
  (or (apply #'lookup-solver-key 
             (append (list #'omminv) xs))
      (apply #'register-solver-key 
             (append (list #'omminv
                           (apply #'screamer:minv xs))
                     xs))))

(cl:defun cartx2 (xs)
  :icon 235
  (mapcar
   #'reverse
   (remove-duplicates
    (all-values (let* ((list (list (a-member-of xs)
                                   (a-member-of xs))))
                  (if (equalp (car list) (cadr list))
                      (fail)
                    list)))
    :test #'(lambda (xs ys) (or (equalp xs ys) (equalp xs (reverse ys)))))))

(cl:defun variable-label (x) (symbol-name (screamer::variable-name x)))



(cl:defun combinations-of2 (list) (list2comb list)) ; delete
(cl:defun list2comb (list)
  (let ((stack nil))
    (labels
        ((internal (xs)
           (cond ((null xs) nil)
                 ((cdr xs)
                  (mapcar #'(lambda (y)
                              (push (list (car xs) y)
                                    stack))
                          (cdr xs))
                  (internal (cdr xs)))
                 (t t))))
      (internal list)
      (reverse stack))))

(cl:defun group-sequence-on (test sequence)
  (let ((test (or test #'equalp)))
    (labels
        ((group-sequence (group-list group seqc)                        
           (cond ((null seqc) (append group-list (list group)))
                 ((null group) 
                  (group-sequence group-list (list (car seqc)) (cdr seqc)))
                 ((not (funcall test (last-item group) (car seqc)))
                  (group-sequence (append group-list (list group)) nil seqc))
                 (T (group-sequence group-list (append group (list (car seqc))) (cdr seqc)))))
         (last-item (list) (car (last list))))
      (group-sequence nil nil sequence))))
                        

(cl:defun has-null-values (list)   
  (cond ((null list) t)
        ((atom list) nil)
        ((some #'null (flatt list)) t)
        (t nil)))

(cl:defun list-excerpt (list percent &optional items)
  (cond 
   ((null list) list)
   ((= (length list) 1) list)
   (t
    (let* ((start-index (min 
                         (round (float (* (float (/ (min (abs percent)) 100.0))
                                          (length list))))
                         (1- (length list))))
           (end-index (if items                         
                          (min (length list) (+ start-index items))
                        (length list))))
      (subseq list start-index end-index)))))

(cl:defun seteql (a b) (and (every #'(lambda (x) (member x b)) a) (every #'(lambda (y) (member y a)) b)))

(cl:defun alleq (list &key test)
  :icon 235
  (every #'(lambda (xs) (funcall (or test #'equal) (car xs) (cadr xs)))
         (cartx2 list)))


(cl:defun remove-consecutive-duplicates (list &key test)
  (let ((test (or test #'equalp)))
    (labels
        ((transform (list candidates)
           (cond
            ((null candidates) list)
            ((null list)
             (transform (list (car candidates)) (cdr candidates)))
            ((funcall test (car (reverse list)) (car candidates))
             (transform list (cdr candidates)))
            (T
             (transform (append list (list (car candidates))) (cdr candidates))))))
      (transform nil list))))

(cl:defun all-differentv (x &rest xs)
  ;; from 'Screamer' library documentation (see screams.lisp)
  ;; Functionally the same as (apply #'/=v list), but faster.
  (labels ((all-different (x xs)
             (if (null xs)
                 t
                 (andv (notv (=v x (car xs)))
                       (all-different x (cdr xs))
                       (all-different (car xs) (cdr xs))))))
    (all-different x xs)))
(cl:defun distinct!eq (x &rest xs)
  ;; from 'Screamer' library documentation (see screams.lisp)
  ;; Functionally the same as (apply #'/=v list), but faster.
  (labels ((all-different (x xs)
             (if (null xs)
                 t
                 (andv (notv (equalv x (car xs)))
                       (all-different x (cdr xs))
                       (all-different (car xs) (cdr xs))))))
    (all-different x xs)))
(cl:defun distinct-values-countv (list)
  (count-trues-in-listv
   (mapcar #'(lambda (xs)
               (omnotv (apply #'equalv xs)))
           (combinations-of2 list))))

(cl:defun member-of-sequencev (x sequence)
  :icon 235
  (labels
      ((member-of-sequencev-internal (sequence)
         (map-orv #'(lambda (y) (omequalv x y)) sequence)))
  (or (apply #'lookup-solver-key (append (list #'member-of-sequencev x) sequence))
      (apply #'register-solver-key (append (list #'member-of-sequencev
                                                 (member-of-sequencev-internal sequence))
                                           (list x)
                                           sequence)))))

(cl:defun member-of-sequencev (x sequence)
  :icon 235
  (labels
      ((member-of-sequencev-internal (sequence)
         (cond ((null sequence) T)
               ((cdr sequence)
                (orv
                 (member-of-sequencev-internal (list (car sequence)))
                 (member-of-sequencev-internal (cdr sequence))))
               (T (omequalv x (car sequence))))))
  (or (apply #'lookup-solver-key (append (list #'member-of-sequencev x) sequence))
      (apply #'register-solver-key (append (list #'member-of-sequencev
                                                 (member-of-sequencev-internal sequence))
                                           (list x)
                                           sequence)))))

;(cl:defun member-of-number-sequencev (x xs)
;  :icon 235
;  (labels ((member-of-number-sequencev-internal (sequence)
;             (map-orv #'(lambda (y) (om=v x y)) sequence)))
;    (or (apply #'lookup-solver-key (append (list #'member-of-number-sequencev x) xs))
;      (apply #'register-solver-key (append (list #'member-of-number-sequencev
;                                                 (member-of-number-sequencev-internal xs))
;                                           (list x)
;                                           xs)))))

(cl:defun member-of-number-sequencev (x xs)
  :icon 235
  (labels ((member-of-number-sequencev-internal (sequence)
             (cond ((null sequence) T)
                   ((cdr sequence)
                    (orv
                     (member-of-number-sequencev-internal (list (car sequence)))
                     (member-of-number-sequencev-internal (cdr sequence))))
                   (T (om=v x (car sequence))))))
    (or (apply #'lookup-solver-key (append (list #'member-of-number-sequencev x) xs))
      (apply #'register-solver-key (append (list #'member-of-number-sequencev
                                                 (member-of-number-sequencev-internal xs))
                                           (list x)
                                           xs)))))

;(cl:defun not-member-of-sequencev (x xs)
;  :icon 235
;  (labels ((not-member-of-sequencev-internal (sequence)
;             (map-andv #'(lambda (y) (notv (equalv x y))) sequence)))
;    (or (apply #'lookup-solver-key (append (list #'not-member-of-sequencev x) xs))
;      (apply #'register-solver-key (append (list #'not-member-of-sequencev
;                                                 (not-member-of-sequencev-internal xs))
;                                           (list x)
;                                           xs)))))

(cl:defun not-member-of-sequencev (x xs)
  :icon 235
  (labels ((not-member-of-sequencev-internal (sequence)
             (cond ((null sequence) T)
                   ((cdr sequence)
                    (andv
                     (not-member-of-sequencev-internal (list (car sequence)))
                     (not-member-of-sequencev-internal (cdr sequence))))
                   (T (omnotv (omequalv x (car sequence)))))))
    (or (apply #'lookup-solver-key (append (list #'not-member-of-sequencev x) xs))
      (apply #'register-solver-key (append (list #'not-member-of-sequencev
                                                 (not-member-of-sequencev-internal xs))
                                           (list x)
                                           xs)))))

(cl:defun not-member-of-number-sequencev (x xs)
  :icon 235
  (labels ((not-member-of-number-sequencev-internal (sequence)
             (cond ((null sequence) T)
                   ((cdr sequence)
                    (andv
                     (not-member-of-number-sequencev-internal (list (car sequence)))
                     (not-member-of-number-sequencev-internal (cdr sequence))))
                   (T (om/=v x (car sequence))))))
    (or (apply #'lookup-solver-key (append (list #'not-member-of-number-sequencev x) xs))
      (apply #'register-solver-key (append (list #'not-member-of-number-sequencev
                                                 (not-member-of-number-sequencev-internal xs))
                                           (list x)
                                           xs)))))

(cl:defun not-member-of-number-sequencev (x xs)
  :icon 235
  (labels ((not-member-of-number-sequencev-internal (sequence)
             (map-andv #'(lambda (y) (om/=v x y)) sequence)))
    (or (apply #'lookup-solver-key (append (list #'not-member-of-number-sequencev x) xs))
      (apply #'register-solver-key (append (list #'not-member-of-number-sequencev
                                                 (not-member-of-number-sequencev-internal xs))
                                           (list x)
                                           xs)))))

(cl:defun all-items-inv (list sequence)
  :icon 235
  (labels
      ((all-members-of (xs)
         (cond ((null xs) T)
               ((cdr xs)
                (andv
                 (all-members-of (list (car xs)))
                 (all-members-of (cdr xs))))
               (T (member-of-sequencev (car xs) sequence)))))
    (all-members-of list)))

(cl:defun all-items-!inv (list sequence)
  :icon 235
  (labels
      ((all-not-members-of (xs)
         (cond ((null xs) T)
               ((cdr xs)
                (andv
                 (all-not-members-of (list (car xs)))
                 (all-not-members-of (cdr xs))))
               (T (not-member-of-sequencev (car xs) sequence)))))
    (all-not-members-of list)))

(cl:defun all-integer-items-inv (list sequence) ; delete
  :icon 235
  (labels
      ((all-members-of (xs sequence)
         (cond ((null xs) T)
               ((cdr xs)
                (andv
                 (all-members-of (list (car xs)) sequence)
                 (all-members-of (cdr xs) sequence)))
               (T (member-of-number-sequencev (car xs) sequence)))))
    (cond ((null list) (some #'null sequence))
          ((null sequence) nil)
          ((and (some #'null list)
                (some #'null sequence))
           (all-members-of (remove nil list)
                           (remove nil sequence)))
          ((some #'null list) nil)
          ((some #'null sequence)
           (all-members-of list
                           (remove nil sequence)))
          (t
           (all-members-of list sequence)))))

(cl:defun all-integer-items-!inv (list sequence)
  :icon 235
  (labels
      ((all-not-members-of (xs sequence)
         (cond ((null xs) T)
               ((cdr xs)
                (andv
                 (all-not-members-of (list (car xs)) sequence)
                 (all-not-members-of (cdr xs) sequence)))
               (T (not-member-of-number-sequencev (car xs) sequence)))))
    (cond ((null list) nil)
          ((null sequence) nil)
          ((and (some #'null list)
                (some #'null sequence))
           (all-not-members-of (remove nil list)
                           (remove nil sequence)))
          ((some #'null list) nil)
          ((some #'null sequence)
           (all-not-members-of list
                               (remove nil sequence)))
          (t
           (all-not-members-of list sequence)))))
(cl:defun count-distinct-valuesv (xs)
  :icon 176
  (apply 
   #'count-truesv 
   (mapcar #'(lambda (ys)
               (let ((x (car ys))
                     (y (cadr ys)))
                 (cond ((and (null x) (null y)) nil)
                       ((null x) t)
                       ((null y) t)
                       ; ((not (ground? ys)) nil)
                       (t (/=v x y)))))
           (let ((rs 
                  (remove-duplicates 
                    (all-values
                      (let ((x (a-member-of (flatt xs)))
                            (y (a-member-of (flatt xs))))
                        (unless (not (equalp x y))
                          (fail))
                        (list x y)))
                    :test #'(lambda (xs ys)
                              (and (find (car xs) ys)
                                   (find (cadr xs) ys)
                                   (find (car ys) xs)
                                   (find (cadr ys) xs))))))
             ; (assert! (ground? rs))
             rs))))

;(cl:defun all-integer-items-!inv (list sequence)
;  :icon 235
;  (labels
;      ((all-not-members-of (xs sequence)
;         (map-andv #'(lambda (x) (not-member-of-number-sequencev x sequence)) xs)))
;    (cond ((null list) nil)
;          ((null sequence) nil)
;          ((and (some #'null list)
;                (some #'null sequence))
;           (all-not-members-of (remove nil list)
;                           (remove nil sequence)))
;          ((some #'null list) nil)
;          ((some #'null sequence)
;           (all-not-members-of list
;                               (remove nil sequence)))
;          (t
;           (all-not-members-of list sequence)))))

(cl:defun first-atom (xs)
  :icon 235
  (cond ((null xs) nil)
        ((listp xs) (first-atom (car xs)))
        (T xs)))

(cl:defun variable-names-in (x)
  (labels ((getnameof (y)
             (cond ((null y) nil)
                   ((screamer::variable? y)
                    (screamer::variable-name y))
                   (t y))))
    (cond ((null x) nil)
          ((listp x) 
           (map-func #'getnameof x))
          (t (getnameof x)))))

(cl:defun all?variables-in (list) 
  (remove-duplicates (remove nil (flatt list))))

(cl:defun symxlat (list map)
  (map-func
   #'(lambda (k) 
       (let ((entry (assoc k map)))
         (if entry (cdr entry) nil)))
   list))

(cl:defun symxlatv (keys map)
  (let ((map-keys (mapcar #'car map))
        (map-entr (remove-duplicates (flat1 (mapcar #'cdr map)))))
    (labels
        ((lookxl (k)
           (assert! (memberv k (mapcar #'car keys)))
           (let ((var (make-variable)))
             (assert! (memberv var map-entr))
             (labels
                 ((xlat (key entries) ;; generate rule for VAR
                    (cond
                     ((null entries) nil)
                     (T (let ((entry (car entries)) 
                              (keyeq (omequalv key (caar entries))))
                          (orv 
                           (andv keyeq (ommemberv var (cdr entry)))
                           (andv (omnotv keyeq) (xlat key (cdr entries)))))))))
               (assert! (xlat k map))
               var))))
      (map-func #'lookxl keys))))

(cl:defun intxlatv (keys map)
  (let ((map-keys (mapcar #'car map))
        (map-entr (remove-duplicates (flat1 (mapcar #'cdr map)))))
    (let (; (mkmax (apply #'max map-keys))
          ; (mkmin (apply #'min map-keys))
          (memax (apply #'max map-entr))
          (memin (apply #'min map-entr)))
      ; (print (format nil "map-keys: ~A map-entr: ~A mkmin: ~A mkmax: ~A" map-keys map-entr mkmin mkmax))
      (labels
          ((lookxl (k)
             ; (assert! (>=v k mkmin))
             ; (assert! (<=v k mkmax))
             (let ((var (an-integerv)))
               (assert! (>=v var memin))
               (assert! (<=v var memax))
               (labels
                   ((xlat (key entries) ;; generate rule for VAR
                      (cond
                       ((null entries) nil)
                       (T (let ((entry (car entries)) 
                                (key= (om=v key (caar entries))))
                            (orv 
                             (andv key= (items-inv var (cdr entry) :numeric T)); (ommemberv var (cdr entry)))
                             (xlat key (cdr entries))))))))
                 (assert! (xlat k map))
                 var))))
        (map-func #'lookxl keys :ignore-null-input T)))))

(cl:defun reverse-xlatmap (map) 
  (flat1 (mapcar #'(lambda (entry) (mapcar #'(lambda (k) (list k (car entry))) (cdr entry))) map)))

(cl:defun xlatsymv (keys map) (symxlatv keys (reverse-xlatmap map)))
(cl:defun xlatintv (keys map) (intxlatv keys (reverse-xlatmap map)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;           solver-input and solver-output
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar *backup-solver-input* (list (cons :backup nil)))
(defvar *backup-solver-output* (list (cons :backup nil)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;           variables registry
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar *solver-keys* nil)
(defvar *register-solver-keys* T)
(defvar *sk-seteql-functions* (list #'ommaxv 
                                    #'omminv
                                    #'omandv
                                    #'omorv
                                    #'om=v
                                    #'om/=v
                                    #'omequalv))
(defvar *sk-seteql-function* 
  #'(lambda (list1 list2)
      (and (equalp (car list1) (car list2))
           (seteql (cdr list1) (cdr list2)))))
(defvar *sk-member-of-functions* (list 
                                  #'ommemberv
                                  #'member-of-sequencev
                                       #'member-of-number-sequencev
                                       #'not-member-of-sequencev
                                       #'not-member-of-number-sequencev
                                       #'all-items-inv
                                       #'all-integer-items-inv
                                       #'all-items-!inv
                                       #'all-integer-items-!inv))
(defvar *sk-member-of-function*
  #'(lambda (list1 list2)
      (and (equalp (car list1) (car list2))
           (equalp (cadr list1) (cadr list2))
           (seteql (cddr list1) (cddr list2)))))
(defvar *sk-unordered-list-functions* (list #'om+v
                                            #'om*v
                                            #'omavgv
                                            #'distinct-values-countv))
(defvar *sk-unordered-list-function*
  #'(lambda (list1 list2)
      (and 
       (equalp (car list1) (car list2))
       (cond
        ((and (null list1) (null list2)) T) ;; ??
        ((null list1) nil)
        ((null list2) nil)
        ((not (= (length list1) (length list2))) nil)
        (T
         (labels ((indexof-1st-match (x list)
                    (let ((i (loop named loop-1 for i from 0 to (1- (length list))
                                   do (if (equal x (elt list i))
                                          (return-from loop-1 i)))))
                      (cond ((null i) nil)
                            ((= i (length list)) nil)
                            (T i))))
                  (remove-element-at-index (i list)
                    (cond ((= i 0) (cdr list))
                          ((= i (1- (length list))) (butlast list))
                          (T (append (subseq list 0 i)
                                     (subseq list (1+ i) (length list))))))
                  (compare (list1 list2)
                    (cond ((null list1) (null list2))
                          ((null list2) nil)
                          (T (let ((i (indexof-1st-match (car list1) list2)))
                               (if i
                                   (compare (cdr list1) (remove-element-at-index i list2))
                                 nil))))))                          
           (compare (cdr list1) (cdr list2))))))))

(cl:defun enable-variable-cache-map () (setf *register-solver-keys* t))

(cl:defun disable-variable-cache-map () (setf *register-solver-keys* nil) (setf *solver-keys* nil))

(cl:defun vregister (make-variable-fn key-fn &rest key-fn-arguments)
  (or (apply #'lookup-solver-key (append (list key-fn) key-fn-arguments))
      (apply #'register-solver-key (append (list key-fn (funcall make-variable-fn)) key-fn-arguments))))

(cl:defun lookup-solver-key (fn &rest arguments) 
  (if (not *register-solver-keys*)
      nil
    (cdr (assoc (append (list fn) arguments)
                *solver-keys* 
                :test (cond 
                       ((find fn *sk-seteql-functions*)  *sk-seteql-function*)
                       ((find fn *sk-unordered-list-functions*) *sk-unordered-list-function*)
                       ((find fn *sk-member-of-functions*) *sk-member-of-function*)
                       (t #'equalp))))))

(cl:defun register-solver-key (fn value &rest arguments)
  (cond ((null *register-solver-keys*) value)
        ((or (null *solver-keys*)
             (null (lookup-solver-key (append (list fn) arguments))))
         (push (cons (append (list fn) arguments) value) *solver-keys*)
         value)
        (t value)))

(cl:defun print-solver-registry-info ()
  (cond 
   ((not (or (null *solver-keys*)
             (= 0 (length *solver-keys*))))
    ; (format om-lisp:*om-stream* "~A~%" (remove-duplicates (mapcar #'caar *solver-keys*)))
    (mapcar #'print 
            (mapcar #'(lambda (fn) (list fn (length (remove-if-not #'(lambda (entry) (equal fn (caar entry))) t2l::*solver-keys*)))) 
                    (remove-duplicates (mapcar #'caar t2l::*solver-keys*)))))
   (T 0))
  (if *solver-keys* 
      (length *solver-keys*)
    nil))

(cl:defun reset-solver-registry ()
  (cond 
   ((not (or (null *solver-keys*)
             (= 0 (length *solver-keys*))))
    (print-solver-registry-info)
    (let ((status (length *solver-keys*)))
      (print (format nil "deleting ~A..." (length *solver-keys*)))
      (loop for i from 0 while *solver-keys* do (pop *solver-keys*))
      status))
   (T 0)))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;           console messages
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defvar *print-warnings* nil)
(defvar *print-warnings-level* 0)

(cl:defun print-warnings (&optional x) 
  :icon 129 
  (setf *print-warnings* 10)
  (setf *print-warnings-level* 10)
  *print-warnings*)

(cl:defun hide-warnings () 
   :icon 129
   (setf *print-warnings* nil)
   (setf *print-warnings-level* 0))

(cl:defun print-warning (str &rest xs)
  (unless (null *print-warnings*)
    (print (apply #'format (append (list nil str) xs)))
    nil))

(cl:defun alert2 (input &key label print-label-only) ; delete
  (cond 
   (*print-warnings*
    (cond 
     ((null label)
      (print (format nil "~A" input)))
     (t
      (cond (print-label-only (print label))
            (t (print (format nil "~A: ~A" label input))))))))
  input)

(cl:defun format-alert (text &rest args)
  (unless (null *print-warnings*)
    (apply #'format (append (list om-lisp::*om-stream* (concatenate 'string text "~%")) args))
    nil))




