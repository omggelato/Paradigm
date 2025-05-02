(in-package :SCREAMER)

(defvar *infinity* 1e38)

(defun ?variables-in (x) (reverse (?variables-in-internal x '())))

(defun ?variables-in-internal (x variables)
  (cond
   ((null x) variables)
   ((screamer::variable? x)
    (if (find x variables :test #'eq)
        variables
      (cons x variables)))
   ((consp x)
    (?variables-in-internal (cdr x) (?variables-in-internal (car x) variables)))
   (T variables)))

(defun ?xs-in (input)
  (?xs-in-internal input '()))

(defun ?xs-in-internal (x variables)
  (cond
   ((null x) variables)
   ((or (symbolp x) (numberp x) (screamer::variable? x))
    (if (find x variables :test #'eq)
        variables
      (cons x variables)))
   ((consp x)
    (?xs-in-internal (cdr x) (?xs-in-internal (car x) variables)))
   (T variables)))

(defun ?numbers-in-internal (x variables)
  (cond
   ((null x) variables)
   ((or (numberp x) (screamer::variable? x))
    (if (find x variables :test #'eq)
        variables
      (cons x variables)))
   ((consp x)
    (?numbers-in-internal (cdr x) (?numbers-in-internal (car x) variables)))
   (T variables)))

(defun ?numbers-in (x)
  (?numbers-in-internal x '()))


(defun flatt (x)
  (append
   (cond
    ((null x) nil)
    ((and (consp x) (consp (car x)))
     (flatt (car x)))
    ((consp x)
     (list (car x)))
    (T (list x)))
   (if (and (consp x) 
            (cdr x))
       (flatt (cdr x)) 
     NIL)))

(cl:defun rewrite-?template (template map)
  (let ((sequence-numbers '()))
    (labels
        ((designates-variable? (x)
           (and x (symbolp x) (> (length (string x)) 1) (char= #\? (aref (string x) 0))))
         (increment-sym? (x) (char= #\+ (aref (string x) (1- (length (string x))))))
         (continue-sym? (x) (char= #\# (aref (string x) (1- (length (string x))))))
         (symbol-parts (x)
           (cond
            ((and x (symbolp x))
             (let ((name (string x))
                   (i -1)
                   prefix
                   label
                   sequence-no)
               (setf prefix (if (char= #\? (aref name 0)) '? nil))
               (if (>= (length name) 3)
                   (loop for j from 1
                         for k = (- (length name) j)
                         while (and (<= j (length name))
                                    (parse-integer (subseq name k (1+ k)) :junk-allowed T)) do 
                           (setf i k)))
               (setf sequence-no
                     (if (= i -1)
                         (if (or (increment-sym? x) (continue-sym? x))
                             (intern (subseq name (1- (length name)) (length name)))
                           nil)
                       (multiple-value-bind (n r) (parse-integer (subseq name i (length name))) n)))
               (setf label (intern (subseq name (if prefix 1 0) (if (= i -1) (if sequence-no (1- (length name)) (length name)) i))))
               (values prefix label sequence-no))))))

      (map-func #'(lambda (x) 
                    (if (designates-variable? x)
                        (cl:multiple-value-bind (prefix label sequence-no) (symbol-parts x)
                          (if (and sequence-no (not (increment-sym? x)) (not (continue-sym? x)))
                              (cond 
                               ((and (assoc label sequence-numbers)
                                     (> sequence-no (cdr (assoc label sequence-numbers))))
                                (rplacd (assoc label sequence-numbers) sequence-no))
                               ((not (assoc label sequence-numbers))
                                (push (cons label sequence-no) sequence-numbers))
                               (T T))))))
                template)

      (map-func #'(lambda (x) 
                    (if (designates-variable? x)
                        (cl:multiple-value-bind (prefix label sequence-no) (symbol-parts x)
                          (if (not (assoc label sequence-numbers))
                              (push (cons label 0) sequence-numbers)))))
                template)
      
      (map-func #'(lambda (x)
                    (cond 
                     ((designates-variable? x)
                        (cl:multiple-value-bind (prefix label sequence-no) (symbol-parts x)
                          (cond
                          ((continue-sym? x)
                           (intern (format nil "?~A~A" (string label) (cdr (assoc label sequence-numbers))) (symbol-package x)))
                          ((increment-sym? x)
                           (rplacd (assoc label sequence-numbers) (1+ (cdr (assoc label sequence-numbers))))
                           (intern (format nil "?~A~A" (string label) (cdr (assoc label sequence-numbers))) (symbol-package x)))
                          ((and map (assoc x map))
                           (cdr (assoc x map)))
                          (T x))))
                     (T x)))
                template))))

(defun ?template (template &optional map)
  (multiple-value-bind (xs vars) 
      (if map 
          (screamer::template-internal (rewrite-?template template map) map)
        (screamer:template (rewrite-?template template map)))
    (values xs vars)))

;;;;;;;;;;;;;;;;;;;;;;;


(defmacro every!! (x &rest ys)
  (let ((args (loop for i from 0 while (<= i (length ys)) collect
                      (cond
                       ((= i 0) x)
                       (T (nth (1- i) ys))))))
    (cond
     ((null x) nil)
     ((= 1 (length args))
      `(assert! ,@args))
     (T
      (let ((input (elt ys (1- (length ys))))
            (variables (loop for i from 0 while (< i (1- (length args))) collect (nth i args))))
        `(every!!-internal ,input ,@variables))))))

(defmacro every!!-internal (x &rest ys)
  `(progn
     ,@(mapcar #'(lambda (y) 
                     `(assert! ,y)) ys)
     ,x))

(defmacro assert!! (x &rest ys)
  (if *nondeterministic-context?*
      `(assert!!-nondeterministic-functionmode ,x ,@ys)
  `(assert!!-funcall-mode ,x ,@ys)))

(defun assert!!-funcall-mode (x &rest fns)
  (cond
   ((null fns) x)
   ((not (functionp (car fns)))
    (apply #'assert!!-funcall-mode x (cdr fns)))
   (T
    (assert! (funcall (car fns) x))
    (apply #'assert!!-funcall-mode x (cdr fns)))))

(defun assert!!-nondeterministic-functionmode (x &rest fns)
  (cond
   ((null fns) x)
   ((and (not (nondeterministic-function? (car fns)))
         (not (functionp (car fns))))
    (apply-nondeterministic #'assert!!-nondeterministic-functionmode x (cdr fns)))
   (T
    (assert! (funcall-nondeterministic (car fns) x))
    (apply-nondeterministic #'assert!!-nondeterministic-functionmode x (cdr fns)))))

(defmacro apply?cont (x y &rest ys)
  (let ((args (loop for i from 0 while (<= i (1+ (length ys))) collect
                      (cond
                       ((= i 0) x)
                       ((= i 1) y)
                       (T (nth (- i 2) ys))))))
    (let ((input x)
          (functions (loop for i from 0 while (< i (1- (length args))) collect (nth (1+ i) args))))
  (if *nondeterministic-context?*
      `(assert!!-nondeterministic-functionmode ,input ,@functions)
  `(assert!!-funcall-mode ,input ,@functions)))))

;; ?solution
(defun ?solution (x &key force-fun cost-fun terminate? order skip cut-after onmatch collect-to abort-after)

  (cond
   ((every #'null (list force-fun cost-fun terminate? order skip cut-after onmatch collect-to abort-after))
    (solution x (static-ordering #'linear-force)))

   ((null skip) (?solution x :onmatch onmatch :skip 0 :cut-after cut-after
                           :abort-after abort-after :collect-to collect-to :force-fun force-fun 
                           :cost-fun cost-fun :terminate? terminate? :order order))

   ((or (null cut-after)
        (= 0 cut-after)) (?solution x :onmatch onmatch :skip skip :cut-after (1- (- *infinity* skip))
                                    :abort-after abort-after :collect-to collect-to :force-fun force-fun 
                                    :cost-fun cost-fun :terminate? terminate? :order order))

   (T
    (let ((default-cost-fun (let ((variables (remove-duplicates (variables-in (value-of x)) :from-end T)))
                              #'(lambda (y) (or (position y variables) 100))))
          (default-terminate-test #'(lambda (y) (declare (ignore y)) nil))
          (force-fun 
           (cond
            ((null force-fun) #'linear-force)
            ((functionp force-fun) force-fun)     
            ((nondeterministic-function? force-fun) force-fun)
            ((find (symbol-name force-fun) '("LINEAR-FORCE" "LF" ) :test #'string=) #'linear-force)
            ((find (symbol-name force-fun) '("DIVIDE-AND-CONQUER-FORCE" "DACF") :test #'string=) #'divide-and-conquer-force)
            ((find (symbol-name force-fun) '("RANDOM-FORCE" "RANDOM") :test #'string=) #'random-force)
            (T #'linear-force)))
          (match-count 0)
          (cycle-count 0)
          (start-timestamp (get-universal-time)))
        (let (abort-timestamp abort?)
          (let ((terminate? 
                 (cond
                  (abort-after
                   (global (setf abort-timestamp (+ start-timestamp (round (* abort-after 60 1)))))
                   (cond 
                    (terminate?
                     #'(lambda (y)
                         (global 
                          (setf cycle-count (1+ cycle-count))
                          (or (setf abort? (> (get-universal-time) abort-timestamp))
                              (funcall terminate? y)))))
                    (T #'(lambda (y) 
                           (global 
                            (setf cycle-count (1+ cycle-count))
                            (setf abort? (> (get-universal-time) abort-timestamp)))))))
                  (T (or terminate? 
                         #'(lambda (y) 
                             (declare (ignore y)) 
                             (global (setf cycle-count (1+ cycle-count)))
                             nil)))))

                (cost-fun
                 (cond 
                  ((null cost-fun) default-cost-fun)
                  ((functionp cost-fun) cost-fun)
                  ((find (symbol-name cost-fun) '("DOMAIN-SIZE") :test #'string=) #'domain-size)
                  ((find (symbol-name cost-fun) '("RANGE-SIZE") :test #'string=) #'range-size)
                  (T default-cost-fun)))

                (order
                 (cond
                  ((null order) #'<)
                  ((functionp order) order)
                  ((string= (symbol-name order) "<") #'<)
                  ((string= (symbol-name order) ">") #'>)
                  (T #'<)))

                (onmatch 
                 (cond ((and (find-package :OPENMUSIC) collect-to (typep collect-to (find-symbol "STORE" :OPENMUSIC)))
                        (cond (onmatch #'(lambda (xs)                       
                                           (unless (not (find-package :OPENMUSIC))
                                             (funcall (find-symbol "COLLECT-TO" :OPENMUSIC) collect-to (cdr (assoc :match xs)))
                                             (funcall onmatch (if collect-to (append xs (list (cons :STORE collect-to)) xs))))))
                              (T #'(lambda (xs) 
                                     (unless (not (find-package :OPENMUSIC))
                                       (funcall (find-symbol "COLLECT-TO" :OPENMUSIC) collect-to (cdr (assoc :match xs))))))))
                
                       (collect-to (cond (onmatch #'(lambda (xs) 
                                                      (funcall onmatch (append xs (list (cons :STORE collect-to)) xs))))
                                         (T #'(lambda (xs) T))))

                       (T (if onmatch onmatch #'(lambda (x) nil)))))

                (fail? (gensym "fail"))                
                (last-count -1)
                (count 0))
            (let ((last-match fail?))
              (either 
                (let ((value (solution x (reorder cost-fun terminate? order force-fun))))
                  (when abort? (fail))
                  (global (setf last-match value))
                  (global (setf count (1+ count)))
                  (when (< count skip) (fail))
                  (when (> count (+ skip (1- cut-after))) (fail))
                  (funcall onmatch (list (cons :match-count (incf match-count))
                                         (cons :cycle-count cycle-count)
                                         (cons :timestamp start-timestamp)
                                         (cons :match value)))
                  (global (setf last-count count))
                  value)
                (progn
                  (when (eq last-match fail?)
                    (fail))
                  (when (> count (1- (+ skip (1- cut-after))))
                    (fail))
                  (when (= count last-count)
                    (fail))
                  last-match)))))))))
              

(defvar *echo-stream* (if (find-package :OM-LISP) 
                          (find-symbol "*om-stream*" :om-lisp)
                        *standard-output*))

(defun fecho (message &rest args)
  (apply #'format 
         (append (list *echo-stream* (concatenate 'string message "~%")) args)))

; x &rest xs functions
(defun ?oper-arguments (x list) (flatt (append (list x) list)))

(cl:defun paradigm--format-timestamp (timestamp)
    (multiple-value-bind
        (second minute hour date month year day-of-week dst-p tz)
        (decode-universal-time timestamp)
      (concatenate 'string
                   (format nil "~2,'0d:~2,'0d:~2,'0d ~d/~2,'0d"
                           hour minute second month date))))

;(defun paradigm--save-to-solver-output (value)
;  (global
;    (if (null t2l::*findall2-values*)
;        (setf t2l::*findall-last-value-cons* (list value)
;              t2l::*findall2-values* t2l::*findall-last-value-cons*)
;      (setf (rest t2l::*findall-last-value-cons*) (list value)
;
;            t2l::*findall-last-value-cons* (rest t2l::*findall-last-value-cons*)))))
;  T)

;(defun paradigm--clear-solver-output ()
;  (global
;    (setf t2l::*findall2-values* '())
;    (setf t2l::*findall-last-value-cons* nil)))
;  T)




;; from screamer docs:
(defun a-partition-of (x)
  (if (null x)
      x
    (let ((y (a-partition-of (rest x))))
      (either
        (cons (list (first x)) y)
        (let ((z (a-member-of y)))
          (cons (cons (first x) z) (remove z y :test #'eq :count 1)))))))

(defun an-ordered-partition-of (x)
  (cond ((null x) nil)
        ((null (cdr x)) (list x))
        (t 
         (let ((y (an-ordered-partition-of (rest x))))
           (either
             (cons (list (first x)) y)
             (cons (cons (first x) (first y)) (rest y)))))))

(defun n-partitions-of-idx-list-internal2 (s cs)  
  (cond
   ((and (> s 0) (null cs)) (fail))
   ((null cs) nil)
   ((< s 0) (fail))
   ((< s (apply #'+ (mapcar #'car cs))) (fail))
   ((and (every #'cdr cs)
         (> s (apply #'+ (mapcar #'cdr cs)))) (fail))
   (t
    (let ((c (car cs)))
      (let ((min (car c))
            (max (cond
                  ((cdr c) (min (cdr c) (- s (apply #'+ (mapcar #'car (cdr cs))))))
                  (t (- s (apply #'+ (mapcar #'car (cdr cs))))))))
        (let ((a (an-integer-between min max)))
          (append (list a)
                  (n-partitions-of-idx-list-internal2 (- s a) (cdr cs)))))))))

(defun n-partitions-of2 (cs x)
  (let ((s (n-partitions-of-idx-list-internal2 (length x) cs)))
    (cond
     ((null s) nil)
     (t
      (reverse 
       (maplist #'(lambda (is) 
                    (let ((a (- (length x) (apply #'+ is)))
                          (b (- (length x) (if (cdr is) (apply #'+ (cdr is)) 0))))
                      (subseq x a b)))
                (reverse s)))))))

(defun flat1 (list)
  (cond ((null list) nil)
        ((and (listp list)
              (every #'listp list))
         (apply #'append list))
        ((listp list)
         (flat1 (mapcar #'(lambda (x) (cond ((listp x) x)
                                            (t (list x))))
                        list)))
        (t list)))

(defun list-structure-equal (list1 list2 &key test)
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

(defun list-depth (list)
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

(defun flatt (x)
  (append
   (cond
    ((null x) nil)
    ((not (consp (car x))) (list (car x)))
    (T (flatt (car x))))
   (if (cdr x)
       (flatt (cdr x)))))

(defun group-list-on (test sequence)
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

(defun ?=any (x sequence)
  (cond
   ((null sequence) T)
   (T (orv (=v x (car sequence))
           (?=any x (cdr sequence))))))

(defun mat-trans (matrix)
  (when matrix
    (apply #'mapcar #'list matrix)))
   
;;; (imported from screamer docs) prolog-union prolog-intersect prolog-difference prolog-delete prolog+v3 prolog/v3 prolog*v3 prolog-v3 setof
(defun prolog-union (y z w)
  (either 
    (let ((a (make-variable))
          (y1 (make-variable)))
      (assert! (equalv y (cons a y1)))
      (either 
        (progn 
          (assert! (memberv a z))
          (prolog-union y1 z w))
        (progn 
          (assert! (notv (memberv a z)))
          (let ((w1 (make-variable)))
            (assert! (equalv w (cons a w1)))
            (prolog-union y1 z w1)))))
    (progn 
      (assert! (equalv y nil))
      (assert! (equalv z w)))))

(defun prolog-intersect (y m z)
  (either
    (progn 
      (assert! (equalv y nil))
      (assert! (equalv z nil)))
    (let ((a (make-variable))
          (y1 (make-variable))
          (m1 (make-variable))
          (z1 (make-variable)))
      (assert! (equalv y (cons a y1)))
      (assert! (equalv m m1))
      (either 
        (progn 
          (assert! (memberv a m))
          (let ((z1 (make-variable)))
            (assert! (equalv z (cons a z1)))
            (prolog-intersect y1 m1 z1)))
        (progn
          (assert! (notv (memberv a m)))
          (assert! (equalv z z1))
          (prolog-intersect y1 m1 z1))))))

(defun prolog-difference (y m z)
  (either
    (progn 
      (assert! (equalv y nil))
      (assert! (equalv z nil)))
    (let ((a (make-variable))
          (y1 (make-variable))
          (m1 (make-variable))
          (z1 (make-variable)))
      (assert! (equalv y (cons a y1)))
      (assert! (equalv m m1))
      (either 
        (progn 
          (assert! (memberv a m))
          (assert! (equalv z z1))
          (prolog-difference y1 m1 z1))
        (progn
          (assert! (equalv z (cons a z1)))
          (prolog-difference y1 m1 z1))))))

(defun prolog-delete (a x z) ; iffy
  (print (list 'prolog-delete 'a a 'x x 'z z))
  (either
    (progn 
      (assert! (equalv x (cons a z))))
    (let ((a1 (make-variable))
          (b (make-variable))
          (x1 (make-variable))
          (z1 (make-variable)))
      (assert! (equalv a a1))
      (assert! (equalv x (cons b x1)))
      (assert! (equalv z (cons b z1)))
      (prolog-delete a1 x1 z1))))

(defun setof (l)
  (let ((c)
        (r (make-variable))
        (l1 (make-variable)))
    (prolog-maplist3 (lambda (x)
                       (cond ((memberv x c) x)
                             (t 
                              (push x c)
                              x)))
                       l
                       l1)
    (prolog-reverse c r)
    r))

(defun prolog+v3 (y z value)
  (either 
   (progn 
     (assert! (equalv nil y))
     (assert! (equalv z nil)))
   (progn 
     (assert! (numberpv y))
     (assert! (equalv z (+v y value))))
   (progn 
     (let ((y1 (make-variable))
	   (y2 (make-variable)))
       (assert! (equalv y (cons y1 y2)))
       (let ((z1 (make-variable))
	     (z2 (make-variable)))
         (assert! (equalv z (cons z1 z2)))
	 (prolog+v3 y1 z1 value)
	 (prolog+v3 y2 z2 value))))
   (progn 
     (assert! (notv (numberpv y)))
     (assert! (equalv z 0)))))

(defun prolog-v3 (y z value)
  (either 
   (progn 
     (assert! (equalv nil y))
     (assert! (equalv z nil)))
   (progn 
     (assert! (numberpv y))
     (assert! (equalv z (-v y value))))
   (progn 
     (let ((y1 (make-variable))
	   (y2 (make-variable)))
       (assert! (equalv y (cons y1 y2)))
       (let ((z1 (make-variable))
	     (z2 (make-variable)))
	 (prolog-v3 y1 z1 value)
	 (prolog-v3 y2 z2 value)
	 (assert! (equalv z (cons z1 z2))))))
   (progn 
     (assert! (notv (numberpv y)))
     (assert! (equalv z 0)))))


(defun prolog*v3 (y z value)
  (either 
   (progn 
     (assert! (equalv nil y))
     (assert! (equalv z nil)))
   (progn 
     (assert! (numberpv y))
     (assert! (equalv z (*v y value))))
   (progn 
     (let ((y1 (make-variable))
	   (y2 (make-variable)))
       (assert! (equalv y (cons y1 y2)))
       (let ((z1 (make-variable))
	     (z2 (make-variable)))
	 (prolog*v3 y1 z1 value)
	 (prolog*v3 y2 z2 value)
	 (assert! (equalv z (cons z1 z2))))))
   (progn 
     (assert! (notv (numberpv y)))
     (assert! (equalv z 0)))))

(defun prolog/v3 (y z value)
  (assert! (notv (=v value 0)))
  (either 
   (progn 
     (assert! (equalv nil y))
     (assert! (equalv z 0)))
   (progn 
     (assert! (numberpv y))
     (assert! (equalv z (/v y value))))
   (progn 
     (let ((y1 (make-variable))
	   (y2 (make-variable)))
       (assert! (equalv y (cons y1 y2)))
       (let ((z1 (make-variable))
	     (z2 (make-variable)))
	 (prolog/v3 y1 z1 value)
	 (prolog/v3 y2 z2 value)
	 (assert! (equalv z (cons z1 z2))))))
   (progn 
     (assert! (notv (numberpv y)))
     (assert! (equalv z 0)))))


;  append([X|Y],Z,[X|W]) :- append(Y,Z,W).
;  append([],X,X).
(defun prolog-append (x y z)
  (either 
    (progn 
      (assert! (equalv x nil))
      (assert! (equalv y z)))
    (let ((x1 (make-variable))
          ;(y1 (make-variable))
          (z1 (make-variable))
          (a (make-variable)))
      (assert! (equalv x (cons a x1)))
      ;(assert! (equalv y y1))
      (assert! (equalv z (cons a z1)))
      (prolog-append x1 y z1))))

(defun prolog-reverse (x y)
  (prolog-reverse-append x nil y))

;  reverse([X|Y],Z,W) :- reverse(Y,[X|Z],W).
;  reverse([],X,X).
(defun prolog-reverse-append (x y z)
  (either 
    (progn 
      (assert! (equalv x nil))
      (assert! (equalv y z)))
    (let ((x1 (make-variable))
          (y1 (make-variable))
          ;(z1 (make-variable))
          (a (make-variable)))
      (assert! (equalv x (cons a x1)))
      (assert! (equalv y1 (cons a y)))
      ;(assert! (equalv z z1))
      (prolog-reverse-append x1 y1 z))))

(defun prolog-flatten (l f)
  (let ((f1 (make-variable)))
    (prolog-flatten3 l nil f1)))

(defun prolog-flatten3 (i l f)
  (either 
    (progn 
      (assert! (equalv nil i))
      (assert! (equalv l f)))
    (let ((i1 (make-variable))
          (i2 (make-variable))
          (f1 (make-variable))
          (l1 (make-variable)))
      (assert! (equalv i (cons i1 i2)))
      (assert! (equalv f f1))
      (prolog-flatten3 i1 l1 f1)
      (prolog-flatten3 i2 l l1))
    (progn 
      (assert! (equalv f (cons i l))))))

(defun prolog-subseq (sequence f l subseq &optional prcs-excluded)
  (assert! (integerpv f))
  (assert! (integerpv l))
  (let ((seqc-length (lengthv sequence)))
    (assert! (>=v f 0))
    (assert! (<=v f l))
    (assert! (<=v l seqc-length))
    (assert! (>=v (-v l f) 0))
    (assert! (<=v l (lengthv sequence)))
    (either 
      (progn 
        (assert! (notv (equalv nil subseq)))
        (assert! (<v f l))
        (let ((lf (-v l f))
              (r (-v seqc-length l)))    
          (assert! (>=v lf 0))  
          (assert! (>=v r 0))
          (let ((a (funcallv #'make-var-sequence f))
                (b (funcallv #'make-var-sequence lf))
                (c (funcallv #'make-var-sequence r))
                (ab (make-variable)))
            (prolog-append a b ab)
            (prolog-append ab c sequence)
            (either 
              (progn 
                (assert! (not (null prcs-excluded)))
                (prolog-append a c subseq))
              (progn 
                (assert! (equalv subseq b)))))))
      (progn 
        (assert! (=v f l))
        (equalv subseq nil)))))

;;	list_to_set(+List, ?Set) is det.
;
;	True when Set has the same elements  as  List in the same order.
; The left-most copy of the duplicate  is retained. The complexity
;	of this operation is |List|^2.
;
;	@see sort/2.
(defun prolog-remove-duplicates (list set)
  (let ((set0 (make-variable)))
    (prolog-remove-duplicates-internal list set0)
    (assert! (equalv set set0))))

(defun prolog-remove-duplicates-internal (list set)
  (either
    (progn
      (assert! (equalv list nil))
      (prolog-close-list set))
    (let ((a (make-variable))
          (b (make-variable))
          (c (a-booleanv)))
      (assert! (equalv list (cons a b)))
      (assert! (equalv c (one-value (memberv a set) nil)))
      (either
       (progn 
         (assert! (equalv c t))
         (prolog-remove-duplicates-internal b set))
       (let ((set1 (make-variable)))     
         (prolog-insert a set set)
         (prolog-remove-duplicates-internal b set))))))

(defun prolog-close-list (list)
  (one-value
   (assert! (equalv list nil))
    (let ((a (make-variable))
          (b (make-variable)))
      (assert! (equalv list (cons a b)))
      (prolog-close-list b))))


(defun prolog-subset (x y)
  (either 
    (progn 
      (assert! (equalv nil x))
      (assert! (equalv nil y)))
    (progn 
      (let ((x1 (make-variable))
            (x2 (make-variable))
            (y1 (make-variable))
            (y2 (make-variable)))
        (assert! (equalv x (cons x1 x2)))
        (assert! (equalv x1 y1))
        (prolog-subset x2 y2)
        (either 
          (assert! (equalv y (cons y1 y2)))
          (assert! (equalv y y2)))))))

;  subset([X|R],S) :- member(X,S), subset(R,S).
;  subset([],_).
(defun prolog-subset? (x s)
  (either
    (let ((a (make-variable))
          (x1 (make-variable))
          (s1 (make-variable)))
      (assert! (equalv x (cons a x1)))
      (assert! (memberv a s))
      (prolog-subset x1 s))
    (progn 
      (assert! (equalv nil x))
      t)))

;(defun prolog-rotate-list (x y n) ; iffy
;  (let ((l (make-variable))
;        (r (make-variable))
;        (len (lengthv x)))
;    (prolog-subseq x 0 n l)
;    (prolog-subseq x n len r)
;    (prolog-append l r x)
;    (prolog-append r l y)))

(defun prolog-rotate (l r)
  (let ((a (make-variable))
        (b (make-variable)))
    (assert! (notv (equalv nil b)))
    (prolog-append b a l)
    (prolog-append a b r)))

; rotate(List, [H|R]):- rotate(List, R, H).
; rotate([H], [], H).
; rotate([H|T], [H|T1], R) :- rotate(T, T1, R). 
(defun prolog-rotate-list (l r)
  (let ((h (make-variable))
        (r1 (make-variable)))
    (assert! (equalv r (cons h r1)))
    (prolog-rotate-list3 l r1 h)))

(defun prolog-rotate-list3 (l r h)
  (either 
    (progn 
      (assert! (equalv r nil))
      (assert! (equalv l (cons h nil))))
    (let ((a  (make-variable))
          (l1 (make-variable))
          (r1 (make-variable)))
      (assert! (equalv l (cons a l1)))
      (assert! (equalv r (cons a r1)))
      (prolog-rotate-list3 l1 r1 h))))

(defun prolog-member? (e x s)
  (either
    (progn 
      (assert! (equalv nil x))
      (assert! (equalv s (equalv nil e))))
    (let ((x1 (make-variable))
          (x2 (make-variable)))
      (assert! (equalv x (cons x1 x2)))
      (one-value 
        (progn
          (assert! (equalv e x1))
          (assert! (equalv s t)))
        (prolog-member? e x2 s)))))

;  takeout(X,[X|R],R).
;  takeout(X,[F|R],[F|S]) :- takeout(X,R,S).
(defun prolog-takeout (x y z)
  (either 
    (progn 
      (assert! (equalv y (cons x z))))
    (let (;(x1 (make-variable))
          (y1 (make-variable))
          (z1 (make-variable))
          (a (make-variable)))
      ;(assert! (equalv x x1))
      (assert! (equalv y (cons a y1)))
      (assert! (equalv z (cons a z1)))
      (prolog-takeout x y1 z1))))

;  perm([X|Y],Z) :- perm(Y,W), takeout(X,Z,W).   
;  perm([],[]).
(defun prolog-perm (y z)
  (either 
    (progn 
      (assert! (equalv y nil))
      (assert! (equalv z nil)))
    (let ((w (make-variable))
          (x (make-variable))
          (y1 (make-variable)))
          ;(z1 (make-variable)))
      (assert! (equalv y (cons x y1)))
      ;(assert! (equalv z z1))
      (prolog-perm y1 w)
      (prolog-takeout x z w))))

(defun prolog-nperm (n y z &optional debug)
  (assert! (orv (equalv t debug)
                (<=v n (funcallv #'n! (lengthv (funcallv #'remove-duplicates (apply-substitution y)))))))
  (let ((tmp (funcallv #'make-sequence 'list (value-of n)))
        (a (make-variable)))
    (prolog-maplist3 (lambda (x)
                       (make-variable))
                     tmp
                     a)
    (prolog-maplist3 (lambda (x)
                       (progn
                         (prolog-perm y x)
                         x))
                     a
                     z)
    (prolog-all-distinctv? z t)))

(defun prolog-nperms-an-iteration-idx-list (n y)
  (let* ((s (prime-facts 0 (1- n) 1))
         (o (a-member-of (prime-facts 2 (1- n) 1)))
         (s1 (mapcar #'(lambda (x) 
                         (mod x n))
                     (prime-facts 0 (* n o) o))))
   ; (if (and (> (length s) 5) (equalp s s1)) (fail))
    (unless (or (< (length s) 5)
              (null (set-difference s s1))) (fail))
    s1))

(defun prolog-nperms (n y z &optional debug fail-form)
  (prolog-nperms4 n y z (an-integer-between 0 512) debug fail-form))

(defun prolog-nperms4 (n y z offset &optional debug fail-form)
  (assert! (orv (notv (equalv nil debug))
                (<=v n (funcallv #'n! (minv 10 (lengthv y))))))
  (let ((tmp (prolog-nperms-an-iteration-idx-list n y))
        (tmp1 (make-variable)))
    ;(print (list 'tmp tmp))
    (prolog+v3 tmp tmp1 offset)
    (prolog-maplist3 (lambda (x)
                       (ith-value x 
                                  (let ((a (make-variable)))
                                    (prolog-perm y a)
                                    (solution a (static-ordering #'divide-and-conquer-force)))
                                  fail-form))
                     tmp1
                     z)))

(defun prolog-insert (elem list1 list2)
  (either
    (progn
      (assert! (equalv list1 nil))
      (assert! (equalv list2 (cons elem nil))))
    (let ((h (make-variable))
          (l (make-variable))
          (r (make-variable)))
      (assert! (equalv list1 (cons h l)))
      (assert! (equalv list2 (cons h r)))
      (prolog-insert elem l r))))
                         
(defun prolog-select (elem list1 list2)
  (either
    (assert! (equalv list1 (cons elem list2)))
    (let ((h (make-variable))
          (l (make-variable))
          (r (make-variable)))
      (assert! (equalv list1 (cons h l)))
      (assert! (equalv list2 (cons h r)))
      (prolog-select elem l r))))

(defun prolog-perm2 (l hp)
  (either 
    (progn 
      (assert! (equalv l nil))
      (assert! (equalv hp nil)))
    (let ((h (make-variable))
          (p (make-variable))
          (nl (make-variable)))
      (assert! (equalv hp (cons h p)))
      (prolog-select h l nl)
      (prolog-perm3 nl h p))))

(defun prolog-perm3 (l h ip)
  (either 
    (progn
      (assert! (equalv l nil))
      (assert! (equalv ip nil)))
    (let ((i (make-variable))
          (p (make-variable))
          (nl (make-variable)))
      (assert! (equalv ip (cons i p)))
      (prolog-select i l nl)
      (prolog-perm3 nl i p))))                         

(defun all-distinctv? (x)
  (let ((var (make-variable)))
    (prolog-all-distinctv? x var)
    var))

(defun necessarily?equalv (x y) (necessarily? (solution (equalv x y) (static-ordering #'linear-force))))

(defun possibly?equalv (x y) (possibly? (solution (equalv x y) (static-ordering #'linear-force))))

(defun prolog-all-distinctv? (x s)
  (either 
    (progn 
      (assert! (funcallv #'atom x))
      (assert! (equalv s t)))    
    (prolog-all-distinctv?3 nil nil x s)))
  

(defun prolog-all-distinctv?3 (x y z s)  
  (either 
    (progn 
      (assert! (equalv y nil))
      (assert! (equalv z nil))
      (assert! (equalv s t))) 
    (progn 
      (assert! (equalv z nil))
      (assert! (equalv s (notv (memberv y x)))))
    (let ((x1 (make-variable))
          (y1 (make-variable))
          (z1 (make-variable))
          (s1 (make-variable)))
      (assert! (equalv z (cons y1 z1)))
      (assert! (equalv x1 (cons y x)))
      (prolog-all-distinctv?3 x1 y1 z1 s1)
      (assert! (equalv s (andv s1
                               (orv (equalv nil y)
                                    (andv (notv (memberv y x)) 
                                          (notv (memberv y z))))))))))

(defun prolog-maplist (fn list &rest args)
  (prolog-maplist-internal fn list args))

(defun prolog-maplist-internal (fn list args)
  (either 
    (progn 
      (assert! (equalv nil args))
      (assert! (equalv nil list)))
    (let ((a (make-variable))
          (b (make-variable))
          (c (make-variable))
          (list1 (make-variable)))
      (prolog-lists-firsts-rests args a b)
      (assert! (equalv c (funcall-nondeterministic fn a)))
      (prolog-append list1 c list)
      (prolog-maplist-internal fn list1 b))))

;(defun prolog-mapn (fn lists)
;  (either 
;    (let ((f (make-variable))
;          (r (make-variable)))
;      (prolog-lists-firsts-rests lists f r)
;      (funcall-nondeterministic fn f)
;      (prolog-mapn fn r))
;    nil))

; transpose(Ms, Ts) :- 
; must_be(list(list), Ms), 
;  ( Ms = [] -> Ts = [] 
;  ; Ms = [F|_], 
;    transpose(F, Ms, Ts)
;  ).

(defun prolog-mapn (fn ms)
  (either 
    (progn 
      (assert! (equalv ms nil))
      (assert! (equalv ts nil)))
    (let ((f (make-variable))
          (m1 (make-variable))
          (ts (make-variable)))
      (assert! (equalv ms (cons f m1)))
      (prolog-transpose ts ms)
      (prolog-mapnx fn f ms ts))))

;  transpose([], _, []). 
;  transpose([_|Rs], Ms, [Ts|Tss]) 
;    :-  lists_firsts_rests(Ms, Ts, Ms1), transpose(Rs, Ms1, Tss).
(defun prolog-mapnx (fn rs ms ts)
  (either 
    (progn 
      (assert! (equalv nil rs))
      (assert! (equalv nil ts)))
    (let ((r1 (make-variable))
          (r (make-variable))
          (m1 (make-variable))
          (t1 (make-variable))
          (tss (make-variable)))
      (assert! (equalv rs (cons r r1)))
      (assert! (equalv ts (cons t1 tss)))
      (prolog-lists-firsts-rests ms t1 m1)
      (funcall-nondeterministic fn t1)
      (prolog-mapnx fn r1 m1 tss))))

(defun prolog-map (fn list)
  (either 
    (assert! (equalv list nil))
    (let ((a (make-variable))
          (b (make-variable)))
      (assert! (equalv list (cons a b)))
      (funcall-nondeterministic fn a)
      (prolog-map fn b))))

(defun prolog-map2 (fn list1 list2)
  (either 
    (progn 
      (assert! (equalv list1 nil))
      (assert! (equalv list2 nil)))
    (let ((a (make-variable))
          (b (make-variable))
          (c (make-variable))
          (d (make-variable)))
      (assert! (equalv list1 (cons a b)))
      (assert! (equalv list2 (cons c d)))
      (funcall-nondeterministic fn a c)
      (prolog-map2 fn b d))))

(defun prolog-map3 (fn list1 list2 list3)
  (either 
    (progn 
      (assert! (equalv list1 nil))
      (assert! (equalv list2 nil))
      (assert! (equalv list3 nil)))
    (let ((a (make-variable))
          (b (make-variable))
          (c (make-variable))
          (d (make-variable))
          (e (make-variable))
          (f (make-variable)))
      (assert! (equalv list1 (cons a b)))
      (assert! (equalv list2 (cons c d)))
      (assert! (equalv list2 (cons e f)))
      (funcall-nondeterministic fn a c e)
      (prolog-map3 fn b d f))))

;  maplist(_C, [], []).
;  maplist( C, [X|Xs], [Y|Ys]) :-
;     call(C, X, Y),
;     maplist( C, Xs, Ys).
(defun prolog-maplist3 (fn x y)
  (either 
    (progn 
      (assert! (equalv nil x))
      (assert! (equalv nil y)))
    (let ((a (make-variable))
          (b (make-variable))
          (xs (make-variable))
          (ys (make-variable)))
      (assert! (equalv x (cons a xs)))
      (assert! (equalv y (cons b ys)))
      (assert! (equalv b (funcall-nondeterministic fn a)))
      (prolog-maplist3 fn xs ys))))

; heads_and_tails(0, [], [], []).
; heads_and_tails(N, [[H|T]|L1], [H|L2], [T|L3]) :-
; 	N2 is N - 1,
;	heads_and_tails(N2, L1, L2, L3).
(defun prolog-heads-and-tails (n l1 l2 l3)
  (either
    (progn 
      (assert! (equalv n 0))
      (assert! (equalv l1 nil))
      (assert! (equalv l2 nil))
      (assert! (equalv l3 nil)))
    (let ((n1 (-v n 1))
          (l1t (make-variable))
          (l2t (make-variable))
          (l3t (make-variable))
          (head (make-variable))
          (tail (make-variable)))
      (assert! (equalv l1 (cons (cons head tail) l1t)))
      (assert! (equalv l2 (cons head l2t)))
      (assert! (equalv l3 (cons tail l3t)))
      (prolog-heads-and-tails n1 l1t l2t l3t))))

(defun prolog-nth (n hr h r)
  (either
    (progn
      (assert! (=v n 0))
      (assert! (equalv hr (cons h r))))
    (let ((n1 (-v n 1))
          (a  (make-variable))
          (r0 (make-variable))
          (r1 (make-variable)))
      (assert! (equalv hr (cons a r0)))
      (assert! (equalv r  (cons a r1)))
      (prolog-nth n1 r0 h r1))))

;  maplist(_C, [], []).
;  maplist( C, [X|Xs], [Y|Ys]) :-
;     call(C, X, Y),
;     maplist( C, Xs, Ys).
(defun prolog-maplist3a (fn x y)
  (either 
    (progn 
      (assert! (equalv nil x))
      (assert! (equalv nil y)))
    (let ((a (make-variable))
          (b (make-variable))
          (xs (make-variable))
          (ys (make-variable)))
      (assert! (equalv x (cons a xs)))
      (assert! (equalv y (cons b ys)))
      (assert! (equalv b (funcallv fn a)))
      (prolog-maplist3a fn xs ys))))


(defun prolog-first (a b)  
  (let ((a1 (make-variable))
        (a2 (make-variable)))
    (one-value 
     (progn
       (assert! (notv (equalv a (cons a1 a2))))
       (assert! (equalv a b)))
     (progn 
       (assert! (equalv a (cons a1 a2)))
       (prolog-first a1 b)))))

(defun prolog-last (a b)
  (let* ((c (make-variable))
         (d (make-variable))
         (e (make-variable)))
    (prolog-reverse a e)
    (assert! (equalv e (cons c d)))
    (either 
      (progn
        (assert! (funcallv #'consp c))
        (prolog-last c b))
      (progn 
        (assert! (funcallv #'atom c))
        (assert! (equalv b c))))))

;  lists_firsts_rests([], [], []). 
;  lists_firsts_rests([[F|Os]|Rest], [F|Fs], [Os|Oss]) 
;    :- lists_firsts_rests(Rest, Fs, Oss).
(defun prolog-lists-firsts-rests (r f o)
  (either 
    (progn 
      (assert! (equalv r nil))
      (assert! (equalv f nil))
      (assert! (equalv o nil)))
    (let ((a (make-variable))
          (b (make-variable))
          (r1 (make-variable))
          (f1 (make-variable))
          (o1 (make-variable)))
      (assert! (equalv f (cons a f1)))
      (assert! (equalv o (cons b o1)))
      (assert! (equalv r (cons (cons a b) r1)))
      (prolog-lists-firsts-rests r1 f1 o1))))

; transpose(Ms, Ts) :- 
; must_be(list(list), Ms), 
;  ( Ms = [] -> Ts = [] 
;  ; Ms = [F|_], 
;    transpose(F, Ms, Ts)
;  ).
(defun prolog-transpose (ms ts)
  (either 
    (progn 
      (assert! (equalv ms nil))
      (assert! (equalv ts nil)))
    (let ((f (make-variable))
          (m1 (make-variable)))
      (assert! (equalv ms (cons f m1)))
      (prolog-transpose3 f ms ts))))
    
;  transpose([], _, []). 
;  transpose([_|Rs], Ms, [Ts|Tss]) 
;    :-  lists_firsts_rests(Ms, Ts, Ms1), transpose(Rs, Ms1, Tss).
(defun prolog-transpose3 (rs ms ts)
  (either 
    (progn 
      (assert! (equalv nil rs))
      (assert! (equalv nil ts)))
    (let ((r (make-variable))
          (r1 (make-variable))
          (m1 (make-variable))
          (t1 (make-variable))
          (tss (make-variable)))
      (assert! (equalv rs (cons r r1)))
      (assert! (equalv ts (cons t1 tss)))
      (prolog-lists-firsts-rests ms t1 m1)
      (prolog-transpose3 r1 m1 tss))))

(defun prolog-flatten-once (l f)
  (let ((f1 (make-variable)))
    (prolog-maplist3 (lambda (x)
                       (cond ((funcallv #'consp x) x)
                             (t (cons x nil))))
                     l
                     f1)
    (prolog-flatten-once-internal f1 f)))

(defun prolog-flatten-once-internal (l f)
  (either 
    (progn 
      (assert! (equalv l nil))
      (assert! (equalv f nil)))
    (let ((l1 (make-variable))
          (l2 (make-variable))
          (f1 (make-variable))
          (f2 (make-variable)))
      (assert! (equalv l (cons l1 l2)))
      (assert! (equalv l1 f1))    
      (prolog-append f1 f2 f)
      (prolog-flatten-once-internal l2 f2))))


(defun prolog-ordered? (m s &key key (order 'asc))
  (either 
    (progn 
      (assert! (equalv m nil))
      (assert! (equalv s nil)))
    (let ((l (make-variable))
          (r (make-variable))
          (l1 (make-variable))
          (r1 (make-variable))
          (m1 (make-variable))
          (s1 (make-variable))
          (s2 (make-variable)))
      (assert! (equalv m (cons l (cons r m1))))
      (cond (key 
             (assert! (equalv l1 (funcallv key l)))
             (assert! (equalv r1 (funcallv key r))))
            (t 
             (assert! (equalv l1 l))
             (assert! (equalv r1 r))))                
      (assert! (realpv l1))
      (assert! (realpv r1))
      (cond ((eq order 'asc) 
             (assert! (equalv s1 (<=v l1 r1))))
            (t 
             (assert! (equalv s1 (>=v l1 r1)))))
      (either 
        (progn 
          (assert! (notv (equalv m1 nil)))
          (prolog-ordered? (cons r m1) s2 :key key :order order))
        (progn 
          (assert! (equalv m1 nil))
          (assert! (equalv s2 t))))
      (assert! (equalv s (andv s1 s2))))))

(defun prolog-quicksort (l s &key key)
  (either 
    (progn 
      (assert! (equalv l nil))
      (assert! (equalv s nil)))
    (let ((a (make-variable))
          (b (make-variable))
          (sl  (make-variable))
          (sl1 (make-variable))
          (bl  (make-variable))
          (bl1 (make-variable))
          (ab1 (make-variable)))
      (assert! (equalv l (cons a b)))
      (assert! (equalv ab1 (cons a bl1)))
      (prolog-split a b sl bl :key key)
      (prolog-quicksort sl sl1 :key key)
      (prolog-quicksort bl bl1 :key key)
      (prolog-concatenate sl1 ab1 s))))


(defun prolog-split (i ht sl bl &key key)
  (either 
    (progn 
      (assert! (equalv ht nil))
      (assert! (equalv sl nil))
      (assert! (equalv bl nil)))
    (let ((h1  (make-variable))
          (t1  (make-variable))
          (sl1 (make-variable)))
      (assert! (equalv ht (cons h1 t1))) 
      (either 
        (let ((sl1 (make-variable)))
          (assert! (equalv sl (cons h1 sl1)))
          (cond (key
                 (assert! (>=v (funcallv key i) 
                               (funcallv key h1))))
                (t
                 (assert! (>=v i h1))))
          (prolog-split i t1 sl1 bl :key key))
        (let ((bl1 (make-variable)))
          (assert! (equalv bl (cons h1 bl1)))
          (cond (key
                 (assert! (<v (funcallv key i) 
                              (funcallv key h1))))
                (t
                 (assert! (<v i h1))))
          (prolog-split i t1 sl bl1 :key key))))))

(defun prolog-concatenate (x y z)
  (either 
    (progn 
      (assert! (equalv x nil))
      (assert! (equalv y z)))
    (let ((a (make-variable))
          (x1 (make-variable))
          ;(y1 (make-variable))
          (z1 (make-variable)))
      (assert! (equalv x (cons a x1)))
      ;(assert! (equalv y y1))
      (assert! (equalv z (cons a z1)))
      (prolog-concatenate x1 y z1))))

(defun a-subset-of (x)
  (if (null x)
      nil
      (let ((y (a-subset-of (rest x)))) 
        (either (cons (first x) y) y))))

(defun a-subset-size= (x size)
  (let ((r (a-subset-of x)))
    (if (or (null r)
            (not (= (length r) size)))
        (fail)
      r)))

(defun a-permutation-of (list)
  (if (null list)
      nil
    (let ((i (an-integer-between 0 (1- (length list)))))
      (append (list (elt list i))
              (a-permutation-of 
               (append (subseq list 0 i)
                       (subseq list (1+ i) (length list))))))))

(cl:defun a-permutation-ofv (list &key symbol-mode)
  (let ((vars (mapcar #'(lambda (x) 
                          (let ((v (an-integerv)))
                            (assert! (memberv v list))
                            v))
                      list))
        (perms (all-values (a-permutation-of list))))
    (assert! (reduce
              #'orv                            
              (mapcar #'(lambda (p) (lists=v p vars))
                      perms)))
    vars))

(defun takeout (x y &key (test #'equalp))
  (cond ((not (position x y :test test)) y)
        (t 
         (let ((p (a-partition-of y)))
           (unless (= (length p) 2) (fail))
           (either 
             (progn 
               (unless (funcall test x (caadr p)) (fail))
               (append (print (car p)) (print (cdadr p))))
             (progn
               (unless (funcall test x (car (reverse (car p)))) (fail))
               (append (reverse (cdr (reverse (car p))))
                       (cadr p))))))))

(defun group-list-nondeterministic (list segmentation mode)
  (cond
   ((null list) nil)
   ((not (some #'(lambda (x) (<= x (length list))) segmentation))
     (fail))
   (T
    (let ((items (a-member-of segmentation)))
      (unless (<= items (length list))
        (fail))
      (cons (subseq list 0 items)
              (group-list-nondeterministic (subseq list items (length list))
                                           segmentation
                                           mode))))))
  


