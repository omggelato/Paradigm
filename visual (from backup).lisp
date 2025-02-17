(in-package :OPENMUSIC)

(defmethod! ?template (template &key min max map (generator :an-integer))
  :initvals '(nil nil nil nil :an-integer)
  :numouts 2
  :menuins '((4 (("make-variable" :make-variable)
                 ("an-integer" :an-integer)
                 ("a-real" :a-real)
                 ("a-number" :a-number))))
  :doc "Copies an aggregate object, replacing any symbol beginning with a question mark with a newly created variable. 

If the same symbol appears more than once in x, only one variable is created for that symbol, the same variable replacing any occurrences of that symbol. Thus (template '(a b (?c d ?e) ?e)) has the same effect as: 
            (LET ((?C (MAKE-VARIABLE))
                  (?E (MAKE-VARIABLE)))
              (LIST 'A 'B (LIST C 'D E) E)).

This is useful for creating patterns to be unified with other structures. "
  (let (variables-from-input)
    (labels
        ((apply-conditions-from-arguments (x)

           (if min
               (s:assert! (s:>=v x min)))
           (if max
               (s:assert! (s:<=v x max)))

           (cond
            ((eq :an-integer generator)
             (s:assert! (s:integerpv x)))
            ((eq :a-real generator)
             (s:assert! (s:realpv x)))
            ((eq :a-number generator)
             (s:assert! (s:numberpv x))))

           x)
         (process-input-sym (x)
           (let ((input (cond
                         ((null x) nil)
                         ((and (symbolp x)
                               (string= "_" (symbol-name x)))
                          (apply-conditions-from-arguments (s:make-variable)))
                         ((numberp x)
                          (make-variable-from-number x))
                         (T x))))
             (if (and (s::variable? input) (not (find input variables-from-input :test 'equal)))
                 (push input variables-from-input))
             input))
         (make-variable-from-number (input)
           (cond
            ((= input (floor input))
             (let ((x (cond
                       ((eq :an-integer generator) (s:an-integerv))
                       (T (s:a-numberv)))))
               (s:assert! (s:=v x input))
               x))
            (T
             (let ((x (cond
                       ((eq :a-real generator) (s:a-realv))
                       (T (s:a-numberv)))))
               (s:assert! (s:=v x input))
               x))))
         (lookup (x)
           (cond
            ((find x (mapcar #'car map))
             (cdr (assoc x map)))
            (T x))))

      (let ((template (cond 
                       ((null template) nil)
                       ((listp template)
                        (map-func #'process-input-sym template))
                       (T (process-input-sym template)))))
        (cond
         (map
          (let ((template (cond ((null template) nil)
                                ((listp template)
                                 (map-func #'lookup template))
                                (t (lookup template)))))
            (multiple-value-bind (template2 map2) (screamer:template template)
              (values template2
                      (append map map2)))))
         (T
          (multiple-value-bind (variables map) (screamer:template (symincr template :reset-counter T))
            (mapcar #'(lambda (x)
                        (unless (find x variables-from-input :test 'equal)
                          (apply-conditions-from-arguments x)))
                    (mapcar #'cdr map))
            (values variables map))))))))

;;; ?TEMPLATE functions
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

; delete
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
;;;; generators
(defmethod! make?variable (&optional name) (if name (screamer:make-variable name) (screamer:make-variable)))
(defmethod! make?variables (list &key min max integers-mode floats-mode symbol-mode) (t2l::make-screamer-variables list :min min :max max :integers-mode integers-mode :floats-mode floats-mode :symbol-mode symbol-mode))
(defmethod! ?a-number (&optional name) (if name (screamer:a-numberv name) (screamer:a-numberv)))
(defmethod! ?a-real (&optional name) (if name (screamer:a-realv name) (screamer:a-realv)))
(defmethod! ?a-real-above (low &optional name) (if name (screamer:a-real-abovev low name) (screamer:a-real-abovev low)))
(defmethod! ?a-real-below (high &optional name) (if name (screamer:a-real-belowv high name) (screamer:a-real-belowv high)))
(defmethod! ?a-real-between (low high &optional name) (if name (screamer:a-real-betweenv low high name) (screamer:a-real-betweenv low high)))
(defmethod! ?an-integer (&optional name) (if name (screamer:an-integerv name) (screamer:an-integerv)))
(defmethod! ?an-integer-above (low &optional name) (if name (screamer:an-integer-abovev low name) (screamer:an-integer-abovev low)))
(defmethod! ?an-integer-below (high &optional name) (if name (screamer:an-integer-belowv high name) (screamer:an-integer-belowv high)))
(defmethod! ?an-integer-between (low high &optional name) (if name (screamer:an-integer-betweenv low high name) (screamer:an-integer-betweenv low high)))
(defmethod! ?an-integer= (x &optional name)
  (let ((var (if name 
                 (screamer:an-integerv name)
               (screamer:an-integerv))))
    (assert!! (?equal var x))
    var))
(defmethod! ?a-real= (x &optional name)
  (let ((var (if name 
                 (screamer:a-realv name)
               (screamer:a-realv))))
    (assert!! (?equal var x))
    var))
(defmethod! ?a-memberof (sequence) (screamer:a-member-ofv sequence))
(defmethod! ?variables-in (input) (remove-duplicates (remove-if-not #'(lambda (y) (screamer::variable? y)) (remove nil (flatt (list input))))))
(defmethod! ?xs-in (input) (remove-duplicates (remove-if-not #'(lambda (y) (or (numberp y) (screamer::variable? y))) (remove nil (flatt (list input))))))

;;;; rules
(defmethod! ?integerp (x) (screamer:integerpv x))
(defmethod! ?realp (x) (screamer:numberpv x))
(defmethod! ?numberp (x) (screamer:numberpv x))
(defmethod! ?booleanp (x) (screamer:booleanpv x))
(defmethod! ?avg (&rest xs) (apply #'t2l::avgv (flatt (list xs))))
(defmethod! ?list-avg (list) (t2l::list-avgv list))
(defmethod! ?and (x &rest ys) (apply #'screamer:andv (flatt (list (append (list x) ys)))))
(defmethod! ?or (x &rest ys) (apply #'screamer:orv (flatt (list (append (list x) ys)))))
(defmethod! ?not (x) (screamer:notv x))
(defmethod! ?abs (x) (t2l::absv x))
(defmethod! ?% (n d) (t2l::om%v n d))
(defmethod! paradigm--modulo-calls-native-function ()
  (setf t2l::*paradigm--modulo-function* t2l::*paradigm--modulo-calls-native-function*))
(defmethod! paradigm--modulo-restricts-bounds ()
  (setf t2l::*paradigm--modulo-function* t2l::*paradigm--modulo-restricts-bounds*))
(defmethod! paradigm--?%-calls-native-function () (paradigm--modulo-calls-native-function))
(defmethod! paradigm--?%-restricts-bounds () (paradigm--modulo-restricts-bounds))
(defmethod! ?+ (&rest xs) (apply #'s:+v xs))
(defmethod! ?- (x &rest xs) (apply #'s:-v (append (list x) xs)))
(defmethod! ?* (&rest xs) (apply #'s:*v xs))
(defmethod! ?/ (&rest xs) (apply #'s:/v xs))
(defmethod! ?1+ (x) (s:+v 1 x))
(defmethod! ?-1 (x) (s:-v x 1))
(defmethod! ?< (x y &rest xs) (apply #'s:<v (append (list x y) xs)))
(defmethod! ?> (x y &rest xs) (apply #'s:>v (append (list x y) xs)))
(defmethod! ?<= (x y &rest xs) (apply #'s:<=v (append (list x y) xs)))
(defmethod! ?>= (x y &rest xs) (apply #'s:>=v (append (list x y) xs)))
(defmethod! ?= (x &rest xs) (apply #'s:=v (append (list x) xs)))
(defmethod! ?/= (x y &rest xs) (apply #'s:/=v (append (list x y) xs)))
(defmethod! ?equal (x y) (s:equalv x y))
(defmethod! ?eql (xs ys) (t2l::omeqlv xs ys))
(defmethod! ?not-eql (xs ys) (t2l::om!eqlv xs ys))
(defmethod! ?between (input min max)
  (cond
   ((null input) T)
   ((consp input)
    (apply 
     #'s:andv
     (mapcar 
      #'(lambda (x) 
          (cond
           ((and (null min)
                 (null max)) T)
           ((and min (null max))
            (s:>=v x min))
           ((and (null min) max)
            (s:<=v x max))
           (T
            (s:andv (s:>=v x min)
                    (s:<=v x max)))))
      (?xs-in input))))
   (T
    (cond
     ((and (null min)
           (null max)) T)
     ((and min (null max))
      (s:>=v input min))
     ((and (null min) max)
      (s:<=v input max))
     (T
      (s:andv 
       (s:>=v input min)
       (s:<=v input max)))))))
(defmethod! ?<> (x from to) (s:orv (s:<v from x to) (s:>v from x to)))
(defmethod! ?<>= (x from to) (s:orv (s:<=v from x to) (s:>=v from x to)))
(defmethod! ?max (&rest xs) (apply #'t2l::ommaxv (flatt (list xs))))
(defmethod! ?min (&rest xs) (apply #'t2l::omminv (flatt (list xs))))

;;;; assert!!
(defmethod! assert!! (x &rest xs)
  :icon 161 
  :doc "SCREAMER:ASSERT!
Restricts X to T. No meaningful result is returned. The argument X can be
either a variable or a non-variable.

This assertion may cause other assertions to be made due to noticers attached
to X.

A call to ASSERT! fails if X is known not to equal T prior to the assertion or
if any of the assertions performed by the noticers result in failure.

Except for the fact that one cannot write #'ASSERT!, ASSERT! behaves like a
function, even though it is implemented as a macro.

The reason it is implemented as a macro is to allow a number of compile time
optimizations. Expressions like \(ASSERT! \(NOTV X)), \(ASSERT! \(NUMBERPV X))
and \(ASSERT! \(NOTV \(NUMBERV X))) are transformed into calls to functions
internal to Screamer which eliminate the need to create the boolean
variable\(s) normally returned by functions like NOTV and NUMBERPV. Calls to
the functions NUMBERPV, REALPV, INTEGERPV, MEMBERV, BOOLEANPV, =V, <V, <=V,
>V, >=V, /=V, NOTV, FUNCALLV, APPLYV and EQUALV which appear directly nested
in a call to ASSERT!, or directly nested in a call to NOTV which is in turn
directly nested in a call to ASSERT!, are similarly transformed.
"
  (apply #'t2l::assert!! (append (list x) xs)))

;; solver functions (to be renamed)
(defmethod! find-any (x &key force-function cost-fun terminate-test order)
  :doc "FIND-ANY"
  (t2l::find-any2 x 
                  :force-function force-function
                  :cost-fun cost-fun
                  :terminate-test terminate-test
                  :order order))

(defmethod! find-all (i input &key points-system catalog force-function cost-fun terminate-test order)
  :doc "FIND-ALL"
  (t2l::find-all2 i 
                  input
                  :points-system points-system
                  :catalog catalog 
                  :force-function force-function
                  :cost-fun cost-fun
                  :terminate-test terminate-test
                  :order order))

(defmethod! solver-input (&optional input catalog) :icon 215 (t2l::solver-input input catalog))

(defmethod! solver-output (&optional value &key label) :icon 215 (t2l::solver-output value :label label))


(in-package :screamer)

;;;; SCREAMER:SOLUTION
(defun ?solution (x &key onmatch save-matches cut-after abort-after force-fun cost-fun terminate? order)
  (let ((force-fun 
         (cond
          ((null force-fun) #'linear-force)
          ((functionp force-fun) force-fun)
          ((find (symbol-name force-fun) '("lf" "linear-force")) #'linear-force)
          ((find (symbol-name force-fun) '("dacf" "divide-and-conquer-force")) #'divide-and-conquer-force)
          (T #'linear-force))))
  (cond
   ((and (null onmatch) 
         (null save-matches) 
         (null cut-after)
         (null abort-after)
         (null cost-fun)
         (null terminate?)
         (null order))
    (solution x (static-ordering force-fun)))

   ;;;; 
   (T
    (let ((default-cost-fun (let ((variables (remove-duplicates (variables-in (value-of x)) :from-end T)))
                              #'(lambda (y) (or (position y variables) 100))))
          (default-terminate-test #'(lambda (y) (declare (ignore y)) nil))
          (match-count 0)
          (cycle-count 0)
          (start-timestamp (get-universal-time)))
      (let (abort-timestamp abort?)
        (let ((terminate? (cond
                           (abort-after
                            (setf abort-timestamp (+ start-timestamp (round (* abort-after 60 1))))
                            (cond 
                             (terminate?
                              #'(lambda (y)
                                  (incf cycle-count) 
                                  (or (setf abort? (> (get-universal-time) abort-timestamp))
                                      (funcall terminate? y))))
                             (T #'(lambda (y) 
                                    (incf cycle-count) 
                                    (setf abort? (> (get-universal-time) abort-timestamp))))))
                           (T (or terminate? 
                                  #'(lambda (y) 
                                      (declare (ignore y)) 
                                      (incf cycle-count) 
                                      nil)))))

              (cost-fun
               (cond 
                ((null cost-fun) default-cost-fun)
                ((functionp cost-fun) cost-fun)
                ((find (symbol-name cost-fun) '("domain-size" "domain")) #'domain-size)
                ((find (symbol-name cost-fun) '("range-size" "range")) #'range-size)
                (T default-cost-fun)))

              (order
               (cond
                ((null order) #'<)
                ((functionp order) order)
                ((string= (symbol-name order) "<") #'<)
                ((string= (symbol-name order) ">") #'>)
                (T #'<)))

              (onmatch 
               (cond 
                ((and 
                  save-matches 
                  (typep save-matches 'om::store))
                 (format om-lisp::*om-stream* 
                         "...SOLUTION-->~A (~Ax, ~A ~A)~%~%"
                         save-matches
                         (length (om::?variables-in (list x))) 
                         (paradigm--format-timestamp start-timestamp)
                         (if abort-timestamp
                             (format nil "until ~A" (paradigm--format-timestamp abort-timestamp))
                           ""))
                 (cond 
                  (onmatch 
                   #'(lambda (xs) 
                       (om::collect-to save-matches (cdr (assoc :match xs)))
                       (paradigm--save-to-solver-output (cdr (assoc :match xs)))
                       (funcall onmatch xs)))
                  (T #'(lambda (xs) 
                         (om::collect-to save-matches (cdr (assoc :match xs)))
                         (paradigm--save-to-solver-output (cdr (assoc :match xs)))))))
                
                (save-matches
                 (format om-lisp::*om-stream* 
                         "...SOLUTION-->(SOLVER-OUTPUT) (~Ax, ~A ~A)~%~%" 
                         (length (om::?variables-in (list x))) 
                         (paradigm--format-timestamp start-timestamp)
                         (if abort-timestamp
                             (format nil "until ~A" (paradigm--format-timestamp abort-timestamp))
                           ""))
                 (cond 
                  (onmatch 
                   #'(lambda (xs) 
                       (paradigm--save-to-solver-output (cdr (assoc :match xs)))
                       (funcall onmatch xs)))
                  (T #'(lambda (xs) 
                         (paradigm--save-to-solver-output (cdr (assoc :match xs)))))))

                (T (if onmatch
                       onmatch
                     #'(lambda (x) nil))))))

          (cond
           (cut-after
            (first 
             (reverse
              (n-values 
               cut-after 
               (let ((value
                      (solution x (reorder cost-fun 
                                           terminate?
                                           order
                                           force-fun))))
                 (when abort?
                   (fail))
                 (if abort-after
                     (progn
                       (setf start-timestamp (get-universal-time))
                       (setf abort-timestamp (+ start-timestamp (round (* abort-after 60 1))))
                       (setf abort? nil)))
                 (funcall onmatch (list (cons :match-count (incf match-count))
                                        (cons :cycle-count cycle-count)
                                        (cons :timestamp start-timestamp)
                                        (cons :match value)))
                 value)))))
           (T
            (let ((value
                   (solution x (reorder cost-fun
                                        terminate?
                                        order
                                        force-fun))))
              (when abort?
                (fail))
              (funcall onmatch (list (cons :match-count (incf match-count))
                                     (cons :cycle-count cycle-count)
                                     (cons :timestamp start-timestamp)
                                     (cons :match value)))
              value))))))))))

(defun paradigm--format-timestamp (timestamp)
  (global
    (multiple-value-bind
        (second minute hour date month year day-of-week dst-p tz)
        (decode-universal-time timestamp)
      (concatenate 'string
                   (format nil "~2,'0d:~2,'0d:~2,'0d ~d/~2,'0d"
                           hour minute second month date)))))

(defun paradigm--save-to-solver-output (value)
  (global
    (if (null t2l::*findall2-values*)
        (setf t2l::*findall-last-value-cons* (list value)
              t2l::*findall2-values* t2l::*findall-last-value-cons*)
      (setf (rest t2l::*findall-last-value-cons*) (list value)
            t2l::*findall-last-value-cons* (rest t2l::*findall-last-value-cons*)))))

(defun paradigm--clear-solver-output ()
  (global
    (setf t2l::*findall2-values* '())
    (setf t2l::*findall-last-value-cons* nil)))

(defun choice-box (list)
  (cond
   ((null list) (fail))
   ((nondeterministic-function? (car list))
    (either (funcall-nondeterministic (car list)) (choice-box (cdr list))))
   ((functionp (car list))
    (either (funcall (car list)) (choice-box (cdr list))))
   (T (either (car list) (choice-box (cdr list))))))

(defun function-choice-box (functions) ;; ????
  (cond
   ((null functions) (fail))
   (T (either (funcall-nondeterministic (car functions)) (function-choice-box (cdr functions))))))

(defun bt-group-list (input groups)
  (let ((xs (t2l::an-ordered-partition-of input)))
    (unless (= (length xs) groups) (fail))
    xs))

(defun multiple-choice-list-element (thischoice randomized)
  (cond ((nondeterministic-function? thischoice) 
         (funcall-nondeterministic thischoice))
        ((functionp thischoice) 
         (funcall thischoice))
        ((listp thischoice)
         (if randomized
             (a-random-member-of thischoice)
           (a-member-of thischoice)))
        (T thischoice)))

(defun multiple-choice-list (template &rest choice-list)
  (cond 
   ((null template) nil)

   ((null (car template))
    (append (list nil) 
            (apply-nondeterministic #'multiple-choice-list 
                                    (if (cdr template) (cdr template) nil) 
                                    choice-list)))

   ((consp (car template)) ; ??
    (append (list (car template))
            (apply-nondeterministic #'multiple-choice-list 
                                    (if (cdr template) (cdr template) nil) 
                                    choice-list)))

   ;; random choice (2?, 3?, ... calls a-random-member-of
   ((and (symbolp (car template))
         (> (length (symbol-name (car template))) 1)
         (string= "?" (subseq (symbol-name (car template)) 
                              (1- (length (symbol-name (car template)))) 
                              (length (symbol-name (car template)))))
         (parse-integer (subseq (symbol-name (car template)) 
                                0 (1- (length (symbol-name (car template)))))
                                :junk-allowed t))
    (let ((thischoice (elt choice-list (1- (parse-integer (subseq (symbol-name (car template)) 
                                                                  0 (1- (length (symbol-name (car template))))))))))
      (append 
       (list (multiple-choice-list-element thischoice T))
       (apply-nondeterministic #'multiple-choice-list (cdr template) choice-list))))
   
   ;; append symbol from template
   ((not (numberp (car template)))
    (append 
     (list (car template))
     (apply-nondeterministic #'multiple-choice-list 
                             (if (cdr template) (cdr template) nil) 
                             choice-list)))

   ;; not randomized
   (T
    (let ((thischoice (elt choice-list (1- (car template)))))
      (append 
       (list (multiple-choice-list-element thischoice nil))
       (apply-nondeterministic #'multiple-choice-list (cdr template) choice-list))))))

(defun grouplist-nondeterministic (list min-groupsize max-groupsize wpartitioncount wmeangroupsize wdeviation wdistancefrommedian )
  (let ((min-groupsize (min (or min-groupsize 1) (length list)))
        (max-groupsize (or max-groupsize (length list)))
        (wpartitioncount (or wpartitioncount 0))
        (wmeangroupsize (or wmeangroupsize 0.5))
        (wdeviation (or wdeviation 0.5))
        (wdistancefrommedian (or wdistancefrommedian 1))
        (points (a-realv)))
    (let ((min2maxseries (let ((w min-groupsize)
                               (stack nil))
                           (dotimes (- max-groupsize min-groupsize) 
                             (push (incf w) stack))
                           (reverse stack)))
          (min-count (ceiling (/ (length list) max-groupsize)))
          (max-count (floor (/ (length list) min-groupsize))))
      (let ((median-groupsize (nth (floor (/ (length min2maxseries) 2)) min2maxseries)))
        ;(print (format nil " grouplist-nondeterministic min-count ~A, max-count ~A for min-groupsize ~A, max-groupsize ~A; median-groupsize: ~A" min-count max-count min-groupsize max-groupsize median-groupsize))
        (om::map-func
         (let (items)
           (dolist (x list)
             (push x items))
           (setf items (nreverse items))
           #'(lambda (x) (pop items)))
         (mapcar #'make-list (car         
          (best-value
           (let ((partitions (an-integer-between min-count max-count)))
             (let ((group-counts (mapcar #'(lambda (x) (an-integer-betweenv min-groupsize max-groupsize)) 
                                         (make-list partitions))))
               (let ((total (apply #'+v group-counts))
                     (mean-groupsize (a-realv)))
                 (assert! (=v total (length list)))
                 (assert! (equalv mean-groupsize (/v total partitions)))
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
                     (assert! (=v points (+v points-for-partitioncount 
                                             points-for-meangroupsize
                                             points-for-deviation
                                             points-for-distancefrommedian)))
                   ;(print (format nil  "~A points [points-for-partitioncount: ~A points-for-meangroupsize: ~A points-for-deviation: ~A points-for-distancefrommedian: ~A" points points-for-partitioncount points-for-meangroupsize points-for-deviation points-for-distancefrommedian))
                     T)
                   
                   (let ((rs (solution group-counts (static-ordering #'linear-force))))
                     ;(print (list partitions :points points :list rs))
                     rs)))))
           points))))))))

(defun calltrain1x (function-sequence argument-sequence) ; DELETE -- 
  (cond
   ((null function-sequence) nil)
   ((nondeterministic-function? (car function-sequence))
    (append (list
             (funcall-nondeterministic (car function-sequence) (car argument-sequence)))
            (calltrain1x (cdr function-sequence) (cdr argument-sequence))))
   ((functionp (car function-sequence))
    (append (list
             (funcall (car function-sequence) (car argument-sequence)))
            (calltrain1x (cdr function-sequence) (cdr argument-sequence))))
   (T
    (append (list (car function-sequence))
            (calltrain1x (cdr function-sequence) argument-sequence)))))
            
(defun map-func-nondeterministic-internal (fn x level)
  (cond
   ((or (null x)
        (not (consp x)))
    (funcall-nondeterministic fn x level))

   (T (cons (map-func-nondeterministic-internal fn (car x) (1+ level))
            (if (cdr x) (map-func-nondeterministic-internal fn (cdr x) level))))))

(defun map-func-nondeterministic (fn x &key with-levels)
  (map-func-nondeterministic-internal 
   (if with-levels
       #'(lambda (x level) (funcall-nondeterministic fn x level))
     #'(lambda (x level) (funcall-nondeterministic fn x)))
   x 0))

(defun map2func-nondeterministic-internal (fn x y)
  (cond
   ((and (null x) (null y))
    (funcall-nondeterministic fn x y))

   ((or (null x) (null y))
    (error (format nil "input mismatch, #1 - x: ~A y: ~A" x y)))

   ((and (not (consp x))
         (not (consp y)))
    (funcall-nondeterministic fn x y))

   ((or (not (consp x))
        (not (consp y)))
    (error (format nil "input mismatch, #2 - x: ~A y: ~A" x y)))

   ((and (consp (car x))
         (consp (car y)))
    (cons (map2func-nondeterministic-internal fn (car x) (car y))
          (if (and (cdr x) (cdr y))
              (map2func-nondeterministic-internal fn (cdr x) (cdr y)))))

   ((or (consp (car x))
        (consp (car y)))
    (error (format nil "input mismatch, #3 - x: ~A y: ~A" x y)))

   (T (cons (funcall-nondeterministic fn (car x) (car y))
            (if (and (cdr x) (cdr y))
                (map2func-nondeterministic-internal fn (cdr x) (cdr y)))))))

(defun map2func-nondeterministic (fn x y)
  (map2func-nondeterministic-internal fn x y))

(defun map?car-nondeterministic (fn list)
  (cond
   ((null list) nil)
   (T (append (list (funcall-nondeterministic fn (car list)))
              (map?car-nondeterministic fn (cdr list))))))

(defun map2?car-nondeterministic (fn list1 list2)
  (cond
   ((null list1) nil)
   ((null list2) nil)
   (T (append (list (funcall-nondeterministic fn (car list1) (car list2)))
              (map2?car-nondeterministic fn (cdr list1) (cdr list2))))))

(defun map3?car-nondeterministic (fn list1 list2 list3)
  (cond
   ((null list1) nil)
   ((null list2) nil)
   ((null list3) nil)
   (T (append (list (funcall-nondeterministic fn (car list1) (car list2) (car list3)))
              (map3?car-nondeterministic fn (cdr list1) (cdr list2) (cdr list3))))))

(defun map4?car-nondeterministic (fn list1 list2 list3 list4)
  (cond
   ((null list1) nil)
   ((null list2) nil)
   ((null list3) nil)
   ((null list4) nil)
   (T (append (list (funcall-nondeterministic fn (car list1) (car list2) (car list3) (car list4)))
              (map4?car-nondeterministic fn (cdr list1) (cdr list2) (cdr list3) (cdr list4))))))

(defun absv (k) (maxv k (*v k -1)))

(in-package :OPENMUSIC)

(defmethod get-boxcallclass-fun ((self (eql 'assert!))) 'screamerboxes)
(defmethod get-boxcallclass-fun ((self (eql 'value-of))) 'screamerboxes)
(defmethod get-boxcallclass-fun ((self (eql 'apply-substitution))) 'screamerboxes)
(defmethod get-boxcallclass-fun ((self (eql 'bound?))) 'screamerboxes)
(defmethod get-boxcallclass-fun ((self (eql 'ground?))) 'screamerboxes)
(defmethod get-boxcallclass-fun ((self (eql 'applyv))) 'screamerboxes)
(defmethod get-boxcallclass-fun ((self (eql 'funcallv))) 'screamerboxes)
(defmethod get-boxcallclass-fun ((self (eql 'equalv))) 'screamerboxes)
(defmethod get-boxcallclass-fun ((self (eql 'booleanpv))) 'screamerboxes)
(defmethod get-boxcallclass-fun ((self (eql 'numberpv))) 'screamerboxes)
(defmethod get-boxcallclass-fun ((self (eql 'memberv))) 'screamerboxes)
(defmethod get-boxcallclass-fun ((self (eql 'notv))) 'screamerboxes)
(defmethod get-boxcallclass-fun ((self (eql 'count-truesv))) 'screamerboxes)
(defmethod get-boxcallclass-fun ((self (eql 'known?))) 'screamerboxes)
(defmethod get-boxcallclass-fun ((self (eql 'decide))) 'screamerboxes)
(defmethod get-boxcallclass-fun ((self (eql 'andv))) 'screamerboxes)
(defmethod get-boxcallclass-fun ((self (eql 'orv))) 'screamerboxes)
(defmethod get-boxcallclass-fun ((self (eql '<v))) 'screamerboxes)
(defmethod get-boxcallclass-fun ((self (eql '<=v))) 'screamerboxes)
(defmethod get-boxcallclass-fun ((self (eql '=v))) 'screamerboxes)
(defmethod get-boxcallclass-fun ((self (eql '>=v))) 'screamerboxes)
(defmethod get-boxcallclass-fun ((self (eql '>v))) 'screamerboxes)
(defmethod get-boxcallclass-fun ((self (eql '/=v))) 'screamerboxes)

(defmethod get-boxcallclass-fun ((self (eql 'make-variable))) 'screamerboxes)
(defmethod get-boxcallclass-fun ((self (eql 'a-booleanv))) 'screamerboxes)
(defmethod get-boxcallclass-fun ((self (eql 'a-member-ofv))) 'screamerboxes)
(defmethod get-boxcallclass-fun ((self (eql 'a-numberv))) 'screamerboxes)
(defmethod get-boxcallclass-fun ((self (eql 'a-realv))) 'screamerboxes)
(defmethod get-boxcallclass-fun ((self (eql 'a-real-abovev))) 'screamerboxes)
(defmethod get-boxcallclass-fun ((self (eql 'a-real-belowv))) 'screamerboxes)
(defmethod get-boxcallclass-fun ((self (eql 'a-real-betweenv))) 'screamerboxes)
(defmethod get-boxcallclass-fun ((self (eql 'an-integerv))) 'screamerboxes)
(defmethod get-boxcallclass-fun ((self (eql 'an-integer-abovev))) 'screamerboxes)
(defmethod get-boxcallclass-fun ((self (eql 'an-integer-belowv))) 'screamerboxes)
(defmethod get-boxcallclass-fun ((self (eql 'an-integer-betweenv))) 'screamerboxes)
; (defmethod get-boxcallclass-fun ((self (eql 'minv))) 'screamerboxes)
; (defmethod get-boxcallclass-fun ((self (eql 'maxv))) 'screamerboxes)
(defmethod get-boxcallclass-fun ((self (eql '+v))) 'screamerboxes)
(defmethod get-boxcallclass-fun ((self (eql '-v))) 'screamerboxes)
(defmethod get-boxcallclass-fun ((self (eql '*v))) 'screamerboxes)
(defmethod get-boxcallclass-fun ((self (eql '/v))) 'screamerboxes)
(defmethod get-boxcallclass-fun ((self (eql 'absv))) 'screamerboxes)

(defmethod get-boxcallclass-fun ((self (eql 'a-boolean))) 'screamerboxes)
(defmethod get-boxcallclass-fun ((self (eql 'an-integer))) 'screamerboxes)
(defmethod get-boxcallclass-fun ((self (eql 'an-integer-above))) 'screamerboxes)
(defmethod get-boxcallclass-fun ((self (eql 'an-integer-below))) 'screamerboxes)

(defmethod get-boxcallclass-fun ((self (eql '?solution))) 'screamerboxes) 
(defmethod get-boxcallclass-fun ((self (eql 'choice-box))) 'screamerboxes)
(defmethod get-boxcallclass-fun ((self (eql 'function-choice-box))) 'screamerboxes)
(defmethod get-boxcallclass-fun ((self (eql 'bt-group-list))) 'screamerboxes)
(defmethod get-boxcallclass-fun ((self (eql 'multiple-choice-list))) 'screamerboxes)
(defmethod get-boxcallclass-fun ((self (eql 'grouplist-nondeterministic))) 'screamerboxes)
(defmethod get-boxcallclass-fun ((self (eql 'calltrain1x))) 'screamerboxes)
(defmethod get-boxcallclass-fun ((self (eql 'map-func-nondeterministic))) 'screamerboxes)
(defmethod get-boxcallclass-fun ((self (eql 'map2func-nondeterministic))) 'screamerboxes)
(defmethod get-boxcallclass-fun ((self (eql 'map?car-nondeterministic))) 'screamerboxes)
(defmethod get-boxcallclass-fun ((self (eql 'map2?car-nondeterministic))) 'screamerboxes)
(defmethod get-boxcallclass-fun ((self (eql 'map3?car-nondeterministic))) 'screamerboxes)
(defmethod get-boxcallclass-fun ((self (eql 'map4?car-nondeterministic))) 'screamerboxes)

(defmethod! assert! (x) (s:assert! x))
(defmethod! value-of (x) (s:value-of x))
(defmethod! apply-substitution (x) (s:apply-substitution x))
(defmethod! bound? (x) (bound? x))
(defmethod! ground? (x) (ground? x))
(defmethod! applyv (f x &rest xs) (apply #'s:applyv (append (list f x) xs)))
(defmethod! funcallv (f &rest x) (apply #'s:funcallv (append (list f x) xs)))
(defmethod! equalv (x y) (s:equalv x y))
(defmethod! booleanpv (x) (s:booleanpv x))
(defmethod! numberpv (x) (s:numberpv x))
(defmethod! memberv (x sequence) (s:memberv x sequence))
(defmethod! notv (x) (s:notv x))
(defmethod! count-truesv (&rest xs) (apply #'s:count-truesv xs))
(defmethod! known? (x) (s:known? x))
(defmethod! decide (x) (s:decide x))
(defmethod! andv (&rest xs) (apply #'s:andv xs))
(defmethod! orv (&rest xs) (apply #'s:orv xs))
(defmethod! <v (x &rest xs) (apply #'s:<v (append (list x) xs)))
(defmethod! <=v (x &rest xs)(apply #'s:<=v (append (list x) xs)))
(defmethod! =v (x &rest xs) (apply #'s:=v (append (list x) xs)))
(defmethod! >v (x &rest xs) (apply #'s:>v (append (list x) xs)))
(defmethod! >=v (x &rest xs) (apply #'s:>=v (append (list x) xs)))
(defmethod! /=v (x &rest xs) (apply #'s:/=v (append (list x) xs)))
(defmethod! make-variable (&optional name) (if name (s:make-variable name) (s:make-variable)))
(defmethod! a-booleanv (&optional name) (if name (s:a-booleanv name) (s:a-booleanv)))
(defmethod! a-member-ofv (values &optional name) (if name (s:a-member-ofv values name) (s:a-member-ofv values)))
(defmethod! a-numberv (&optional name) (if name (s:a-numberv name) (s:a-numberv)))
(defmethod! a-realv (&optional name) (if name (s:a-realv name) (s:a-realv)))
(defmethod! a-real-abovev (low &optional name) (if name (s:a-real-abovev low name) (s:a-real-abovev low)))
(defmethod! a-real-belowv (high &optional name) (if name (s:a-real-belowv high name) (s:a-real-belowv high)))
(defmethod! a-real-betweenv (low high &optional name) (if name (s:a-real-betweenv low high name) (s:a-real-betweenv low high)))
(defmethod! an-integerv (&optional name) (if name (s:an-integerv name) (s:an-integerv)))
(defmethod! an-integer-abovev (low &optional name) (if name (s:an-integer-abovev low name) (s:an-integer-abovev low)))
(defmethod! an-integer-belowv (high &optional name) (if name (s:an-integer-belowv high name) (s:an-integer-belowv high)))
(defmethod! an-integer-betweenv (low high &optional name) (s:an-integer-betweenv low high name) (s:an-integer-betweenv low high))

(defmethod! a-boolean () (s:a-boolean))
(defmethod! an-integer-above (low) (s:an-integer-above low))
(defmethod! an-integer-below (high) (s:an-integer-below high))

(defmethod! minv (x &rest xs) (apply #'t2l::omminv (append (list x) xs)))
(defmethod! maxv (x &rest xs) (apply #'t2l::ommaxv (append (list x) xs)))
(defmethod! +v (&rest xs) (apply #'s:+v xs))
(defmethod! -v (&rest xs) (apply #'s:-v xs))
(defmethod! *v (&rest xs) (apply #'s:*v xs))
(defmethod! /v (&rest xs) (apply #'s:/v xs))
(defmethod! %v (n d) (t2l::om%v n d))
(defmethod! absv (k) (t2l::absv k))
(defmethod! ?solution (x &key onmatch save-matches cut-after abort-after force-fun cost-fun terminate? order)
 :doc "
Documentation from https://nikodemus.github.io/screamer/

"
 :icon 150
 (s::?solution x 
            :onmatch onmatch
            :save-matches save-matches
            :cut-after cut-after
            :abort-after abort-after
            :force-fun force-fun
            :cost-fun cost-fun
            :terminate? terminate?
            :order order))
(defmethod! choice-box (list) :icon 235 (s::choice-box list))
(defmethod! function-choice-box (functions) :icon 147 (s::function-choice-box functions))
(defmethod! bt-group-list (input groups) (s::bt-group-list input groups))
(defmethod! multiple-choice-list (template &rest choice-list) (apply-nondeterministic #'s::multiple-choice-list (append (list template) choice-list)))
(defmethod! grouplist-nondeterministic (list min-groupsize max-groupsize wpartitioncount wmeangroupsize wdeviation wdistancefrommedian)
  :icon 235 
  :indoc '("list" "min-groupsize" "max-groupsize" "wpartitioncount" "wmeangroupsize" "wdeviation" "wdistancefrommedian")
  :initvals '(nil 1 5 0 1 1 -1)
  (s::grouplist-nondeterministic min-groupsize max-groupsize wpartitioncount wmeangroupsize wdeviation wdistancefrommedian))
(defmethod! calltrain1x (function-sequence argument-sequence) :icon 147 (s::calltrain1x function-sequence argument-sequence))
(defmethod! map-func-nondeterministic (fn x &key with-levels) :icon 147 (s::map-func-nondeterministic fn x :with-levels with-levels))
(defmethod! map2func-nondeterministic (fn x y) :icon 147 (s::map2func-nondeterministic fn x y))
(defmethod! map?car-nondeterministic (fn list) :icon 147 (map?car-nondeterministic fn list))
(defmethod! map2?car-nondeterministic (fn list1 list2) :icon 147 (map2?car-nondeterministic fn list1 list2))
(defmethod! map3?car-nondeterministic (fn list1 list2 list3) :icon 147 (map3?car-nondeterministic fn list1 list2 list3))
(defmethod! map4?car-nondeterministic (fn list1 list2 list3 list4) :icon 147 (map4?car-nondeterministic fn list1 list2 list3 list4))


(in-package :OPENMUSIC)


(defmethod! ?mapprules (input
                        prules 
                        &key symbol-mode
                             get-symbol-list
                             ; process-chunk-size
                             ; input-process-increment
                             ; continue
                             ; init
                             listdxx
                             max
                             min
                             ordered-partitions-nondeterministic-values-cap
                             superset
                             params
                             print-graph-info)
  :numouts 3
  :doc "Applies symbol-grammars to screamer variables. With the forward-chaining inferencing engine in screamer (ONE-VALUES, ALL-VALUES, SOLUTION), all possible expressions of a given symbol-grammar that apply to a list of values can be explored. The code is based on the process outlined in 'Graph representation of context-free grammars', Alex Shkotin (arXiv:cs/0703015 http://arxiv.org/abs/cs/0703015) (jan 2013)"
  (multiple-value-bind 
      (var list dmg)
      (t2l::mapprules input
                      prules
                      ; :process-chunk-size process-chunk-size
                      :get-symbol-list get-symbol-list
                      ; :input-process-increment input-process-increment
                      ; :continue continue
                      ; :init init
                      :listdxx listdxx
                      :max max
                      :min min
                      ; :ordered-partitions-nondeterministic-values-cap ordered-partitions-nondeterministic-values-cap
                      :superset superset
                      :symbol-mode symbol-mode
                      :params params
                      :print-graph-info print-graph-info)
    (values var list dmg)))

(defmethod! map?and (fn list)
  :icon 147
  :doc "map?and is equivalent to (apply #'andv (mapcar fn list)) where fn yields either true, false (nil), or a constraint variable representing true-or-false."
  (t2l::map-andv fn list))

;; delete  
(defmethod! map2?and (fn list1 list2)
  :icon 147
  :doc "map?and is equivalent to (apply #'andv (mapcar fn list)) where fn yields either true, false (nil), or a constraint variable representing true-or-false."
  (t2l::map2andv fn list1 list2))

(defmethod! map3?and (fn list1 list2 list3)
  :icon 147
  :doc "map?and is equivalent to (apply #'andv (mapcar fn list)) where fn yields either true, false (nil), or a constraint variable representing true-or-false."
  (t2l::map3andv fn list1 list2 list3))

(defmethod! map4?and (fn list1 list2 list3 list4)
  :icon 147
  :doc "map?and is equivalent to (apply #'andv (mapcar fn list)) where fn yields either true, false (nil), or a constraint variable representing true-or-false."
  (t2l::map4andv fn list1 list2 list3 list4))

(defmethod! map5?and (fn list1 list2 list3 list4 list5)
  :icon 147
  :doc "map?and is equivalent to (apply #'andv (mapcar fn list)) where fn yields either true, false (nil), or a constraint variable representing true-or-false."
  (t2l::map5andv fn list1 list2 list3 list4 list5))

(defmethod! map?or (fn list)
  :icon 147
  :doc "map?or is equivalent to (apply #'orv (mapcar fn list)) where fn yields either true, false (nil), or a constraint variable representing true-or-false."
  (t2l::map-orv fn list))

;; delete
(defmethod! map2?or (fn list1 list2)
  :icon 147
  :doc "map?or is equivalent to (apply #'orv (mapcar fn list)) where fn yields either true, false (nil), or a constraint variable representing true-or-false."
  (t2l::map2orv fn list1 list2))
  
(defmethod! map3?or (fn list1 list2 list3)
  :icon 147
  :doc "map?or is equivalent to (apply #'orv (mapcar fn list)) where fn yields either true, false (nil), or a constraint variable representing true-or-false."
  (t2l::map3orv fn list1 list2 list3))

(defmethod! map4?or (fn list1 list2 list3 list4)
  :icon 147
  :doc "map?or is equivalent to (apply #'orv (mapcar fn list)) where fn yields either true, false (nil), or a constraint variable representing true-or-false."
  (t2l::map4orv fn list1 list2 list3 list4))
  
(defmethod! map5?or (fn list1 list2 list3 list4 list5)
  :icon 147
  :doc "map?or is equivalent to (apply #'orv (mapcar fn list)) where fn yields either true, false (nil), or a constraint variable representing true-or-false."
  (t2l::map5orv fn list1 list2 list3 list4 list5))
   
(defmethod! maplist?and (fn list)
  :icon 147
  :doc "maplist?and is equivalent to (apply #'andv (maplist fn list)) where fn yields either true, false (nil), or a constraint variable representing true-or-false."
  (t2l::maplist-andv fn list))
  
(defmethod! maplist2?and (fn list1 list2)
  :icon 147
  :doc "maplist?and is equivalent to (apply #'andv (maplist fn list)) where fn yields either true, false (nil), or a constraint variable representing true-or-false."
   (t2l::maplist2andv fn list1 list2))

(defmethod! maplist3?and (fn list1 list2 list3)
  :icon 147
  :doc "maplist?and is equivalent to (apply #'andv (maplist fn list)) where fn yields either true, false (nil), or a constraint variable representing true-or-false."
   (t2l::maplist3andv fn list1 list2 list3))

(defmethod! maplist4?and (fn list1 list2 list3 list4)
  :icon 147
  :doc "maplist?and is equivalent to (apply #'andv (maplist fn list)) where fn yields either true, false (nil), or a constraint variable representing true-or-false."
  (t2l::maplist4andv fn list1 list2 list3 list4))

(defmethod! maplist?or (fn list)
  :icon 147
  :doc "maplist?and is equivalent to (apply #'orv (maplist fn list)) where fn yields either true, false (nil), or a constraint variable representing true-or-false."
   (t2l::maplist-orv fn list))

(defmethod! maplist2?or (fn list1 list2)
  :icon 147
  :doc "maplist?and is equivalent to (apply #'orv (maplist fn list)) where fn yields either true, false (nil), or a constraint variable representing true-or-false."
   (t2l::maplist2orv fn list1 list2))
   
(defmethod! maplist3?or (fn list1 list2 list3)
  :icon 147
  :doc "maplist?and is equivalent to (apply #'orv (maplist fn list)) where fn yields either true, false (nil), or a constraint variable representing true-or-false."
   (t2l::maplist3orv fn list1 list2 list3))
   
(defmethod! maplist4?or (fn list1 list2 list3 list4)
  :icon 147
  :doc "maplist?and is equivalent to (apply #'orv (maplist fn list)) where fn yields either true, false (nil), or a constraint variable representing true-or-false."
   (t2l::maplist4orv fn list1 list2 list3 list4))

(defmethod! ?all2comb (fn list)
  :icon 147
  :doc "?all2comb is equivalent to (apply #'?and (map2comb (fn x y) list)"
  (t2l::?all2comb fn list))

(defmethod! ?any2comb (fn list)
  :icon 147
  :doc "?any2comb is equivalent to (apply #'?or (map2comb (fn x y) list)"
  (t2l::?any2comb fn list))

(defmethod! list2comb (list) :icon 235 (t2l::list2comb list))

(defmethod! map2comb (fn list)
  :icon 147
  :doc "map2comb"
  (t2l::map2comb fn list))

(cl:defun listXv (fn xs value) (map-func #'(lambda (x) (funcall fn x value)) xs :ignore-null-input T))
(defmethod! ?list+ (xs value) (listXv #'?+ xs value))
(defmethod! ?list- (xs value) (listXv #'?- xs value))
(defmethod! ?list* (xs value) (listXv #'?* xs value))
(defmethod! ?list/ (xs value) (listXv #'?/ xs value))
(defmethod! ?list% (xs value) (listXv #'?% xs value))
(defmethod! ?listabs (xs) (map-func #'?abs xs :ignore-null-input T))
(defmethod! ?listdx (list)
  :icon 235
  (cond 
   ((null list) nil)
   ((cdr list)
    (append (if (or (null (car list))
                    (null (cadr list)))
                (list nil)
              (list (s:-v (cadr list) (car list))))
            (?listdx (cdr list))))
   (T nil)))
(defmethod! ?listmin (xs) :icon 209 (apply #'?min xs))
(defmethod! ?listmax (xs) :icon 209 (apply #'?max xs))
(defmethod! ?lists= (xs ys) (t2l::lists=v xs ys))
(defmethod! ?lists/= (xs ys) (t2l::lists/=v xs ys))
(cl:defun list-compare-to-value (fn xs value) (map?and #'(lambda (x) (funcall fn x value)) (remove nil (flatt xs))))
(defmethod! ?list< (xs value) (list-compare-to-value #'?< xs value))
(defmethod! ?list<= (xs value) (list-compare-to-value #'?<= xs value))
(defmethod! ?list> (xs value) (list-compare-to-value #'?> xs value))
(defmethod! ?list>= (xs value) (list-compare-to-value #'?>= xs value))
(defmethod! ?list= (xs value) (list-compare-to-value #'?= xs value))
(defmethod! ?list/= (xs value) (list-compare-to-value #'?/= xs value))
(defmethod! ?listeq (xs value) (list-compare-to-value #'?equal xs value))
(defmethod! ?list!eq (xs value) (list-compare-to-value #'(lambda (x) (?not (?equal x value))) xs))
(defmethod! ?all+ (xs) (apply #'+v xs)) 
(defmethod! ?all- (xs) (apply #'-v xs))
(defmethod! ?all* (xs) (apply #'*v xs))
(defmethod! ?all/ (xs) (apply #'/v xs))
(defmethod! ?all< (xs) (apply #'<v xs))
(defmethod! ?all<= (xs) (apply #'<=v xs))
(defmethod! ?all> (xs) (apply #'>v xs))
(defmethod! ?all>= (xs) (apply #'>=v xs))
(defmethod! ?all/= (xs) (apply #'/=v xs))
(defmethod! ?all= (xs) (apply #'=v xs))
(defmethod! ?all-different (x &rest xs)
  (labels ((all-different (x xs)
             (if (null xs)
                 t
               (?and (?not (?= x (car xs)))
                     (?all-different x (cdr xs))
                     (?all-different (car xs) (cdr xs))))))
    (all-different x xs)))
(defmethod! ?all-equal (xs) (map?and #'(lambda (ys) (t2l::omequalv (car ys) (cadr ys))) (combinations-of2 xs)))
(defmethod! ?all/equal (xs) (map?and #'(lambda (ys) (t2l::omnotv (t2l::omequalv (car ys) (cadr ys)))) xs))
(defmethod! ?all-between (list min max)
  (map?and #'(lambda (y) (?and (?>= y min) (?<= y max))) (?xs-in list)))
(defmethod! ?all-between!! (list min max) :icon 1100 (?between!! list min max))
(defmethod! ?between!! (input min max)
  :icon 1100
  (cond
   ((null input) T)
   ((consp input)
    (?between!! (car input) min max)
    (?between!! (cdr input) min max))
   ((or (screamer::variable? input)
        (numberp input))
    (unless (null min)
      (s:assert! (s:>=v input min)))
    (unless (null max)
      (s:assert! (s:<=v input max))))
   (T T))
  input)
(defmethod! ?all<> (list from to) (map?and #'(lambda (x) (?<> x from to)) list))
(defmethod! ?all<>= (list from to) (map?and #'(lambda (x) (?<>= x from to)) list))
(defmethod! ?anyeq (xs value) (t2l::anyequalv xs value))
(defmethod! ?any!eq (xs value) (t2l::anynotequalv xs value))
(defmethod! ?any= (xs value) (t2l::any=v xs value))
(defmethod! ?any/= (xs value) (t2l::any/=v xs value))
(defmethod! ?any< (xs value) (t2l::any<v xs value))
(defmethod! ?any> (xs value) (t2l::any>v xs value))
(defmethod! ?any<= (xs value) (t2l::any<=v xs value))
(defmethod! ?any>= (xs value) (t2l::any>=v xs value))
(defmethod! ?member (x sequence) (s:memberv x sequence))
(defmethod! ?membersof (list sequence)
  (map?and #'(lambda (x) (?member x sequence)) (flatt list)))
(defmethod! !membersof (list sequence)
  (map?and #'(lambda (x) (?not (?member x sequence))) (flatt list)))
(defmethod! ?count-trues (x &rest xs) (apply #'screamer:count-truesv (flatt (append (list x) xs))))
(defmethod! ?count-trues-in-list (xs) :icon 235 (apply #'screamer:count-truesv (flatt xs)))
(defmethod! ?items-in (list sequence &key numeric fast-crosscheck)
  (t2l::items-inv list sequence :numeric numeric :fast-crosscheck fast-crosscheck))
(defmethod! ?items-in!! (list sequence &key numeric)
  :icon 1100
  (dolist (x (?xs-in list))
    (assert! (memberv x sequence)))
  list)
(defmethod! phrase-modality-c (input fundamental modepcs)
  :icon 1100
  (let ((variables (?xs-in input)))
    (format om-lisp::*om-stream* "phrase-modality-c: ~A" (length variables))
    (cond
     ((null variables) input)
     (T
      (let* ((transposition-pcs (mapcar #'(lambda (x) (mod x 12)) (om+ modepcs fundamental)))
             (sequence (reverse (sort (remove-duplicates (flatt (mapcar #'(lambda (x) (om+ transposition-pcs x)) '(-12 0 12 24 36 48 60 72 84 96 112 124)))) #'<))))
        
        (if (or (null variables) (null sequence))
            T
          (progn
            (dolist (x variables)
              (assert! (memberv x sequence))
              (format om-lisp::*om-stream* "."))
      
            (format om-lisp::*om-stream* "~%")
            input)))))))

(defmethod! ?items!in (list sequence &key numeric fast-crosscheck)
  (t2l::items!inv list sequence :numeric numeric :fast-crosscheck fast-crosscheck))

(defmethod! ?integers-in (list sequence) ; delete
  (labels
      ((all-members-of (xs sequence)
         (cond ((null xs) T)
               ((cdr xs)
                (?and
                 (all-members-of (list (car xs)) sequence)
                 (all-members-of (cdr xs) sequence)))
               (T (t2l::member-of-number-sequencev (car xs) sequence)))))
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

(defmethod! ?integers!in (list sequence)
   (labels
      ((all-not-members-of (xs sequence)
         (cond ((null xs) T)
               ((cdr xs)
                (?and
                 (all-not-members-of (list (car xs)) sequence)
                 (all-not-members-of (cdr xs) sequence)))
               (T (t2l::not-member-of-number-sequencev (car xs) sequence)))))
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

(defmethod! ?floor (x) (s:funcallv #'floor x))
(defmethod! ?ceiling (x) (s:funcallv #'ceiling x))
(defmethod! ?expt (a b) (t2l::exptv a b)) ;;;;;;;; 
(defmethod! ?funcall (fn &rest x) :icon 147 (apply #'screamer:funcallv (append (list fn) x)))
(defmethod! ?apply (fn x &rest xs) :icon 147 (apply #'screamer:applyv (append (list fn x) xs)))
(defmethod! ?counttrues (&rest xs) :icon 235 (apply #'screamer:count-truesv (if (listp xs) xs (list xs))))
(defmethod! ?countof (x list &key numeric) :icon 235 (apply #'screamer:count-truesv (mapcar #'(lambda (y) (if numeric (?= x y) (?equal x y))) list)))
(defmethod! symkeys (key sym &rest entries)
  :icon 230
  :doc ""
  (mapcar #'(lambda (xs)
              (let ((k (car xs))
                    (e (if (cdr xs) (cadr xs) nil)))
                (cons k e)))
          (nsucc (append (list key sym) entries) 2 :step 2)))
;; symxlat
(defmethod! symxlat (x map) :icon 147 (t2l::symxlat x map))
(defmethod! ?symxlat (k map) :icon 147 (t2l::symxlatv k map))
(defmethod! ?xlatsym (k map) :icon 147 (t2l::xlatsymv k map))
(defmethod! ?intxlat (k map) :icon 147 (t2l::intxlatv k map))
(defmethod! ?xlatint (k map) :icon 147 (t2l::xlatintv k map))

(defmethod! ?input-pcsin (input fundamental &rest xs) (apply #'input-pcsin-mode (append (list input fundamental) xs))) ; deleted

(defmethod! ?input-pcsin-mode (input fundamental &rest xs) ; replaced
  (let* ((fundamental (or fundamental 0))
         (variables (?xs-in input)))
    (labels
        ((in-mode (pcs)
           (?items-in (?list% variables 12) (?list% (?list+ pcs fundamental) 12) :numeric T)))
    (cond
     ((null xs) T)
     (T (map?or #'in-mode xs))))))



            
(defmethod! next-solver-input (&optional catalog)
  :icon 215
  (let ((catalog (or catalog :backup)))
    (cond
     ((null (find catalog (mapcar #'car t2l::*backup-solver-input*))) nil)
     ((null (cdr (assoc catalog t2l::*backup-solver-input*))) nil)
     (T
      (let ((next (car (cdr (assoc catalog t2l::*backup-solver-input*)))))
        (rplacd (assoc catalog t2l::*backup-solver-input*)
                (cdr (cdr (assoc catalog t2l::*backup-solver-input*))))
        next)))))

(defmethod! next-solver-output (&optional catalog) :icon 215 (t2l::next-solver-output catalog))
(defmethod! prev-solver-output (&optional catalog) :icon 215 (t2l::prev-solver-output catalog))
(defmethod! reset-solver-output (&optional catalog) :icon 340 (t2l::reset-solver-output))
(defmethod! reset-solver-registry () :icon 340 (t2l::reset-solver-registry))

(defmethod! collect-to ((self store) value &key first)
  (labels
      ((append-to (store value)
         (cond
          ((null (slot-value self 'value))
           (setf (slot-value self 'value) (list value))
           (set-slot self 'value (slot-value self 'value)))
          ((not (consp (slot-value self 'value)))
           (setf (slot-value self 'value) (list (slot-value self 'value)))
           (append-to self value))
          ((and (consp (slot-value self 'value))
                (cdr (slot-value self 'value))
                (not (consp (cdr (slot-value self 'value))))) ; x . y
           (setf (cdr (slot-value self 'value)) (list (slot-value self 'value)))
           (append-to self value))
          (T
           (rplacd (last (slot-value self 'value)) (list value))
           (set-slot self 'value (slot-value self 'value))))
         value)
       (insert-into (store value)
         (cond
          ((and (slot-value self 'value)
                (not (consp (slot-value self 'value))))
           (setf (slot-value self 'value) (list (slot-value self 'value)))
           (insert-into self value))
          (T 
           (push value (slot-value self 'value))    
           (set-slot self 'value (slot-value self 'value))))
         value))
    (if first
        (insert-into self value)
      (append-to self value))))

(defmethod! append-to ((self store) value) (collect-to store value))


(defmethod! reset-store ((self store))
  (set-slot self 'value nil))

;;;; misc

(defmethod! compute-fm-sidebands (frequency c-ratio m-ratio m-index)
  :doc "from 'The Rule of Thumb Method of Calculating the Position of Sidebands' (https://ccrma.stanford.edu/software/clm/compmus/clm-tutorials/fm2.html)"
  :indoc '("center frequency" "carrier-ratio" "modulator-ratio" "modulation-index")
  (let ((upper-sideband) (lower-sideband) xs)
    (loop for k from 1 to (+ m-index 2) do
            (setf upper-sideband (+ (* frequency c-ratio) (* k frequency m-ratio)))
            (setf lower-sideband (abs (- (* frequency c-ratio) 
                                         (* k frequency m-ratio))))
            (push (list k upper-sideband lower-sideband) xs)
            (print (format nil "~%with k at ~A, upper sideband: ~A Hz; lower sideband: ~A Hz" 
                    k   upper-sideband         lower-sideband)))
    xs))

(defmethod! ?alleq (list)
  :icon 235
  (map?and #'(lambda (xs) (?equal (car xs) (cadr xs)))
           (list2comb list)))

(defmethod! ?all!eq (list) :icon 235 (?alldifferent list))

(defmethod! ?alldifferent (list)
  :icon 235   
  (map?and #'(lambda (xs) (?not (?equal (car xs) (cadr xs))))
           (list2comb list)))

(defmethod! write-textfile (input label ext &optional timezone) (t2l::write-textfile input label ext timezone))

(defmethod! format-alert (text &rest args) (apply #'t2l::format-alert (append (list text) args)))

(defmethod! cartx2 (xs) :icon 235 (t2l::cartx2 xs))

(defmethod! nsucc (input n &key step list-padding pad-character) :icon 235 (t2l::nsucc input n :step step :list-padding list-padding :pad-character pad-character))

(defmethod! remove-successive-duplicates (list &key test) :icon 235 (t2l::remove-successive-duplicates list :test test))

(defmethod! all?variables-in (list) :icon 235 (t2l::all?variables-in list))

(defmethod! list-excerpt (list percent &optional items)
   (om::list-excerpt list percent items))

(defmethod! has-null-values (x) :icon 235 :doc "T if x either has or is a null value" (t2l::has-null-values x))

(defmethod! list-excerpt (list percent &optional items)
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

(defmethod! ?variable-names (xs) (t2l::variable-names-in xs))

(defmethod! ?variable-name (x) (cond 
                                ((null x) nil)
                                ((screamer::variable? x) (screamer::variable-name x))
                                (T x)))


(defmethod! apply-definition-in-segments (sequence selector rule-definition)
  :icon 908            
  :doc "selector function ...

rule-definition function inputs
- variables as flat lists + continuation
- segment
- next-segment"
  (let ((segments (group-by-motion-type (car sequence) (cadr sequence))))
    (cond 
     ((null segments) T)
     (T
      (maplist?and
       #'(lambda (xs)
           (if (funcall selector (car xs))
               (funcall rule-definition
                        (mapcar #'(lambda (ys zs) (append ys (list zs))) 
                                (flatten-seqc (car xs)) 
                                (if (cdr xs) 
                                    (mapcar #'car (flatten-seqc (cadr xs)))
                                  (mapcar #'(lambda (y) nil) sequence)))
                        (car xs)
                        (if (cdr xs) (cadr xs) nil))
             T))
       segments)))))

(defmethod! all-defn-in-segments (sequence selector rule-definition &rest additional-definitions)
  :icon 908            
  :doc "selector function ...

rule-definition function inputs
- variables as flat lists + continuation
- segment
- next-segment"
  (map?and #'(lambda (defn) (if defn 
                                (apply-definition-in-segments sequence selector defn)
                              T)) (append (list rule-definition) additional-definitions)))

(defmethod! any-defn-in-segments (sequence selector rule-definition &rest additional-definitions)
  :icon 908            
  :doc "selector function ...

rule-definition function inputs
- variables as flat lists + continuation
- segment
- next-segment"
  (map?or #'(lambda (defn) (if defn 
                               (apply-definition-in-segments sequence selector defn)
                             T)) (append (list rule-definition) additional-definitions)))

(defmethod! any-defn-each-segment (sequence selector rule-definition &rest additional-definitions)
  :icon 908
  (let ((segments (group-by-motion-type (car sequence) (cadr sequence)))
        (rule-definition-list (append (list rule-definition) additional-definitions)))
    (cond 
     ((null segments) T)
     (T
      (maplist?and
       #'(lambda (xs)
           (if (funcall selector (car xs))
               (map?or
                #'(lambda (defn)
                    (funcall defn
                             (mapcar #'(lambda (ys zs) (append ys (list zs))) 
                                     (flatten-seqc (car xs)) 
                                     (if (cdr xs) 
                                         (mapcar #'car (flatten-seqc (cadr xs)))
                                       (mapcar #'(lambda (y) nil) sequence)))
                             (car xs)
                             (if (cdr xs) (cadr xs) nil)))
                rule-definition-list)
             T))
       segments)))))

(defmethod! transform-input-and-apply-rules (input transform-function &rest rules-functions)
  :icon 908
  (cond 
   ((null rules-functions) T)
   (T
    (let ((transfn (if transform-function
                       transform-function
                     #'(lambda (xs) xs))))
      (let ((transin (funcall transfn input)))
        (map?and #'(lambda (rulefn) 
                     (cond ((null rulefn) T)
                           ((functionp rulefn) (funcall rulefn transin))
                           ((screamer::variable? rulefn) rulefn)
                           (T 
                            (format om-lisp::*om-stream* "Error ~A is not a function or screamer-variable T or F; ignoring~%" rulefn)
                            (not (null rulefn)))))
                 rules-functions))))))
   
(cl:defun map-func (fn list &key with-levels level-min level-max ignore-null-input) 
  (t2l::map-func fn list 
                 :with-levels with-levels
                 :level-min level-min
                 :level-max level-max 
                 :ignore-null-input ignore-null-input))

(cl:defun map2func (fn list1 list2) (t2l::map2func fn list1 list2))

(cl:defun map3func (fn list1 list2 list3) (t2l::map3func fn list1 list2 list3))


(defmethod! flatt (lst &optional level) :icon 235 (t2l::flatt lst level))

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

(defmethod! last-item (list)
  :icon 235
  (cond ((null list) nil)
        ((not (listp list)) list)
        (t
         (car (reverse list)))))

(defun combinations-of2 (xs) ; delete
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
      (internal xs)
      (reverse stack))))

(defmethod! alleq (list &key test)
  :icon 235
  (every #'(lambda (xs) (funcall (or test #'equal) (car xs) (cadr xs)))
         (t2l::combinations-of2 list)))

(defun map-by-level (fn tree &key level-max)
  "Recursively applies fn to list elements (see 'map-func'). map-by-level accepts a function with two inputs - the first input is the list-element from the 'tree' input being processed. The second input indicates the 'depth' of that element within the list. The function counts from 1; elements of a simple list that does not contain any other list will be labelled level 1." 
  (t2l::map-by-level fn tree :level-max level-max))

(defmethod! list-depth (list) :icon 235 (t2l::list-depth list))
  

(defmethod! list-structure-equal (list1 list2 &key test) :icon 235 (t2l::list-structure-equal list1 list2 :test test))

(defmethod! test-returns-t (x y) T)

(defmethod! group-sequence-on (test sequence &key key) (t2l::group-sequence-on test sequence :key key))

; (defmethod! flatten-seqc (list &optional enable-suspensions) :icon 235 (t2l::flatten-seqc list enable-suspensions))

(defmethod! fuseseqc (seqc1 seqc2) :icon 230 (t2l::fuseseqc seqc1 seqc2))
 
(defmethod! mergevoiceup (voices new-voice) :icon 231 (append voices (list new-voice)))

(defmethod! mergevoicedown (new-voice voices) :icon 231 (append (list new-voice) voices))

(defmethod! group-by-motion-type (voice1 voice2)
  :icon 235
  (let* ((level1-atoms->lists
          (or (null voice1)
              (atom voice1)
              (null voice2)
              (atom voice2)))
         (sequence (if level1-atoms->lists
                       (mapcar #'(lambda (voice)
                                   (cond
                                    ((null voice) (list voice))
                                    ((atom voice) (list voice))
                                    ((= 1 (length voice)) voice)
                                    (T (list voice))))
                               (list voice1 voice2))
                     (list voice1 voice2))))
    (let ((groups (mat-trans sequence)))
      groups)))

(defmethod! apply-definitions-in-segments (sequence selector mode defn &rest definitions)
  :icon 908
  :initvals '(nil nil :ALL nil nil)
  :menuins '((2 (("all" :ALL) ("any" :ANY))))
  (let (fragments-registry)
    (labels
        ((group-sequence (sequence)
           (mapcar
            #'unflatten
            (mapcar #'mat-trans
                    (group-sequence-on
                     (let ((i -1))
                       #'(lambda (xs ys)
                           (block test
                             (print (format nil ">>> xs: ~A ys: ~A" xs ys))
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
         (unflatten (seqc)
           (mapcar #'(lambda (voice) 
                       (cond 
                        ((= 1 (length voice)) (car voice))
                        (T voice)))
           (mapcar #'remove-consecutive-duplicates seqc)))
         (continuation-to (seqc next)
           (let ((next 
                  (if next 
                      (mapcar #'list (mapcar #'car (mapcar #'flat (mapcar #'list next))))
                    (create-list (length seqc) 'nil))))
             (let ((continued (mat-trans (list seqc next))))               
               (print (format nil "continuation-to seqc: ~A next: ~A is ~A" seqc next continued))
               continued)))
         (lookup-fragment (xs) (find xs fragments-registry :test #'equal))
         (register-fragment (key seqc next)
           ; (print (format nil "register-fragment key: ~A seqc: ~A next: ~A" key seqc next))
           (let ((entry (lookup-fragment (list key seqc next))))
             (cond ((null entry) 
                    (push (list key seqc next) fragments-registry))
                   (T entry))))
         (firsts (sequence) (mapcar #'car (mapcar #'list (mapcar #'flatt sequence)))))


      (maplist #'(lambda (sequence-list)
               (let* ((seqc1 (car sequence-list))
                     (seqc2 (if (cdr sequence-list) (cadr sequence-list) nil)))

                 (mapcar #'(lambda (seqc-flattened seqc-voice12 next-voice12)
                               ; (print (format nil "~% > seqc-flattened ~A~% > seqc-voice12 ~A~% > next-voice12 ~A" seqc-flattened seqc-voice12 next-voice12))
                               
                             ; (print (format nil "OK1 K: ~A S: ~A N: ~A" seqc-flattened seqc-voice12 next-voice12))
                             (register-fragment seqc-flattened seqc-voice12 next-voice12)
                             T)
                          (combinations-of2 (flatten-seqc (continuation-to seqc1 seqc2)))
                          (combinations-of2 seqc1)
                          (combinations-of2 seqc2))))
        (group-sequence sequence))

      (mapcar #'(lambda (voice12x)
                   (maplist #'(lambda (sequence-list)
                                (let* ((seqc1 (car sequence-list))
                                        (seqc2 (if (cdr sequence-list) (cadr sequence-list) nil))
                                        (key (flatten-seqc (continuation-to seqc1 seqc2))))
                                  ; (print (format nil "OK2 K: ~A S: ~A N: ~A" key seqc1 seqc2))
                                  (register-fragment key seqc1 seqc2)
                                  T))
                            (group-sequence voice12x)))
              (combinations-of2 sequence))

      (progn
        (mapcar #'(lambda (entry)
                    (print (format nil " > registry entry: Key ~A Seqc ~A Next ~A" (car entry) (cadr entry) (caddr entry))))
                fragments-registry))

      (if (null fragments-registry)
          T
        (map?and #'(lambda (entry)
                     (let ((key (car entry))
                           (seqc (cadr entry))
                           (next (caddr entry)))
                       (if (funcall selector seqc)
                           (funcall defn key seqc next)
                         T)))
                 fragments-registry)))))
      
(defmethod! sequence->poly (sequence duration-values time-signature modulus &key tempo)
  :icon 949
  (unless tempo
    (setf tempo 60))
  (make-instance
   'om::poly
   :voices (reverse
            (mapcar
             #'(lambda (voice-midic voice-rhythm-tree)
                 (make-instance
                  'om::voice
                  :chords voice-midic
                  :tree voice-rhythm-tree
                  :tempo tempo))
             (mapcar #'(lambda (xs) (om* 100 (remove nil (flatt xs)))) sequence)
             (t2l::scale-ms-events (mapcar #'flatt duration-values)
                                   time-signature 
                                   modulus)))))   

(defmethod! sequence->voices (sequence duration-values time-signature modulus &key tempo)
  :icon 949
  (unless tempo
    (setf tempo 60))
  (reverse
   (mapcar
    #'(lambda (voice-midic voice-rhythm-tree)
        (make-instance
         'om::voice
         :chords voice-midic
         :tree voice-rhythm-tree
         :tempo tempo))
    (mapcar #'(lambda (xs) (om* 100 (remove nil (flatt xs)))) sequence)
    (t2l::scale-ms-events (mapcar #'flatt duration-values)
                          time-signature 
                          modulus))))

(defmethod! timepoints->voice-obj (midinotes duration-values time-signature modulus &key tempo)
  :icon 949
  (unless tempo
    (setf tempo 60))
  (make-instance 'om::voice
                 :chords (om* 100 (remove nil (flatt midinotes)))
                 :tree (car (t2l::scale-ms-events (list (flatt duration-values)) time-signature modulus))
                 :tempo tempo))
  

(defmethod! listprint (xs)
  (cond ((null xs) (print nil))
        ((atom xs) (print xs))
        (T (mapcar #'print xs)))
  (cond ((null xs) 0)
        ((atom xs) 1)
        (T (length xs))))

(defmethod! remove-consecutive-duplicates (list &key test)
  :icon 235
  (t2l::remove-consecutive-duplicates list :test test))

(defmethod! mc->midi ((self note))
   (if (null self) (error "No NOTE object to process! (null)"))
   (if (null (midic self)) (error "No notes to process in object. Use the editor to add a note (~A)" self))
  (labels
      ((convert (midic) 
         (if (= 0 (mod midic 100)) 
             (multiple-value-bind (f r) (floor (/ midic 100)) 
               f)
           (/ midic 100))))
    (convert (midic self))))

(defmethod! mc->midi ((self chord-seq))
  (if (null self) (error "No NOTE object to process! (null)"))
  (if (null (lmidic self)) (error "No notes to process in object. Use the editor to add a note (~A)" self))
  (labels
      ((convert (midic) 
         (if (= 0 (mod midic 100)) 
             (multiple-value-bind (f r) (floor (/ midic 100)) 
               f)
           (/ midic 100))))
    (map-func #'convert (flatt (lmidic self)))))

;;; FLATTEN-SEQC
(defmethod! flatten-seqc (list)
  :icon 261
  (let ((list (map-func #'(lambda (x) (if x x "nil")) list)))
    (labels 
        ((flatn (x)
           (cond ((null x) nil)
                 (t (append (car x) (flatn (cdr x))))))

         (flat1 (list)
           (cond ((null list) nil)
                 ((and (listp list)
                       (every #'listp list))
                  (apply #'append list))
                 ((listp list)
                  (flat1 (mapcar #'(lambda (x) (cond ((listp x) x)
                                                     (t (list x))))
                                 list)))
                 (t list)))
       
         (contains-list (list)
           (cond ((null list) nil)
                 ((listp list) (or (listp (car list))
                                   (contains-list (cdr list))))
                 (t nil)))

         (prcs-sublist (x) 
           (cond ((contains-list x) 
                  (match-sublist-lens (atoms2list x)
                                      (find-largest-sublist-len (atoms2list x))))
                 (t (mapcar 'list x))))
                  
                  
         (find-largest-sublist-len (x) 
           (cond ((null (car x)) 0)
                 ((listp (car x)) (max (length (car x)) 
                                       (find-largest-sublist-len (cdr x))))
                 ((atom (car x)) (max 1 
                                      (find-largest-sublist-len (cdr x))))
                 (t 0)))

         (match-sublist-lens (x len) 
           (cond ((null (car x)) nil)
                 ((< (length (car x)) len) 
                  (append (list (append (car x)
                                        (make-sequence 'list
                                                       (- len (length (car x)))
                                                       :initial-element (car (reverse (flat (car x)))))))
                          (match-sublist-lens (cdr x) len)))
                 (t (append (list (car x)) (match-sublist-lens (cdr x) len)))))
                      
         (atoms2list (x) 
           (cond ((null (car x)) nil)
                 ((listp (car x)) (append (list (car x)) (atoms2list (cdr x))))
                 ((atom (car x)) (append (list (list (car x))) (atoms2list (cdr x))))
                 (t nil))))
    
      (let ((flat-list 
             (mapcar #'flatn
                     (mat-trans (mapcar #'(lambda (x)                                                                  
                                            (cond ((contains-list (flat1 x))
                                                   (flatten-seqc x))
                                                  (t (prcs-sublist x))))
                                        (mat-trans (prcs-sublist list)))))))
        (map-func #'(lambda (x) (if (and (stringp x)
                                         (string= "nil" x)) nil x)) flat-list)))))

(defmethod! merge-sequences (sequence-a sequence-b)
  :icon 230
  (labels ((sequence-contains-single-atoms (xs) (some #'(lambda (x) (not (consp x))) xs))
           (process (sequence) 
             (if (sequence-contains-single-atoms sequence)
                 (mapcar #'list sequence)
               sequence)))
    (mapcar #'(lambda (list-a list-b) (append list-a list-b))
            (process sequence-a)
            (process sequence-b))))

(defmethod! merge-sequence-list (&rest list)
  :icon 230
  (mat-trans (flat1 (mapcar #'mat-trans list))))

(defmethod! group-and-apply-definitions (sequence selector-fn definition &rest defns)
  :icon 908
  (let (fragments)
    (labels
        ((group-sequence (sequence)
           (mapcar
            #'unflatten
            (mapcar 
             #'mat-trans
             (group-sequence-on
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
            (stream-rulests "~A out ~A fragments" (length fragments-to-process) (length fragments))
            (map?and #'(lambda (entry) 
                         (map?and #'(lambda (defn)
                                      (funcall defn (first entry) (second entry) (third entry)))
                                  (append (list definition) defns)))
                     fragments-to-process)))))))                

(defmethod! group-and-apply-definitions!! (sequence selector-fn definition &rest defns)
  :icon 1100
  (let (fragments)
    (labels
        ((group-sequence (sequence)
           (mapcar
            #'unflatten
            (mapcar 
             #'mat-trans
             (group-sequence-on
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
      
      (dolist (voices2x (list2comb sequence))
        (register-fragments (group-sequence voices2x)))
              

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
            (format om-lisp::*om-stream* "group-and-apply-definitions!! ~A out ~A fragments" (length fragments-to-process) (length fragments))
            (dolist (entry fragments-to-process)
              (dolist (defn (append (list definition) defns))
                (format om-lisp::*om-stream* ".")
                (s:assert! (funcall defn (first entry) (second entry) (third entry)))))
            (format om-lisp::*om-stream* "~%")
            sequence))))))


(defmethod! paradigm--enable-variable-cache-map () (t2l::enable-variable-cache-map))
(defmethod! paradigm--disable-variable-cache-map () (t2l::disable-variable-cache-map))

(defmethod! stream-rulests (format-string &rest args)
  (apply #'cl:format (append (list om-lisp::*om-stream* (concatenate 'string " >> " format-string "... ")) args)))
(defmethod! print-warning (str &rest xs) :icon 129 (apply #'t2l::print-warning (append (list str) xs)))
(defmethod! alert2 (input &key label print-label-only) :icon 129 (t2l::alert2 input :label label :print-label-only print-label-only))
(defmethod! print-warnings (&optional x) :icon 129 (t2l::print-warnings x))
(defmethod! hide-warnings () :icon 129 (t2l::hide-warnings))