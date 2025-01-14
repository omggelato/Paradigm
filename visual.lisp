(in-package :om)

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

;;;; additional functions for OM-Backtrack
(in-package :screamer)
(cl:defun interval-in-minutes (start-timestamp end-timestamp)
  (/ (/ (- end-timestamp start-timestamp) 1000) 60))
(cl:defun timestamp-in-minutes (timestamp)
  (/ (/ timestamp 1000) 60))
(cl:defun extract-screamer-variables (xs)
  (cond 
   ((null xs) nil)
   ((not (listp xs))
    (extract-screamer-variables (list xs)))
   (T
    (remove-duplicates (remove-if-not #'variable? (t2l::flatt xs)) :from-end T))))
(cl:defun save-to-solver-output (value)
  (global
   (if (null t2l::*findall2-values*)
       (setf t2l::*findall-last-value-cons* (list value)
             t2l::*findall2-values* t2l::*findall-last-value-cons*)
     (setf (rest t2l::*findall-last-value-cons*) (list value)
           t2l::*findall-last-value-cons* (rest t2l::*findall-last-value-cons*)))))
(cl:defun clear-solver-output ()
    (global
     (progn
         (setf t2l::*findall2-values* '())
         (setf t2l::*findall-last-value-cons* nil))))

;;;; SCREAMER:SOLUTION
(defun ?solution-internal (x &key 
                             force-function
                             cost-fun
                             terminate-test 
                             order 
                             onmatch)
                (let ((count 0))
                  (let ((soln (solution x (t2l::generate-ordering-force-function
                                           :force-function force-function
                                           :cost-fun (if cost-fun
                                                         cost-fun
                                                       #'domain-size)
                                           :terminate-test terminate-test
                                           :order (if order
                                                      order
                                                    #'<)))))
                    (global
                     (setf count (1+ count))
                     (funcall onmatch (list (cons :count count)
                                            (cons :timestamp (get-internal-real-time))
                                            (cons :match soln))))
                    soln)))


(defun ?solution (x &key 
                    force-function
                    cost-fun
                    terminate-test 
                    order 
                    onmatch
                    save-matches-to-solver-output
                    abort-after 
                    fail-after-count 
                    fail-unbound)
  
  (when save-matches-to-solver-output
    (t2l::reset-solver-registry))

  (let* ((start-timestamp (get-internal-real-time))
         (abort-timestamp (if abort-after
                              (+ (* (* abort-after 60) 1000) start-timestamp)
                            -1))
         (match-count 0))
    (let ((terminate-test (cond (abort-after #'(lambda (x)
                                                 (let ((timestamp (get-internal-real-time)))
                                                   (let ((terminate? (or (> timestamp abort-timestamp)
                                                                         (and terminate-test
                                                                              (funcall terminate-test x)))))
                                                     (if terminate?
                                                         (progn
                                                           (print (t2l::format-with-timestamp 
                                                                   " > more than ~A minutes since last match. aborting..." 
                                                                   abort-after))
                                                           (fail)))
                                                     terminate?))))))
          (onmatch (cond (save-matches-to-solver-output
                          (cond (onmatch 
                                 #'(lambda (xs) 
                                     (let ((value (cdr (assoc :match xs))))
                                        (save-to-solver-output value))
                                     (funcall onmatch xs)))
                                (T #'(lambda (xs) 
                                       (let ((value (cdr (assoc :match xs))))
                                         (save-to-solver-output value))))))
                         (T onmatch))))
      (let ((onmatch (cond (abort-after
                            #'(lambda (xs) 
                                (if onmatch (funcall onmatch xs))
                                (global
                                 (setf start-timestamp (get-universal-time))
                                 (setf abort-timestamp (+ (* (* abort-after 60) 1000) start-timestamp)))))
                           (T (if onmatch
                                  onmatch
                                #'(lambda (x) nil))))))
                                
        (cond
         (fail-after-count
          (n-values 
           fail-after-count
           (?solution-internal x 
                               :force-function force-function
                               :cost-fun cost-fun
                               :terminate-test terminate-test
                               :order order 
                               :onmatch onmatch)))
         (T (?solution-internal x 
                                :force-function force-function
                                :cost-fun cost-fun
                                :terminate-test terminate-test
                                :order order 
                                :onmatch onmatch)))))))


(in-package :om)
(defmethod get-boxcallclass-fun ((self (eql '?solution))) 'screamerboxes) 
(defmethod! ?solution (x &key 
                    force-function
                    cost-fun
                    terminate-test 
                    order 
                    onmatch
                    save-matches-to-solver-output
                    abort-after 
                    fail-after-count 
                    fail-unbound)
 :doc "
Documentation from https://nikodemus.github.io/screamer/

"
 :icon 150
 nil)

(in-package :screamer)
(defun bt-group-list (input groups)
  (let ((xs (t2l::an-ordered-partition-of input)))
    (unless (= (length xs) groups) (fail))
    xs))

(in-package :om)
(defmethod get-boxcallclass-fun ((self (eql 'bt-group-list))) 'screamerboxes)
(defmethod! bt-group-list (input groups)
  (s::bt-group-list input groups))


(in-package :screamer)
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

   ((and (car template)
         (listp (car template)))
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
      (append (list (multiple-choice-list-element thischoice T))
              (apply-nondeterministic #'multiple-choice-list (cdr template) choice-list))))
   
   ;; append symbol from template
   ((not (numberp (car template)))
    (append (list (car template)) 
            (apply-nondeterministic #'multiple-choice-list 
                                    (if (cdr template) (cdr template) nil) 
                                    choice-list)))

   ;; not randomized
   (T
    (let ((thischoice (elt choice-list (1- (car template)))))
      (append (list (multiple-choice-list-element thischoice nil))
              (apply-nondeterministic #'multiple-choice-list (cdr template) choice-list))))))



(in-package :om)
(defmethod get-boxcallclass-fun ((self (eql 'choice-box))) 'screamerboxes)
(defmethod get-boxcallclass-fun ((self (eql 'function-choice-box))) 'screamerboxes)
(defmethod get-boxcallclass-fun ((self (eql 'multiple-choice-list))) 'screamerboxes)
(defmethod! choice-box (list) :icon 235 nil)
(defmethod! function-choice-box (functions) :icon 147 nil)
(defmethod! multiple-choice-list (template &rest choice-list) nil)

(in-package :screamer)
(defun fail-unbound (xs) 
  (unless (ground? xs)
    (fail))
  xs)
(in-package :om)
(defmethod! fail-unbound (xs) nil)
(defmethod get-boxcallclass-fun ((self (eql 'fail-unbound))) 'screamerboxes)

(in-package :om)
(defmethod get-boxcallclass-fun ((self (eql 'best-value))) 'screamer-valuation-boxes)
(defmethod get-real-funname ((self (eql 'best-value))) self)
(defmethod! best-value (form1 objective-form &optional (form2 nil form2?))
  :initvals '(nil nil nil)
  :indoc '("form1" "objective-form" "form2")
  :doc "First evaluates OBJECTIVE-FORM, which should evaluate to constraint variable V.

Then repeatedly evaluates FORM1 in non-deterministic context till it fails. If
previous round of evaluation produced an upper bound B for V, the during the
next round any change to V must provide an upper bound higher than B, or that
that change fails.

If the last successful evaluation of FORM produced an upper bound for V,
returns a list of two elements: the the primary value of FORM1 from that
round, and the upper bound of V.

Otherwise if FORM2 is provided, returns the result of evaluating it, or else
calls fails.

Note: this documentation string is entirely reverse-engineered. Lacking
information on just how BEST-VALUE was intended to work, it is hard to tell
what is a bug, an accident of implementation, and what is a feature. If you
have any insight into BEST-VALUE, please send email to
nikodemus@random-state.net.

https://nikodemus.github.io/screamer/#index-best_002dvalue-153"
  nil) ;; (screamer:best-value form1 objective-form form2))


(in-package :om)

;;;; generators
(defmethod! ?template (template &key min max map make-integer make-real)
  :doc "Copies an aggregate object, replacing any symbol beginning with a question mark with a newly created variable. 

If the same symbol appears more than once in x, only one variable is created for that symbol, the same variable replacing any occurrences of that symbol. Thus (template '(a b (?c d ?e) ?e)) has the same effect as: 
            (LET ((?C (MAKE-VARIABLE))
                  (?E (MAKE-VARIABLE)))
              (LIST 'A 'B (LIST C 'D E) E)).

This is useful for creating patterns to be unified with other structures. "
  :numouts 2
  (t2l::om-template template :map map :min min :max max :map map :make-integer make-integer :make-real make-real))

; (defmethod get-boxcallclass-fun ((self (eql 'solution))) 'screamerboxes)
; (defmethod get-real-funname ((self (eql 'solution))) self)
; (defmethod! solution (x force-function)
;   :doc "solution"
  ; (screamer:solution x (screamer:static-ordering #'screamer:linear-force)))

(defmethod! ?=q (x y) nil)
(defmethod get-boxcallclass-fun ((self (eql '?=q))) 'screamerboxes)
(in-package :screamer)
(defun ?=q (x y) (=v x y))
(in-package :om)

(defmethod! make?variable (&optional name) (if name (screamer:make-variable name) (screamer:make-variable)))

(defmethod! make?variables (list &key min max integers-mode floats-mode symbol-mode) :icon 215 :doc "" (t2l::make-screamer-variables list :min min :max max :integers-mode integers-mode :floats-mode floats-mode :symbol-mode symbol-mode))

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
(defmethod! ?variables-in (x) (screamer::variables-in x))


;;;; rules
(defmethod! ?integerp (x) (screamer:integerpv x))
(defmethod! ?realp (x) (screamer:numberpv x))
(defmethod! ?numberp (x) (screamer:numberpv x))
(defmethod! ?booleanp (x) (screamer:booleanpv x))
(defmethod! ?avg (&rest list) (t2l::om/v (apply #'t2l::om+v list) (length list)))
(defmethod! ?and (x &rest ys) (apply #'t2l::omandv (append (list x) ys)))
(defmethod! ?or (x &rest ys) (apply #'t2l::omorv (append (list x) ys)))
(defmethod! ?not (x) (t2l::omnotv x))
(defmethod! ?abs (x) (t2l::omabsv x))
(defmethod! ?% (n d) (t2l::om%v n d))
(defmethod! paradigm--?%-calls-native-function ()
            (setf t2l::*paradigm--modulo-function* t2l::*paradigm--modulo-calls-native-function*)
            t2l::*paradigm--modulo-function*)
(defmethod! paradigm--?%-restricts-bounds ()
            (setf t2l::*paradigm--modulo-function* t2l::*paradigm--modulo-restricts-bounds*)                        t2l::*paradigm--modulo-function*)
(defmethod! ?+ (&rest xs) (apply #'t2l::om+v xs))
(defmethod! ?- (x &rest xs) (apply #'t2l::om-v (append (list x) xs)))
(defmethod! ?* (&rest xs) (apply #'t2l::om*v xs))
(defmethod! ?/ (&rest xs) (apply #'t2l::om/v xs))
(defmethod! ?1+ (x) (?+ 1 x))
(defmethod! ?-1 (x) (?- x 1))
(defmethod! ?< (x y &rest xs) (apply #'t2l::om<v (append (list x y) xs)))
(defmethod! ?> (x y &rest xs) (apply #'t2l::om>v (append (list x y) xs)))
(defmethod! ?<= (x y &rest xs) (apply #'t2l::om<=v (append (list x y) xs)))
(defmethod! ?>= (x y &rest xs) (apply #'t2l::om>=v (append (list x y) xs)))
(defmethod! ?= (x y &rest xs) (apply #'t2l::om=v (append (list x y) xs)))
(defmethod! ?/= (x y &rest xs) (apply #'t2l::om/=v (append (list x y) xs)))
(defmethod! ?equal (x y) (t2l::omequalv x y))
(defmethod! ?eql (xs ys) (t2l::omeqlv xs ys))
(defmethod! ?not-eql (xs ys) (t2l::om!eqlv xs ys))
(defmethod! ?between (x min max) (?and (?>= x min) (?<= x max)))
(defmethod! ?<> (x from to) (?or (?< from x to) (?> from x to)))
(defmethod! ?<>= (x from to) (?or (?<= from x to) (?>= from x to)))
(defmethod! ?max (&rest xs) (apply #'t2l::ommaxv xs))
(defmethod! ?min (&rest xs) (apply #'t2l::omminv xs))
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
(cl:defun list-compare-to-value (fn xs value) (map?and #'(lambda (x) (funcall fn x value)) xs))
(defmethod! ?list< (xs value) (list-compare-to-value #'?< xs value))
(defmethod! ?list<= (xs value) (list-compare-to-value #'?<= xs value))
(defmethod! ?list> (xs value) (list-compare-to-value #'?> xs value))
(defmethod! ?list>= (xs value) (list-compare-to-value #'?>= xs value))
(defmethod! ?list= (xs value) (list-compare-to-value #'?= xs value))
(defmethod! ?list/= (xs value) (list-compare-to-value #'?/= xs value))
(defmethod! ?listeq (xs value) (list-compare-to-value #'?equal xs value))
(defmethod! ?list!eq (xs value) (list-compare-to-value #'(lambda (x) (?not (?equal x value))) xs))
(defmethod! ?all+ (xs) (apply #'t2l::om+v xs)) 
(defmethod! ?all- (xs) (apply #'t2l::om-v xs))
(defmethod! ?all* (xs) (apply #'t2l::om*v xs))
(defmethod! ?all/ (xs) (apply #'t2l::om/v xs))
(defmethod! ?all< (xs) (apply #'t2l::om<v xs))
(defmethod! ?all<= (xs) (apply #'t2l::om<=v xs))
(defmethod! ?all> (xs) (apply #'t2l::om>v xs))
(defmethod! ?all>= (xs) (apply #'t2l::om>=v xs))
(defmethod! ?all/= (xs) (apply #'t2l::om/=v xs))
(defmethod! ?all= (xs) (apply #'t2l::om=v xs))
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
(defmethod! ?all-between (xs min max)
  (map?and #'(lambda (y) (?and (?>= y min) (?<= y max))) xs))
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
; (defmethod get-boxcallclass-fun ((self (eql '?items-in))) 'omboxcall)

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
(defmethod! ?input-pcsin (input fundamental &rest xs) (apply #'input-pcsin-mode (append (list input fundamental) xs)))
(defmethod! ?input-pcsin-mode (input fundamental &rest xs)
  :initvals '((69 62 64 65 67 65) 62 (0 2 3 5 7 8 10))
  :indoc '("voice midinotes" 
           "midinote representing the fundamental (tonic)"
           "untransposed midinote series representing a mode or scale")
   (cond
   ((null xs) T)
   (T
    (labels
        ((%12 (x) (?% x 12))
         (transp?se (xs level) (map-func #'%12 (?list+ xs level))))
         (let* ((sequence input);   (map-func #'(lambda (x) (?an-integer-between 0 127)) input :ignore-null-input T))
                (sequence%12 (mapcar #'%12 (all?variables-in sequence))))
           ;(?and
            ;(?equal input sequence)
            (map?or #'(lambda (ys) (?items-in sequence%12 (transp?se ys fundamental) :numeric T)) xs)
            )))))
    


;         (transp?se (xs level)
;           (map-func #'%12 (?list+ xs level))))
;      (let ((input%12 (mapcar #'%12 (all?variables-in input))))
;        (map?or #'(lambda (xs)
;                    (?items-in input%12 (transp?se xs fundamental) :numeric T))
;                modes))))))
            
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
(defmethod! reset-solver-output (&optional catalog) :icon 340 (t2l::reset-solver-output))
(defmethod! reset-solver-registry () :icon 340 (t2l::reset-solver-registry))


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
   (map?and #'(lambda (xs) (?equal (car xs) (cadr xs)))
           (t2l::combinations-of2 list)))

(defmethod! ?all!eq (list) :icon 235 (?alldifferent list))
(defmethod! ?alldifferent (list)
   (map?and #'(lambda (xs) (?not (?equal (car xs) (cadr xs))))
           (t2l::combinations-of2 list)))

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

(defmethod! group-sequence-on (test sequence) (t2l::group-sequence-on test sequence))

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
                    (group-sequence-on #'(lambda (chord next)
                                           (let ((count 0))
                                             (mapcar #'(lambda (x y) (if (not (equal x y)) (incf count)))
                                                     chord
                                                     next)
                                             (= count 1)))
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
                

(defmethod! group-and-apply-definitions (sequence selector-fn definition &rest defns)
  :icon 908
  (let (fragments)
    (labels
        ((group-sequence (sequence)
           (mapcar
            #'unflatten
            (mapcar 
             #'mat-trans
             (group-sequence-on (let ((i -1))
                     #'(lambda (xs ys)
                         (or (equal xs ys)
                             (let ((count 0)) 
                               (mapcar #'(lambda (x y) (if (not (equal x y)) (incf count))) xs ys)
                               (if (= count 1)
                                   (loop for j from 0 while (< j (length xs)) do
                                           (if (not (equal (elt xs j) (elt ys j)))
                                               (cond 
                                                ((or (= i -1) (= i j))
                                                 (setf i j)
                                                 (return T))
                                                (T 
                                                 (setf i -1)
                                                 (return nil)))))
                                 (progn
                                   (setf i -1)
                                   nil))))))
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

      ; (mapcar #'print fragments)

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

(cl:defun consecutive-open-intervals-2-internal-fn (voice2x)
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
                 ((interval%12 (x y) (?% (?- y x) 12)))
               (let ((chords-repeat (?and (?lists= chord1 chord2)))
                     (interval1 (interval%12 (car chord1) (cadr chord1)))
                     (interval2 (interval%12 (car chord2) (cadr chord2))))
        ; (print (format nil "chord1 ~A chord2 ~A chords-repeat ~A interval1 ~A interval2 ~A" chord1 chord2 chords-repeat interval1 interval2))
        (?or 
         (?and (?not (?and (?= interval1 7) (?= interval2 7)))
               (?not (?and (?= interval1 0) (?= interval2 0))))
         chords-repeat)))))))))
   (mat-trans voice2x)))

(defmethod! paradigm--enable-variable-cache-map () (t2l::enable-variable-cache-map))
(defmethod! paradigm--disable-variable-cache-map () (t2l::disable-variable-cache-map))

(defmethod! stream-rulests (format-string &rest args)
  (apply #'cl:format (append (list om-lisp::*om-stream* (concatenate 'string " >> " format-string "... ")) args)))
(defmethod! print-warning (str &rest xs) :icon 129 (apply #'t2l::print-warning (append (list str) xs)))
(defmethod! alert2 (input &key label print-label-only) :icon 129 (t2l::alert2 input :label label :print-label-only print-label-only))
(defmethod! print-warnings (&optional x) :icon 129 (t2l::print-warnings x))
(defmethod! hide-warnings () :icon 129 (t2l::hide-warnings))