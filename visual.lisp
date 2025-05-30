(in-package :OPENMUSIC)

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
(defmethod get-boxcallclass-fun ((self (eql 'count-truesv))) 'screamerboxes)
(defmethod get-boxcallclass-fun ((self (eql 'known?))) 'screamerboxes)
(defmethod get-boxcallclass-fun ((self (eql 'decide))) 'screamerboxes)
(defmethod get-boxcallclass-fun ((self (eql 'template))) 'screamerboxes)
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
(defmethod get-boxcallclass-fun ((self (eql 'minv))) 'screamerboxes)
(defmethod get-boxcallclass-fun ((self (eql 'maxv))) 'screamerboxes)
(defmethod get-boxcallclass-fun ((self (eql 'a-boolean))) 'screamerboxes)
(defmethod get-boxcallclass-fun ((self (eql 'an-integer))) 'screamerboxes)
(defmethod get-boxcallclass-fun ((self (eql 'an-integer-above))) 'screamerboxes)
(defmethod get-boxcallclass-fun ((self (eql 'an-integer-below))) 'screamerboxes)
(defmethod get-boxcallclass-fun ((self (eql '?template))) 'screamerboxes)
(defmethod get-boxcallclass-fun ((self (eql '?apply))) 'screamerboxes)
(defmethod get-boxcallclass-fun ((self (eql '?funcall))) 'screamerboxes)
(defmethod get-boxcallclass-fun ((self (eql '?solution))) 'screamerboxes)
(defmethod get-boxcallclass-fun ((self (eql 'assert!))) 'screamerboxes)
(defmethod get-boxcallclass-fun ((self (eql 'any!!))) 'screamerboxes)
(defmethod get-boxcallclass-fun ((self (eql 'all!!))) 'screamerboxes)
(defmethod get-boxcallclass-fun ((self (eql 'and!!))) 'screamerboxes)
(defmethod get-boxcallclass-fun ((self (eql 'every!!))) 'screamerboxes)
(defmethod get-boxcallclass-fun ((self (eql 'assert!!-and))) 'screamerboxes)
(defmethod get-boxcallclass-fun ((self (eql 'assert!!))) 'screamerboxes)
(defmethod get-boxcallclass-fun ((self (eql 'apply?cont))) 'screamerboxes)
(defmethod get-boxcallclass-fun ((self (eql 'choice-box))) 'screamerboxes)
(defmethod get-boxcallclass-fun ((self (eql 'function-choice-box))) 'screamerboxes)
(defmethod get-boxcallclass-fun ((self (eql 'bt-group-list))) 'screamerboxes)
(defmethod get-boxcallclass-fun ((self (eql 'multiple-choice-list))) 'screamerboxes)
(defmethod get-boxcallclass-fun ((self (eql 'calltrain1x))) 'screamerboxes)

(defmethod get-boxcallclass-fun ((self (eql 'split-list))) 'screamerboxes)
(defmethod get-boxcallclass-fun ((self (eql 'an-ordered-partition-of))) 'screamerboxes)
(defmethod get-boxcallclass-fun ((self (eql 'a-subset-of))) 'screamerboxes)
(defmethod get-boxcallclass-fun ((self (eql 'a-permutation-of))) 'screamerboxes)
(defmethod get-boxcallclass-fun ((self (eql 'setof))) 'screamerboxes)

(defmethod get-boxcallclass-fun ((self (eql 'prolog-append))) 'screamerboxes)
(defmethod get-boxcallclass-fun ((self (eql 'prolog-difference))) 'screamerboxes)
(defmethod get-boxcallclass-fun ((self (eql 'prolog-first))) 'screamerboxes)
(defmethod get-boxcallclass-fun ((self (eql 'prolog-flatten))) 'screamerboxes)
(defmethod get-boxcallclass-fun ((self (eql 'prolog-heads-and-tails))) 'screamerboxes)
(defmethod get-boxcallclass-fun ((self (eql 'prolog-insert))) 'screamerboxes)
(defmethod get-boxcallclass-fun ((self (eql 'prolog-intersect))) 'screamerboxes)
(defmethod get-boxcallclass-fun ((self (eql 'prolog-last))) 'screamerboxes)
(defmethod get-boxcallclass-fun ((self (eql 'prolog-perm))) 'screamerboxes)
(defmethod get-boxcallclass-fun ((self (eql 'prolog-reverse))) 'screamerboxes)
(defmethod get-boxcallclass-fun ((self (eql 'prolog-rotate))) 'screamerboxes)
(defmethod get-boxcallclass-fun ((self (eql 'prolog-select))) 'screamerboxes)
(defmethod get-boxcallclass-fun ((self (eql 'prolog-split))) 'screamerboxes)
(defmethod get-boxcallclass-fun ((self (eql 'prolog-subset))) 'screamerboxes)
(defmethod get-boxcallclass-fun ((self (eql 'prolog-subset?))) 'screamerboxes)
(defmethod get-boxcallclass-fun ((self (eql 'prolog-takeout))) 'screamerboxes)
(defmethod get-boxcallclass-fun ((self (eql 'prolog-transpose))) 'screamerboxes)
(defmethod get-boxcallclass-fun ((self (eql 'prolog-union))) 'screamerboxes)



(defmethod! ?template (template &optional map)
  :numouts 2
  (multiple-value-bind (xs map) (s::?template template)
    (values xs map)))

(defmethod! rewrite-?template (template map) (s::rewrite-?template template map))

(defmethod! make-screamer-variables (template &optional map)
  :numouts 2
  (multiple-value-bind (xs map) (s::template (rewrite-?template template map))
    (values xs map)))

(defmethod! ?apply (f x &rest xs)
  (apply #'s:applyv (append (list f x) xs)))

(defmethod! ?funcall (f &rest xs)
  (apply #'s:funcallv (append (list f) xs)))

(defmethod! ?solution (input &key force-fun cost-fun terminate-fun order-fun valuation skip cut-after onmatch collect-to abort-after)
  :icon 150            
  (s::?solution input
            :force-fun force-fun
            :cost-fun cost-fun
            :terminate-fun terminate-fun
            :order-fun order-fun
            :valuation valuation
            :skip skip
            :cut-after cut-after         
            :onmatch onmatch
            :collect-to collect-to
            :abort-after abort-after))

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
 -1)

(defmethod! apply?cont (x &rest xs) -1)
(defmethod! and!! (x &rest xs) -1)
(defmethod! every!! (x &rest xs) :icon 161 -1)
(defmethod! any!! (x &rest xs) :icon 161 -1)
(defmethod! all!! (x &rest xs) -1)
(defmethod! assert!!-and (x &rest xs) -1)
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

(defmethod! flatt (x) 
  :icon 235
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

(defmethod! flat1 (list) :icon 235 (s::flat1 list))

(defmethod! has-null-values (x)
  (cond
   ((null x) T) ;; ?
   ((not (consp x)) nil)
   (T
    (block checking
      (map-func #'(lambda (y) 
                    (if (null y) 
                        (return-from checking T)
                      NIL))
                x)
      NIL))))

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

(defmethod! reset-store ((self store)) :icon 237
  (set-slot self 'value nil))

(defmethod! grouplist-nondeterministic (list min-groupsize max-groupsize wpartitioncount wmeangroupsize wdeviation wdistancefrommedian)
  :icon 235 
  :indoc '("list" "min-groupsize" "max-groupsize" "wpartitioncount" "wmeangroupsize" "wdeviation" "wdistancefrommedian")
  :initvals '(nil 1 5 0 1 1 -1)
  (s::grouplist-nondeterministic min-groupsize max-groupsize wpartitioncount wmeangroupsize wdeviation wdistancefrommedian))

(defmethod! has-null-values (x)
  :icon 235            
  (cond
   ((null x) T) ;; ?
   ((not (consp x)) nil)
   (T
    (block checking
      (map-func #'(lambda (y) 
                    (if (null y) 
                        (return-from checking T)
                      NIL))
                x)
      NIL))))

(defmethod! ?xs-in (input) (s::?xs-in input))

(defmethod! ?variables-in (input) (s::?variables-in input))

(defmethod! ?/=any (x sequence) (s::?/=any x sequence))

(defmethod! each?/=any (xs sequence) (s::each?/=any xs sequence))

(defmethod! group-list-on (test sequence &key key)
  (let ((test (or test #'equalp)))
    (let ((test (if key #'(lambda (x y)
                            (funcall test (funcall key x) (funcall key y)))
                  test)))
      (labels
          ((group-sequence (group-list group seqc)                        
             (cond ((null seqc) (append group-list (list group)))
                   ((null group) 
                    (group-sequence group-list (list (car seqc)) (cdr seqc)))
                   ((not (funcall test (last-item group) (car seqc)))
                    (group-sequence (append group-list (list group)) nil seqc))
                   (T (group-sequence group-list (append group (list (car seqc))) (cdr seqc)))))
           (last-item (list) (car (last list))))
        (group-sequence nil nil sequence)))))

(defmethod! symxlat (list map)
  (map-func
   #'(lambda (k) 
       (let ((entry (assoc k map)))
         (if entry (cdr entry) k)))
   list))

(defmethod! list-structure-equal (list1 list2 &key test)
   :icon 235
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

(defmethod! equal-sets? (a b) 
  (and (every #'(lambda (x) (member x b)) a) 
       (every #'(lambda (y) (member y a)) b)))

(defmethod! list-depth (list)
  :icon 235
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

(defmethod! list2comb (list)
  :icon 235 
  (let ((stack nil))
    (labels
        ((internal (xs)
           (cond 
            ((null xs) nil)
            ((cdr xs)
             (mapcar #'(lambda (y)
                         (push (list (car xs) y)
                               stack))
                     (cdr xs))
             (internal (cdr xs)))
            (t t))))
      (internal list))
    (reverse stack)))


(defmethod! fecho (input message &rest args)
  (if (and (null args) (search "~A" message))
      (apply #'format 
             (append (list *om-stream* (concatenate 'string message "~%")) (list input)))
    (apply #'format 
           (append (list *om-stream* (concatenate 'string message "~%")) args)))
  input)

(defparameter *paradigm--print-warnings* nil)

(defmethod! paradigm--print-warnings ()
            (setf s::*echo-stream* om-lisp::*om-stream*)
            (setf *paradigm--print-warnings* 10))
            
(defmethod! paradigm--hide-warnings ()
            (setf s::*echo-stream* om-lisp::*om-stream*)
            (setf *paradigm--print-warnings* nil))

(defmethod! wecho (input message &rest args)
   (if *paradigm--print-warnings* 
       (apply #'fecho (append (list input message) args))))

(defmethod! split-list (list)
  (s::split-list list))

(defmethod! an-ordered-partition-of (x)
  nil)
(defmethod! setof (l)
  nil)
(defmethod! a-subset-of (x)
  nil)
(defmethod! a-permutation-of (list)
  nil)
(defmethod! prolog-append (x y z)
  (s::prolog-append x y z))
(defmethod! prolog-difference (y m z)
  nil)
(defmethod! prolog-first (a b) 
  nil)
(defmethod! prolog-flatten (l f)
  nil)
(defmethod! prolog-heads-and-tails (n l1 l2 l3)
  nil)
(defmethod! prolog-insert (elem list1 list2)
  nil)
(defmethod! prolog-intersect (y m z)
  nil)
(defmethod! prolog-last (a b)
  nil)
(defmethod! prolog-perm (y z)
  nil)
(defmethod! prolog-reverse (x y)
  nil)
(defmethod! prolog-rotate (l r)
  nil)
(defmethod! prolog-select (elem list1 list2)
  nil)
(defmethod! prolog-split (i ht sl bl &key key)
  nil)
(defmethod! prolog-subset (x y)
  nil)
(defmethod! prolog-subset? (x s)
  nil)
(defmethod! prolog-takeout (x y z)
  nil)
(defmethod! prolog-transpose (ms ts)
  nil)
(defmethod! prolog-union (y z w)
  nil)


(defmethod! value-of (x) (s:value-of x))
(defmethod! apply-substitution (x) (s:apply-substitution x))
(defmethod! bound? (x) (s:bound? x))
(defmethod! ground? (x) (s:ground? x))
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
(defmethod! +v (&rest xs) (apply #'s:+v xs))
(defmethod! -v (&rest xs) (apply #'s:-v xs))
(defmethod! *v (&rest xs) (apply #'s:*v xs))
(defmethod! /v (&rest xs) (apply #'s:/v xs))
(defmethod! minv (x &rest xs) (apply #'s:minv (append (list x) xs)))
(defmethod! maxv (x &rest xs) (apply #'s:maxv (append (list x) xs)))
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


;; calculations.lisp
(defmethod get-boxcallclass-fun ((self (eql '?+))) 'screamerboxes)
(defmethod get-boxcallclass-fun ((self (eql '?-))) 'screamerboxes)
(defmethod get-boxcallclass-fun ((self (eql '?*))) 'screamerboxes)
(defmethod get-boxcallclass-fun ((self (eql '?/))) 'screamerboxes)
(defmethod get-boxcallclass-fun ((self (eql '?1+))) 'screamerboxes)
(defmethod get-boxcallclass-fun ((self (eql '?-1))) 'screamerboxes)
(defmethod get-boxcallclass-fun ((self (eql '?%))) 'screamerboxes)
(defmethod get-boxcallclass-fun ((self (eql '?sum))) 'screamerboxes)
(defmethod get-boxcallclass-fun ((self (eql '?list+))) 'screamerboxes)
(defmethod get-boxcallclass-fun ((self (eql '?list-))) 'screamerboxes)
(defmethod get-boxcallclass-fun ((self (eql '?list*))) 'screamerboxes)
(defmethod get-boxcallclass-fun ((self (eql '?list/))) 'screamerboxes)
(defmethod get-boxcallclass-fun ((self (eql '?list%))) 'screamerboxes)
(defmethod get-boxcallclass-fun ((self (eql '?listabs))) 'screamerboxes)
(defmethod get-boxcallclass-fun ((self (eql '?listavg))) 'screamerboxes)
(defmethod get-boxcallclass-fun ((self (eql '?listdx))) 'screamerboxes)
(defmethod get-boxcallclass-fun ((self (eql 'a?boolean))) 'screamerboxes)
(defmethod get-boxcallclass-fun ((self (eql 'a?member-of))) 'screamerboxes)
(defmethod get-boxcallclass-fun ((self (eql 'a?number))) 'screamerboxes)
(defmethod get-boxcallclass-fun ((self (eql 'a?real))) 'screamerboxes)
(defmethod get-boxcallclass-fun ((self (eql 'a?real-above))) 'screamerboxes)
(defmethod get-boxcallclass-fun ((self (eql 'a?real-below))) 'screamerboxes)
(defmethod get-boxcallclass-fun ((self (eql 'a?real-between))) 'screamerboxes)
(defmethod get-boxcallclass-fun ((self (eql 'an?integer))) 'screamerboxes)
(defmethod get-boxcallclass-fun ((self (eql 'an?integer-above))) 'screamerboxes)
(defmethod get-boxcallclass-fun ((self (eql 'an?integer-below))) 'screamerboxes)
(defmethod get-boxcallclass-fun ((self (eql 'an?integer-between))) 'screamerboxes)
(defmethod get-boxcallclass-fun ((self (eql '?count-trues))) 'screamerboxes)

(defmethod! ?+ (x &rest xs) (apply #'s:+v (append (list x) xs)))
(defmethod! ?- (x &rest xs) (apply #'s:-v (append (list x) xs)))
(defmethod! ?* (x &rest xs) (apply #'s:*v (append (list x) xs)))
(defmethod! ?/ (x &rest xs) (apply #'s:/v (append (list x) xs)))
(defmethod! ?1+ (x) (s::?1+ x))
(defmethod! ?-1 (x) (s::?-1 x))
(defmethod! ?% (n d) (s::?% n d))
(defmethod! ?%-restricts-bounds () (s::?%-restricts-bounds))
(defmethod! ?%-calls-native-function () (s::?%-calls-native-function))
(defmethod! a?boolean (&optional name) (if name (s:a-booleanv name) (s:a-booleanv)))
(defmethod! a?member-of (values &optional name) (if name (s:a-member-ofv values name) (s:a-member-ofv values)))
(defmethod! a?number (&optional name) (if name (s:a-numberv name) (s:a-numberv)))
(defmethod! a?real (&optional name) (if name (s:a-realv name) (s:a-realv)))
(defmethod! a?real-above (low &optional name) (if name (s:a-real-abovev low name) (s:a-real-abovev low)))
(defmethod! a?real-below (high &optional name) (if name (s:a-real-belowv high name) (s:a-real-belowv high)))
(defmethod! a?real-between (low high &optional name) (if name (s:a-real-betweenv low high name) (s:a-real-betweenv low high)))
(defmethod! an?integer (&optional name) (if name (s:an-integerv name) (s:an-integerv)))
(defmethod! an?integer-above (low &optional name) (if name (s:an-integer-abovev low name) (s:an-integer-abovev low)))
(defmethod! an?integer-below (high &optional name) (if name (s:an-integer-belowv high name) (s:an-integer-belowv high)))
(defmethod! an?integer-between (low high &optional name) (s:an-integer-betweenv low high name) (s:an-integer-betweenv low high))
(defmethod! ?abs (k) (s::?abs k))
(defmethod! ?avg (&rest xs) (apply #'s::?avg xs))
(defmethod! ?expt (a b) (s:funcallv #'expt a b))
(defmethod! ?floor (x) (s:funcallv #'floor x))
(defmethod! ?ceiling (x) (s:funcallv #'ceiling x))
(defmethod! ?sum (xs) (s::?+ xs))
(defmethod! ?count-trues (list) (apply #'s:count-truesv list))

(defmethod! ?listdx (list)
  :icon 235
  (s::?listdx list))

(defmethod! ?list+ (x value) (s::list+v x value))
(defmethod! ?list- (x value) (s::list-v x value))
(defmethod! ?list* (x value) (s::list*v x value))
(defmethod! ?list/ (x value) (s::list/v x value))
(defmethod! ?list% (x value) (s::list%v x value))

(defmethod! list+v (x value) (s::list+v x value))
(defmethod! list-v (x value) (s::list-v x value))
(defmethod! list*v (x value) (s::list*v x value))
(defmethod! list/v (x value) (s::list/v x value))
(defmethod! list%v (x value) (s::list%v x value))

;; comparisons.lisp
(defmethod get-boxcallclass-fun ((self (eql '?<))) 'screamerboxes)
(defmethod get-boxcallclass-fun ((self (eql '?>))) 'screamerboxes)
(defmethod get-boxcallclass-fun ((self (eql '?<=))) 'screamerboxes)
(defmethod get-boxcallclass-fun ((self (eql '?>=))) 'screamerboxes)
(defmethod get-boxcallclass-fun ((self (eql '?=))) 'screamerboxes)
(defmethod get-boxcallclass-fun ((self (eql '?/=))) 'screamerboxes)
(defmethod get-boxcallclass-fun ((self (eql '?max))) 'screamerboxes)
(defmethod get-boxcallclass-fun ((self (eql '?min))) 'screamerboxes)
(defmethod get-boxcallclass-fun ((self (eql '?list<))) 'screamerboxes)
(defmethod get-boxcallclass-fun ((self (eql '?list<=))) 'screamerboxes)
(defmethod get-boxcallclass-fun ((self (eql '?list>))) 'screamerboxes)
(defmethod get-boxcallclass-fun ((self (eql '?list>=))) 'screamerboxes)
(defmethod get-boxcallclass-fun ((self (eql '?list=))) 'screamerboxes)
(defmethod get-boxcallclass-fun ((self (eql '?list/=))) 'screamerboxes)
(defmethod get-boxcallclass-fun ((self (eql '?and))) 'screamerboxes)
(defmethod get-boxcallclass-fun ((self (eql '?or))) 'screamerboxes)
(defmethod get-boxcallclass-fun ((self (eql 'list?and))) 'screamerboxes)
(defmethod get-boxcallclass-fun ((self (eql 'list?or))) 'screamerboxes)
(defmethod get-boxcallclass-fun ((self (eql '?not))) 'screamerboxes)
(defmethod get-boxcallclass-fun ((self (eql '?equal))) 'screamerboxes)
(defmethod get-boxcallclass-fun ((self (eql '?member))) 'screamerboxes)
(defmethod get-boxcallclass-fun ((self (eql '!member))) 'screamerboxes)
(defmethod get-boxcallclass-fun ((self (eql '?=member))) 'screamerboxes)
(defmethod get-boxcallclass-fun ((self (eql '?/=any))) 'screamerboxes)
(defmethod get-boxcallclass-fun ((self (eql 'each?/=any))) 'screamerboxes)
(defmethod get-boxcallclass-fun ((self (eql '?integerp))) 'screamerboxes)
(defmethod get-boxcallclass-fun ((self (eql '?realp))) 'screamerboxes)
(defmethod get-boxcallclass-fun ((self (eql '?numberp))) 'screamerboxes)
(defmethod get-boxcallclass-fun ((self (eql '?booleanp))) 'screamerboxes)
(defmethod get-boxcallclass-fun ((self (eql 'all-differentv))) 'screamerboxes)
(defmethod get-boxcallclass-fun ((self (eql '?between))) 'screamerboxes)
(defmethod get-boxcallclass-fun ((self (eql '?within))) 'screamerboxes)
(defmethod get-boxcallclass-fun ((self (eql 'within!))) 'screamerboxes)
(defmethod get-boxcallclass-fun ((self (eql '?list<))) 'screamerboxes)
(defmethod get-boxcallclass-fun ((self (eql '?list>))) 'screamerboxes)
(defmethod get-boxcallclass-fun ((self (eql '?list<=))) 'screamerboxes)
(defmethod get-boxcallclass-fun ((self (eql '?list>=))) 'screamerboxes)
(defmethod get-boxcallclass-fun ((self (eql '?list=))) 'screamerboxes)
(defmethod get-boxcallclass-fun ((self (eql '?list/=))) 'screamerboxes)
(defmethod get-boxcallclass-fun ((self (eql '?lists=))) 'screamerboxes)
(defmethod get-boxcallclass-fun ((self (eql '?lists/=))) 'screamerboxes)
(defmethod get-boxcallclass-fun ((self (eql 'one?=))) 'screamerboxes)
(defmethod get-boxcallclass-fun ((self (eql 'one?eq))) 'screamerboxes)
(defmethod get-boxcallclass-fun ((self (eql 'not-one?=))) 'screamerboxes)
(defmethod get-boxcallclass-fun ((self (eql 'each?=oneof))) 'screamerboxes)
(defmethod get-boxcallclass-fun ((self (eql 'group-list-nondeterministic))) 'screamerboxes)
(defmethod! group-list-nondeterministic (list segmentation mode)
  :icon 235
  (s::group-list-nondeterministic list segmentation mode))

(defmethod! ?and (x &rest xs) (apply #'s::?and (append (list x) xs)))
(defmethod! ?or (x &rest xs)  (apply #'s::?or (append (list x) xs)))
(defmethod! ?not (x) (s:notv x))
(defmethod! list?and (xs) (s::list?and xs))
(defmethod! list?or (xs) (s::list?or xs))
(defun ?comparison-argument-list (x list) (flatt (append (list x) list)))
(defmethod! ?< (x &rest xs) (apply #'s:<v (?comparison-argument-list x xs)))
(defmethod! ?> (x &rest xs) (apply #'s:>v (?comparison-argument-list x xs)))
(defmethod! ?<= (x &rest xs) (apply #'s:<=v (?comparison-argument-list x xs)))
(defmethod! ?>= (x &rest xs) (apply #'s:>=v (?comparison-argument-list x xs)))
(defmethod! ?= (x &rest xs) (apply #'s:=v (?comparison-argument-list x xs)))
(defmethod! ?/= (x &rest xs) (apply #'s:/=v (?comparison-argument-list x xs)))
(defmethod! ?list< (xs value) (s::?list< xs value))
(defmethod! ?list<= (xs value) (s::?list<= xs value)) 
(defmethod! ?list> (xs value) (s::?list< xs value)) 
(defmethod! ?list>= (xs value) (s::?list>= xs value))
(defmethod! ?list= (xs value) (s::?list= xs value)) 
(defmethod! ?list/= (xs value) (s::?list/= xs value))

(defmethod! ?items-in (list sequence &key numeric) ; delete
  (let ((sequence (reverse sequence)))
    (labels
        ((numeric? (x) (and x (or (numberp x) (s::variable? x))))
         (number-or-symbol? (x) (or x (s::variable? x)))
         (member? (x) (s:memberv x sequence))
         (each=any (xs)
           (cond 
            ((null xs) T)
            (T (andv (item-in (car xs))
                     (each=any (cdr xs))))))
         (item-in (x) (apply #'s:orv (mapcar #'(lambda (y) (s:=v x y)) sequence))))
      (cond
       ((null list) (s:memberv nil sequence))
       (numeric
        (each=any (remove-duplicates (remove-if-not #'numeric? (flatt (list list))))))
       (T
        (apply #'s:andv (mapcar #'member? (remove-duplicates (remove-if-not #'number-or-symbol? (flatt (list list)))))))))))

(defmethod! ?integerp (x) (s::?integerp x))
(defmethod! ?realp (x) (s::?realp x))
(defmethod! ?numberp (x)  (s::?numberp x))
(defmethod! ?booleanp (x)  (s::?booleanp x))
(defmethod! ?member (x sequence) (s::?member x sequence))
(defmethod! !member (x sequence) (s::!member x sequence))
(defmethod! ?/=any (x sequence) (s::?/=any x sequence))
(defmethod! ?equal (x y) (s::?equal x y))
(defmethod! ?max (x &rest xs) :icon 209 (apply #'s::?max (append (list x) xs)))
(defmethod! ?min (x &rest xs) :icon 209 (apply #'s::?min (append (list x) xs)))
(defmethod! all-differentv (x &rest xs) (apply #'s::all-differentv (append (list x) xs)))
(defmethod! ?between (x min max) (s::?between x min max))
(defmethod! ?within (x min max) (s::?within x min max))
(defmethod! within! (x min max) (s::within! x min max))
(defmethod! ?list< (x value) (s::?list< x value))
(defmethod! ?list<= (x value) (s::?list<= x value))
(defmethod! ?list> (x value) (s::?list> x value))
(defmethod! ?list>= (x value) (s::?list>= x value))
(defmethod! ?list= (x value) (s::?list= x value)) 
(defmethod! ?list/= (x value) (s::?list/= x value))
(defmethod! ?lists= (xs ys) (s::?lists= xs ys))
(defmethod! ?lists/= (xs ys) (s::?lists/= xs ys))
(defmethod! one?= (x sequence) (s::one?= x sequence))
(defmethod! each?=oneof (x sequence) (s::each?=oneof x sequence))
(defmethod! one?eq (x sequence) (s::one?eq x sequence))
;; transformations.lisp
(defmethod get-boxcallclass-fun ((self (eql 'map?car))) 'screamerboxes)
(defmethod get-real-funname ((self (eql 'map?car))) self)
(defmethod get-boxcallclass-fun ((self (eql 'map?list))) 'screamerboxes)
(defmethod get-real-funname ((self (eql 'map?list))) self)
(defmethod get-boxcallclass-fun ((self (eql 'map?and))) 'screamerboxes)
(defmethod get-real-funname ((self (eql 'map?and))) self)
(defmethod get-boxcallclass-fun ((self (eql 'map?or))) 'screamerboxes)
(defmethod get-real-funname ((self (eql 'map?or))) self)
(defmethod get-boxcallclass-fun ((self (eql 'maplist?and))) 'screamerboxes)
(defmethod get-real-funname ((self (eql 'maplist?and))) self)
(defmethod get-boxcallclass-fun ((self (eql 'maplist?or))) 'screamerboxes)
(defmethod get-real-funname ((self (eql 'maplist?or))) self)
(defmethod get-boxcallclass-fun ((self (eql 'map?func))) 'screamerboxes)
(defmethod get-real-funname ((self (eql 'map?func))) self)
(defmethod get-boxcallclass-fun ((self (eql 'map?levels))) 'screamerboxes)
(defmethod get-real-funname ((self (eql 'map?levels))) self)
(defmethod get-boxcallclass-fun ((self (eql 'map?lcr))) 'screamerboxes)
(defmethod get-real-funname ((self (eql 'map?lcr))) self)
(defmethod! map?car (fn &rest xs)
  (apply #'mapcar (append (list fn) xs)))
(defmethod! map?list (fn &rest xs)
  (apply #'maplist (append (list fn) xs)))
(defmethod! map?and (fn &rest xs)
  (reduce #'andv (apply #'mapcar (append (list fn) xs))))
(defmethod! map?or (fn &rest xs)
  (reduce #'orv (apply #'mapcar (append (list fn) xs))))
(defmethod! maplist?and (fn &rest xs)
  (reduce #'andv (apply #'maplist (append (list fn) xs))))
(defmethod! maplist?or (fn &rest xs)
  (reduce #'orv (apply #'maplist (append (list fn) xs))))
(defmethod! map?func (fn &rest xs)
  (error (format nil "MAP?FUNC improper visual-box call: fn: ~A [~A]" fn xs)))
(defmethod! map?levels (fn &rest xs)
  (error (format nil "MAP?LEVELS improper visual-box call: fn: ~A [~A]" fn xs)))

(defmethod! map-func (fn list &key with-levels level-min level-max)
  (s::map-func fn list :with-levels with-levels :level-min level-min :level-max level-max))

(defmethod! map2func (fn list1 list2)
  (s::map2func fn list1 list2))

(defmethod! map3func (fn list1 list2 list3)
  (s::map3func fn list1 list2 list3))

(defmethod! map-levels>= (fn threshold xs)
  (s::map-levels>= fn threshold xs))

(defmethod! map-list-levels>= (fn threshold xs)
  (s::map-list-levels>= fn threshold xs))

(defmethod! map?lcr (fn list)
  (error (format nil "MAP?LCR improper visual-box call: fn: ~A [~A]" fn xs)))

(defmethod! maplcr (fn list) 
  :icon 147
  (labels ((maplcr-internal (l c r)
             (append 
              (list (funcall fn l (car c) r))
              (if r                         
                  (maplcr-internal (append l c)
                                   (list (car r))
                                   (cdr r))
                nil))))
    (maplcr-internal nil (list (car list)) (cdr list))))

(defmethod! nsucc (input n &key step list-padding pad-character)
  :icon 235            
  (cond
   ((null input) nil)
   ((null n) (nsucc input (length input) :step step :list-padding list-padding :pad-character pad-character))
   ((null step) (nsucc input n :step 1 :list-padding list-padding :pad-character pad-character))
   ((= step 0) input)
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

(defmethod! sequenc (x &rest xs)
  :icon 161
  (if xs
      (apply #'sequenc (cdr (append (list x) xs)))
  x))

(defun paradigm--format-timestamp (timestamp)
  (multiple-value-bind
      (second minute hour date month year day-of-week dst-p tz)
      (decode-universal-time timestamp)
    (concatenate 'string
                 (format nil "~2,'0d:~2,'0d:~2,'0d ~d/~2,'0d"
                         hour minute second month date))))

;; mapprules.lisp
; (defmethod get-boxcallclass-fun ((self (eql 'mapprules))) 'screamerboxes)

; Graph representation of context-free grammars
; Alex Shkotin arXiv:cs/0703015 http://arxiv.org/abs/cs/0703015
; 
; jan 2013 
(defmethod! mapprules (list prules
                        &key continuation-mode
                        ordered-partitions-nondeterministic-values-cap
                        symbol-mode
                        get-symbol-list
                        params
                        print-graph-info
                        listdxx)
  (s::mapprules list prules :print-graph-info print-graph-info))




