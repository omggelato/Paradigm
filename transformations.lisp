(in-package :SCREAMER)

(defmacro-compile-time write-map?car-fun (n)
  (let ((name (intern (format nil "MAP~A?CAR-NONDETERMINISTIC-INTERNAL" n) :SCREAMER))
        (arguments (loop for i from 1 while (<= i n) collect
                           (intern (format nil "LIST~A" n)))))
    (export name :SCREAMER)
    `(DEFUN ,name (FN ,@arguments)
       (IF (OR ,@(mapcar #'(lambda (x) `(NULL ,x)) arguments))
           NIL
         (CONS
          (FUNCALL-NONDETERMINISTIC FN ,@(mapcar #'(lambda (x) `(CAR ,x)) arguments))
          (IF (AND ,@(mapcar #'(lambda (x) `(CDR ,x)) arguments))
              (,name FN ,@(mapcar #'(lambda (x) `(CDR ,x)) arguments))
            NIL))))))

(progn 
  (write-map?car-fun 1)
  (write-map?car-fun 2)
  (write-map?car-fun 3)
  (write-map?car-fun 4)
  (write-map?car-fun 5)
  (write-map?car-fun 6)
  (write-map?car-fun 7)
  (write-map?car-fun 8))

(defmacro-compile-time map?car-internal (fn &rest xs)
  `(IF (SOME #'NULL (LIST ,@xs))
       NIL
     (CONS
      (FUNCALL ,fn ,@(mapcar #'(lambda (x) `(CAR ,x)) xs))
      (IF (EVERY #'CDR (LIST ,@xs))
          (MAP?CAR-INTERNAL ,fn ,@(mapcar #'(lambda (x) `(CDR ,x)) xs))
        NIL))))


(defmacro-compile-time map?list-internal (fn &rest xs)
  `(IF (SOME #'NULL (LIST ,@xs))
       NIL
     (CONS
      (FUNCALL ,fn ,@xs)
      (IF (EVERY #'CDR (LIST ,@xs))
          (MAP?LIST-INTERNAL ,fn ,@(mapcar #'(lambda (x) `(CDR ,x)) xs))
        NIL))))

(defmacro-compile-time map?car (fn &rest xs)
  (let ((callfn (intern (format nil "MAP~A?CAR-NONDETERMINISTIC-INTERNAL" (length xs)) :SCREAMER)))
    (if *nondeterministic-context?*
        `(,callfn ,fn ,@xs)
      `(map?car-internal ,fn ,@xs))))

(defmacro-compile-time map?and (fn &rest xs)
  (let ((callfn (intern (format nil "MAP~A?AND-NONDETERMINISTIC-INTERNAL" (length xs)) :SCREAMER)))
    (if *nondeterministic-context?*
        `(,callfn ,fn ,@xs)
      `(map?and-internal ,fn ,@xs))))

(defmacro-compile-time map?or (fn &rest xs)
  (let ((callfn (intern (format nil "MAP~A?OR-NONDETERMINISTIC-INTERNAL" (length xs)) :SCREAMER)))
    (if *nondeterministic-context?*
        `(,callfn ,fn ,@xs)
      `(map?or-internal ,fn ,@xs))))

(defmacro-compile-time map?and-internal (fn &rest xs)
  `(IF (SOME #'NULL (LIST ,@xs))
       T
     (ANDV
      (FUNCALL ,fn ,@(mapcar #'(lambda (x) `(CAR ,x)) xs))
      (IF (EVERY #'CDR (LIST ,@xs))
          (MAP?AND-INTERNAL ,fn ,@(mapcar #'(lambda (x) `(CDR ,x)) xs))
        T))))

(defmacro-compile-time map?or-internal (fn &rest xs)
  `(IF (SOME #'NULL (LIST ,@xs))
       NIL
     (ORV
      (FUNCALL ,fn ,@(mapcar #'(lambda (x) `(CAR ,x)) xs))
      (IF (EVERY #'CDR (LIST ,@xs))
          (MAP?OR-INTERNAL ,fn ,@(mapcar #'(lambda (x) `(CDR ,x)) xs))
        NIL))))

(defmacro-compile-time write-map?and-fun (n)
  (let ((name (intern (format nil "MAP~A?AND-NONDETERMINISTIC-INTERNAL" n) :SCREAMER))
        (arguments (loop for i from 1 while (<= i n) collect
                           (intern (format nil "LIST~A" n)))))
    (export name :SCREAMER)
    `(SCREAMER::DEFUN ,name (FN ,@arguments)
       (IF (OR ,@(mapcar #'(lambda (x) `(NULL ,x)) arguments))
           T
         (ANDV
          (FUNCALL-NONDETERMINISTIC FN ,@(mapcar #'(lambda (x) `(CAR ,x)) arguments))
          (IF (AND ,@(mapcar #'(lambda (x) `(CDR ,x)) arguments))
              (,name FN ,@(mapcar #'(lambda (x) `(CDR ,x)) arguments))
            T))))))

(defmacro-compile-time write-map?or-fun (n)
  (let ((name (intern (format nil "MAP~A?OR-NONDETERMINISTIC-INTERNAL" n) :SCREAMER))
        (arguments (loop for i from 1 while (<= i n) collect
                           (intern (format nil "LIST~A" n)))))
    (export name :SCREAMER)
    `(SCREAMER::DEFUN ,name (FN ,@arguments)
       (IF (OR ,@(mapcar #'(lambda (x) `(NULL ,x)) arguments))
           NIL
         (ORV
          (FUNCALL-NONDETERMINISTIC FN ,@(mapcar #'(lambda (x) `(CAR ,x)) arguments))
          (IF (AND ,@(mapcar #'(lambda (x) `(CDR ,x)) arguments))
              (,name FN ,@(mapcar #'(lambda (x) `(CDR ,x)) arguments))
            NIL))))))

(progn 
  (write-map?and-fun 1)
  (write-map?and-fun 2)
  (write-map?and-fun 3)
  (write-map?and-fun 4)
  (write-map?and-fun 5)
  (write-map?and-fun 6)
  (write-map?and-fun 7)
  (write-map?and-fun 8))

(progn 
  (write-map?or-fun 1)
  (write-map?or-fun 2)
  (write-map?or-fun 3)
  (write-map?or-fun 4)
  (write-map?or-fun 5)
  (write-map?or-fun 6)
  (write-map?or-fun 7)
  (write-map?or-fun 8))

(defmacro-compile-time map?list (fn &rest xs)
  (let ((callfn (intern (format nil "MAP~A?LIST-NONDETERMINISTIC-INTERNAL" (length xs)) :SCREAMER)))
    (if *nondeterministic-context?*
        `(,callfn ,fn ,@xs)
      `(map?list-internal ,fn ,@xs))))

(defmacro-compile-time maplist?and (fn &rest xs)
  (let ((callfn (intern (format nil "MAPLIST~A?AND-NONDETERMINISTIC-INTERNAL" (length xs)) :SCREAMER)))
    (if *nondeterministic-context?*
        `(,callfn ,fn ,@xs)
      `(maplist?and-internal ,fn ,@xs))))

(defmacro-compile-time maplist?and (fn &rest xs)
  (let ((callfn (intern (format nil "MAPLIST~A?OR-NONDETERMINISTIC-INTERNAL" (length xs)) :SCREAMER)))
    (if *nondeterministic-context?*
        `(,callfn ,fn ,@xs)
      `(maplist?or-internal ,fn ,@xs))))

(defmacro-compile-time maplist?and-internal (fn &rest xs)
  `(IF (SOME #'NULL (LIST ,@xs))
       T
     (ANDV
      (FUNCALL ,fn ,@xs)
      (IF (EVERY #'CDR (LIST ,@xs))
          (MAPLIST?AND-INTERNAL ,fn ,@(mapcar #'(lambda (x) `(CDR ,x)) xs))
        T))))

(defmacro-compile-time maplist?or-internal (fn &rest xs)
  `(IF (SOME #'NULL (LIST ,@xs))
       NIL
     (ORV
      (FUNCALL ,fn ,@xs)
      (IF (EVERY #'CDR (LIST ,@xs))
          (MAPLIST?OR-INTERNAL ,fn ,@(mapcar #'(lambda (x) `(CDR ,x)) xs))
        NIL))))

(defmacro-compile-time write-map?list-fun (n)
  (let ((name (intern (format nil "MAP~A?LIST-NONDETERMINISTIC-INTERNAL" n) :SCREAMER))
        (arguments (loop for i from 1 while (<= i n) collect
                           (intern (format nil "LIST~A" n) :SCREAMER))))
    (export name :SCREAMER)
    `(SCREAMER::DEFUN ,name (FN ,@arguments)
       (IF (OR ,@(mapcar #'(lambda (x) `(NULL ,x)) arguments))
           NIL
         (CONS
          (FUNCALL-NONDETERMINISTIC FN ,@arguments)
          (IF (AND ,@(mapcar #'(lambda (x) `(CDR ,x)) arguments))
              (,name FN ,@(mapcar #'(lambda (x) `(CDR ,x)) arguments))
            NIL))))))


(defmacro-compile-time write-maplist?and-fun (n)
  (let ((name (intern (format nil "MAPLIST~A?AND-NONDETERMINISTIC-INTERNAL" n) :SCREAMER))
        (arguments (loop for i from 1 while (<= i n) collect
                           (intern (format nil "LIST~A" n) :SCREAMER))))
    `(SCREAMER::DEFUN ,name (FN ,@arguments)
       (IF (OR ,@(mapcar #'(lambda (x) `(NULL ,x)) arguments))
           T
         (ANDV
          (FUNCALL-NONDETERMINISTIC FN ,@arguments)
          (IF (AND ,@(mapcar #'(lambda (x) `(CDR ,x)) arguments))
              (,name FN ,@(mapcar #'(lambda (x) `(CDR ,x)) arguments))
            T))))))

(defmacro-compile-time write-maplist?or-fun (n)
  (let ((name (intern (format nil "MAPLIST~A?OR-NONDETERMINISTIC-INTERNAL" n) :SCREAMER))
        (arguments (loop for i from 1 while (<= i n) collect
                           (intern (format nil "LIST~A" n)))))
    `(SCREAMER::DEFUN ,name (FN ,@arguments)
       (IF (OR ,@(mapcar #'(lambda (x) `(NULL ,x)) arguments))
           NIL
         (ORV
          (FUNCALL-NONDETERMINISTIC FN ,@arguments)
          (IF (AND ,@(mapcar #'(lambda (x) `(CDR ,x)) arguments))
              (,name FN ,@(mapcar #'(lambda (x) `(CDR ,x)) arguments))
            NIL))))))

(progn 
  (write-map?list-fun 1)
  (write-map?list-fun 2)
  (write-map?list-fun 3)
  (write-map?list-fun 4)
  (write-map?list-fun 5)
  (write-map?list-fun 6)
  (write-map?list-fun 7)
  (write-map?list-fun 8))

(progn 
  (write-maplist?and-fun 1)
  (write-maplist?and-fun 2)
  (write-maplist?and-fun 3)
  (write-maplist?and-fun 4)
  (write-maplist?and-fun 5)
  (write-maplist?and-fun 6)
  (write-maplist?and-fun 7)
  (write-maplist?and-fun 8))

(progn 
  (write-maplist?or-fun 1)
  (write-maplist?or-fun 2)
  (write-maplist?or-fun 3)
  (write-maplist?or-fun 4)
  (write-maplist?or-fun 5)
  (write-maplist?or-fun 6)
  (write-maplist?or-fun 7)
  (write-maplist?or-fun 8))




(defclass mpr-node () 
  ((mpr-name :accessor mpr-name
         :initarg :name 
         :initform nil)
   (mpr-data :accessor mpr-data
         :initarg :data
         :initform nil)
   (mpr-next-nodes :accessor mpr-next-nodes
               :initarg :next-nodes
               :initform nil)))

(cl:defun ?mapprules (list prules
                        &key continuation-mode
                        ordered-partitions-nondeterministic-values-cap
                        symbol-mode
                        get-symbol-list
                        params
                        print-graph-info
                        listdxx)
  "from 'Graph representation of context-free grammars: arXiv:cs/0703015 http://arxiv.org/abs/cs/0703015"
  (let ((prules
         (if listdxx
             prules
           (mapcar #'(lambda (xs)
                       (if (not (cdr xs))
                           xs
                         (append (list (car xs))
                                 (reverse (cdr xs)))))
                   prules))))
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
         (symeq (x y)
           (eql x y))
         (underscore? (x) (or (string= "_" x) (string= "_" (symbol-name x))))
         (last-atom (l)
           (cond ((null l) nil)
                 ((atom l) l)
                 (t (last-atom (car (reverse l))))))
         (push-end (item place)
           (cond
            ((= 0 (length place))
             (push item place))
            (T
             (rplacd (last place) (list item))))
           place)
         (flat (list) (flatt list))
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
                                      (cons sym (make-instance 'mpr-node :name sym)))
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
                                     (mapcar #'mpr-name (mpr-next-nodes (system::cdr-assoc x dmgassoc)))))))
                (labels 
                    ((make-edges (node)
                       (cond ((or-sym? (mpr-name node))
                              (mapcar #'(lambda (sym) 
                                          (push (system::cdr-assoc sym dmgassoc) (mpr-next-nodes node)))
                                      (remove-duplicates
                                       (flat1
                                        (mapcar 
                                         #'cdr
                                         (remove-if-not
                                          #'(lambda (x) (symeq (car x) (mpr-name node)))
                                          dnd)))
                                       :test #'symeq
                                       :from-end t)))
                             ((and-sym? (mpr-name node))
                              (mapcar #'(lambda (sym)
                                          ;(print (list 'and-sym? 'sym sym 'node node))
                                          (if (mpr-next-nodes node)
                                              (push-end (system::cdr-assoc sym dmgassoc) (mpr-next-nodes node))
                                            (setf (mpr-next-nodes node) (list (system::cdr-assoc sym dmgassoc))))
                                          (mpr-next-nodes node))
                                      (flat1
                                       (mapcar 
                                        #'cdr
                                        (remove-if-not
                                         #'(lambda (x) (symeq (car x) (mpr-name node)))
                                         dnd)))))
                             (t nil)))
                     (make-list-input-variable () 
                       (cond (symbol-mode (make-variable))
                             (t (make-variable)))))
                  (mapcar #'make-edges (mapcar #'cdr dmgassoc))
                  (dolist (n (mapcar #'car dmgassoc))
                    (setf (system::cdr-assoc n dmg-sym-assoc) 
                          (mapcar #'mpr-name (mpr-next-nodes (system::cdr-assoc n dmgassoc)))))
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
                                    (let ((rs (mapcar #'findmax (system::cdr-assoc s dmg-sym-assoc))))
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
                                    (let ((rs (mapcar #'findmin (system::cdr-assoc s dmg-sym-assoc))))
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
                                     (remove-if-not #'and-sym? (system::cdr-assoc s dmg-sym-assoc))
                                     (mapcar #'findsyms (remove-if-not #'or-sym? (system::cdr-assoc s dmg-sym-assoc))))
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
                                     (remove-if-not #'terminal-sym? (system::cdr-assoc s dmg-sym-assoc))
                                     (mapcar #'findsyms (remove-if-not #'or-sym? (system::cdr-assoc s dmg-sym-assoc))))))))
                             (remove-duplicates (flat (findsyms or-sym)) :test #'symeq :from-end t)))))
                    (dolist (n (mapcar #'car dmgassoc))
                      (setf (system::cdr-assoc n dmg-wordsize-assoc)
                            (cons (dmg-min-wordsize n)
                                  (dmg-max-wordsize n))))
                    (labels
                        ((min-wordsize (sym) (car (system::cdr-assoc sym dmg-wordsize-assoc)))
                         (max-wordsize (sym) (cdr (system::cdr-assoc sym dmg-wordsize-assoc))))
                      (if print-graph-info
                          (progn
                            (print (format nil "~A graph symbols" (length dmgassoc)))
                            (print (format nil "dmg-sym-assoc ~A" dmg-sym-assoc))
                            (print (format nil "dmg-wordsize-assoc ~A" dmg-wordsize-assoc))
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
                                                  (assert! (equalv var x))
                                                  var))))
                                   list))
                            (rule-card-assoc
                             (mapcar
                              #'(lambda (sym)
                                  (append (list sym)
                                          (mapcar #'(lambda (x) (system::cdr-assoc x dmg-wordsize-assoc))
                                                  (system::cdr-assoc sym dmg-sym-assoc))))
                              (remove-if-not #'and-sym? (mapcar #'car dmgassoc))))
                            (or-sym-xs-assoc nil)
                            (term-sym-xs-assoc nil)
                            (or-and-sym-assoc nil)
                            (or-sym-domain-assoc nil))
                        (dolist (x (remove-if-not #'terminal-sym? (mapcar #'car dmg-sym-assoc)))
                          (setf (system::cdr-assoc x term-sym-xs-assoc) nil))
                        (dolist (x (remove-if-not #'or-sym? (mapcar #'car dmg-sym-assoc)))
                          (let ((and-syms (dmg-and-syms x)))
                            (let ((cards (remove-duplicates
                                          (mapcar #'(lambda (y) (system::cdr-assoc y rule-card-assoc)) 
                                                  (dmg-and-syms x))
                                          :test #'equalp
                                          :from-end t)))
                              (setf (system::cdr-assoc x or-and-sym-assoc)
                                    (mapcar
                                     #'(lambda (c)
                                         (remove-if-not 
                                          #'(lambda (y) 
                                              (equalp c (system::cdr-assoc y rule-card-assoc))) 
                                          and-syms))
                                     cards)))))
                        (dolist (x (remove-if-not #'or-sym? (mapcar #'car dmg-sym-assoc)))
                          (setf (system::cdr-assoc x or-sym-domain-assoc) (dmg-sym-domain x)))
                        (if print-graph-info
                            (progn
                              (print (format nil "___________~%~A" rule-card-assoc))
                              (mapcar #'(lambda (s) (print (format nil "sym: ~A  ~A" s (system::cdr-assoc s rule-card-assoc))))
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
                                   (apply
                                    #'orv
                                    (mapcar
                                     #'(lambda (p)
                                         (apply
                                          #'orv
                                          (mapcar
                                           #'(lambda (r1)
                                               (apply
                                                #'andv
                                                (mapcar
                                                 #'(lambda (x y) (maprule x y))
                                                 p
                                                 (system::cdr-assoc r1 dmg-sym-assoc))))
                                           r)))
                                     xs)))
                                  ((and (terminal-sym? r)
                                        (not (cdr xs)))
                                   (cond
                                    ((assoc (car xs) (system::cdr-assoc r term-sym-xs-assoc))
                                     (system::cdr-assoc (car xs) (system::cdr-assoc r term-sym-xs-assoc)))
                                    (t
                                     (setf (system::cdr-assoc (car xs) (system::cdr-assoc r term-sym-xs-assoc))
                                           (cond
                                            ((or symbol-mode
                                                 (not (numberp r)))
                                             (equalv (car xs) r))
                                            (t 
                                             (=v (car xs) r))))
                                     (system::cdr-assoc (car xs) (system::cdr-assoc r term-sym-xs-assoc)))))
                                  ((terminal-sym? r) nil)
                                  ((and-sym? r) 
                                   (let* ((xs-length (length xs))
                                          (rcard (cond
                                                  ((and continuation-mode
                                                        (< xs-length (length (system::cdr-assoc r rule-card-assoc)))
                                                        (position last-var xs))
                                                   (subseq (system::cdr-assoc r rule-card-assoc) 0 xs-length))
                                                  (t
                                                   (system::cdr-assoc r rule-card-assoc))))
                                          (rcard-length (length rcard)))
                                     (cond
                                      ((< xs-length rcard-length)
                                       nil)
                                      (t 
                                       (let* ((xs-partitions 
                                               (s:all-values (s::n-partitions-of2 rcard list)))
                                              (next-syms
                                               (cond
                                                ((and (= (length xs) 1)
                                                      continuation-mode
                                                      (eq (car xs) last-var))
                                                 (remove-if #'or-sym? (system::cdr-assoc r dmg-sym-assoc)))
                                                (t (system::cdr-assoc r dmg-sym-assoc)))))
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
                                       #'(lambda (x) (maprule (s:all-values (s::n-partitions-of2 (system::cdr-assoc (car x) rule-card-assoc) xs)) x))
                                       (system::cdr-assoc r or-and-sym-assoc))))
                                    (t 
                                     (let ((existing-var (find (cons r xs) or-sym-xs-assoc :key #'car :test #'equalp)))
                                       (cond (existing-var (cadr existing-var))
                                             (t 
                                              (let ((var (apply 
                                                          #'orv
                                                          (mapcar
                                                           #'(lambda (term) (maprule xs term))
                                                           (system::cdr-assoc r or-sym-domain-assoc)))))
                                                (push (list (cons r xs) var) or-sym-xs-assoc)
                                                var)))))))
                                  (t nil))))
                            (assert! (andv (apply #'andv (mapcar #'(lambda (x) (memberv x terminals)) vars)) (maprule vars (mpr-name dmg))))
                            list)))))))))))))))



;; map?func
(defmacro-compile-time map?func (fn &rest xs)
  (let ((callfn (intern (format nil "MAP~A?FUNC-NONDETERMINISTIC-INTERNAL" (length xs)) :SCREAMER)))
    (if *nondeterministic-context?*
        `(,callfn ,fn ,@xs)
      `(map?func-internal ,fn ,@xs))))

(defmacro-compile-time map?func-internal (fn &rest xs)
  `(COND ((EVERY #'NULL (LIST ,@xs)) 
          NIL)
         ((SOME #'NULL (LIST ,@xs))
          (RETURN-FROM MAP?FUNC-INTERNAL nil))
         ((AND ,@(mapcar #'(lambda (x) `(NOT (CONSP ,x))) xs))
          (FUNCALL ,fn ,@xs))
         ((OR ,@(mapcar #'(lambda (x) `(NOT (CONSP ,x))) xs))
          (RETURN-FROM MAP?FUNC-INTERNAL nil))
         ((AND ,@(mapcar #'(lambda (x) `(CONSP (CAR ,x))) xs))
          (CONS (MAP?FUNC-INTERNAL ,fn ,@(mapcar #'(lambda (x) `(CAR ,x)) xs))
                (MAP?FUNC-INTERNAL ,fn ,@(mapcar #'(lambda (x) `(CDR ,x)) xs))))
         ((OR ,@(mapcar #'(lambda (x) `(CONSP (CAR ,x))) xs))
          (RETURN-FROM MAP?FUNC-INTERNAL nil))
         (T
          (CONS (FUNCALL ,fn ,@(mapcar #'(lambda (x) `(CAR ,x)) xs))
                (IF (AND ,@(mapcar #'(lambda (x) `(CDR ,x)) xs))
                    (MAP?FUNC-INTERNAL ,fn ,@(mapcar #'(lambda (x) `(CDR ,x)) xs))
                  NIL)))))

(defmacro-compile-time write-map?func (n)
  (let ((name (intern (format nil "MAP~A?FUNC-NONDETERMINISTIC-INTERNAL" n) :SCREAMER))
        (arguments (loop for i from 1 while (<= i n) collect
                           (intern (format nil "LIST~A" n)))))
    `(SCREAMER::DEFUN ,name (FN ,@arguments)
       (COND ((AND ,@(mapcar #'(lambda (x) `(NULL ,x)) arguments))
              NIL)
             ((OR ,@(mapcar #'(lambda (x) `(NULL ,x)) arguments)) 
              (RETURN-FROM ,name NIL))
             ((AND ,@(mapcar #'(lambda (x) `(NOT (CONSP ,x))) arguments))
              (FUNCALL-NONDETERMINISTIC FN ,@arguments))
             ((OR ,@(mapcar #'(lambda (x) `(NOT (CONSP ,x))) arguments))
              (RETURN-FROM ,name NIL))
             ((AND ,@(mapcar #'(lambda (x) `(CONSP (CAR ,x))) arguments))
              (CONS (,name FN ,@(mapcar #'(lambda (x) `(CAR ,x)) arguments))
                    (,name FN ,@(mapcar #'(lambda (x) `(CDR ,x)) arguments))))
             ((OR ,@(mapcar #'(lambda (x) `(CONSP (CAR ,x))) arguments)) 
              (RETURN-FROM ,name NIL))
             (T
              (CONS (FUNCALL-NONDETERMINISTIC FN ,@(mapcar #'(lambda (x) `(CAR ,x)) arguments))
                    (IF (AND ,@(mapcar #'(lambda (x) `(CDR ,x)) arguments))
                        (,name FN ,@(mapcar #'(lambda (x) `(CDR ,x)) arguments))
                      NIL)))))))

(progn
  (write-map?func 1)
  (write-map?func 2)
  (write-map?func 3)
  (write-map?func 4)
  (write-map?func 5)
  (write-map?func 6)
  (write-map?func 7)
  (write-map?func 8))

;; map-levels
(defmacro-compile-time map?levels (fn &rest xs)
  (let ((callfn (intern (format nil "MAP~A?LEVELS-NONDETERMINISTIC-INTERNAL" (length xs)) :SCREAMER)))
    (if *nondeterministic-context?*
        `(,callfn ,fn 0 ,@xs)
      `(map?levels-internal ,fn 0 ,@xs))))

(defmacro-compile-time map?levels-internal (fn level &rest xs)
  `(COND ((EVERY #'NULL (LIST ,@xs)) 
          NIL)
         ((SOME #'NULL (LIST ,@xs))
          (RETURN-FROM MAP?LEVELS-INTERNAL nil))
         ((AND ,@(mapcar #'(lambda (x) `(NOT (CONSP ,x))) xs))
          (FUNCALL ,fn ,@xs ,level))
         ((OR ,@(mapcar #'(lambda (x) `(NOT (CONSP ,x))) xs))
          (RETURN-FROM MAP?LEVELS-INTERNAL nil))
         ((AND ,@(mapcar #'(lambda (x) `(CONSP (CAR ,x))) xs))
          (CONS (MAP?LEVELS-INTERNAL ,fn (1+ ,level) ,@(mapcar #'(lambda (x) `(CAR ,x)) xs))
                (MAP?LEVELS-INTERNAL ,fn ,level ,@(mapcar #'(lambda (x) `(CDR ,x)) xs))))
         ((OR ,@(mapcar #'(lambda (x) `(CONSP (CAR ,x))) xs))
          (RETURN-FROM MAP?LEVELS-INTERNAL nil))
         (T
          (CONS (FUNCALL ,fn ,@(mapcar #'(lambda (x) `(CAR ,x)) xs) ,level)
                (IF (AND ,@(mapcar #'(lambda (x) `(CDR ,x)) xs))
                    (MAP?LEVELS-INTERNAL ,fn ,level ,@(mapcar #'(lambda (x) `(CDR ,x)) xs))
                  NIL)))))

(defmacro-compile-time write-map?levels (n)
  (let ((name (intern (format nil "MAP~A?LEVELS-NONDETERMINISTIC-INTERNAL" n) :SCREAMER))
        (arguments (loop for i from 1 while (<= i n) collect
                           (intern (format nil "LIST~A" n)))))
    `(SCREAMER::DEFUN ,name (FN LEVEL ,@arguments)
       (COND ((AND ,@(mapcar #'(lambda (x) `(NULL ,x)) arguments))
              NIL)
             ((OR ,@(mapcar #'(lambda (x) `(NULL ,x)) arguments)) 
              (RETURN-FROM ,name NIL))
             ((AND ,@(mapcar #'(lambda (x) `(NOT (CONSP ,x))) arguments))
              (FUNCALL-NONDETERMINISTIC FN ,@arguments LEVEL))
             ((OR ,@(mapcar #'(lambda (x) `(NOT (CONSP ,x))) arguments))
              (RETURN-FROM ,name NIL))
             ((AND ,@(mapcar #'(lambda (x) `(CONSP (CAR ,x))) arguments))
              (CONS (,name FN (1+ LEVEL) ,@(mapcar #'(lambda (x) `(CAR ,x)) arguments))
                    (,name FN LEVEL ,@(mapcar #'(lambda (x) `(CDR ,x)) arguments))))
             ((OR ,@(mapcar #'(lambda (x) `(CONSP (CAR ,x))) arguments)) 
              (RETURN-FROM ,name NIL))
             (T
              (CONS (FUNCALL-NONDETERMINISTIC FN ,@(mapcar #'(lambda (x) `(CAR ,x)) arguments) LEVEL)
                    (IF (AND ,@(mapcar #'(lambda (x) `(CDR ,x)) arguments))
                        (,name FN LEVEL ,@(mapcar #'(lambda (x) `(CDR ,x)) arguments))
                      NIL)))))))

(progn
  (write-map?levels 1)
  (write-map?levels 2)
  (write-map?levels 3)
  (write-map?levels 4)
  (write-map?levels 5)
  (write-map?levels 6)
  (write-map?levels 7)
  (write-map?levels 8))

;; map-func
(defun map-func-internal (fn list level level-min level-max with-levels)
  (cond 
   ((not (consp list)) 
    (if (and (or (null level-min)
                 (>= level level-min))
             (or (null level-max)
                 (<= level level-max)))
        (if with-levels
          (funcall fn list level)
         (funcall fn list))
      list))
   ((and level-max (> level level-max)) list) ; abort
   (T
    (cons (map-func-internal fn (car list) (1+ level) level-min level-max with-levels)
          (if (cdr list) (map-func-internal fn (cdr list) level level-min level-max with-levels))))))

(defun map-func (fn list &key with-levels level-min level-max)
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
  (cond
   ((null list) nil)
   (T
    (let ((level-min (if level-min (1- level-min) nil))
          (level-max (if level-max (1- level-max) nil)))
      (map-func-internal fn list 0 level-min level-max with-levels)))))

;; map2func
(defun map2func (fn list1 list2)
  (cond ((and (null list1)
              (null list2)) nil)         

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
         (cons (map2func fn (car list1) (car list2))
               (if (cdr list1) (map2func fn (cdr list1) (cdr list2)))))

        ((or (consp (car list1))
             (consp (car list2)))
         (return-from map2func nil))

        (T
         (cons (funcall fn (car list1) (car list2))
               (if (cdr list1) (map2func fn (cdr list1) (cdr list2)))))))

;; map3func
(defun map3func (fn list1 list2 list3)
  (cond 
   ((and (null list1)
         (null list2)
         (null list3)) nil)

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
    (cons (map3func fn (car list1) (car list2) (car list3))
          (map3func fn (cdr list1) (cdr list2) (cdr list3))))

   ((or (consp (car list1))
        (consp (car list2))
        (consp (car list3)))
    (return-from map3func nil))

   (T
    (cons (funcall fn (car list1) (car list2) (car list3))
          (map3func fn (cdr list1) (cdr list2) (cdr list3))))))

(defun maplcr (fn list) 
  (maplcr-internal fn nil (list (car list)) (cdr list)))

(defun maplcr-internal (fn l c r)
  (append 
   (list (funcall fn l (car c) r))
   (if r                         
       (maplcr-internal fn (append l c) (list (car r)) (cdr r))
     nil)))

(defun map?lcr (fn list) 
  (map?lcr-internal fn nil (list (car list)) (cdr list)))

(defun map?lcr-internal (fn l c r)
  (append 
   (list (funcall-nondeterministic fn l (car c) r))
   (if r                         
       (map?lcr-internal fn (append l c) (list (car r)) (cdr r))
     nil)))


