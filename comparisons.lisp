(in-package :SCREAMER)

(defmacro write-list-compareto-x-function (comparison)
  (let ((name (intern (format nil "LIST~A" (symbol-name comparison)) :SCREAMER))
        (internal-fn (intern (format nil "LIST~A-INTERNAL" (symbol-name comparison)) :SCREAMER)))
    `(PROGN
       (SCREAMER::DEFUN ,internal-fn (X VALUE VARIABLES)
         (COND
          ((NULL X)
           (VALUES T VARIABLES))
          ((OR (NUMBERP X)
               (VARIABLE? X))
           (LET ((BINDING (ASSOC X VARIABLES :TEST #'EQ)))
             (IF BINDING
                 (VALUES (CDR BINDING) VARIABLES)
               (LET ((Y (,comparison X VALUE)))
                 (VALUES Y (CONS (CONS X Y) VARIABLES))))))
          ((CONSP X)
           (MULTIPLE-VALUE-BIND (CAR-X CAR-VARIABLES)
               (,internal-fn (CAR X) VALUE VARIABLES)
             (MULTIPLE-VALUE-BIND (CDR-X CDR-VARIABLES)
                 (,internal-fn (CDR X) VALUE CAR-VARIABLES)
               (VALUES (ANDV CAR-X CDR-X) CDR-VARIABLES))))
          (T (VALUES T VARIABLES))))

       (SCREAMER::DEFUN ,name (X VALUE)
         (MULTIPLE-VALUE-BIND (Y VARIABLES) (,internal-fn X VALUE '())
           Y)))))

(write-list-compareto-x-function <v)
(write-list-compareto-x-function >v)
(write-list-compareto-x-function <=v)
(write-list-compareto-x-function >=v)

(defmacro write-list-check-function (check)
  (let ((name (intern (format nil "LIST-~A" (symbol-name check)) :SCREAMER))
        (internal-fn (intern (format nil "LIST-~A-INTERNAL" (symbol-name check)) :SCREAMER)))
    `(PROGN
       (SCREAMER::DEFUN ,internal-fn (X)
         (COND
          ((NULL X) NIL)
          ((AND (CONSP X) (CDR X))
           (ANDV (,internal-fn (CAR X))
                 (,internal-fn (CDR X))))
          ((CONSP X) (,internal-fn (CAR X)))
          (T (,check X))))

       (SCREAMER::DEFUN ,name (X)
         (MULTIPLE-VALUE-BIND (Y VARIABLES) (,internal-fn (?XS-IN X))
           Y)))))

(write-list-check-function s:integerpv)
(write-list-check-function s:realpv)
(write-list-check-function s:numberpv)
(write-list-check-function s:booleanpv)

(defun ?integerp (x) (list-integerpv x))
(defun ?realp (x) (list-realpv x))
(defun ?numberp (x) (list-numberpv x))
(defun ?booleanp (x) (list-booleanpv x))

(defun ?member-internal (x sequence)
  (cond ((null x) (memberv nil sequence))
        ((and (consp x) (cdr x))
         (andv (?member-internal (car x) sequence)
               (?member-internal (cdr x) sequence)))
        ((consp x) (?member-internal (car x) sequence))
        (T (memberv x sequence))))

(defun ?member (x sequence) 
  (?member-internal (?xs-in x) sequence))

(defun !member-internal (x sequence)
  (cond ((null x) (notv (memberv nil sequence)))
        ((and (consp x) (cdr x))
         (andv (!member-internal (car x) sequence)
               (!member-internal (cdr x) sequence)))
        ((consp x) (!member-internal (car x) sequence))
        (T (notv (memberv x sequence)))))

(defun !member (x sequence) 
  (!member-internal (?xs-in x) sequence))

(defun one?= (x sequence)
  (cond
   ((null sequence) nil)
   (T
    (apply #'orv (map?car #'(lambda (y) (=v x y)) sequence)))))

(defun one?eq (x sequence)
  (cond
   ((null sequence) nil)
   (T (orv (equalv x (car sequence))
           (one?eq x (cdr sequence))))))

;(defun each?=oneof-internal (x sequence)
;  (cond ((null x) nil)
;        ((and (consp x) (cdr x))
;         (andv (each?=oneof-internal (car x) sequence)
;               (each?=oneof-internal (cdr x) sequence)))
;        ((consp x) (each?=oneof-internal (car x) sequence))
;        (T (one?= x sequence))))

(defun each?=oneof (x sequence)
  (apply #'andv (map?car #'(lambda (y) (one?= y sequence)) (?xs-in x))))

(defun ?/=any (x sequence)
  (cond
   ((null sequence) T)
   (T (apply #'andv (map?car #'(lambda (y) (/=v x y)) sequence)))))

;(defun each?/=any-internal (xs sequence)
;  (cond ((null xs) nil)
;        ((and (consp xs) (cdr xs))
;         (andv (each?/=any-internal (car xs) sequence)
;               (each?/=any-internal (cdr xs) sequence)))
;        ((consp xs) (each?/=any-internal (car xs) sequence))
;        (T (?/=any xs sequence))))
(defun each?/=any (x sequence) 
  (apply #'andv (map?car #'(lambda (y) (?/=any y sequence)) (?xs-in x))))

(defun ?comparison-arguments (x list) (flatt (append (list x) list)))
(defun ?< (x &rest xs) (apply #'<v (?oper-arguments x xs)))
(defun ?> (x &rest xs) (apply #'>v (?oper-arguments x xs)))
(defun ?<= (x &rest xs) (apply #'<=v (?oper-arguments x xs)))
(defun ?>= (x &rest xs) (apply #'>=v (?oper-arguments x xs)))
(defun ?= (x &rest xs) (apply #'=v (?oper-arguments x xs)))
(defun ?/= (x &rest xs) (apply #'/=v (?oper-arguments x xs)))
(defun ?max (x &rest xs) (apply #'maxv (?xs-in (append (list x) xs))))
(defun ?min (x &rest xs) (apply #'minv (?xs-in (append (list x) xs))))
(defun ?and (x &rest xs) (apply #'andv (flatt (append (list x) xs))))
(defun ?or (x &rest xs) (apply #'orv (flatt (append (list x) xs))))
(defun ?not (x) (notv x))
(defun ?equal (x y) (equalv x y))

(defun all-differentv-internal (x xs)
  (if (null xs)
      t
    (andv (notv (=v x (car xs)))
          (all-differentv-internal x (cdr xs))
          (all-differentv-internal (car xs) (cdr xs)))))

(defun all-differentv (x &rest xs)
  (let ((list (flatt (append (list x) xs))))
    (all-differentv-internal (car list) (cdr list))))

(defun ?between-internal (x min max)
  (cond ((null x) T)
        ((and (consp x) (cdr x))
         (andv (?between-internal (car x) min max)
               (?between-internal (cdr x) min max)))
        ((consp x) (?between-internal (car x) min max))
        ((and min max) (<=v min x max))
        (min (>=v x min))
        (max (<=v x max))
        (T T)))

(defun ?between (input min max)
  (?between-internal (?xs-in input) min max))

(defun ?within-internal (list min max)
  (if list
      (append
       (list (cond
              ((and min max)
               (andv (>=v (car list) min) (<=v (car list) max)))
              (min (>=v (car list) min))
              (max (<=v (car list) max))
              (T T)))
       (?within-internal (cdr list) min max))
    '(T)))

(defun ?within (xs min max)
  (apply #'andv (?within-internal (reverse (?xs-in xs)) min max)))

(defun within!-internal (list min max)
  (if list
      (progn
        (if min 
            (assert! (>=v (car list) min)))
        (if max 
            (assert! (<=v (car list) max)))
        (within!-internal (cdr list) min max))))

(defun within! (input min max)
  (within!-internal (reverse (?xs-in input)) min max)
  input)

(defun ?list<-internal (x value) (if x (andv (<v (car x) value) (?list< (cdr x) value)) T))
(defun ?list< (x value) (?list<-internal (?xs-in x) value))
(defun ?list<=-internal (x value) (if x (andv (<=v (car x) value) (?list<= (cdr x) value)) T))
(defun ?list<= (x value) (?list<=-internal (?xs-in x) value))
(defun ?list>-internal (x value) (if x (andv (>v (car x) value) (?list> (cdr x) value)) T))
(defun ?list> (x value) (?list>-internal (?xs-in x) value))
(defun ?list>=-internal (x value) (if x (andv (>=v (car x) value) (?list>= (cdr x) value)) T))
(defun ?list>= (x value) (?list>=-internal (?xs-in x) value))
(defun ?list=-internal (x value) (if x (andv (=v (car x) value) (?list= (cdr x) value)) T))
(defun ?list= (x value) (?list=-internal (?xs-in x) value))
(defun ?list/=-internal (x value) (if x (andv (/=v (car x) value) (?list/= (cdr x) value)) T))
(defun ?list/= (x value) (?list/=-internal (?xs-in x) value))

(defun ?lists= (xs ys)
  (cond ((and (null xs) (null ys)) T)
        ((or (null xs) (null ys)) nil)
        ((and (consp xs) (consp ys))
         (andv (?lists= (car xs) (car ys))
               (?lists= (cdr xs) (cdr ys))))
        ((or (consp xs) (consp ys)) nil)
        (T (=v xs ys))))

(defun ?lists/= (xs ys)
  (cond ((and (null xs) (null ys)) T)
        ((or (null xs) (null ys)) nil)
        ((and (consp xs) (consp ys))
         (andv (?lists= (car xs) (car ys))
               (?lists= (cdr xs) (cdr ys))))
        ((or (consp xs) (consp ys)) nil)
        (T (/=v xs ys))))

(defun list?and (xs) (apply #'andv (flatt xs)))

(defun list?or (xs) (apply #'orv (flatt xs)))
