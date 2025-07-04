(in-package :SCREAMER)

(defun ?+ (x &rest xs) (apply #'+v (?oper-arguments x xs)))
(defun ?- (x &rest xs) (apply #'-v (?oper-arguments x xs)))
(defun ?* (x &rest xs) (apply #'*v (?oper-arguments x xs)))
(defun ?/ (x &rest xs) (apply #'/v (?oper-arguments x xs)))
(defun ?1+ (x) (+v 1 x))
(defun ?-1 (x) (-v x 1))

(cl:defun %v-restricts-bounds ()
  (screamer::defun %v (n d)
    (cond
     ((null n) nil)
     ((consp n) 
      (cons (%v (car n) d)
            (if (cdr n) (%v (cdr n) d) NIL)))
     (T 
      (let ((x (-v n (*v d (an-integerv)))))
        (assert! (integerpv x))
        (assert! (>=v x 0))
        (assert! (<v x d))
        x)))))

(cl:defun %v-calls-native-function ()
  (screamer::defun %v (n d) (s:funcallv #'mod n d)))

(cl:defun ?%-restricts-bounds () (%v-restricts-bounds))
(cl:defun ?%-calls-native-function () (%v-calls-native-function))

(%v-restricts-bounds)
; (%v-calls-native-function)

(defun ?% (n d) (%v n d))

(defun ?avg (&rest xs) 
  (/v (apply #'+v xs) (length xs)))

(defun ?abs (k) 
  (cond
   ((null k) nil)
   ((consp k) 
    (cons (?abs (car k))
          (if (cdr k) (?abs (cdr k)) NIL)))
   (T
    (let ((x (maxv k (*v k -1))))
      (assert! (>=v x 0))
      x))))

(defun ?sum (xs) (?+ xs))
(defun ?listavg (list) (apply #'?avg list))
(defun ?listdx (list)
  (cond 
   ((null list) nil)
   ((cdr list)
    (cond
     ((and (car list) (cadr list))
      (append (list (s:-v (cadr list) (car list))) (?listdx (cdr list))))
     ((and (null (car list)) (null (cadr list)))
      (?listdx (append '(nil) (cddr list))))
     ((cadr list)
      (append '(nil) (?listdx (cdr list))))
     (T
      (?listdx (cdr list)))))
   (T nil)))

(defmacro write-list-oper-function (oper)
  (let ((name (intern (format nil "LIST~A" (symbol-name oper)) :SCREAMER))
        (internal-fn (intern (format nil "LIST~A-INTERNAL" (symbol-name oper)) :SCREAMER)))   
    `(PROGN
       (SCREAMER::DEFUN ,internal-fn (X VALUE VARIABLES)
         (COND
          ((OR (NUMBERP X)
               (VARIABLE? X))
           (LET ((BINDING (ASSOC X VARIABLES :TEST #'EQ)))
             (IF BINDING
                 (VALUES (CDR BINDING) VARIABLES)
               (LET ((Y (,oper X VALUE)))
                 (VALUES Y (CONS (CONS X Y) VARIABLES))))))
          ((CONSP X)
           (MULTIPLE-VALUE-BIND (CAR-X CAR-VARIABLES)
               (,internal-fn (CAR X) VALUE VARIABLES)
             (MULTIPLE-VALUE-BIND (CDR-X CDR-VARIABLES)
                 (,internal-fn (CDR X) VALUE CAR-VARIABLES)
               (VALUES (CONS CAR-X CDR-X) CDR-VARIABLES))))
          (T (VALUES X VARIABLES))))

       (SCREAMER::DEFUN ,name (X VALUE)
         (MULTIPLE-VALUE-BIND (Y VARIABLES) (,internal-fn X VALUE '())
           Y)))))

(write-list-oper-function s:+v)
(write-list-oper-function s:-v)
(write-list-oper-function s:*v)
(write-list-oper-function s:/v)
(write-list-oper-function s::%v)

(defun ?list+ (x value) (list+v-internal x value '()))
(defun ?list- (x value) (list-v-internal x value '()))
(defun ?list* (x value) (list*v-internal x value '()))
(defun ?list/ (x value) (list/v-internal x value '()))
(defun ?list% (x value) (list%v-internal x value '()))

(defun a?boolean (&optional name) (if name (s:a-booleanv name) (s:a-booleanv)))
(defun a?member-of (sequence) (screamer:a-member-ofv sequence))
(defun a?number (&optional name) (if name (screamer:a-numberv name) (screamer:a-numberv)))
(defun a?real (&optional name) (if name (screamer:a-realv name) (screamer:a-realv)))
(defun a?real-above (low &optional name) (if name (screamer:a-real-abovev low name) (screamer:a-real-abovev low)))
(defun a?real-below (high &optional name) (if name (screamer:a-real-belowv high name) (screamer:a-real-belowv high)))
(defun a?real-between (low high &optional name) (if name (screamer:a-real-betweenv low high name) (screamer:a-real-betweenv low high)))
(defun an?integer (&optional name) (if name (screamer:an-integerv name) (screamer:an-integerv)))
(defun an?integer-above (low &optional name) (if name (screamer:an-integer-abovev low name) (screamer:an-integer-abovev low)))
(defun an?integer-below (high &optional name) (if name (screamer:an-integer-belowv high name) (screamer:an-integer-belowv high)))
(defun an?integer-between (low high &optional name) (if name (screamer:an-integer-betweenv low high name) (screamer:an-integer-betweenv low high)))
(defun ?count-trues (list) (apply #'count-truesv list))

