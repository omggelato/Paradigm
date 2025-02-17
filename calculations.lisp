(in-package :SCREAMER)

(defun ?+ (x &rest xs) (apply #'+v (?oper-arguments x xs)))
(defun ?- (x &rest xs) (apply #'-v (?oper-arguments x xs)))
(defun ?* (x &rest xs) (apply #'*v (?oper-arguments x xs)))
(defun ?/ (x &rest xs) (apply #'/v (?oper-arguments x xs)))
(defun ?1+ (x) (+v 1 x))
(defun ?-1 (x) (-v x 1))

(defun ?% (n d)
  (cond
   ((null n) nil)
   ((consp n) 
    (cons (?% (car n) d)
          (if (cdr n) (?% (cdr n) d))))
   (T 
    (let ((x (-v n (*v d (an-integerv)))))
      (assert! (integerpv x))
      (assert! (>=v x 0))
      (assert! (<v x d))
      x))))

(cl:defun ?%-restricts-bounds ()
  (screamer::defun ?% (n d)
    (cond
     ((null n) nil)
     ((consp n) 
      (cons (?% (car n) d)
            (if (cdr n) (?% (cdr n) d) NIL)))
     (T 
      (let ((x (-v n (*v d (an-integerv)))))
        (assert! (integerpv x))
        (assert! (>=v x 0))
        (assert! (<v x d))
        x)))))

(cl:defun ?%-calls-native-function ()
  (screamer::defun ?% (n d)
    (cond
     ((null n) nil)
     ((consp n) 
      (cons (?% (car n) d)
            (if (cdr n) (?% (cdr n) d) NIL)))
     (T 
      (let ((x (an-integer-betweenv 0 (1- d))))
        (assert! (equalv x (funcallv #'mod n d)))
        x)))))

(defvar *paradigm--modulo-function* nil)

;(defvar *paradigm--modulo-calls-native-function* 
;  #'(lambda (n d)
;      (let ((var (an-integer-betweenv 0 (1- d))))
;        (assert! (equalv var (funcallv #'mod n d)))
;        var)))

(defun ?avg (&rest xs) (/v (apply #'+v xs) (length xs)))
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
    (append (list (s:-v (cadr list) (car list)))
            (?listdx (cdr list))))
   (T nil)))
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


