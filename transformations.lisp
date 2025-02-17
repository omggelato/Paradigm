(in-package :SCREAMER)

(defmacro write-map?car-fun (n)
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

(defmacro map?car-internal (fn &rest xs)
  `(IF (SOME #'NULL (LIST ,@xs))
       NIL
     (CONS
      (FUNCALL ,fn ,@(mapcar #'(lambda (x) `(CAR ,x)) xs))
      (IF (EVERY #'CDR (LIST ,@xs))
          (MAP?CAR-INTERNAL ,fn ,@(mapcar #'(lambda (x) `(CDR ,x)) xs))
        NIL))))

(defmacro map?car (fn &rest xs)
  (let ((callfn (intern (format nil "MAP~A?CAR-NONDETERMINISTIC-INTERNAL" (length xs)) :SCREAMER)))
    (if *nondeterministic-context?*
        `(,callfn ,fn ,@xs)
      `(map?car-internal ,fn ,@xs))))

(defmacro map?and (fn &rest xs)
  (let ((callfn (intern (format nil "MAP~A?AND-NONDETERMINISTIC-INTERNAL" (length xs)) :SCREAMER)))
    (if *nondeterministic-context?*
        `(,callfn ,fn ,@xs)
      `(map?and-internal ,fn ,@xs))))

(defmacro map?or (fn &rest xs)
  (let ((callfn (intern (format nil "MAP~A?OR-NONDETERMINISTIC-INTERNAL" (length xs)) :SCREAMER)))
    (if *nondeterministic-context?*
        `(,callfn ,fn ,@xs)
      `(map?or-internal ,fn ,@xs))))

(defmacro map?and-internal (fn &rest xs)
  `(IF (SOME #'NULL (LIST ,@xs))
       T
     (ANDV
      (FUNCALL ,fn ,@(mapcar #'(lambda (x) `(CAR ,x)) xs))
      (IF (EVERY #'CDR (LIST ,@xs))
          (MAP?AND-INTERNAL ,fn ,@(mapcar #'(lambda (x) `(CDR ,x)) xs))
        T))))

(defmacro map?or-internal (fn &rest xs)
  `(IF (SOME #'NULL (LIST ,@xs))
       NIL
     (ORV
      (FUNCALL ,fn ,@(mapcar #'(lambda (x) `(CAR ,x)) xs))
      (IF (EVERY #'CDR (LIST ,@xs))
          (MAP?OR-INTERNAL ,fn ,@(mapcar #'(lambda (x) `(CDR ,x)) xs))
        NIL))))

(defmacro write-map?and-fun (n)
  (let ((name (intern (format nil "MAP~A?AND-NONDETERMINISTIC-INTERNAL" n) :SCREAMER))
        (arguments (loop for i from 1 while (<= i n) collect
                           (intern (format nil "LIST~A" n)))))
    (export name :SCREAMER)
    `(SCREAMER::DEFUN ,name (FN ,@arguments)
       (IF (OR ,@(mapcar #'(lambda (x) `(NULL ,x)) arguments))
           NIL
         (ANDV
          (FUNCALL-NONDETERMINISTIC FN ,@(mapcar #'(lambda (x) `(CAR ,x)) arguments))
          (IF (AND ,@(mapcar #'(lambda (x) `(CDR ,x)) arguments))
              (,name FN ,@(mapcar #'(lambda (x) `(CDR ,x)) arguments))
            T))))))

(defmacro write-map?or-fun (n)
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

(defmacro map?list (fn &rest xs)
  (let ((callfn (intern (format nil "MAP~A?LIST-NONDETERMINISTIC-INTERNAL" (length xs)) :SCREAMER)))
    (if *nondeterministic-context?*
        `(,callfn ,fn ,@xs)
      `(map?list-internal ,fn ,@xs))))

(defmacro maplist?and (fn &rest xs)
  (let ((callfn (intern (format nil "MAPLIST~A?AND-NONDETERMINISTIC-INTERNAL" (length xs)) :SCREAMER)))
    (if *nondeterministic-context?*
        `(,callfn ,fn ,@xs)
      `(maplist?and-internal ,fn ,@xs))))

(defmacro maplist?and (fn &rest xs)
  (let ((callfn (intern (format nil "MAPLIST~A?OR-NONDETERMINISTIC-INTERNAL" (length xs)) :SCREAMER)))
    (if *nondeterministic-context?*
        `(,callfn ,fn ,@xs)
      `(maplist?or-internal ,fn ,@xs))))

(defmacro maplist?and-internal (fn &rest xs)
  `(IF (SOME #'NULL (LIST ,@xs))
       T
     (ANDV
      (FUNCALL ,fn ,@xs)
      (IF (EVERY #'CDR (LIST ,@xs))
          (MAPLIST?AND-INTERNAL ,fn ,@(mapcar #'(lambda (x) `(CDR ,x)) xs))
        T))))

(defmacro maplist?or-internal (fn &rest xs)
  `(IF (SOME #'NULL (LIST ,@xs))
       NIL
     (ORV
      (FUNCALL ,fn ,@xs)
      (IF (EVERY #'CDR (LIST ,@xs))
          (MAPLIST?OR-INTERNAL ,fn ,@(mapcar #'(lambda (x) `(CDR ,x)) xs))
        T))))

(defmacro write-map?list-fun (n)
  (let ((name (intern (format nil "MAPLIST~A?AND-NONDETERMINISTIC-INTERNAL" n) :SCREAMER))
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

(defmacro write-maplist?and-fun (n)
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

(defmacro write-maplist?or-fun (n)
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

;; map?func
(defmacro map?func (fn &rest xs)
  (let ((callfn (intern (format nil "MAP~A?FUNC-NONDETERMINISTIC-INTERNAL" (length xs)) :SCREAMER)))
    (if *nondeterministic-context?*
        `(,callfn ,fn ,@xs)
      `(map?func-internal ,fn ,@xs))))

(defmacro map?func-internal (fn &rest xs)
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

(defmacro write-map?func (n)
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
(defmacro map?levels (fn &rest xs)
  (let ((callfn (intern (format nil "MAP~A?LEVELS-NONDETERMINISTIC-INTERNAL" (length xs)) :SCREAMER)))
    (if *nondeterministic-context?*
        `(,callfn ,fn 0 ,@xs)
      `(map?levels-internal ,fn 0 ,@xs))))

(defmacro map?levels-internal (fn level &rest xs)
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

(defmacro write-map?levels (n)
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


