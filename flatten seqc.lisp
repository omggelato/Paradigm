(in-package :OPENMUSIC)


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