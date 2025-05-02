(in-package :OPENMUSIC)

(defmethod! eventsms (durations
                      time-signature-list
                      modulus)
  :initvals '(((1 2 3 4 5 6 7)) (4 4) (8 (1 1)))
  :icon 252
  :doc "'events-measure returns (openmusic or enp format) tree, beats, beat sizes, pulses in each beat for a given durations series, the time signature list, designating a  series of measures or a default time signature, and an input for the unit time interval for the excerpt"
  (let ((emit-voice-rhythm-tree (some #'atom durations)))
    (let ((rhythm-tree 
           (scale-ms-events (if emit-voice-rhythm-tree 
                                (list durations) durations) time-signature-list modulus 
                            :enp nil :list-mode nil :print-warnings nil 
                            :proportional-mode nil)))
      (if emit-voice-rhythm-tree (car rhythm-tree) rhythm-tree))))

(defmethod! process-duration-groups (ms timepoints modulus ratio &key proportional-mode)
  :initvals '(((5 8) (6 8) (7 8) (4 8)) (1 2 3 4 5 6 7 8 9 10) 16 (3 2) nil)
  :indoc '("ms" "timepoints" "modulus" "ratio" "proportional-mode") ; an string list with short docs
  :icon 225  ; the icon
  :doc "" 
  (let* ((rratio (/ (car ratio) (cadr ratio)))
         (msdmax (apply #'max (mapcar #'cadr ms)))
         (tscale (/ msdmax modulus))
         (beat-partitions (mapcar #'(lambda (p ms) (om* rratio
                                                        (om* p (/ msdmax (cadr ms)))))
                                  (if proportional-mode
                                      (mapcar #'list (mapcar #'car ms))
                                    (partn-list '(2 3) (mapcar #'car ms)))
                                  ms))
         (timepoints-scaled (mapcar #'(lambda (xs)
                            (cond ((listp xs)
                                   (list (* (car xs) tscale) (cadr xs)))
                                  (t (* xs tscale))))
                        timepoints)))
    ;(print (format nil "process-duration-groups timepoints-scaled ~A" timepoints-scaled))
    ;(print (format nil "important! beat-partitions: ~A" beat-partitions))
    (process-duration-groups-internal (flat beat-partitions) timepoints-scaled)))

(cl:defstruct timee value flag)

(defun setf-timee-value (x d)
  (cond ((listp (timee-value x))
         (setf (car (timee-value x)) d))
        (t 
         (setf (timee-value x) d))))

(defun scale-to-int (ll)
  (cond ((numberp ll)
         (cond ((< ll 0) -1)
               ((= ll 0) 0)
               (t 1)))
        (t
         (cond ((= (length ll) 0) (list (scale-to-int 0)))
               ((= (length ll) 1) (list (scale-to-int (car ll))))
               (t 
                (let* ((gcm (gcm (mapcar (lambda (x)
                                             (if (rationalp x)
                                                 (denominator x)
                                               1))
                                           (mapcar #'rationalize ll))))
                       (r (om* ll gcm))
                       (gcd1 (gcd1 r)))
                  (values (mapcar
                           #'(lambda (x y)
                               (cond ((floatp x) (float y))
                                     (t y)))
                           ll
                           (mapcar #'floor (om/ r gcd1)))
                          gcm
                          gcd1)))))))

(defun gcm (&rest n)
  (cond ((listp (car n))
         (apply #'gcm (car n)))
        (t (apply #'*
                  (mapcar (lambda (x) 
                            (apply #'expt x))
                          (remove-duplicates
                           (sort (apply #'append (mapcar #'prime-facts n))
                                 (lambda (x y)
                                   (cond ((= (car x) (car y))
                                          (> (cadr x) (cadr y)))
                                         (t (< (car x) (car y))))))
                           :test #'= 
                           :key #'car
                           :from-end t))))))

(defun prime-facts (x) ; copied from om::prime-facts 
  (let ((ip 1) (r) (n 0))
    (loop while (and (> x 1) 
                     (<= (* (aref *prime-numbers* ip) 
                            (aref *prime-numbers* ip))
                         x)) do
      (when (= 0 (mod x (aref *prime-numbers* ip)))
        (setq n 1)
        (loop while (= 0 
                       (progn (setq x (/ x (aref *prime-numbers* ip))) 
                              (mod x (aref *prime-numbers* ip)))) do
              (incf n))
        (push  (list (aref *prime-numbers* ip) n) r))
      (incf ip))
    (when (/= x 1)   (push  (list x 1) r))
    (or (reverse r) (list (list 1 1)))))

(defun gcd1 (&rest n)
  (cond ((listp (car n))
         (apply #'gcd1 (car n)))
        (t (let* ((factors (mapcar #'prime-facts n))
                  (shared-terms (reduce #'intersection 
                                        (mapcar (lambda (x)
                                                  (mapcar #'car x))
                                                factors)))
                  (shared-factors (mapcan (lambda (x)
                                            (if (position (car x) shared-terms)
                                                (list x)))
                                          (apply #'append factors))))  
             (apply #'*
                    (mapcar (lambda (x) 
                              (apply #'expt x))
                            (remove-duplicates
                             (sort shared-factors
                                   (lambda (x y)
                                     (cond ((= (car x) (car y))
                                            (< (cadr x) (cadr y)))
                                           (t (< (car x) (car y))))))
                             :test #'= 
                             :key #'car
                             :from-end t)))))))
(defun treelen (ll)
  (cond ((null ll) 0)
	((null (car ll)) (+ 0 (treelen (cdr ll))))
	((listp (car ll)) (+ (treelen (car ll)) (treelen (cdr ll))))
	(t (+ 1 (treelen (cdr ll))))))
(defun list2int (ll)
  (cond ((null ll) nil)	
	((listp ll) (cond ((cdr ll) (append (list2int (car ll)) (list2int (cdr ll))))
			  (t (list2int (car ll)))))
	(t (let ((flll (car (multiple-value-list (floor ll)))))
	     (cond ((listp flll) (list (car flll)))
		   (t (list flll)))))))
(defun to-fractn (n) (reduce-fractn n 1))
(defun reduce-fractn (n d)
  (if (not (= (mod n 1) 0))
      (reduce-fractn (* n 10) (* d 10))
    (let ((fs (remove 1 (intersection (ftor n) (ftor d)))))
      (if fs
          (let ((gcf (list-max fs)))
            (reduce-fractn (/ n gcf) (/ d gcf)))
        (list n d)))))		    
(defun to-om-ms-den-list (ms)
  (to-ms-den-list ms))
(defun to-ms-den-list (ms)
  (if (car ms) (append (list (car (cdr (car ms)))) (to-ms-den-list (cdr ms))) nil))
(defun test-merge-ms-partns (partns groups)
  (merge-ms-partns partns groups))
(defun merge-ms-partns (partns groups)
  (setq grouprz (if (> (treelen partns) (treelen groups))
		    (append groups (make-sequence 'list
                                                  (- (treelen partns) (treelen groups)) 
                                                  :initial-element (list -1)))
		  groups))
  (if (car partns)
      (append (list (merge-partn-group (car partns) (subseq grouprz 0 (length (car partns)))))
	      (merge-ms-partns (cdr partns) (nthcdr (length (car partns)) grouprz)))
    nil))
(defun merge-partn-group (partn group) 
  (if (car partn)
      (append (list (list (car partn) (car group))) (merge-partn-group (cdr partn) (cdr group)))
    nil))	
(defun to-ms-numr-list (ms den)
  (if (car ms)
      (append (list (to-ms-numr (car ms) den)) (to-ms-numr-list (cdr ms) den))
    nil))
(defun to-ms-numr (ms den)
  (* (car ms) (/ den (car (cdr ms)))))
(defun list-fnappl (fn ll)
  (cond ((cdr ll) (funcall fn (car ll) (list-fnappl fn (cdr ll))))
	(t (car ll))))
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
(in-package :SCREAMER-USER)
;(defun an-expanded-list-internal (len templates)
;  (either    
;    (mapcar #'(lambda (x) (a-member-ofv templates) (make-sequence 'list len))
;    (an-expanded-list-internal (1+ len) templates))))
;(defun an-expanded-list (templates rules) ; delete
;  (let ((r (solution (an-expanded-list-internal 1 templates)
;                     (static-ordering #'linear-force))))
;    (unless (every #'(lambda (rule) (funcall rule r)) (if (consp rules) rules (list rules))) (fail))
;    r))


(defun an-expanded-list (templates rules &key randomize-choices) ; delete
  (let ((r (solution (an-expanded-list-rec 1 templates :randomize-choices randomize-choices)
                     (static-ordering #'linear-force))))
    (unless (every #'(lambda (rule) (funcall rule r)) (if (consp rules) rules (list rules))) (fail))
    r))
(defun an-expanded-list-rec (len templates &key randomize-choices)
  (either    
    (mapcar #'(lambda (x) (a-member-ofv templates)) (make-sequence 'list len))
    (an-expanded-list-rec (1+ len) templates :randomize-choices randomize-choices)))

(in-package :OPENMUSIC)
(defun prcs-ms-timepoint-signatures (ms partns &key list-mode)
  (if (car ms)
      (append (list (append (list (cond (list-mode ms)
                                        (t (format-mssign (car ms)))))
			    (list (car partns)))) 
	      (prcs-ms-timepoint-signatures (cdr ms) (cdr partns)))
    nil))
(defun format-mssign (ms)
  (read-from-string (concatenate 'string (write-to-string (car ms)) "//" (write-to-string (cadr ms)))))

;(defun contains-list (ll)
;  (cond ((null ll) nil)
;        ((listp ll) (or (listp (car ll))
;                        (contains-list (cdr ll))))
;        (t nil)))

;(defun contains-atom (ll)
;  (cond ((null ll) nil)
;        ((listp ll) (or (atom (car ll))
;                        (contains-atom (cdr ll))))
;        (t nil)))

(defun process-duration-groups-internal (l segs)
  ;(print (format nil "ENTER process-duration-groups-internal l: ~A segments: ~A" l segs))
  (labels
      ((strip-zeros (xs) (mapcar (lambda (x) (remove 0 x :test 'equalp)) xs))
       (duration (group)
         (cond ((null group) 0)
               ((and (listp group)
                     (> (length group) 1)
                     (atom (car group))
                     (listp (cadr group)))
                (abs (car group)))
               ((listp group) 
                ;(print "process-duration-groups-internal#duration is being called for a list that is not a (x (. . .)) type pulse marker")
                0)
               (t (abs group))))
       (group? (input)
         (and (not (null input))
              (listp input)
              (listp (car input))
              (not (listp (cadr input)))))
       (group-value (group)
         (cond ((null group) 0)
               ((and (listp group)
                     (> (length group) 1)
                     (atom (car group))
                     (listp (cadr group)))
                (car group))
               ((listp group)
                ;(print "process-duration-groups-internal#group-value is being called for a list that is not a (x (. . .)) type pulse marker")
                0)
               (t group)))
       (process-durations (ll segs-l segs-r)
         ;(print (format nil "process-duration-groups-internal#process-durations ll: ~A segs-l: ~A segs-r: ~A" ll segs-l segs-r))
         (cond ((and (null ll) (not (null segs-l)) (null segs-r)) (list segs-l))
               ((null ll) nil)
               ((and (null segs-l) (null segs-r)) (process-durations ll (list (* (float -1) (float (car ll)))) nil))
               ((null segs-l) (process-durations ll (list (car segs-r)) (cdr segs-r)))
               ((and (null segs-r) (< (reduce #'+ (mapcar #'duration segs-l)) (car ll)))
                (let* ((segs-l-duration (reduce #'+ (mapcar #'duration segs-l)))
                       (diff (float (* -1 (- (car ll) segs-l-duration)))))
                  (process-durations ll (append segs-l (list diff)) nil)))
               (t 
                (let ((diff (- (duration (car ll)) (reduce #'+ (mapcar #'duration segs-l)))))
                  (cond ((> diff 0)
                         (process-durations ll 
                                 (append segs-l (list (car segs-r)))
                                 (cdr segs-r)))
                        ((< diff 0)
                         (let ((next-segment (car (reverse segs-l))))
                           (cond
                          ((listp next-segment)
                           (let* ((a (duration next-segment))
                                  (c (/ (* -1 diff) a))
                                  (b (/ (+ (duration next-segment) diff) a))
                                  (s (reduce #'+ (mapcar #'duration (cadar (reverse segs-l))))) ; sum of the internal subdivisions of the group
                                  (d (* b s))
                                  (e (* c s))
                                  (groups (process-duration-groups-internal (list d e) (cadar (reverse segs-l)))))
                               (let* ((stak (copy-seq (cadar (reverse segs-l)))))
                                 (let ((groups2
                                        (map-func
                                         #'(lambda (x)
                                             (let ((top (pop stak)))
                                               (cond ((floatp top) (float x))
                                                     (t x))))
                                         groups)))
                                  ; (print (format nil "-> process-durations-groups-internal#process-durations a: ~A c: ~A b: ~A s: ~A groups: ~A groups2: ~A next-segment: ~A segments: ~A" a c b s groups groups2 (car (reverse segs-l)) segs-l))
                               (process-durations ll 
                                       (append (butlast segs-l) 
                                               (list (list (+ (duration next-segment) diff)
                                                           (car groups2))))
                                       (append (list (list (* -1 diff) (cadr groups2)))
                                             segs-r))))))
                          (t (process-durations ll
                                     (append (butlast segs-l)
                                             (list (+ (duration (car (reverse segs-l))) diff)))
                                     (append (list (* -1 diff))
                                             segs-r))))))
                        (t
                         (append (list segs-l)
                                 (process-durations (cdr ll)
                                                    nil
                                                    segs-r))))))))
       (process-durations->timees (ps)
         (mapcar ; ((1 2) (1 1))
          #'(lambda (m)
              (mapcar
               #'(lambda (n)
                   (make-timee :value n :flag t))
               m))
          ps))
       (process-timee-flags (re)
         (let ((init-s (map-func #'abs segs)) ; 
               (init-p (flat re)))
           (labels ((prcs (s p &optional (cont nil))
                      (cond ((and (null s) (null p)) nil)
                            ((null p) nil)
                            ((null s) (mapcar #'(lambda (x) 
                                                  (setf (timee-flag x) nil))
                                              p))
                            (t (let ((diff (- (duration (car s))
                                              (duration (timee-value (car p))))))
                                 (cond ((> diff 0)
                                        (progn 
                                          (setf (timee-flag (car p)) (not cont))
                                          (prcs (append (list diff) (cdr s))
                                                (cdr p)
                                                t)))
                                       ((< diff 0)
                                        (progn 
                                          (setf (timee-flag (car p)) (not cont))
                                          (prcs (cdr s)
                                                (cdr p))))
                                       (t
                                        (progn 
                                          (setf (timee-flag (car p)) (not cont))
                                          (prcs (cdr s) (cdr p))))))))))
             (prcs init-s init-p))))
       (expand-timee-groups (re)
         ; (print (format nil "expand-timee-groups re: ~A" re))
         (mapcar #'(lambda (xs)
                     ; (print (format nil "expand-timee-groups calling scale-to-int with ~A" (mapcar #'group-value (mapcar #'timee-value xs))))
                     (let ((x-values (scale-to-int (mapcar #'group-value (mapcar #'timee-value xs)))))
                       (mapcar #'(lambda (struct value)                                 
                                 (progn 
                                   ; (print (format nil "calling setf-timee-value struct: ~A value: ~A" struct value))
                                   (setf-timee-value struct value))
                                   struct)
                               xs
                               x-values)))
                 re))
       (convert-timee-values (re) 
         (mapcar #'(lambda (x)
                     (setf-timee-value x (if (timee-flag x)
                                               (floor (group-value (timee-value x)))
                                             (float (group-value (timee-value x))))))
                 (flat re)))
       (process-timee-signs (re segs)
         (let ((stak (copy-seq segs)))
           (push nil stak)
           (mapcar #'(lambda (x)
                     (if (timee-flag x) (pop stak))
                     (cond ((null stak) x)
                           ((and (< (group-value (car stak)) 0)
                                 (> (timee-value x) 0))
                            (setf (timee-value x) (* -1 (timee-value x)))
                            x)
                           (t x)))
                   (flat re))))
       )
    (let ((durations (strip-zeros (process-durations l nil segs))))
      (let ((duration-obj-list (process-durations->timees durations)))
        (process-timee-flags duration-obj-list)
        (expand-timee-groups duration-obj-list)
        (process-timee-signs duration-obj-list segs)
        (convert-timee-values duration-obj-list)
        (map-func #'timee-value duration-obj-list)))))

(defun partn-list (keys input)
  (if (null input)
      nil
    (append
     (list
      (partn-list-elem (car input) keys))
     (partn-list keys (cdr input)))))

(defun partn-list-elem (e keys)
  (labels
      ((find-sumof (xs) (apply #'+ (flat xs))))
    (cond ((null e) nil)
          (t (screamer:one-value 
              (screamer-user::an-expanded-list keys #'(lambda (x) (= (find-sumof x) e))))))))

(defun sequence->note-names (sequence)
  :icon 138
  (labels
      ((between (x low hi) (and (>= x low) (<= x hi))))
    (map-func #'(lambda (x)                                    
                  (cond ((null x) 'om::?)
                        ((screamer::variable? x) 'om::?v)
                        ((not (numberp x)) x)
                        (t
                         (let ((pc (mod x 12)))
                           (cond
                            ((between x 0 11) ; -2
                             (cond ((= pc 0) 'om::C-2)
                                   ((= pc 1) 'om::Db-2)
                                   ((= pc 2) 'om::D-2)
                                   ((= pc 3) 'om::Eb-2)
                                   ((= pc 4) 'om::E-2)
                                   ((= pc 5) 'om::F-2)
                                   ((= pc 6) 'om::Gb-2)
                                   ((= pc 7) 'om::G-2)
                                   ((= pc 8) 'om::Ab-2)
                                   ((= pc 9) 'om::A-2)
                                   ((= pc 10) 'om::Bb-2)
                                   ((= pc 11) 'om::B-2)))
                            ((between x 12 23) ; -1                           
                             (cond ((= pc 0) 'om::C-1)
                                   ((= pc 1) 'om::Db-1)
                                   ((= pc 2) 'om::D-1)
                                   ((= pc 3) 'om::Eb-1)
                                   ((= pc 4) 'om::E-1)
                                   ((= pc 5) 'om::F-1)
                                   ((= pc 6) 'om::Gb-1)
                                   ((= pc 7) 'om::G-1)
                                   ((= pc 8) 'om::Ab-1)
                                   ((= pc 9) 'om::A-1)
                                   ((= pc 10) 'om::Bb-1)
                                   ((= pc 11) 'om::B-1)))
                            ((between x 24 35) ; 0
                             (cond ((= pc 0) 'om::C0)
                                   ((= pc 1) 'om::Db0)
                                   ((= pc 2) 'om::D0)
                                   ((= pc 3) 'om::Eb0)
                                   ((= pc 4) 'om::E0)
                                   ((= pc 5) 'om::F0)
                                   ((= pc 6) 'om::Gb0)
                                   ((= pc 7) 'om::G0)
                                   ((= pc 8) 'om::Ab0)
                                   ((= pc 9) 'om::A0)
                                   ((= pc 10) 'om::Bb0)
                                   ((= pc 11) 'om::B0)))
                            ((between x 36 47) ; 1                           
                             (cond ((= pc 0) 'om::C1)
                                   ((= pc 1) 'om::Db1)
                                   ((= pc 2) 'om::D1)
                                   ((= pc 3) 'om::Eb1)
                                   ((= pc 4) 'om::E1)
                                   ((= pc 5) 'om::F1)
                                   ((= pc 6) 'om::Gb1)
                                   ((= pc 7) 'om::G1)
                                   ((= pc 8) 'om::Ab1)
                                   ((= pc 9) 'om::A1)
                                   ((= pc 10) 'om::Bb1)
                                   ((= pc 11) 'om::B1)))
                            ((between x 47 59) ; 2                          
                             (cond ((= pc 0) 'om::C2)
                                   ((= pc 1) 'om::Db2)
                                   ((= pc 2) 'om::D2)
                                   ((= pc 3) 'om::Eb2)
                                   ((= pc 4) 'om::E2)
                                   ((= pc 5) 'om::F2)
                                   ((= pc 6) 'om::Gb2)
                                   ((= pc 7) 'om::G2)
                                   ((= pc 8) 'om::Ab2)
                                   ((= pc 9) 'om::A2)
                                   ((= pc 10) 'om::Bb2)
                                   ((= pc 11) 'om::B2)))
                            ((between x 60 71) ; 3                        
                             (cond ((= pc 0) 'om::C3)
                                   ((= pc 1) 'om::Db3)
                                   ((= pc 2) 'om::D3)
                                   ((= pc 3) 'om::Eb3)
                                   ((= pc 4) 'om::E3)
                                   ((= pc 5) 'om::F3)
                                   ((= pc 6) 'om::Gb3)
                                   ((= pc 7) 'om::G3)
                                   ((= pc 8) 'om::Ab3)
                                   ((= pc 9) 'om::A3)
                                   ((= pc 10) 'om::Bb3)
                                   ((= pc 11) 'om::B3)))
                            ((between x 72 83) ; 4
                             (cond ((= pc 0) 'om::C4)
                                   ((= pc 1) 'om::Db4)
                                   ((= pc 2) 'om::D4)
                                   ((= pc 3) 'om::Eb4)
                                   ((= pc 4) 'om::E4)
                                   ((= pc 5) 'om::F4)
                                   ((= pc 6) 'om::Gb4)
                                   ((= pc 7) 'om::G4)
                                   ((= pc 8) 'om::Ab4)
                                   ((= pc 9) 'om::A4)
                                   ((= pc 10) 'om::Bb4)
                                   ((= pc 11) 'om::B4)))
                            ((between x 84 95) ; 5
                             (cond ((= pc 0) 'om::C5)
                                   ((= pc 1) 'om::Db5)
                                   ((= pc 2) 'om::D5)
                                   ((= pc 3) 'om::Eb5)
                                   ((= pc 4) 'om::E5)
                                   ((= pc 5) 'om::F5)
                                   ((= pc 6) 'om::Gb5)
                                   ((= pc 7) 'om::G5)
                                   ((= pc 8) 'om::Ab5)
                                   ((= pc 9) 'om::A5)
                                   ((= pc 10) 'om::Bb5)
                                   ((= pc 11) 'om::B5)))
                            ((between x 96 107) ; 6
                             (cond ((= pc 0) 'om::C6)
                                   ((= pc 1) 'om::Db6)
                                   ((= pc 2) 'om::D6)
                                   ((= pc 3) 'om::Eb6)
                                   ((= pc 4) 'om::E6)
                                   ((= pc 5) 'om::F6)
                                   ((= pc 6) 'om::Gb6)
                                   ((= pc 7) 'om::G6)
                                   ((= pc 8) 'om::Ab6)
                                   ((= pc 9) 'om::A6)
                                   ((= pc 10) 'om::Bb6)
                                   ((= pc 11) 'om::B6)))
                            ((between x 108 119) ; 7
                             (cond ((= pc 0) 'om::C7)
                                   ((= pc 1) 'om::Db7)
                                   ((= pc 2) 'om::D7)
                                   ((= pc 3) 'om::Eb7)
                                   ((= pc 4) 'om::E7)
                                   ((= pc 5) 'om::F7)
                                   ((= pc 6) 'om::Gb7)
                                   ((= pc 7) 'om::G7)
                                   ((= pc 8) 'om::Ab7)
                                   ((= pc 9) 'om::A7)
                                   ((= pc 10) 'om::Bb7)
                                   ((= pc 11) 'om::B7)))
                            ((between x 120 132) ; 8
                             (cond ((= pc 0) 'om::C8)
                                   ((= pc 1) 'om::Db8)
                                   ((= pc 2) 'om::D8)
                                   ((= pc 3) 'om::Eb8)
                                   ((= pc 4) 'om::E8)
                                   ((= pc 5) 'om::F8)
                                   ((= pc 6) 'om::Gb8)
                                   ((= pc 7) 'om::G8)
                                   ((= pc 8) 'om::Ab8)
                                   ((= pc 9) 'om::A8)
                                   ((= pc 10) 'om::Bb8)
                                   ((= pc 11) 'om::B8))))))))
              sequence)))

(defun scale-ms-events (durations 
                           time-signature-list
                           modulus
                           &key enp
                           list-mode
                           print-warnings
                           proportional-mode)
  :initvals '(((1 2 3 4 5 6 7)) (4 4) (8 (1 1)) nil nil nil nil)
  :doc "returns (openmusic or enp format) tree, beats, beat sizes, pulses in each beat for a given durations series, the time signature list, designating a  series of measures or a default time signature, and an input for the unit time interval for the excerpt"
  :icon 225
      (unless time-signature-list
        (setf time-signature-list '(4 4)))
      (unless modulus
        (setf modulus '(4 (1 1))))  
  ;(if (some #'listp (remove nil durations))
  ;    (setf durations (list durations)))
  ; replacing the measure list where the input is just one measure '(4 4) instead of a list of time signatures i.e. ((5 4) (3 4) (3 4) (4 4)))
      (let* ((ms (cond ((< (om::list-depth time-signature-list) 2) ; figure out how many bars fit the rhythm if it isn't already given as a list a time signatures 
                        (let* ((timepoints 
                                (reduce 
                                 #'max 
                                 (mapcar #'(lambda (x) 
                                             (reduce #'+ (mapcar #'(lambda (y) (abs (if (listp y) (car y) y))) x))) durations)))
                         ; 1st measure duration in terms of the modulus (time unit)
                               (ms-timepoints (* (car time-signature-list)
                                                 (/ (/ (car modulus) (cadr time-signature-list)
                                                       )
                                                    (/ (cadadr modulus) ; i.e. the 3 from (8 (3 2)) or triplet eighth notes                                              
                                                       (caadr modulus)))))
                               (ms-count (ceiling (/ timepoints ms-timepoints))))
                          (loop for i from 0 while (< i ms-count) collect time-signature-list)))
                       (t time-signature-list)))
             (tree-list nil)
             (partns-mrg-list nil)
             (partns-list nil)
             (beats-list nil)
             (voice-enp (cond
                         ((null enp) (make-sequence 'list (length durations)))
                         ((and (listp enp) (every #'listp enp)) enp)
                         (t (make-sequence 'list (length durations) :initial-element enp))))
             (msbeats (ms-beat-count ms modulus)))
        (let ((tps (mapcar #'(lambda (x) (adjust-durations x msbeats)) durations)))
          (mapcar #'rewrite-rhythm-tree
                  (mapcar #'(lambda (x)
                              (let ((temp nil))
                                (dotimes (j 3)
                                  (setf temp (process-measure-durations ms
                                                                        (process-duration-groups ms x (car modulus) (cadr modulus) :proportional-mode proportional-mode)
                                                                        :enp nil
                                                                        :list-mode nil
                                                                        :proportional-mode proportional-mode)))
                                temp))
                          tps)))))

(defun convert-mnlist-repeats (mnl)
  (reverse (convert-mnlist-repeats-rec (reverse mnl))))
(defun convert-mnlist-repeats-rec (mnl)
  (cond ((null mnl) nil)
        ((cdr mnl) (append (list (cond ((equalp (car mnl) (cadr mnl)) 
                                        (om* (car mnl) 1.0))
                                       (t (car mnl))))
                           (convert-mnlist-repeats-rec (cdr mnl))))
        (t (list (car mnl)))))

(defun merge-voice-sequences (list1 list2)
  ;(mat-trans (list list1 list2)))
  (cond ((not (every #'listp list1))
         (mat-trans (list list1 list2)))
        (t
         (mat-trans (append (mat-trans list1) (list list2))))))

(defmethod! seqc->poly (seqc rhythm-trees &key bpm reverse)
  :indoc '("seqc" "trees")
  :icon 160
  :doc ""
  (make-instance 'om::poly 
                 :voices (seqc->voices seqc 
                                       rhythm-trees 
                                       :bpm bpm)))

(defmethod! seqc->voices (seqc rhythm-trees &key bpm)
  :indoc '("seqc" "trees")
  :icon 225
  :doc ""
  (reverse
  (mapcar #'(lambda (voice-midinotes rhythm-tree) 
             (let ((obj
                      (make-instance 'om::voice
                                     :chords (om* (remove nil (flat voice-midinotes)) 100)
                                     :tree rhythm-tree)))
                 (if bpm (setf (om::tempo obj) bpm))
                 obj))
          seqc
          rhythm-trees)))


(defmethod! seqc->voice (midics rhythm-tree &key bpm)
  :indoc '("midics" "rhythm-tree")
  :icon 225
  :doc ""
  (let ((obj
         (make-instance 'om::voice
                        :chords (remove nil (flat midics))
                        :tree rhythm-tree)))
    (if bpm (setf (om::tempo obj) bpm))
    obj))

(defun ms-beat-count (measures modulus)
  :initvals '(((2 4) (2 4)) (8 (3 2)))            
  :indoc '("measures" "modulus")
  :icon 225
  :doc ""
  (apply #'+ 
         (mapcar 
          #'(lambda (ms)
              (ceiling
               (* (car ms)
                  (/ (car modulus)
                     (cadr ms))
                  (/ (caadr modulus)
                     (cadadr modulus)))))
          measures)))

(defun adjust-durations (list total)
  (labels
      ((duration (x) 
         (cond
          ((null x) 0)
          ((listp x) (duration (car x)))
          (t (abs x))))
       (adjusted-duration (x value)
         (assert x)
         (cond
          ((listp x)
           (list (adjusted-duration (car x) value)
                 (cadr x)))
          ((and (floatp x)
                (< x 0))
           (float (* -1 (abs value))))
          ((floatp x)
           (float (abs value)))
          ((< x 0)
           (floor (* -1 (abs value))))
          (t (abs value)))))
    (let ((seq nil))
      (dotimes (i (length list))
        (let* ((j (- (length list) i))
               (subseq (subseq list 0 j))
               (s (reduce #'+ (mapcar #'duration subseq))))
          ;(lprint 'subseq subseq 's s 'total total)
          (when (or (<= s total)
                    (and (> s total)
                         (< (reduce #'+ (mapcar #'duration (subseq subseq 0 (1- (length subseq)))))
                            total)))
            ;(lprint 'returning 's s)
            (setf seq subseq)
            ;(print (format nil "seqxxxxxxxxx: ~A" seq))
            (RETURN))))
      (let ((duration (if seq (reduce #'+ (mapcar #'duration seq)) 0)))
        (assert (> duration 0))
        seq))))
(defun adjust-timelist (list total)
  (labels
      ((duration (x) 
         (cond
          ((null x) 0)
          ((listp x) (duration (car x)))
          (t (abs x))))
       (adjusted-duration (x value)
         (assert x)
         (cond
          ((listp x)
           (list (adjusted-duration (car x) value)
                 (cadr x)))
          ((and (floatp x)
                (< x 0))
           (float (* -1 (abs value))))
          ((floatp x)
           (float (abs value)))
          ((< x 0)
           (floor (* -1 (abs value))))
          (t (abs value)))))
    (let ((seq nil))
      (dotimes (i (length list))
        (let* ((j (- (length list) i))
               (subseq (subseq list 0 j))
               (s (apply #'+ (mapcar #'duration subseq))))
          ;(lprint 'subseq subseq 's s 'total total)
          (when (or (<= s total)
                    (and (> s total)
                         (< (apply #'+ (mapcar #'duration (subseq subseq 0 (1- (length subseq)))))
                            total)))
            ;(lprint 'returning 's s)
            (setf seq subseq)
            (RETURN))))
      (let ((duration (if seq (apply #'+ (mapcar #'duration seq)) 0)))
        (assert (> duration 0))
        (cond
         ((> duration total)
          (append (butlast seq)
                  (let ((last (car (reverse seq))))
                    (list (adjusted-duration last (- (duration last) (- duration total)))))))
         (t seq))))))

(defun add-ms-duration-symbol (list)
  (list "?" (list list)))

(defun rewrite-rhythm-tree (tree)
  (labels
      ((tree (tree)
         (list (car tree) ; ? symbol or measure count
               (mapcar #'measure (cadr tree))))
       (group? (elem) (and (not (null elem)) (listp elem) (not (listp (car elem)))))
       (duration (elem)
         (cond ((group? elem) (abs (car elem)))
               (t (abs elem))))
       (measure (ms)
         (list (car ms) ; time-signature
               (pulses (cadr ms))))
       (pulses (pulses)
         (apply
          #'append
          (mapcar #'(lambda (p)
                      (cond ((group? p)
                             (cond ((= (car p) (reduce #'+ (mapcar #'duration (cadr p))))
                                    (pulses (cadr p)))
                                   (t 
                                    (cond ((and (= 1 (length (cadr p)))
                                                (not (group? (caadr p))))
                                           (let ((inner (caadr p)))
                                             (list (cond ((and (floatp inner)
                                                               (< inner 0))
                                                          (float (* -1 (car p))))
                                                         ((floatp inner)
                                                          (float (car p)))
                                                         ((< inner 0)
                                                          (* -1 (car p)))
                                                         (t (car p))))))
                                          (t (list (list (floor (car p))
                                                         (pulses (cadr p)))))))))
                            (t (list p))))
                  pulses))))
    (tree tree)))

(defun process-measure-durations (ms durations &key enp list-mode proportional-mode)
  :initvals '(((5 8) (6 8) (7 8) (4 8)) (1 2 3 4 5 6 7 8 9 10) nil nil nil)
  :indoc '("" "" "enp part mode") ; an string list with short docs
  :icon 225  ; the icon
  :doc ""
  (let* ((ms-max-denom (apply #'max (mapcar #'cadr ms)))
         (partns (if proportional-mode (mapcar #'list (mapcar #'car ms)) (partn-list '(2 3) (mapcar #'car ms))))
         (beats (if (> (treelen partns) (length durations))
                    (append durations (make-sequence 'list
                                                (- (treelen partns) (length durations)) 
                                                :initial-element (list -1)))
                  durations))
         (partns-mrg (merge-ms-partns partns beats))
         (enp-midics (if (listp enp) (map-func #'(lambda (x) (cond ((null x) -1) (t x))) (copy-seq enp))))
         (last-enp-midics nil))
    (labels
        ((make-enp-pulse (input)
           (cond
            ((listp input) (make-enp-beat input))
            (t
             (cond
              ((and (listp enp)
                    (floatp input)
                    (or (and last-enp-midics
                             (< (car last-enp-midics) 0))
                        (and enp-midics
                             (< (car enp-midics) 0))))
               (* -1 (floor (abs input))))
              ((and (listp enp)
                    enp-midics
                    (< (car enp-midics) 0))
               (let ((m (pop enp-midics)))
                 (push m last-enp-midics)
                 (* -1 (floor (abs input)))))
              ((and (listp enp)
                    (null enp-midics))
               (* -1 (floor (abs input))))
              (t
               (list 
                input
                :NOTES (list
                        (cond
                         ((listp enp)
                          (cond
                           ((floatp input)
                            (cond
                             (last-enp-midics (floor (car last-enp-midics)))
                             (enp-midics (floor (car enp-midics)))
                             (t 60)))
                           (t
                            (cond
                             (enp-midics 
                              (let ((m (pop enp-midics)))
                                (push m last-enp-midics)
                                (floor m)))
                             (t 60)))))
                         (t 60)))))))))
         (make-enp-beat (beat)
           (list (floor (car beat)) (mapcar #'make-enp-pulse (cadr beat))))
         (make-enp-measure (signature beats)
           (append (mapcar #'make-enp-beat beats) (list :time-signature signature))))
      (let ((tree (cond 
                   (enp (list (mapcar #'make-enp-measure ms partns-mrg)))
                   (list-mode (prcs-ms-timepoint-sigs ms partns-mrg :list-mode t))
                   (t (append (list (read-from-string "?"))
                              (list (prcs-ms-timepoint-signatures ms partns-mrg)))))))
        ;(values tree partns-mrg partns beats)))))
        tree))))
;; 4/2025
;;  Owen Cannon
