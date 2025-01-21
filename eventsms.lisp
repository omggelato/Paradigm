(in-package :OPENMUSIC)

(defmethod! eventsms (durations 
                      time-signature-list
                      modulus
                      &key enp
                      list-mode
                      print-warnings
                      proportional-mode)
  :initvals '(((1 2 3 4 5 6 7)) (4 4) (8 (1 1)) nil nil nil nil)
  :icon 252
  :doc "'events-measure returns (openmusic or enp format) tree, beats, beat sizes, pulses in each beat for a given durations series, the time signature list, designating a  series of measures or a default time signature, and an input for the unit time interval for the excerpt"
  (t2l::scale-ms-events durations time-signature-list modulus :enp enp :list-mode list-mode :print-warnings print-warnings :proportional-mode proportional-mode))

(in-package :t2l)

(cl:defun consecutive-open-intervals! (sequence)
  (print (format nil "CONSECUTIVE-OPEN-INTERVALS"))
  (labels
      ((interval%12 (x y) (%v (om-v (ommaxv x y) (omminv x y)) 12)))
    (mapcar 
     #'(lambda (voice1-2)
         (maplist
          #'(lambda (chord-list)
              (if (or (not (cdr chord-list))
                      (has-null-values (flatt
                                        (list (car chord-list)
                                              (cadr chord-list)))))
                  T
                (let* ((chord1 (car chord-list))
                       (chord2 (cadr chord-list))
                       (interval1 (interval%12 (car chord1)
                                               (cadr chord1)))
                       (interval2 (interval%12 (car chord2)
                                               (cadr chord2)))
                       (assert! (orv
                                 (andv
                                  (notv (andv (=v interval1 7)
                                              (=v interval2 7)))
                                  (notv (and (=v interval1 0)
                                             (=v interval2 0))))
                                 (andv (=v (car chord1) (car chord2))
                                       (=v (cadr chord1) (cadr chord2)))))))))
          (mat-trans (flatten-seqc voice1-2))))
     (combinations-of2 sequence)))
  sequence)
              
(cl:defun parallel-motion1 (sequence)
  :icon 908
  (let ((chord-intervals (mapcar #'(lambda (chord) (mapcar #'(lambda (interval) (%v (absv (-v (cadr interval) (car interval))) 12)) chord)) (mapcar #'(lambda (xs) (mapcar #'(lambda (ys) (remove-if #'has-null-values ys)) xs)) (mapcar #'combinations-of2 (mat-trans (flatten-seqc sequence)))))))
    (apply
     #'andv
     (maplist #'(lambda (xs)
                  (cond ((cdr xs)
                         (andv
                          (notv
                           (andv (memberv 7 (car xs))
                                 (memberv 7 (cadr xs))))
                          (notv 
                           (andv (memberv 0 (car xs))
                                 (memberv 0 (cadr xs))))))
                        
                        (T T)))
              chord-intervals))))

(cl:defun open-intervals-1 (list)
  :icon 908
  (labels
      ((all-note-pairs-in (chord) (remove-if #'has-null-values (combinations-of2 chord)))
       (in-both (list1 list2)
         (remove-duplicates
          (remove-if-not #'(lambda (xs) (and (find xs list1) (find xs list2))) (append list1 list2))))
       (interval%12 (x y) (om%v (absv (om-v y x)) 12))
       (no-consecutive-open-intervals-in (chords-list interval)
         (maplist-andv
          #'(lambda (chords)
              (cond
               ((not (cdr chords)) T)
               (T
                (let ((chord1 (car chords))
                      (chord2 (cadr chords)))
                  (let ((intervals1 (all-note-pairs-in chord1))
                        (intervals2 (all-note-pairs-in chord2)))
                    (let ((repeats (in-both intervals1 intervals2)))
                      (alert2 (format nil "intervals1 ~A intervals2 ~A repeats: ~A" intervals1 intervals2 repeats))
                      (let ((intervals1 (remove-if #'(lambda (xs) (find xs repeats)) intervals1))
                            (intervals2 (remove-if #'(lambda (xs) (find xs repeats)) intervals2)))
                        (or (null intervals1)
                            (null intervals2)
                            (let ((intervals%121 (mapcar #'(lambda (xs) (apply #'interval%12 xs)) intervals1))
                                  (intervals%122 (mapcar #'(lambda (xs) (apply #'interval%12 xs)) intervals2)))
                              (map-andv #'(lambda (x) 
                                            (map-andv #'(lambda (y)
                                                          (omnotv
                                                           (omandv
                                                            (om=v x interval)
                                                            (om=v y interval))))
                                                      intervals%122))
                                        intervals%121))))))))))
          chords-list))

       (no-consecutive-octaves-in (list) 
         (map-andv
          #'(lambda (seqc)
              (maplist-andv
               #'(lambda (chords)
                   (cond
                    ((not (cdr chords)) T)
                    ((has-null-values (car chords)) T)
                    ((has-null-values (cdr chords)) T)
                    (T
                     (omorv
                      (om=v (car (car chords)) (car (cadr chords)))
                      (om=v (cadr (car chords)) (cadr (cadr chords)))
                      (omnotv
                       (omandv (om=v (apply #'interval%12 (car chords)) 0) 
                               (om=v (apply #'interval%12 (cadr chords)) 0)))))))
               (mat-trans (flatten-seqc seqc))))
          (combinations-of2 list))))
          
    (let ((chords (mat-trans (flatten-seqc list))))
      (omandv (no-consecutive-open-intervals-in chords 7)
              (no-consecutive-octaves-in list)))))
;      (omandv (no-consecutive-open-intervals chords 7)
;              (no-consecutive-open-intervals chords 0)))))






(cl:defun process-duration-groups (ms timepoints modulus ratio &key proportional-mode)
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

(cl:defun process-duration-groups-internal (l segs)
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

(cl:defun partn-list (keys input)
  (if (null input)
      nil
    (append
     (list
      (partn-list-elem (car input) keys))
     (partn-list keys (cdr input)))))

(cl:defun partn-list-elem (e keys)
  (labels
      ((find-sumof (xs) (apply #'+ (flat xs))))
    (cond ((null e) nil)
          (t (screamer:one-value 
              (an-expanded-list keys #'(lambda (x) (= (find-sumof x) e))))))))

(cl:defun sequence->note-names (sequence)
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

(cl:defun om-seqc2timepoints-basic (seqc)
  :initvals '(((1 2) 3) 1)    ; an initial values list
  :indoc '("" "converts duplicated integer atoms to float") ; an string list with short docs
  :icon 225 ; the icon
  :doc ""
  (mapcar #'(lambda (voice) (om-voice2timepoints-basic voice)) seqc))

(cl:defun om-voice2timepoints-basic (tree)
  :initvals '(((1 2) 3) 1)    ; an initial values list
  :indoc '("" "converts duplicated integer atoms to float") ; an string list with short docs
  :icon 225 ; the icon
  :doc ""
  (let ((sequence-list (mat-trans tree)))
    (mapcar
     #'flat
     (mat-trans
      (mapcar #'(lambda (sequence)
                  (let ((duration-max (apply #'max (mapcar #'length sequence)))
                        (timepoints
                         (map-func
                          #'(lambda (x) (an-integer-abovev 1))
                          (mapcar #'flat sequence))))
                    (assert! (map-andv
                              #'(lambda (voice-timepoints)
                                  (=v (apply #'+v voice-timepoints) duration-max))
                              timepoints))
                    (one-value (solution timepoints (static-ordering #'linear-force)))))
              (mapcar #'(lambda (sequence)
                          (mapcar #'(lambda (x) (if (listp x) x (list x)))
                                  sequence))
                      sequence-list))))))

(cl:defun voice2timepoints-basic (v &optional modulus)
  (let* ((m (if modulus modulus 1))
         (stack nil))
    (labels ((prcs-time-rec (l c r)
               (cond ((and (null c)
                           (null r)) nil)
                     ((null c) (prcs-time-rec l (car r) (cdr r)))
                     ((and l 
                           (or (and (floatp c)
                                    (= c (car (reverse l))))
                               (and (screamer::variable? c)
                                    (eq c (car (reverse l))))))
                      (append (list (float m))
                              (prcs-time-rec (append l (list c))
                                             (car r)
                                             (cdr r))))
                     (t 
                      (append (list (floor m))
                              (prcs-time-rec (append l (list c))
                                             (car r)
                                             (cdr r))))))
             (time2stack (tl) 
               (loop for n in tl do (if (and stack 
                                             (floatp n))
                                                 
                                        (push (+ 1 (pop stack)) stack)
                                      (push 1 stack)))))
      (let ((r (time2stack (prcs-time-rec nil nil v))))
        (mapcar 'floor (reverse stack))))))

(cl:defun scale-ms-events (durations 
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
  (let* ((ms (cond ((< (list-depth time-signature-list) 2) ; figure out how many bars fit the rhythm if it isn't already given as a list a time signatures 
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

(cl:defun convert-mnlist-repeats (mnl)
  (reverse (convert-mnlist-repeats-rec (reverse mnl))))
(cl:defun convert-mnlist-repeats-rec (mnl)
  (cond ((null mnl) nil)
        ((cdr mnl) (append (list (cond ((equalp (car mnl) (cadr mnl)) 
                                        (om* (car mnl) 1.0))
                                       (t (car mnl))))
                           (convert-mnlist-repeats-rec (cdr mnl))))
        (t (list (car mnl)))))

(cl:defun merge-voice-sequences (list1 list2)
  ;(mat-trans (list list1 list2)))
  (cond ((not (every #'listp list1))
         (mat-trans (list list1 list2)))
        (t
         (mat-trans (append (mat-trans list1) (list list2))))))

(cl:defun raised7thmotion (voice1 voice2 par1 par2 par3 par4 par5 par6 mode-pcset)
  :icon 908
  (if (null mode-pcset)
      t
    (let* ((mode (cond ((listp mode-pcset) (car mode-pcset))
                    (t mode-pcset)))
           (lt (%v (om-v mode 1) 12)))
      (map-andv 
       #'(lambda (voice)
           (map-andv 
            #'(lambda (x-and-y)
                (or (has-null-values x-and-y)
                    (let ((x=vlt (om=v lt (%v (car x-and-y) 12))))
                      (omorv 
                       (omnotv x=vlt)
                       (omandv x=vlt
                             (om=v (cadr x-and-y) (om+v 1 (car x-and-y))))))))
            (nsucc (remove-consecutive-duplicates (flat voice)) 2 :step 1)))
       (list voice1 voice2)))))

(cl:defun seqc->poly (seqc rhythm-trees &key bpm reverse)
  :indoc '("seqc" "trees")
  :icon 160
  :doc ""
  (make-instance 'om::poly 
                 :voices (seqc->voices seqc 
                                       rhythm-trees 
                                       :bpm bpm)))

(cl:defun seqc->voices (seqc rhythm-trees &key bpm)
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

(cl:defun ms-beat-count (measures modulus)
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

(cl:defun adjust-durations (list total)
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
(cl:defun adjust-timelist (list total)
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

(cl:defun rewrite-rhythm-tree (tree)
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
(cl:defun process-measure-durations (ms durations &key enp list-mode proportional-mode)
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




