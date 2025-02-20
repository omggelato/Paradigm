(in-package :OPENMUSIC)
(format *om-stream* "OK~%")
(require-library "OM-Backtrack_1230")
(mapc #'(lambda (module)
          (format *om-stream* "~%Loading '~A' module... " module)
          (let ((file (om-relative-path '(".") module)))
            (compile&load file t)
            (format *om-stream* "OK")))
      (list "screamer ext"
            "calculations"
            "comparisons"
            "transformations" 
            "general"
            "eventsms"
            "flatten seqc"
            "misc"           
            "visual"))
(format *om-stream* "~%")
(cl:defun paradigm--format-timestamp (timestamp)
    (multiple-value-bind
        (second minute hour date month year day-of-week dst-p tz)
        (decode-universal-time timestamp)
      (concatenate 'string
                   (format nil "~2,'0d:~2,'0d:~2,'0d ~d/~2,'0d"
                           hour minute second month date))))
(setf screamer::*echo-stream* *om-stream*)
(fill-library 
 '(("sequences" 
    Nil 
    Nil 
    (om::group-seqc-by-motion-type
     om::flatten-seqc
     om::mat-trans) 
    Nil)))