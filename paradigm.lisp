(in-package :OPENMUSIC)
(require-library "OM-Backtrack_1230")
(mapc #'(lambda (module)
          (format *om-stream* "~%Loading '~A' module... " module)
          (let ((file (om-relative-path '(".") module)))
            (compile&load file t)
            (format *om-stream* "OK")))
      (list "screamer ext" "calculations" "comparisons" "transformations" 
            "general" "eventsms" "flatten seqc" "misc" "visual"))
(format *om-stream* "~%")
(setf screamer::*echo-stream* *om-stream*)
(fill-library '(("sequences" 
                 Nil 
                 Nil 
                 (flatten-seqc) 
                 Nil)))