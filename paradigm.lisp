(unless (find-package :t2l)
  (screamer:define-screamer-package :t2l 
    (:use :om :system)))
; (screamer:unwedge-screamer)
(in-package :t2l)
(cl:defun write*om-stream* (input &rest args) 
  (apply #'format (append (list om::*om-stream* input) args)))
(defvar *paradigm--revision* 1.2)
(defvar *paradigm--library-files* nil)
(defvar *paradigm--package-files* nil)
(defvar *paradigm--source-files* nil)
(setf *paradigm--source-files* (list (om::om-relative-path '(".") "visual")
                                    (om::om-relative-path '(".") "screamer ext")
                                    (om::om-relative-path '(".") "general")
                                    (om::om-relative-path '(".") "misc"))
      *paradigm--library-files* (append *paradigm--source-files* *paradigm--package-files*))
(in-package :OPENMUSIC)
(defmethod! reset-solver-functiondb ()
  (screamer:unwind-trail)
  (screamer:unwedge-screamer)
  (load-om-lib-without-menu (find-library "paradigm"))
  (initWorkSpace *current-workSpace*))
(in-package :OPENMUSIC)
(require-library "OM-Backtrack")
(mapc #'(lambda (file) 
          (t2l::write*om-stream* "Loading '~A' module... " (pathname-name file))
          (compile&load file t)
          (t2l::write*om-stream* "OK~%")) t2l::*paradigm--library-files*)
(fill-library 
 '(("solver" 
    Nil 
    Nil 
    (?template
     make?variables
     ?variable-name
     assert!!
     ?solution
     find-any
     find-all
     solver-input
     solver-output
     next-solver-input
     next-solver-output
     reset-solver-output
     reset-solver-registry) 
    Nil)
  
   ("generators"
    Nil
    Nil
    (make?variable
     ?a-number
     ?a-real
     ?a-real-above
     ?a-real-below
     ?a-real-between
     ?a-real=
     ?an-integer
     ?an-integer-above
     ?an-integer-below
     ?an-integer-between
     ?an-integer=
     ?a-memberof
     make?variables)
    Nil)

   ("rules" 
    Nil
    Nil
    (?and
     ?or
     ?not
     ?abs
     ?%
     ?floor
     ?ceiling
     ?min
     ?max
     ?avg
     ?symxlat
     group-and-apply-definitions
     apply-definition-in-segments
     all-defn-in-segments
     any-defn-in-segments
     any-defn-each-segment
     transform-input-and-apply-rules
     ?+
     ?-
     ?*
     ?/
     ?equal
     ?=
     ?/=
     ?<
     ?>
     ?<=
     ?>=    
     ?<>
     ?<>=
     ?all<
     ?all>
     ?all<=
     ?all>=
     ?all/=
     ?all<>
     ?all<>=
     ?all-equal
     ?all/equal
     ?all=
     ?all-between
     ?all-different
     ?any<
     ?any>
     ?any<=
     ?any>=
     ?any=
     ?any/=
     ?anyeq
     ?any!eq
     ?between
     ?counttrues    
     ?member  
     ?membersof
     !membersof
     ?items-in
     ?items!in
     ?integers-in
     ?integers!in
     ?mapprules
     ?list+
     ?list-
     ?list*
     ?list/
     ?list%
     ?listabs
     ?listmin
     ?listmax
     ?listdx
     ?list<
     ?list>
     ?list<=
     ?list>=
     ?list=
     ?list/=
     ?lists=
     ?lists/=
     ?count-trues
     ?count-trues-in-list 
     ?crossw
     ?input-pcsin-mode)
    Nil)  
   
   ("map functions" 
    Nil
    Nil
    (map?and map2?and map3?and map4?and map5?and map?orv map2?or map3?or map4?or map5?or maplist?and maplist2?and maplist3?and maplist4?and maplist?orv maplist2?or maplist3?or maplist4?or ?any2comb ?all2comb map2comb list2comb map-func map2func map3func map-by-level symxlat symkeys) 
    Nil)    
   
   ("sequences" 
    Nil
    Nil
    (flatten-seqc
     mat-trans
     group-by-motion-type
     fuseseqc
     eventsms
     sequence->poly
     sequence->voices
     timepoints->voice-obj
     bt-group-list)
    Nil)

   ("utility" 
    Nil
    Nil
    (write-textfile
     eventsms     
     t2l::format-timestamp
     t2l::sequence->note-names
     alleq
     cartx2
     nsucc
     remove-consecutive-duplicates
     list-excerpt
     list-structure-equal
     all?variables-in
     ?variable-names)
    Nil)

   ("control panel" 
    Nil
    Nil
    (t2l::enable-variable-cache-map
     t2l::disable-variable-cache-map)
    Nil)
   ))
