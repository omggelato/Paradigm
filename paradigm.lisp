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
(paradigm--print-warnings)
;--------------------------------------------------
; Seting the menu and sub-menu structure, and filling packages
; The sub-list syntax:
; ("sub-pack-name" subpack-lists class-list function-list class-alias-list)
(fill-library '(("calculation"
                 Nil
                 Nil
                 (?+ ?- ?* ?/ ?% ?avg ?abs ?list+ ?list- ?list* ?list/ ?list% ?listdx)
                 Nil)
                ("comparison"
                 Nil
                 Nil
                 (?integerp ?numberp ?booleanp ?member !member one?= one?eq ?/=any each?=oneof each?/=any 
                  ?< ?<= ?= ?> ?>= ?/= ?max ?min
                  ?list< ?list<= ?list= ?list> ?list>= ?list/= ?between 
                  ?and ?or ?not ?equal
                  map?and map?or maplist?and maplist?or)
                 Nil)
                ("transformation"
                 Nil
                 Nil
                 (map?car map?list map?func map-func)
                 Nil)
                ("search context"
                 Nil
                 Nil
                 (?template assert!! every!! ?solution a-partition-of an-ordered-partition-of group-list-nondeterministic 
                  ; setof 
                  ; takeout 
                  one-value print-values ith-value n-values all-values ?variables-in ?xs-in)
                 Nil)
                ("utility" 
                 Nil 
                 Nil 
                 (eventsms
                  fecho
                  flatt flat1 list-structure-equal list-depth
                  mat-trans) 
                 Nil)
                ("sequence-lists" 
                 Nil 
                 Nil 
                 (flatten-seqc) 
                 Nil)
                
                ("backtrack"
                 Nil
                 Nil
                 (either fail trail all-values one-values for-effects ith-value print-values possibly? necessarily? global local a-boolean a-member-of an-integer an-integer-above an-integer-below an-integer-between apply-nondeterministic funcall-nondeterministic multiple-value-call-nondeterministic nondeterministic-function? 
                         ; when-failing 
                         ; count-failures
                         )
                 NIL)
                ("constraints"
                 Nil
                 Nil
                 (make-variable assert! value-of apply-substitution bound? ground? applyv funcallv equalv template)
                 NIL)
                ("booleans"
                 Nil
                 Nil
                 (a-booleanv booleanpv known? decide notv andv orv count-truesv)
                 NIL)
                ("sequences"
                 Nil
                 Nil
                 (a-member-ofv memberv)
                 NIL)
                ("numbers"
                 Nil
                 (a-numberv a-realv a-real-abovev a-real-belowv a-real-betweenv an-integerv an-integer-abovev an-integer-belowv an-integer-betweenv numberpv realpv integerpv minv maxv +v -v *v /v <v <=v =v >v >=v /=v)
                 NIL)
                ("force-functions"
                 Nil
                 (?solution
                  linear-force
                  divide-and-conquer-force
                  random-force
                  domain-size
                  range-size)
                 NIL)
))