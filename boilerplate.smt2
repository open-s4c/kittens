(set-option :opt.priority box)

; -----------------------------------------------------------------------------
; Define sorts and functions
; -----------------------------------------------------------------------------

(declare-datatype
 Operation ((read)
            (write)
            (read-modify-write)
	    (fence)
	    (branch)))

(declare-datatype
 Relation ((po)
           (co)
           (rf)
           ; (fr)
           (w)
           (r)
           (xchg)
	   (faa)
	   (cas-s)
	   (cas-f)
           (f)
           (b)

	   (plain)
           (acq)
           (rlx)
           (sc)
           (release)
           (rel-acq)
	   (not-plain)
           (not-acq)
           (not-rlx)
           (not-sc)
           (not-release)
           (not-rel-acq)

	   (ext)
	   (int)
	   (ctrl-a-dep)
	   (ctrl-b-dep)
	   (data-dep)
	   (addr-dep)
           (loc)
	   (all)))

(declare-datatype
 Marker ((Plain)
	 (ACQ)
	 (RLX)
	 (REL)
	 (SC)))

(declare-datatype
 Argument ((ctrl)
           (addr)
           (data)
	   (reg)))

(declare-datatype
 RMW-Type ((None)
	   (XCHG)
	   (FAA)
	   (CAS)))

(declare-datatype
 Addr-Chain ((no-chain)
             (start-chain)
             (middle-chain)))

(declare-datatype
 Event ((mk-event (uid Int)
                  (eid Int)
                  (tid Int)
                  (porder Int)
                  (corder Int)
                  (addr Int)
                  (val-r Int)
                  (val-w Int)
                  (val-e Int)
                  (val-d Int)
                  (op Operation)
		  (rmw-type RMW-Type)
		  (marker1 Marker)
		  (marker2 Marker)
		  (arg Argument)
		  (obs Bool)
		  (ass Bool)
      (chain-type Addr-Chain)
)))

(declare-datatype
 Edge ((mk-edge (src Event)
                (trg Event)
                (rel Relation))))

(declare-fun inEdgeSet (Edge) Bool)
(declare-fun inEventSet (Event) Bool)

(declare-const rels String)

; -----------------------------------------------------------------------------
; Define base relations
; -----------------------------------------------------------------------------

; Basic constraints for po
(assert (forall ((e Edge))
		(=> (and (= (rel e) (as po Relation)) (inEdgeSet e))
		    (and (= (tid (src e)) (tid (trg e)))
			 (not (= (op (src e)) (as branch Operation)))   ; IF THERE EXISTS A MATCHING CTRL THEN THIS CAN BE BRANCH!!
			 (not (= (op (trg e)) (as branch Operation)))
			 (< (porder (src e)) (porder (trg e)))))))

; Constraints for rf
(assert (forall ((e Edge))
                (=> (and (= (rel e) (as rf Relation)) (inEdgeSet e))
		    (and (or (= (op (src e)) (as write Operation))
			     (= (op (src e)) (as read-modify-write Operation)))
                         (or (= (op (trg e)) (as read Operation))
			     (= (op (trg e)) (as read-modify-write Operation)))
                         (= (addr (src e)) (addr (trg e)))
                         (= (val-w (src e)) (val-r (trg e)))))))

; Constraints for co
(assert (forall ((e Edge))
                (=> (and (= (rel e) (as co Relation)) (inEdgeSet e))
                    (and (= (addr (src e)) (addr (trg e)))
                         (< (corder (src e)) (corder (trg e)))
                         (or (= (op (src e)) (as write Operation))
			     (= (op (src e)) (as read-modify-write Operation)))
                         (or (= (op (trg e)) (as write Operation))
			     (= (op (trg e)) (as read-modify-write Operation)))
                         (not (= (val-w (trg e)) (val-w (src e))))))))

; Constraints for ext
(assert (forall ((e Edge))
                (=> (and (= (rel e) (as ext Relation)) (inEdgeSet e))
                    (not (= (tid (src e)) (tid (trg e)))))))

; Constraints for int
(assert (forall ((e Edge))
                (=> (and (= (rel e) (as int Relation)) (inEdgeSet e))
                    (= (tid (src e)) (tid (trg e))))))

; Constrinats for loc
(assert (forall ((e Edge))
                (=> (and (= (rel e) (as loc Relation)) (inEdgeSet e))
                    (= (addr (src e)) (addr (trg e))))))

; -----------------------------------------------------------------------------
; Define dependencies
; -----------------------------------------------------------------------------

; Constraints for addr-dep
(assert (forall ((e Edge))
		(=> (and (= (rel e) (as addr-dep Relation)) (inEdgeSet e))
		    (and (= (tid (src e)) (tid (trg e)))
			 (= (porder (src e)) (- (porder (trg e)) 1))
			 (or (= (op (src e)) (as read Operation))
			     (= (op (src e)) (as read-modify-write Operation)))
			 (or (= (op (trg e)) (as read Operation))
			     (= (op (trg e)) (as write Operation))
			     (= (op (trg e)) (as read-modify-write Operation)))
			 (= (val-r (src e)) (addr (trg e)))))))

; Constraints for data-dep
(assert (forall ((e Edge))
		(=> (and (= (rel e) (as data-dep Relation)) (inEdgeSet e))
		    (and (= (tid (src e)) (tid (trg e)))
			 (= (porder (src e)) (- (porder (trg e)) 1))
			 (or (= (op (src e)) (as read Operation))
			     (= (op (src e)) (as read-modify-write Operation)))
			 (or (= (op (trg e)) (as write Operation))
			     (= (op (trg e)) (as read-modify-write Operation)))
			 (= (val-r (src e)) (val-w (trg e)))))))

; Constraints for ctrl-a-dep
(assert (forall ((e Edge))
		(=> (and (= (rel e) (as ctrl-a-dep Relation)) (inEdgeSet e))
		    (and (= (tid (src e)) (tid (trg e)))
			 (= (porder (src e)) (- (porder (trg e)) 1))
			 (or (= (op (src e)) (as read Operation))
			     (= (op (src e)) (as read-modify-write Operation)))
			 (= (op (trg e)) (as branch Operation))
			 (= (val-r (src e)) (val-e (trg e)))))))    ; IN THE IF CHECK WE CHECK AGAINST VALUE-EXPECTED

; Constraints for ctrl-b-dep
(assert (forall ((e Edge))
		(=> (and (= (rel e) (as ctrl-b-dep Relation)) (inEdgeSet e))
		    (and (= (tid (src e)) (tid (trg e)))
			 (= (porder (src e)) (- (porder (trg e)) 1))
			 (= (op (src e)) (as branch Operation))
			 (or (= (op (trg e)) (as read Operation))
			     (= (op (trg e)) (as write Operation))
			     (= (op (trg e)) (as fence Operation))
			     (= (op (trg e)) (as read-modify-write Operation)))))))

; -----------------------------------------------------------------------------
; Define event type relations
; -----------------------------------------------------------------------------

; Constraints for [W]
(assert (forall ((e Edge))
                (=> (and (= (rel e) (as w Relation)) (inEdgeSet e))
                    (and (= (op (src e)) (as write Operation))
                         (= (op (trg e)) (as write Operation))))))

; Constraints for [R]
(assert (forall ((e Edge))
                (=> (and (= (rel e) (as r Relation)) (inEdgeSet e))
                    (and (= (op (src e)) (as read Operation))
                         (= (op (trg e)) (as read Operation))))))

; Constraints for [XCHG]
(assert (forall ((e Edge))
                (=> (and (= (rel e) (as xchg Relation)) (inEdgeSet e))
                    (and (= (op (src e)) (as read-modify-write Operation))
                         (= (op (trg e)) (as read-modify-write Operation))
			 (= (rmw-type (src e)) (as XCHG RMW-Type))
			 (= (rmw-type (trg e)) (as XCHG RMW-Type))))))

; Constraints for [FAA]
(assert (forall ((e Edge))
                (=> (and (= (rel e) (as faa Relation)) (inEdgeSet e))
                    (and (= (op (src e)) (as read-modify-write Operation))
                         (= (op (trg e)) (as read-modify-write Operation))
			 (= (rmw-type (src e)) (as FAA RMW-Type))
			 (= (rmw-type (trg e)) (as FAA RMW-Type))))))

; Constraints for [CAS-S]
(assert (forall ((e Edge))
                (=> (and (= (rel e) (as cas-s Relation)) (inEdgeSet e))
                    (and (= (op (src e)) (as read-modify-write Operation))
                         (= (op (trg e)) (as read-modify-write Operation))
			 (= (rmw-type (src e)) (as CAS RMW-Type))
			 (= (rmw-type (trg e)) (as CAS RMW-Type))
			 (= (val-e (src e)) (val-r (src e)))
			 (= (val-w (src e)) (val-d (src e)))
			 (or (exists ((e1 Event)) (and (inEventSet e1) (= (val-w e1) (val-e (src e)))))
			     (= (val-e (src e)) 0))

			; (=> (exists ((ed Edge))
			;	(and (inEdgeSet ed) (= (trg ed) (src e)) (= (rel ed) (as addr-dep Relation))))
			;     (= (val-r (src e)) ()))
))))

; Constraints for [CAS-F]
(assert (forall ((e Edge))
                (=> (and (= (rel e) (as cas-f Relation)) (inEdgeSet e))
                    (and (= (op (src e)) (as read-modify-write Operation))
                         (= (op (trg e)) (as read-modify-write Operation))
			 (= (rmw-type (src e)) (as CAS RMW-Type))
			 (= (rmw-type (trg e)) (as CAS RMW-Type))
		    (not (= (val-e (src e)) (val-r (src e))))
			 (= (val-w (src e)) (val-e (src e)))
		    ;(not (exists ((e1 Event)) (and (not (= (eid e1) (eid (src e)))) (= (val-w e1) (val-e (src e))))))
		    (not (= (val-e (src e)) 0))
))))
; Constraints for [F]
(assert (forall ((e Edge))
                (=> (and (= (rel e) (as f Relation)) (inEdgeSet e))
                    (and (= (op (src e)) (as fence Operation))
                         (= (op (trg e)) (as fence Operation))))))

; Constraints for [B]
(assert (forall ((e Edge))
                (=> (and (= (rel e) (as b Relation)) (inEdgeSet e))
                    (and (= (op (src e)) (as branch Operation))
                         (= (op (trg e)) (as branch Operation))))))

; -----------------------------------------------------------------------------
; Define markers
; -----------------------------------------------------------------------------

; Constraints for [Plain]
(assert (forall ((e Edge))
                (=> (and (= (rel e) (as plain Relation)) (inEdgeSet e))
                    (and (= (marker1 (src e)) (as Plain Marker))
                         (= (marker1 (trg e)) (as Plain Marker))))))
; Constraints for [SC]
(assert (forall ((e Edge))
                (=> (and (= (rel e) (as sc Relation)) (inEdgeSet e))
                    (and (= (marker1 (src e)) (as SC Marker))
                         (= (marker1 (trg e)) (as SC Marker))))))
; Constraints for [ACQ]
(assert (forall ((e Edge))
                (=> (and (= (rel e) (as acq Relation)) (inEdgeSet e))
                    (and (= (marker1 (src e)) (as ACQ Marker))
                         (= (marker1 (trg e)) (as ACQ Marker))))))
; Constraints for [REL]
(assert (forall ((e Edge))
                (=> (and (= (rel e) (as release Relation)) (inEdgeSet e))
                    (and (= (marker1 (src e)) (as REL Marker))
                         (= (marker1 (trg e)) (as REL Marker))))))
; Constraints for [RLX]
(assert (forall ((e Edge))
                (=> (and (= (rel e) (as rlx Relation)) (inEdgeSet e))
                    (and (= (marker1 (src e)) (as RLX Marker))
                         (= (marker1 (trg e)) (as RLX Marker))))))

; Constraints for ~[Plain]
(assert (forall ((e Edge))
                (=> (and (= (rel e) (as not-plain Relation)) (inEdgeSet e))
                    (and (not (= (marker1 (src e)) (as Plain Marker)))
                         (not (= (marker1 (trg e)) (as Plain Marker)))))))
; Constraints for ~[SC]
(assert (forall ((e Edge))
                (=> (and (= (rel e) (as not-sc Relation)) (inEdgeSet e))
                    (and (not (= (marker1 (src e)) (as SC Marker)))
                         (not (= (marker1 (trg e)) (as SC Marker)))))))
; Constraints for ~[ACQ]
(assert (forall ((e Edge))
                (=> (and (= (rel e) (as not-acq Relation)) (inEdgeSet e))
                    (and (not (= (marker1 (src e)) (as ACQ Marker)))
                         (not (= (marker1 (trg e)) (as ACQ Marker)))))))
; Constraints for ~[REL]
(assert (forall ((e Edge))
                (=> (and (= (rel e) (as not-release Relation)) (inEdgeSet e))
                    (and (not (= (marker1 (src e)) (as REL Marker)))
                         (not (= (marker1 (trg e)) (as REL Marker)))))))
; Constraints for ~[RLX]
(assert (forall ((e Edge))
                (=> (and (= (rel e) (as not-rlx Relation)) (inEdgeSet e))
                    (and (not (= (marker1 (src e)) (as RLX Marker)))
                         (not (= (marker1 (trg e)) (as RLX Marker)))))))

(assert (forall ((e1 Event))
		(=> (and (= (op e1) (as read Operation)) (inEventSet e1))
		    (or (= (marker1 e1) (as SC Marker))
			(= (marker1 e1) (as ACQ Marker))
			(= (marker1 e1) (as Plain Marker))
			(= (marker1 e1) (as RLX Marker))))))

(assert (forall ((e1 Event))
		(=> (and (= (op e1) (as write Operation)) (inEventSet e1))
		    (or (= (marker1 e1) (as SC Marker))
			(= (marker1 e1) (as REL Marker))
			(= (marker1 e1) (as Plain Marker))
			(= (marker1 e1) (as RLX Marker))))))

(assert (forall ((e1 Event))
		(=> (and (= (op e1) (as read-modify-write Operation)) (inEventSet e1))
		    (or (= (marker1 e1) (as SC Marker))
			(= (marker1 e1) (as ACQ Marker))
			(= (marker1 e1) (as REL Marker))
			(= (marker1 e1) (as RLX Marker))))))

(assert (forall ((e1 Event))
		(=> (and (= (op e1) (as fence Operation)) (inEventSet e1))
		    (or (= (marker1 e1) (as SC Marker))
			(= (marker1 e1) (as ACQ Marker))
			(= (marker1 e1) (as REL Marker))
			(= (marker1 e1) (as RLX Marker))))))

; -----------------------------------------------------------------------------
; Constraint positive values and addresses of events
; -----------------------------------------------------------------------------

(assert (forall ((e1 Event))
                (=> (inEventSet e1)
                    (and (>= (val-r e1) 0)
			 (< (val-r e1) 50)
			 (=> (not (= (arg e1) (as data Argument)))
          (>= (val-w e1) 1))
	     (=> (= (arg e1) (as data Argument))
          (>= (val-w e1) 0))
       (< (val-w e1) 50)
			 (>= (val-e e1) 0)
			 (< (val-e e1) 50)
			 (>= (val-d e1) 1)
			 (< (val-d e1) 50)
                         (>= (tid e1) 51)
			 (< (tid e1) 100)
			 (>= (corder e1) 300)
			 (< (corder e1) 350)
			 (>= (porder e1) 200)
			 (< (porder e1) 250)
			 (>= (addr e1) 0)
			 (< (addr e1) 50)
			 ))))

(assert (forall ((e1 Event))
		(=> (inEventSet e1)
		    (= (not (= (op e1) (as read-modify-write Operation)))
 		       (= (rmw-type e1) (as None RMW-Type))))))

(assert (forall ((e1 Event))
		(=> (and (inEventSet e1) (= (op e1) (as read-modify-write Operation)))
		    (or (and (= (marker1 e1) (as RLX Marker)) (= (marker2 e1) (as RLX Marker)))
			(and (= (marker1 e1) (as REL Marker)) (= (marker2 e1) (as RLX Marker)))
			(and (= (marker1 e1) (as REL Marker)) (= (marker2 e1) (as ACQ Marker)))
			(and (= (marker1 e1) (as ACQ Marker)) (= (marker2 e1) (as RLX Marker)))
			(and (= (marker1 e1) (as ACQ Marker)) (= (marker2 e1) (as ACQ Marker)))
			(and (= (marker1 e1) (as SC Marker)) (= (marker2 e1) (as RLX Marker)))
			(and (= (marker1 e1) (as SC Marker)) (= (marker2 e1) (as ACQ Marker)))
			(and (= (marker1 e1) (as SC Marker)) (= (marker2 e1) (as SC Marker)))))))

; -----------------------------------------------------------------------------
; definition of events and edges
; -----------------------------------------------------------------------------
