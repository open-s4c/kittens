(set-option :opt.priority box)

; -----------------------------------------------------------------------------
; Define sorts and functions
; -----------------------------------------------------------------------------
(declare-datatype
 Operation ((read)
            (write)
            (read-modify-write)))

(declare-datatype
 Relation ((po)
           (co)
           (rf)
           (fr)
           (W)
           (R)
           (RMW)
	   (ext)))

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
                  (op Operation))))

(declare-datatype
 Edge ((mk-edge (src Event)
                (trg Event)
                (rel Relation))))

(declare-fun inEdgeSet (Edge) Bool)
(declare-fun inEventSet (Event) Bool)

(declare-const rels String)

; -----------------------------------------------------------------------------
; Define relations
; -----------------------------------------------------------------------------

; Basic constraints for po
(assert (forall ((e Edge))
		(=> (and (= (rel e) (as po Relation)) (inEdgeSet e))
		    (and (= (tid (src e)) (tid (trg e)))
			 (< (porder (src e)) (porder (trg e)))))))

; Constraints for rf
(assert (forall ((e Edge))
                (=> (and (= (rel e) (as rf Relation)) (inEdgeSet e))
		    (and (or (= (op (src e)) (as write Operation)) (= (op (src e)) (as read-modify-write Operation)))
                         (or (= (op (trg e)) (as read Operation)) (= (op (trg e)) (as read-modify-write Operation)))
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

; Constraints for [W]
(assert (forall ((e Edge))
                (=> (and (= (rel e) (as W Relation)) (inEdgeSet e))
                    (and (= (eid (src e)) (eid (trg e)))
                         (= (op (src e)) (as write Operation))
                         (= (op (trg e)) (as write Operation))))))

; Constraints for [R]
(assert (forall ((e Edge))
                (=> (and (= (rel e) (as R Relation)) (inEdgeSet e))
                    (and (= (eid (src e)) (eid (trg e)))
                         (= (op (src e)) (as read Operation))
                         (= (op (trg e)) (as read Operation))))))
; Constraints for [RMW]
(assert (forall ((e Edge))
                (=> (and (= (rel e) (as RMW Relation)) (inEdgeSet e))
                    (and (= (eid (src e)) (eid (trg e)))
                         (= (op (src e)) (as read-modify-write Operation))
                         (= (op (trg e)) (as read-modify-write Operation))
			; (=> (not (exists ((e1 Edge)) 
			;	          (and (= (rel e1) (as rf Relation)) (inEdgeSet e1)
			;	 	       (or (and (= (eid (src e)) (eid (src e1))) 
			;				(= (eid (trg e)) (eid (trg e1)))) 
			;			   (and (= (eid (src e)) (eid (src e1))) 
			;				(= (eid (trg e)) (eid (trg e1))))))))			 
			;     (not (= (val-r (src e)) (val-w (trg e)))))
								       ))))

;(=> (and (= (rel e) (as RMW Relation)) (inEdgeSet e) 
;	 (not (exists ((e1 Edge)) 
;	              (and (= (rel e1) (as rf Relation)) (inEdgeSet e1)
;	 	           (or (and (= (eid (src e)) (eid (src e1))) 
;			    	    (= (eid (trg e)) (eid (trg e1)))) 
;			       (and (= (eid (src e)) (eid (src e1))) 
;				    (= (eid (trg e)) (eid (trg e1)))))))))			 
;     (not (= (val-r (src e)) (val-w (trg e)))))
 
; Constraints for ext
(assert (forall ((e Edge))
                (=> (and (= (rel e) (as ext Relation)) (inEdgeSet e))
                    (not (= (tid (src e)) (tid (trg e)))))))

; Constraint rf has only one source
(assert (forall ((e1 Edge) (e2 Edge))
                (=> (and (= (rel e1) (as rf Relation)) (inEdgeSet e1)
                         (= (rel e2) (as rf Relation)) (inEdgeSet e2)
                         (= (eid (trg e1)) (eid (trg e2))))
                    (= (eid (src e1)) (eid (src e2))))))

; Constraint same eid implies all fields same
(assert (forall ((e1 Event) (e2 Event))
                (=> (and (= (eid e1) (eid e2))
                         (inEventSet e1)
                         (inEventSet e2))
                    (and (= (tid e1) (tid e2))
                         (= (corder e1) (corder e2))
                         (= (porder e1) (porder e2))
                         (= (addr e1) (addr e2))
                         (= (val-r e1) (val-r e2))
                         (= (val-w e1) (val-w e2))
                         (= (val-e e1) (val-e e2))
                         (= (op e1) (op e2))))))

; Constraint positive values and addresses of events
(assert (forall ((e1 Event))
                (=> (inEventSet e1)
                    (and (>= (val-r e1) 0)
			 (< (val-r e1) 20)
			 (>= (val-w e1) 1)
			 (< (val-w e1) 20)
			 (>= (val-e e1) 0)
			 (< (val-e e1) 20)
                         (>= (tid e1) 0)
			 (< (tid e1) 50)
			 (>= (corder e1) 300)
			 (< (corder e1) 350)
			 (>= (porder e1) 200)
			 (< (porder e1) 250)
			 (>= (addr e1) 100)
			 (< (addr e1) 150)
			 ))))

; reads and RMW have to read from an rf or from an init event (thus reading 0)
;(assert (forall ((ev Event))
;                (=> (and (inEventSet ev)
;                         (or (= (op ev) (as read Operation))
;			     (= (op ev) (as read-modify-write Operation)))
;			 (not (exists ((e1 Edge))
;                                 (and (inEdgeSet e1)
;                                      (= (eid (trg e1)) (eid ev))
;                                      (= (rel e1) (as rf Relation))
;                                      ))))
;                    (= (val-r ev) 0))))
; 
;(assert
;                (=> (and 
;                         (or (= (op ev) (as read Operation))
;			     (= (op ev) (as read-modify-write Operation)))
;			 (not (exists ((e1 Edge))
;                                 (and (inEdgeSet e1)
;                                      (= (eid (trg e1)) (eid ev))
;                                      (= (rel e1) (as rf Relation))
;                                      ))))
;                    (= (val-r ev) 0)))
 
; -----------------------------------------------------------------------------
; definition of events and edges
; -----------------------------------------------------------------------------

