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
           (RMW)))

(declare-datatype
 Event ((mk-event (uid Int)
                  (eid Int)
                  (tid Int)
                  (porder Int)
                  (corder Int)
                  (addr Int)
                  (val Int)
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
                         (not (= (val (src e)) (val (trg e))))))))

; Constraints for co
(assert (forall ((e Edge))
                (=> (and (= (rel e) (as co Relation)) (inEdgeSet e))
                    (and (= (addr (src e)) (addr (trg e)))
                         (< (corder (src e)) (corder (trg e)))
                         (or (= (op (src e)) (as write Operation))
			     (= (op (src e)) (as read-modify-write Operation)))
                         (= (op (trg e)) (as write Operation))
                         (not (= (val (trg e)) (val (src e))))))))

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
                         (= (op (trg e)) (as read-modify-write Operation))))))

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
                         (= (val e1) (val e2))
                         (= (op e1) (op e2))))))

; Constraint positive values and addresses of events
(assert (forall ((e1 Event))
                (=> (inEventSet e1)
                    (and (>= (val e1) 0)
                         (>= (addr e1) 0)))))

; All
(assert (forall ((ev Event))
                (=> (and (inEventSet ev)
                         (= (op ev) (as read Operation))
                         (exists ((e1 Edge) (e2 Edge))
                                 (and (inEdgeSet e1) (inEdgeSet e2)
                                      (= (eid (trg e1)) (eid ev))
                                      (= (eid (src e2)) (eid ev))
                                      (= (rel e1) (as po Relation))
                                      (= (rel e2) (as po Relation)))))
                    (= (val ev) 0))))

; -----------------------------------------------------------------------------
; definition of events and edges
; -----------------------------------------------------------------------------

