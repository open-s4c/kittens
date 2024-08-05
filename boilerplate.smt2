; -----------------------------------------------------------------------------
; Define sorts and functions
; -----------------------------------------------------------------------------
(declare-datatype
 Operation ((read)
            (write)))

(declare-datatype
 Relation ((po)
           (co)
           (rf)
           (fr)))

(declare-datatype
 Event ((mk-event (eid Int)
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

(declare-fun inSet (Edge) Bool)

; -----------------------------------------------------------------------------
; Define relations
; -----------------------------------------------------------------------------

; Constraints for po
(assert (forall ((e Edge))
                (=> (and (= (rel e) (as po Relation)) (inSet e))
                    (and (< (porder (src e)) (porder (trg e)))
                         (= (tid (src e)) (tid (trg e)))))))
; Constraints for rf
(assert (forall ((e Edge))
                (=> (and (= (rel e) (as rf Relation)) (inSet e))
                    (and (= (op (src e)) (as write Operation))
                         (= (op (trg e)) (as read Operation))
                         (= (addr (src e)) (addr (trg e)))
			 (= (val (src e)) (val (trg e)))))))

; Constraints for fr
(assert (forall ((e Edge))
                (=> (and (= (rel e) (as fr Relation)) (inSet e))
                    (and (= (op (src e)) (as read Operation))
                         (= (op (trg e)) (as write Operation))
                         (< (corder (src e)) (corder (trg e)))
			 (= (addr (src e)) (addr (trg e)))
			 (not (= (val (src e)) (val (trg e))))))))

; Constraints for co
(assert (forall ((e Edge))
                (=> (and (= (rel e) (as co Relation)) (inSet e))
                    (and (= (addr (src e)) (addr (trg e)))
                         (< (corder (src e)) (corder (trg e)))
			 (= (op (src e)) (as write Operation))
                         (= (op (trg e)) (as write Operation))))))

; -----------------------------------------------------------------------------
; definition of events and edges
; -----------------------------------------------------------------------------

