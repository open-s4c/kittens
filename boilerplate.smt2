(set-option :opt.priority box)

; ------------------------------------------------------------------------------
; Define sorts and functions
; ------------------------------------------------------------------------------

(declare-datatype
 Operation ((read)
            (write)
            (rmw)
            (fence)
            (branch)))

(declare-datatype
 Event ((mk-event
         (eid Int)
         (tid Int)
         (op Operation)
         (addr Int)
         (rval Int)
         (wval Int)
         ;(marker Marker)
         )))

(declare-datatype
 Relation ((po)
           (rf)
           (co)
           (po-addr)
           (po-data)
           (po-ctrl)))

(declare-datatype
 Edge ((mk-edge
        (rel Relation)
        (src Int)
        (dst Int))))

(declare-const rels String)

; ------------------------------------------------------------------------------
; definition of events and edges
; ------------------------------------------------------------------------------
