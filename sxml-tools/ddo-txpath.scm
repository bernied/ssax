;; Textual XPath implementation with distinct document order support

;=========================================================================
; Miscellaneous

; Implement 'or' as a function, so that we could 'apply' it
(define (ddo:or . args)
  (if (null? args) #f (or (car args) (apply ddo:or (cdr args)))))

; Definition of types
(define ddo:type-nodeset 'ddo:type-nodeset)
(define ddo:type-number 'ddo:type-number)
(define ddo:type-string 'ddo:type-string)
(define ddo:type-boolean 'ddo:type-boolean)
(define ddo:type-any 'ddo:type-any)


;=========================================================================
; Filtering pos-result with (position-based) predicates and combining
; a filtered pos-result into a distinct document order nodeset
;  pos-result ::= (listof pos-nodeset)
;  pos-nodeset ::= (listof (cons node order-num))
; Each pos-nodeset is a result of applying the axis to a single node in the
; input nodeset. Pos-result can be informally considered as
;  (map axis-pos input-nodeset)
; Each node in the pos-nodeset comes with its order number. An order-num is
; an integer, possibly a negative one. A node precedes another node in
; document order if the order-num of the former node is less than the order-num
; of the latter node. Equal order-nums (in different pos-nodesets) correspond
; to equal nodes.
; Each pos-nodeset is sorted in accordance with the position() of each of its
; members. Consequently, order-nums increase within pos-nodeset for forward
; XPath axes and decrease for reverse XPath axes.

; Whether pos-result in a forward order
; Return #t if in document order, #f if in reverse document order
(define (ddo:pos-result-forward? pos-result)
  (let loop ((pos-res pos-result))
    (cond
      ((null? pos-res)  ; every pos-nodeset has the length of <2
       #t)
      ((or (null? (car pos-res)) (null? (cdar pos-res)))
       ; this pos-nodeset has the length of less or equal to 1
       (loop (cdr pos-res)))
      (else
       (< (cdaar pos-res) (cdadar pos-res))))))

; Unites pos-result into a nodeset in distinct document order
(define (ddo:pos-result->nodeset pos-result)
  (letrec (; Combines 2 pos-nodesets into a single one
           (combine-2-pos-nodesets
            (lambda (chain1 chain2)
              (cond
                ((null? chain1) chain2)                
                ((null? chain2) chain1)
                ; None of the chains are null
                ((eq? (caar chain1) (caar chain2))  ; equal nodes
                 ; the same with (= (cdar chain1) (cdar chain2))
                 (cons (car chain1)
                       (combine-2-pos-nodesets (cdr chain1) (cdr chain2))))
                ((< (cdar chain1) (cdar chain2))
                 (cons (car chain1)
                       (combine-2-pos-nodesets (cdr chain1) chain2)))
                (else
                 (cons (car chain2)
                       (combine-2-pos-nodesets chain1 (cdr chain2))))))))
    (if
     (null? pos-result)  ; nothing to do
     pos-result
     (let ((pos-result (if (ddo:pos-result-forward? pos-result)
                           pos-result
                           (map reverse pos-result))))
      (let loop ((res (car pos-result))
                 (to-scan (cdr pos-result)))
        (if (null? to-scan)
            res
            (loop (combine-2-pos-nodesets res (car to-scan))
                  (cdr to-scan))))))))

;  pos-axis-impl ::= lambda
;  pred-impl-lst ::= (listof lambda)
; Every predicate is called with respect to each node
; Returns:  lambda
;  lambda ::= (lambda (nodeset position+size var-binding) ...)
(define (ddo:location-step-pos pos-axis-impl pred-impl-lst) 
  (lambda (nodeset position+size var-binding)
    (map
     car
     (ddo:pos-result->nodeset
      (map
       (lambda (pos-nodeset)
         (let iter-preds ((nset pos-nodeset)
                          (preds pred-impl-lst))
           (if
            (null? preds)
            nset
            (let ((size (length nodeset)))  ; context size
              (let iter-pairs ((nset nset)
                            (res '())
                            (pos 1))                          
                (if
                 (null? nset)  ; continue with the next predicate
                 (iter-preds (reverse res) (cdr preds))
                 (let ((val ((car preds)  ; predicate value
                             (list (caar nset)) (cons pos size) var-binding)))
                   (iter-pairs (cdr nset)
                               (if (if (number? val)
                                       (= val pos)
                                       (sxml:boolean val))
                                   (cons (car nset) res)
                                   res)
                               (+ pos 1)))))))))
       (pos-axis-impl nodeset))))))

;------------------------------------------------
; Implementation for location step for the other cases

; A location step for the axis which doesn't return a result in the form of
; a pos-nodeset, but instead resulting nodesets for each input node are in
; document order
;  pos-axis-impl ::= lambda
;  pred-impl-lst ::= (listof lambda)
; Every predicate is called with respect to each node
; Returns:  lambda
;  lambda ::= (lambda (nodeset position+size var-binding) ...)
; This function is somewhat similar to 'sxml:xpath-nodeset-filter' from
; "txpath.scm"
(define (ddo:location-step-non-intersect axis-impl pred-impl-lst)
  (lambda (nodeset position+size var-binding)
    (map-union
     (lambda (node)
       (let iter-preds ((nset (axis-impl node))
                        (preds pred-impl-lst))
         (if
          (null? preds)
          nset
          (let ((size (length nodeset)))  ; context size
            (let iter-nodes ((nset nset)
                             (res '())
                             (pos 1))                        
              (if
               (null? nset)  ; continue with the next predicate
               (iter-preds (reverse res) (cdr preds))
               (let ((val ((car preds)  ; predicate value
                           (list (car nset)) (cons pos size) var-binding)))
                 (iter-nodes (cdr nset)
                             (if (if (number? val)
                                     (= val pos)
                                     (sxml:boolean val))
                                 (cons (car nset) res)
                                 res)
                             (+ pos 1)))))))))
     nodeset)))

; A location step doesn't contain position-based predicates
(define (ddo:location-step-non-pos axis-impl pred-impl-lst)
  (lambda (nodeset position+size var-binding)
    (let iter-preds ((nset (axis-impl nodeset))
                     (preds pred-impl-lst))
      (if
       (null? preds)
       nset
       (let ((curr-pred (car preds)))
         (iter-preds
          (filter
           (lambda (node)
             (sxml:boolean
              (curr-pred (list node)
                         (cons 1 1)  ; dummy
                         var-binding)))
           nset)
          (cdr preds)))))))


;=========================================================================
; XPath AST processing
; AST is considered to be properly formed
; In the signature of functions below, the following terms are taken:
;  op - S-expression which represents the operation
;  num-anc - how many ancestors are required in the context after that
;   operation

; {5} <AxisSpecifier> ::= (axis-specifier  <AxisName> )
; {6} <AxisName> ::= (ancestor)
;                    | (ancestor-or-self)
;                    | (attribute)
;                    | (child)
;                    | (descendant)
;                    | (descendant-or-self)
;                    | (following)
;                    | (following-sibling)
;                    | (namespace)
;                    | (parent)
;                    | (preceding)
;                    | (preceding-sibling)
;                    | (self)
;
; single-level? - whether all nodes in the input nodeset are located on the
;  same level of tree hierarchy
; requires-position? - whether context position or context size are required to
;  filter the result produced by the axis
;
; For requires-position?=#f, the function returns
;  (list  axis-lambda
;         num-anc-it-requires
;         single-level?)
; For requires-position?=#t, the function returns
;  (list  axis-lambda
;         num-anc-it-requires
;         single-level?
;         pos-result?)
;  single-level? - whether nodes are in the single level after the axis
;  pos-result? - whether the result of the axis has the form of pos-result.
;   If #f, the axis returns its result in the form of the common nodeset
(define (ddo:ast-axis-specifier op num-anc single-level? requires-position?)
  (cond
    ((not (eq? (car op) 'axis-specifier))  ; AST error
     (draft:signal-semantic-error "not an AxisSpecifier - " op))
    (requires-position?
     (case (caadr op)  ; AxisName
       ((ancestor)
        (list ddo:ancestor-pos
              #f #f #t))
       ((ancestor-or-self)
        (list ddo:ancestor-or-self-pos
              #f #f #t))
       ((attribute)
        (list draft:attribute
              (draft:na-minus-nneg num-anc 1) single-level? #f))
       ((child)
        (if single-level?
            (list draft:child
                  (draft:na-minus-nneg num-anc 1) #t #f)
            (list ddo:child-pos
                  (draft:na-minus-nneg num-anc 1) #f #t)))
       ((descendant)
        (if single-level?
            (list draft:descendant
                  (draft:na-minus-nneg num-anc 1) #f #f)
            (list ddo:descendant-pos
                  (draft:na-minus-nneg num-anc 1) #f #t)))
       ((descendant-or-self)
        (if single-level?
            (list draft:descendant-or-self
                  num-anc #f #f)
            (list ddo:descendant-or-self-pos
                  num-anc #f #t)))
       ((following)
        ; DL: this is incorrect for single-level?=#f
        (list ddo:following-single-level-pos
              #f #f #t))
       ((following-sibling)
        (list (if single-level?
                  ddo:following-sibling-single-level-pos
                  ddo:following-sibling-pos)
              (draft:na-max num-anc 1) single-level? #t))
       ((namespace)
        (list draft:namespace
              (draft:na-minus-nneg num-anc 1) single-level? #f))
       ((parent)
        (list (if single-level? ddo:parent-single-level-pos ddo:parent-pos)
              (draft:na+ num-anc 1) single-level? #t))
       ((preceding)
        ; DL: this is incorrect for single-level?=#f
        (list ddo:preceding-single-level-pos
              #f #f #t))
       ((preceding-sibling)
        (list (if single-level?
                  ddo:preceding-sibling-single-level-pos
                  ddo:preceding-sibling-pos)
              (draft:na-max num-anc 1) single-level? #t))
       ((self)
        (list draft:self num-anc single-level? #f))
       (else
        (draft:signal-semantic-error "unknown AxisName - " op))))
    (else  ; doesn't require to keep position
     (case (caadr op)  ; AxisName
       ((ancestor)
        (list ddo:ancestor #f #f))
       ((ancestor-or-self)
        (list ddo:ancestor-or-self #f #f))
       ((attribute)
        (list draft:attribute
              (draft:na-minus-nneg num-anc 1) single-level?))
       ((child)
        (list (if single-level? draft:child ddo:child)
              (draft:na-minus-nneg num-anc 1) single-level?))
       ((descendant)
        (list (if single-level? draft:descendant ddo:descendant)
              (draft:na-minus-nneg num-anc 1) #f))
       ((descendant-or-self)
        (list (if single-level?
                  draft:descendant-or-self ddo:descendant-or-self)
              num-anc #f))
       ((following)
        (list (if single-level? ddo:following-single-level ddo:following)
              #f #f))
       ((following-sibling)
        (list (if single-level?
                  ddo:following-sibling-single-level ddo:following-sibling)
              (draft:na-max num-anc 1) single-level?))
       ((namespace)
        (list draft:namespace
              (draft:na-minus-nneg num-anc 1) single-level?))
       ((parent)
        (list (if single-level? ddo:parent-single-level ddo:parent)
              (draft:na+ num-anc 1) single-level?))
       ((preceding)
        (list (if single-level? ddo:preceding-single-level ddo:preceding)
              #f #f))
       ((preceding-sibling)
        (list (if single-level?
                  ddo:preceding-sibling-single-level ddo:preceding-sibling)
              (draft:na-max num-anc 1) single-level?))
       ((self)
        (list draft:self num-anc single-level?))
       (else
        (draft:signal-semantic-error "unknown AxisName - " op))))))

; {7} <NodeTest> ::= (node-test (*))
;                    | (node-test (namespace-uri  <String> ))
;                    | (node-test (namespace-uri  <String> )?
;                                 (local-name  <String> ))
;                    | (node-test (comment))
;                    | (node-test (text))
;                    | (node-test (pi <String>? ))
;                    | (node-test (point))
;                    | (node-test (range))
; For processing a node test, 'draft:ast-node-test' from "xpath-context.scm"
; can be used

;-------------------------------------------------
; In this section, each function accepts 3 arguments
;  op - S-expression which represents the operation
;  num-anc - how many ancestors are required in the context after that
;   operation
;  single-level? - for grammar rules that consume the nodeset type as input:
;   whether all nodes in the nodeset are located on the single level of the
;   tree hierarchy. If this is the case, most axes can be evaluated ealier than
;   in the general case.
;
; AST processing functions return either #f, which signals of a
; semantic error, or
;  (cons (lambda (nodeset position+size var-binding) ...)
;        num-anc-it-requires
;        single-level?
;        requires-position?
;        expr-type )
;  position+size - the same to what was called 'context' in TXPath-1
;  requires-position? - whether position() or last() functions are encountered
;   in the internal expression
;  expr-type - the type returned by the expression being process. The type is
;   determined by symbols. Possible types: number, string, boolean, nodeset and
;   any

; {1} <LocationPath> ::= <RelativeLocationPath>
;                        | <AbsoluteLocationPath>
(define (ddo:ast-location-path op num-anc single-level?)
  (case (car op)
    ((absolute-location-path)
     (ddo:ast-absolute-location-path op num-anc single-level?))
    ((relative-location-path)
     (ddo:ast-relative-location-path op num-anc single-level?))
    (else
     (draft:signal-semantic-error "improper LocationPath - " op))))

; {3} <RelativeLocationPath> ::= (relative-location-path  <Step>+ )
(define (ddo:ast-relative-location-path op num-anc single-level?)
  (if
   (not (eq? (car op) 'relative-location-path))
   (draft:signal-semantic-error "not a RelativeLocationPath - " op)
   (and-let*
    ((steps-res (ddo:ast-step-list (cdr op) num-anc single-level?)))
    (cons
     (if
      (null? (cdar steps-res))  ; only a single step
      (caar steps-res)
      (let ((converters (car steps-res)))
        (lambda (nodeset position+size var-binding)
          (let rpt ((nset nodeset)
                    (fs converters))
            (if (null? fs)
                nset
                (rpt ((car fs) nset position+size var-binding)
                     (cdr fs)))))))
     (cdr steps-res)  ; the remaining parameters
     ))))

; {4} <Step> ::= (step  <AxisSpecifier> <NodeTest> <Predicate>* )
;                | (range-to  (expr <Expr>)  <Predicate>* )
(define (ddo:ast-step op num-anc single-level?)
  (cond
    ((eq? (car op) 'range-to)
     (draft:signal-semantic-error "range-to function not implemented"))
    ((eq? (car op) 'step)
     (if
      (null? (cdddr op))  ; no Predicates
      (and-let*
       ((axis-lst (ddo:ast-axis-specifier
                   (cadr op) num-anc single-level? #f))
        (ntest (draft:ast-node-test (caddr op))))
       (let ((axis ((car axis-lst) ntest num-anc)))
         (list
          (lambda (nodeset position+size var-binding)
            (axis nodeset))
          (cadr axis-lst) (caddr axis-lst) #f ddo:type-nodeset)))
      ; There are Predicates
      (and-let*
       ((preds-res (ddo:ast-predicate-list (cdddr op) 0))
        (axis-lst (ddo:ast-axis-specifier
                   (cadr op)
                   (draft:na-max num-anc (cadr preds-res))
                   single-level?
                   (list-ref preds-res 4)  ; whether position required
                   ))
        (ntest (draft:ast-node-test (caddr op))))
       (let ((axis ((car axis-lst)
                    ntest (draft:na-max num-anc (cadr preds-res))))
             (pred-impl-lst (car preds-res)))
         (list
          (cond
            ((list-ref preds-res 4)  ; whether position required
             (ddo:location-step-non-pos axis pred-impl-lst))
            ((list-ref axis-lst 3)  ; pos-result?
             (ddo:location-step-pos axis pred-impl-lst))
            (else  ; non-intersect
             (ddo:location-step-non-intersect axis pred-impl-lst)))                         
          (cadr axis-lst)  ; num-ancestors
          (caddr axis-lst)  ; single-level? after this step
          #f
          ddo:type-nodeset)))))
    (else
     (draft:signal-semantic-error "not a Step - " op))))

; {4a} ( <Step>+ )
; Returns (list (listof step-impl)
;               num-anc single-level? requires-position? expr-type)
; or #f
; TECHNICAL NOTE: To calculate 'single-level?', we need to process steps in
; straight orger. To calculate 'num-anc', we need to process steps in reverse
; order. This thus has to be implemented in 2 passes
(define (ddo:ast-step-list step-lst num-anc single-level?)
  (let (; Calculates single-level? for each step in the step-lst
        ; Returns: (listof single-level?)
        ; where each member of the REVERSED result list corresponds to the step
        ; in the corresponding position of a step-lst
        ; We can notice that when single-level?=#f for some step, it remains
        ; #f for all the subsequent steps
        (calculate-single-level
         (lambda (step-lst single-level?)
           (let iter-steps ((steps step-lst)
                            (sl? single-level?)
                            (res '()))
             (cond
               ((null? steps) res)
               ((not sl?)  ; #f for the remaining steps
                (append (map
                         (lambda (step) #f)
                         step-lst)
                        res))
               (else  ; evaluate single-level? for the current step
                (and-let*
                 ((axis-lst (ddo:ast-axis-specifier
                             (cadar steps)  ; is to be axis specifier
                             0 sl? #f)))
                 (iter-steps (cdr steps)
                             (caddr axis-lst)  ; single-level for next step
                             (cons sl? res)))))))))
    (and-let*
     ((single-level-lst (calculate-single-level step-lst single-level?)))
     (let loop ((steps-to-view (reverse step-lst))
                (sl?-lst single-level-lst)
                (res-lst '())
                (num-anc num-anc))
       (if
        (null? steps-to-view)  ; everyone processed
        (list res-lst
              num-anc (car single-level-lst) #f ddo:type-nodeset)
        (and-let*
         ((step-res
           (ddo:ast-step (car steps-to-view) num-anc (car sl?-lst))))
         (loop
          (cdr steps-to-view)
          (cdr sl?-lst)
          (cons (car step-res) res-lst)
          (cadr step-res))))))))

; {8a} ( <Predicate>+ )
; Returns (list (listof pred-impl)
;               num-anc single-level? requires-position? expr-type)
; or #f
; NOTE: num-anc is dummy here, since it is always 0 for Predicates
; NOTE: single-level? is dummy here, since a Predicate is always called for
;  a single node to be filtered
; NOTE: information about the type for each Predicate is lost
(define (ddo:ast-predicate-list op-lst num-anc single-level?)
  (let ((pred-res-lst (map
                       (lambda (op) (ddo:ast-predicate op 0 #t))
                       op-lst)))
    (if
     (member #f pred-res-lst)  ; error detected
     #f
     (list (map car pred-res-lst)
           (apply draft:na-max (map cadr pred-res-lst))
           #t
           (apply ddo:or (map caddr pred-res-lst))
           ddo:type-any))))


;=========================================================================
; Highest level API functions
; The API is identical to the API of a context-based SXPath (here we even use
; API helpers from "xpath-context.scm"). For convenience, below we repeat
; comments for the API (borrowed from "xpath-context.scm")
;
; xpath-string - an XPath location path (a string)
; ns+na - can contain 'ns-binding' and/or 'num-ancestors' and/or none of them
; ns-binding - declared namespace prefixes (an optional argument)
;  ns-binding ::= (listof (prefix . uri))
;  prefix - a symbol
;  uri - a string
; num-ancestors - number of ancestors required for resulting nodeset. Can
;  generally be omitted and is than defaulted to 0, which denotes a _usual_
;  nodeset. If a negative number, this signals that all ancestors should be
;  remembered in the context
;
; Returns: (lambda (nodeset position+size var-binding) ...)
; position+size - the same to what was called 'context' in TXPath-1
; var-binding - XPath variable bindings (an optional argument)
;  var-binding = (listof (var-name . value))
;  var-name - (a symbol) a name of a variable
;  value - its value. The value can have the following type: boolean, number,
;  string, nodeset. NOTE: a node must be represented as a singleton nodeset

; Helper for constructing several highest-level API functions
(define (ddo:api-helper grammar-parser ast-parser)
  (lambda (xpath-string . ns+na)
    (let-values
        (((ns-binding num-anc) (draft:arglist->ns+na ns+na)))
      (and-let*
       ((ast (grammar-parser xpath-string ns-binding))
        (impl-lst (ast-parser ast num-anc
                              #t  ; we suppose single-level?=#t for src
                              )))
       (let ((res (car impl-lst)))
         (lambda (node . var-binding)
           ((if (and num-anc (zero? num-anc))
                draft:contextset->nodeset
                (lambda (x) x))             
            (res (as-nodeset node) (cons 1 1)
                 (if (null? var-binding) var-binding (car var-binding))))))))))

(define ddo:xpath (ddo:api-helper txp:xpath->ast ddo:ast-location-path))
;(define ddot:xpath-expr (ddo:api-helper txp:expr->ast ddo:ast-expr))
