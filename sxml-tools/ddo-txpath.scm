;; XPath implementation with distinct document order support
;
; This software is in Public Domain.
; IT IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND.
;
; Please send bug reports and comments to:
;   lizorkin@hotbox.ru    Dmitry Lizorkin

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

;------------------------------------------------
; Comparison for nodesets
; In order to compare nodesets produced by conventional SXPath and SXPath with
; distinct document order support, we must take into account that members in
; each of the nodesets being compared can be ordered differently.

; Whether all members from the first nodeset are contained in the second
; nodeset
(define (ddo:nset-contained? nodeset1 nodeset2)
  (cond
    ((null? nodeset1) #t)
    ((memq (car nodeset1) nodeset2)
     (ddo:nset-contained? (cdr nodeset1) nodeset2))
    (else #f)))

(define (ddo:nset-equal? nodeset1 nodeset2)
  (and (ddo:nset-contained? nodeset1 nodeset2)
       (ddo:nset-contained? nodeset2 nodeset1)))


;=========================================================================
; Different cases of nodeset filtering

;------------------------------------------------
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
            (let ((size (length nset)))  ; context size
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
          (let ((size (length nset)))  ; context size
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

;------------------------------------------------
; Implementations for FilterExpr

; Implementing FilterExpr in the general case, for position-based predicates
(define (ddo:filter-expr-general expr-impl pred-impl-lst)
  (lambda (nodeset position+size var-binding)
    (let ((prim-res (expr-impl nodeset position+size var-binding)))
      (cond
        ((not (nodeset? prim-res))
         (sxml:xpointer-runtime-error 
          "expected - nodeset instead of " prim-res)
         '())
        (else
         (let iter-preds ((nset prim-res)
                          (preds pred-impl-lst))
           (if
            (null? preds)
            nset
            (let ((size (length nset)))  ; context size
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
                               (+ pos 1)))))))))))))

; A FilterExpr doesn't contain position-based predicates
; NOTE: This function is very similar to 'ddo:location-step-non-pos'
;  Should think of combining them.
(define (ddo:filter-expr-non-pos expr-impl pred-impl-lst)
  (lambda (nodeset position+size var-binding)
    (let ((prim-res (expr-impl nodeset position+size var-binding)))
      (cond
        ((not (nodeset? prim-res))
         (sxml:xpointer-runtime-error 
          "expected - nodeset instead of " prim-res)
         '())
        (else
         (let iter-preds ((nset prim-res)
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
               (cdr preds))))))))))

;  Filter expression, with a single predicate of the special structure, like
;  [position()=1]
; special-pred-impl ::= (lambda (nodeset) ...)  - filters the nodeset
(define (ddo:filter-expr-special-predicate expr-impl special-pred-impl)
  (lambda (nodeset position+size var-binding)
    (let ((prim-res (expr-impl nodeset position+size var-binding)))
      (if
       (not (nodeset? prim-res))
       (begin
         (sxml:xpointer-runtime-error
          "expected - nodeset instead of " prim-res)
         '())
       (special-pred-impl prim-res)))))


;=========================================================================
; Uniting context-sets, preserving distinct document order
; Is required for XPath UnionExpr

; Returns all contexts of the document, including the ones for attribute nodes
; and for attribute value nodes. All contexts are returned in document order,
; attribute value nodes immediately follow attribute nodes
(define (ddo:all-contexts-in-doc doc)
  (let iter-nodes ((contents (map
                              (lambda (kid) (list kid doc))
                              ((sxml:child sxml:node?) doc)))
                   (res (list doc)))
    (cond
      ((null? contents)  ; every content processed
       (reverse res))
      ((not ((ntype?? '*) (caar contents)))  ; text node or PI or etc.
       (iter-nodes (cdr contents)
                   (cons
                    (draft:make-context (caar contents) (cdar contents))
                    res)))
      (else  ; element node
       (let iter-attrs ((attrs (sxml:attr-list (caar contents)))
                        (res (cons
                              (draft:make-context
                               (caar contents) (cdar contents))
                              res)))
         (cond
           ((null? attrs)  ; all attributes of a given element processed
            (iter-nodes
             (append (map
                      (lambda (kid) (cons kid (car contents)))
                      ((sxml:child sxml:node?) (caar contents)))
                     (cdr contents))
             res))
           ((not (sxml:node? (car attrs)))  ; aux node of SXML 3.0
            (iter-attrs (cdr attrs) res))
           ((null? (cdar attrs))  ; singular attribute
            (iter-attrs (cdr attrs)
                        (cons
                         (draft:make-context (car attrs) (car contents))
                         res)))
           (else  ; an attribute has a value
            (iter-attrs
             (cdr attrs)
             (cons  ; attribute value
              (draft:make-context (cadar attrs)
                                  (cons (car attrs) (car contents)))
              (cons
               (draft:make-context (car attrs) (car contents))
               res))))))))))

; Every context in both context-sets must contain all the ancestors of the
; context node (this corresponds to the num-ancestors=#f)
; All nodes must have one and the same root node (i.e. this function cannot
; correctly unite context-sets whose members belong to different documents)
; Returns the context-set that is a distinct-document-order union of the
; argument context-sets
(define (ddo:unite-2-contextsets cntset1 cntset2)
  (if
   (null? cntset1)  ; nothing to do
   cntset2
   (let loop ((order (ddo:all-contexts-in-doc
                      (draft:list-last
                       (sxml:context->content (car cntset1)))))
              (cntset1 cntset1)
              (cntset2 cntset2)
              (res '()))
     (cond
       ((null? cntset1)
        (append (reverse res) cntset2))
       ((null? cntset2)
        (append (reverse res) cntset1))
       ; order should never be null
       ((eq? (sxml:context->node (car order))
             (sxml:context->node (car cntset1)))
        (loop (cdr order)
              (cdr cntset1)
              (if (eq? (sxml:context->node (car cntset1))
                       (sxml:context->node (car cntset2)))
                  (cdr cntset2)
                  cntset2)
              (cons (car cntset1) res)))
       ((eq? (sxml:context->node (car order))
             (sxml:context->node (car cntset2)))
        (loop (cdr order)
              cntset1
              (cdr cntset2)              
              (cons (car cntset2) res)))
       (else
        (loop (cdr order) cntset1 cntset2 res))))))

; Based on the function for uniting 2 context-sets, unites multiple
; context-sets
(define (ddo:unite-multiple-context-sets . context-sets)
  (if (null? context-sets)  ; nothing to do
      '()
      (let loop ((res (car context-sets))
                 (more (cdr context-sets)))
        (if (null? more)
            res
            (loop (ddo:unite-2-contextsets res (car more))
                  (cdr more))))))


;=========================================================================
; Optimizing special predicates like [position()=1] and the like

; Similar to R5RS list-tail, but returns an empty list when k > (length lst)
(define (ddo:list-tail lst k)
  (if (or (null? lst) (<= k 0))
      lst
      (ddo:list-tail (cdr lst) (- k 1))))

; Takes the first k members of the list
; The whole list is taken when k > (length lst)
(define (ddo:list-head lst k)
  (if (or (null? lst) (<= k 0))
      '()
      (cons (car lst) (ddo:list-head (cdr lst) (- k 1)))))

; Similar to R5RS list-tail, but returns an empty list when
; (or (< k 0) (> k (length lst))
(define (ddo:list-ref lst k)
  (cond ((null? lst) lst)
        ((zero? k) (car lst))
        (else (ddo:list-ref (cdr lst) (- k 1)))))

;-------------------------------------------------
; Checks for a special structure of the predicate in its AST representation

; Checks whether the given op is the AST representation to a function call
; to position()
(define ddo:check-ast-position?
  (let ((ddo:ast-for-position-fun-call   ; evaluate just once
         (txp:expr->ast "position()")))
    (lambda (op)
      (equal? op ddo:ast-for-position-fun-call))))

; If the given op is the AST representation for a number and this number is
; exact, returns this number. Otherwise returns #f
(define (ddo:check4ast-number op)
  (if
   (eq? (car op) 'number)    
   (let ((number (cadr op)))
     (if (and (number? number) (exact? number))
         number #f))
   #f))

;  In case when the predicate has one of the following forms:
; SpecialPredicate ::= [ Number ]
;                      | [ position() CmpOp Number ]
;                      | [ Number CmpOp position() ]
; CmpOp ::= > | < | >= | <= | =
; Number - an integer
;  than returns (lambda (nodeset) ...), where the lambda performs the required
;  filtering as specified by the predicate.
;  For a different sort of a predicate, returns #f
;  The function doesn't signal of any semantic errors.
(define (ddo:check-special-predicate op)
  (if
   (not (eq? (car op) 'predicate))
   #f  ; an improper AST
   (let ((expr (cadr op)))
     (cond
       ((ddo:check4ast-number expr)
        => (lambda (num)
             (lambda (nodeset) (ddo:list-ref nodeset (- num 1)))))
       ((and (memq (car expr) '(= > < >= <=))
             (= (length expr) 3))
        (let-values*
         (((cmp-op num)
           (cond
             ((and (ddo:check-ast-position? (cadr expr))
                   (ddo:check4ast-number (caddr expr)))
              => (lambda (num) (values (car expr) num)))
             ((and (ddo:check-ast-position? (caddr expr))
                   (ddo:check4ast-number (cadr expr)))
              => (lambda (num)
                   (values
                    (cond   ; invert the cmp-op
                      ((assq (car expr) '((< . >) (> . <) (>= . <=) (<= . >=)))
                       => cdr)
                      (else (car expr)))
                    num)))
             (else
              (values #f #f)))))        
         (if
          (not num)
          #f
          (case cmp-op
            ((=)
             (lambda (nodeset) (ddo:list-ref nodeset (- num 1))))
            ((>)
             (lambda (nodeset) (ddo:list-tail nodeset num)))
            ((>=)
             (lambda (nodeset) (ddo:list-tail nodeset (- num 1))))
            ((<)
             (lambda (nodeset) (ddo:list-head nodeset (- num 1))))
            ((<=)
             (lambda (nodeset) (ddo:list-head nodeset num)))
            (else   ; internal error
             #f)))))
       (else  ; not an equality or relational expr with 2 arguments
        #f)))))


;=========================================================================
; Optimization for deeply nested predicates
; For predicates whose level of nesting exceeds 3, these predicates are likely
; to be called for more than n^3 times, where n is the number of nodes in an
; SXML document being processed. For such predicates, it is desirable to
; evaluate them in advance, for every combination of context node, context
; position and context size (the latter two components are not even required
; if the predicate doesn't use position).
; Such an optimization allows achieving a polinomial-time complexity for any
; XPath expression

(define (ddo:generate-pred-id)
  (string->symbol
   (string-append "*predicate-" (symbol->string (gensym)) "*")))

;-------------------------------------------------
; Search for predicate values
; Predicate values are added to var-binding

; Predicate value for a predicate that doesn't require position
; Predicate values are stored in the form of
; pred-values ::= (cons  pred-id
;                        (listof  (cons  node  pred-value)))
; NOTE: A node (and not a context) is used as a key in the alist
(define (ddo:get-pred-value pred-id)
  (lambda (nodeset position+size var-binding)         
    (cond
      ((not (and (nodeset? nodeset)
                 (null? (cdr nodeset))))
       (sxml:xpointer-runtime-error
        "internal DDO SXPath error - "
        "a predicate is supplied with a non-singleton nodeset: " pred-id)
       #f)
      ((assq pred-id var-binding)
       => (lambda (binding)
            (cond
              ((assq (sxml:context->node (car nodeset))
                     (cdr binding))
               => (lambda (pair) (force (cdr pair)))
               ; => cdr   ; DL: was
               )
              (else
               (sxml:xpointer-runtime-error
                "internal DDO SXPath error - predicate value not found: "
                pred-id)
               #f))))
      (else
       (sxml:xpointer-runtime-error
        "internal DDO SXPath error - predicate value not found: " pred-id)
       #f))))

; Predicate value for a predicate that requires position
; Predicate values are stored in the form of
; pred-values ::=
;  (cons pred-id
;        (listof
;         (cons node
;               (listof
;                (cons size
;                      (listof
;                       (cons position pred-value)))))))
; NOTE: A node (and not a context) is used as a key in the alist
(define (ddo:get-pred-value-pos pred-id)
  (lambda (nodeset position+size var-binding)
    (cond
      ((not (and (nodeset? nodeset)
                 (null? (cdr nodeset))))
       (sxml:xpointer-runtime-error
        "internal DDO SXPath error - "
        "a predicate is supplied with a non-singleton nodeset: " pred-id)
       #f)
      ((assq pred-id var-binding)
       => (lambda (binding)
            (cond
              ((assq (sxml:context->node (car nodeset))
                     (cdr binding))
               => (lambda (size-pair)
                    (if
                     (> (cdr position+size)  ; context size
                        (vector-length (cdr size-pair)))
                     (begin
                       (sxml:xpointer-runtime-error
                        "internal DDO SXPath error - "
                        "vector member for context size not found: " pred-id)
                       #f)
                     (let ((pos-vect (vector-ref (cdr size-pair)
                                                 (- (cdr position+size) 1))))
                       (if
                        (> (car position+size)  ; context position
                           (vector-length pos-vect))
                        (begin
                          (sxml:xpointer-runtime-error
                           "internal DDO SXPath error - "
                           "vector member for context position not found: "
                           pred-id)
                          #f)
                        (force (vector-ref pos-vect
                                           (- (car position+size) 1))))))))
              (else
               (sxml:xpointer-runtime-error
                "internal DDO SXPath error - alist entry for node not found: "
                pred-id)
               #f))))
      (else
       (sxml:xpointer-runtime-error
        "internal DDO SXPath error - predicate value not found: " pred-id)
       #f))))

;-------------------------------------------------
; Construct predicate values

; Construct alist of values for a predicate that doesn't require position
; pred-impl - lambda that implements the predicate
; context-set - set of contexts for all nodes in the source document
; var-bindings - include variables supplied by user and the ones formed by
;  deeper level predicates
(define (ddo:construct-pred-values pred-id pred-impl context-set var-binding)  
  (cons
   pred-id
   (map
    (lambda (context)
      (cons (sxml:context->node context)
            (delay
              (sxml:boolean  ; since return type cannot be number
               (pred-impl (list context)
                          (cons 1 1)  ; dummy context position and size
                          var-binding)))))
    context-set)))
         
; Construct alist of values for a predicate that requires position
;  pred-impl - lambda that implements the predicate
;  context-set - set of contexts for all nodes in the source document
;  var-bindings - include variables supplied by user and the ones formed by
; deeper level predicates
;  max-size - maximal context size possible in the document
(define (ddo:construct-pred-values-pos pred-id pred-impl context-set
                                       var-binding max-size)
  (cons
   pred-id
   (map
    (lambda (context)      
      (cons
       (sxml:context->node context)
       (let ((context (list context)))
         (let iter-size ((size 1)
                         (size-lst '()))
           (if
            (> size max-size)  ; iteration is over
            (list->vector (reverse size-lst))
            (let iter-pos ((position 1)
                           (pos-lst '()))
              ;(pp (list context position size))
              (if
               (> position size)  ; iteration is over               
               (iter-size
                (+ size 1)
                (cons (list->vector (reverse pos-lst))
                      size-lst))
               (iter-pos
                (+ position 1)
                (cons
                 (delay
                   (let ((pred-value
                          (pred-impl
                           context (cons position size) var-binding)))
                     (if (number? pred-value)
                         (= pred-value position)
                         (sxml:boolean pred-value))))
                 pos-lst)))))))))
    context-set)))

; Evaluates all predicates specified in deep-predicates
;  deep-predicates ::= (listof (list  pred-id  requires-position?  impl))
; Returns var-bindings extended with predicate values evaluated
; ATTENTION: in deep-predicates, each predicate must come after a predicate it
; is dependent on.
(define (ddo:evaluate-deep-predicates deep-predicates doc var-binding)
  (let* ((context-set (ddo:all-contexts-in-doc doc))
         (max-size (if
                    ; position-required? for at least one deep predicate
                    (not (null? (filter cadr deep-predicates)))
                    (length context-set)
                    1  ; dummy
                    )))
    (let iter-preds ((deep-predicates deep-predicates)
                     (var-binding var-binding))
      (if
       (null? deep-predicates)  ; iteration is over
       var-binding
       (iter-preds
        (cdr deep-predicates)
        (cons
         (if
          (cadar deep-predicates)  ; requires-position?
          (ddo:construct-pred-values-pos (caar deep-predicates)  ; pred-id
                                         (caddar deep-predicates)  ; pred-impl
                                         context-set
                                         var-binding max-size)
          (ddo:construct-pred-values (caar deep-predicates)  ; pred-id
                                     (caddar deep-predicates)  ; pred-impl
                                     context-set
                                     var-binding))
         var-binding))))))


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
; In this section, each function accepts 4 arguments
;  op - S-expression which represents the operation
;  num-anc - how many ancestors are required in the context after that
;   operation
;  single-level? - for grammar rules that consume the nodeset type as input:
;   whether all nodes in the nodeset are located on the single level of the
;   tree hierarchy. If this is the case, most axes can be evaluated ealier than
;   in the general case.
;  pred-nesting - nesting of the expression being processed within predicates.
;   In particular, pred-nesting=0 denotes the outer expression, pred-nesting=1
;   denotes the expression enclosed into a predicate, pred-nesting=2 for an
;   expression that is enclosed into 2 predicates, etc
;
; AST processing functions return either #f, which signals of a
; semantic error, or
;  (list (lambda (nodeset position+size var-binding) ...)
;        num-anc-it-requires
;        single-level?
;        requires-position?
;        expr-type
;        deep-predicates )
;  position+size - the same to what was called 'context' in TXPath-1
;  requires-position? - whether position() or last() functions are encountered
;   in the internal expression
;  expr-type - the type returned by the expression being process. The type is
;   determined by symbols. Possible types: number, string, boolean, nodeset and
;   any
;  deep-predicates - an associative list that contains deeply nested predicates,
;   whose pred-nesting>3:
;  deep-predicates ::= (listof (list  pred-id  requires-position?  impl))
;  pred-id - a symbol that identifies the predicate among others
;  impl - the implementation for this predicate

; {1} <LocationPath> ::= <RelativeLocationPath>
;                        | <AbsoluteLocationPath>
(define (ddo:ast-location-path op num-anc single-level? pred-nesting)
  (case (car op)
    ((absolute-location-path)
     (ddo:ast-absolute-location-path op num-anc single-level? pred-nesting))
    ((relative-location-path)
     (ddo:ast-relative-location-path op num-anc single-level? pred-nesting))
    (else
     (draft:signal-semantic-error "improper LocationPath - " op))))

; {2} <AbsoluteLocationPath> ::= (absolute-location-path  <Step>* )
; NOTE: single-level? is dummy here, since AbsoluteLocationPath always
; starts from a single node - the root of the document
(define (ddo:ast-absolute-location-path op num-anc single-level? pred-nesting)
  (cond
    ((not (eq? (car op) 'absolute-location-path))
     (draft:signal-semantic-error "not an AbsoluteLocationPath - " op))
    ((null? (cdr op))  ; no Steps
     (list
      (lambda (nodeset position+size var-binding)
        (draft:reach-root nodeset))
      #f  ; requires all ancestors
      #t  ; on single level
      #f  ; doesn't require position
      ddo:type-nodeset
      '()  ; no deep predicates
      ))
    (else
     (and-let*
      ((steps-res (ddo:ast-step-list (cdr op) num-anc #t pred-nesting)))
      (cons
       (if
        (null? (cdar steps-res))  ; only a single step
        (let ((step-impl (caar steps-res)))
          (lambda (nodeset position+size var-binding)
            (step-impl
             (draft:reach-root nodeset) position+size var-binding)))
        (let ((converters (car steps-res)))
          (lambda (nodeset position+size var-binding)
            (let rpt ((nset (draft:reach-root nodeset))
                      (fs converters))
              (if (null? fs)
                  nset
                  (rpt ((car fs) nset position+size var-binding)
                       (cdr fs)))))))
       (cons #f  ; all ancestors required
             (cddr steps-res)   ; the remaining parameters
             ))))))

; {3} <RelativeLocationPath> ::= (relative-location-path  <Step>+ )
(define (ddo:ast-relative-location-path op num-anc single-level? pred-nesting)
  (if
   (not (eq? (car op) 'relative-location-path))
   (draft:signal-semantic-error "not a RelativeLocationPath - " op)
   (and-let*
    ((steps-res
      (ddo:ast-step-list (cdr op) num-anc single-level? pred-nesting)))
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
(define (ddo:ast-step op num-anc single-level? pred-nesting)
  (cond
    ((eq? (car op) 'range-to)
     (draft:signal-semantic-error "range-to function not implemented"))
    ((eq? (car op) 'filter-expr)
     (ddo:ast-filter-expr op num-anc single-level? pred-nesting))
    ((eq? (car op) 'lambda-step)  ; created by sxpath
     (let ((proc (cadr op)))
       (list
        (if
         (and num-anc (zero? num-anc))  ; no ancestors required
         (lambda (nodeset position+size var-binding)
           (proc (draft:contextset->nodeset (as-nodeset nodeset))
                 var-binding))
         (lambda (nodeset position+size var-binding)
           (draft:find-proper-context
            (proc (draft:contextset->nodeset (as-nodeset nodeset))
                  var-binding)
            (map sxml:context->content   ; TODO: should add variables
                 (as-nodeset nodeset))
            num-anc)))
        num-anc  ; num-ancestors
        #f  ; single-level? after this step
        #f  ; position-required?
        ddo:type-any
        '()  ; no deep predicates
        )))
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
          (cadr axis-lst)
          (caddr axis-lst)
          #f
          ddo:type-nodeset
          '()  ; no deep predicates
          )))
      ; There are Predicates
      (and-let*
       ((preds-res (ddo:ast-predicate-list
                    (cdddr op) 0 #t (+ pred-nesting 1)))
        (preds-res
         (if (and (list-ref preds-res 3)  ; position required for the predicate
                  (< pred-nesting 3))  ; level of nesting matters
             (ddo:ast-predicate-list  ; the second pass
              (cdddr op) 0 #t
              (+ pred-nesting 2)  ; called for quadratic number of times
              )
             preds-res  ; do not need to change anything
             ))
        (axis-lst (ddo:ast-axis-specifier
                   (cadr op)
                   (draft:na-max num-anc (cadr preds-res))
                   single-level?
                   (list-ref preds-res 3)  ; whether position required
                   ))
        (ntest (draft:ast-node-test (caddr op))))
       (let ((axis ((car axis-lst)
                    ntest (draft:na-max num-anc (cadr preds-res))))
             (pred-impl-lst (car preds-res)))
         (list
          (cond
            ((not (list-ref preds-res 3))  ; whether position required
             (ddo:location-step-non-pos axis pred-impl-lst))
            ((list-ref axis-lst 3)  ; pos-result?
             (ddo:location-step-pos axis pred-impl-lst))
            (else  ; non-intersect
             (ddo:location-step-non-intersect axis pred-impl-lst)))
          (cadr axis-lst)  ; num-ancestors
          (caddr axis-lst)  ; single-level? after this step
          #f  ; position-required?
          ddo:type-nodeset
          (list-ref preds-res 5)  ; deep predicates
          )))))
    (else
     (draft:signal-semantic-error "not a Step - " op))))

; {4a} ( <Step>+ )
; Returns (list (listof step-impl)
;               num-anc single-level? requires-position? expr-type)
; or #f
; TECHNICAL NOTE: To calculate 'single-level?', we need to process steps in
; straight orger. To calculate 'num-anc', we need to process steps in reverse
; order. This thus has to be implemented in 2 passes
(define (ddo:ast-step-list step-lst num-anc single-level? pred-nesting)
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
               ((or (memq (caar steps) '(range-to filter-expr lambda-step))
                    (not sl?))
                ; #f for the remaining steps
                (append (map
                         (lambda (step) #f)
                         steps)   ; DL: was: step-lst
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
                (num-anc num-anc)
                (deep-predicates '()))
       (if
        (null? steps-to-view)  ; everyone processed
        (list res-lst
              num-anc (car single-level-lst) #f
              ddo:type-nodeset deep-predicates)
        (and-let*
         ((step-res
           (ddo:ast-step
            (car steps-to-view) num-anc (car sl?-lst) pred-nesting)))
         (loop
          (cdr steps-to-view)
          (cdr sl?-lst)
          (cons (car step-res) res-lst)
          (cadr step-res)
          (append (list-ref step-res 5) deep-predicates)
          )))))))

; {8} <Predicate> ::= (predicate  <Expr> )
; NOTE: num-anc is dummy here, since it is always 0 for Predicates
; NOTE: single-level? is dummy here, since a Predicate is always called for
;  a single node to be filtered
; NOTE: Unlike 'draft:ast-predicate', we don't implement any filtering here,
;  because it depends on the particular axis in the step. Filtering is
;  performed on the higher level
(define (ddo:ast-predicate op num-anc single-level? pred-nesting)
  (if
   (not (eq? (car op) 'predicate))
   (draft:signal-semantic-error "not an Predicate - " op)
   (and-let*
    ((expr-res (ddo:ast-expr (cadr op) 0 #t pred-nesting)))
    (let ((requires-position?
           (or (cadddr expr-res)  ; predicate expression requires position
               (memq (list-ref expr-res 4)  ; involves position implicitly
                     '(ddo:type-number ddo:type-any)))))      
      (let-values*
       (((pred-impl deep-predicates)
       (if        
        (> pred-nesting 3)  ; this is a deep predicate
        (let ((pred-id (ddo:generate-pred-id)))
          (values
           ((if requires-position?
                ddo:get-pred-value-pos ddo:get-pred-value)
            pred-id)
           (cons
            (list pred-id
                  requires-position?
                  (car expr-res)  ; implementation
                  )
            (list-ref expr-res 5)  ; deep-predicates
            )))
        (values (car expr-res)  ; implementation
                (list-ref expr-res 5)))))
       (list pred-impl
             (cadr expr-res)  ; num-ancestors required
             (caddr expr-res)  ; single-level? - we don't care
             requires-position?
             (list-ref expr-res 4)  ; return type
             deep-predicates))))))

; {8a} ( <Predicate>+ )
; Returns (list (listof pred-impl)
;               num-anc single-level? requires-position? expr-type
;               deep-predicates)
; or #f
; NOTE: num-anc is dummy here, since it is always 0 for Predicates
; NOTE: single-level? is dummy here, since a Predicate is always called for
;  a single node to be filtered
; NOTE: information about the type for each Predicate is lost
(define (ddo:ast-predicate-list op-lst num-anc single-level? pred-nesting)
  (let ((pred-res-lst (map
                       (lambda (op) (ddo:ast-predicate op 0 #t pred-nesting))
                       op-lst)))
    (if
     (member #f pred-res-lst)  ; error detected
     #f
     (list (map car pred-res-lst)
           (apply draft:na-max (map cadr pred-res-lst))
           #t
           (apply ddo:or (map cadddr pred-res-lst))
           ddo:type-any
           (apply append   ; deep-predicates
                  (map
                   (lambda (pred-res) (list-ref pred-res 5))
                   pred-res-lst))))))

; {9} <Expr> ::= <OrExpr>
;                | <AndExpr>
;                | <EqualityExpr>
;                | <RelationalExpr>
;                | <AdditiveExpr>
;                | <MultiplicativeExpr>
;                | <UnionExpr>
;                | <PathExpr>
;                | <FilterExpr>
;                | <VariableReference>
;                | <Literal>
;                | <Number>
;                | <FunctionCall>
;                | <LocationPath>
(define (ddo:ast-expr op num-anc single-level? pred-nesting)
  (case (car op)
    ((or)
     (ddo:ast-or-expr op num-anc single-level? pred-nesting))
    ((and)
     (ddo:ast-and-expr op num-anc single-level? pred-nesting))
    ((= !=)
     (ddo:ast-equality-expr op num-anc single-level? pred-nesting))
    ((< > <= >=)
     (ddo:ast-relational-expr op num-anc single-level? pred-nesting))
    ((+ -)
     (ddo:ast-additive-expr op num-anc single-level? pred-nesting))
    ((* div mod)
     (ddo:ast-multiplicative-expr op num-anc single-level? pred-nesting))
    ((union-expr)
     (ddo:ast-union-expr op num-anc single-level? pred-nesting))
    ((path-expr)
     (ddo:ast-path-expr op num-anc single-level? pred-nesting))
    ((filter-expr)
     (ddo:ast-filter-expr op num-anc single-level? pred-nesting))
    ((variable-reference)
     (ddo:ast-variable-reference op num-anc single-level? pred-nesting))
    ((literal)
     (ddo:ast-literal op num-anc single-level? pred-nesting))
    ((number)
     (ddo:ast-number op num-anc single-level? pred-nesting))
    ((function-call)
     (ddo:ast-function-call op num-anc single-level? pred-nesting))
    ((absolute-location-path)
     (ddo:ast-absolute-location-path op num-anc single-level? pred-nesting))
    ((relative-location-path)
     (ddo:ast-relative-location-path op num-anc single-level? pred-nesting))
    (else
     (draft:signal-semantic-error "unknown Expr - " op))))

; {10} <OrExpr> ::= (or <Expr> <Expr>+ )
; NOTE: num-anc is dummy here, since it is always 0 for OrExpr
(define (ddo:ast-or-expr op num-anc single-level? pred-nesting)
  (let ((expr-res-lst
         (map
          (lambda (expr) (ddo:ast-expr expr 0 single-level? pred-nesting))
          (cdr op))))
    (if
     (member #f expr-res-lst)  ; error detected
     #f
     (let ((expr-impls (map car expr-res-lst)))
     (list
      (lambda (nodeset position+size var-binding)
        (let rpt ((fs expr-impls))
          (cond
            ((null? fs) #f)
            ((sxml:boolean ((car fs) nodeset position+size var-binding)) #t)
            (else (rpt (cdr fs))))))
      (apply draft:na-max (map cadr expr-res-lst))  ; num-ancestors
      #t     ; single-level? after this step
      (apply ddo:or (map cadddr expr-res-lst))  ; position-required?
      ddo:type-boolean
      (apply append   ; deep-predicates
             (map
              (lambda (expr-res) (list-ref expr-res 5))
              expr-res-lst)))))))

; {11} <AndExpr> ::= (and <Expr> <Expr>+ )
; NOTE: num-anc is dummy here, since it is always 0 for AndExpr
(define (ddo:ast-and-expr op num-anc single-level? pred-nesting)
  (let ((expr-res-lst
         (map
          (lambda (expr) (ddo:ast-expr expr 0 single-level? pred-nesting))
          (cdr op))))
    (if
     (member #f expr-res-lst)  ; error detected
     #f
     (let ((expr-impls (map car expr-res-lst)))
     (list
      (lambda (nodeset position+size var-binding)
        (let rpt ((fs expr-impls))
          (cond
            ((null? fs) #t)
            ((not
              (sxml:boolean ((car fs) nodeset position+size var-binding)))
             #f)
            (else (rpt (cdr fs))))))
      (apply draft:na-max (map cadr expr-res-lst))  ; num-ancestors
      #t     ; single-level? after this step
      (apply ddo:or (map cadddr expr-res-lst))  ; position-required?
      ddo:type-boolean
      (apply append   ; deep-predicates
             (map
              (lambda (expr-res) (list-ref expr-res 5))
              expr-res-lst)))))))

; {12} <EqualityExpr> ::= (=  <Expr> <Expr> )
;                         | (!=  <Expr> <Expr> )
; NOTE: num-anc is dummy here, since it is always 0 for EqualityExpr
(define (ddo:ast-equality-expr op num-anc single-level? pred-nesting)
  (and-let*
   ((left-lst (ddo:ast-expr (cadr op) 0 single-level? pred-nesting))
    (right-lst (ddo:ast-expr (caddr op) 0 single-level? pred-nesting)))
   (let ((cmp-op (cadr (assq (car op) `((= ,sxml:equal?)
                                        (!= ,sxml:not-equal?)))))
         (left (car left-lst))
         (right (car right-lst)))
     (list
      (lambda (nodeset position+size var-binding)
        (cmp-op
         (draft:contextset->nodeset
          (left nodeset position+size var-binding))
         (draft:contextset->nodeset
          (right nodeset position+size var-binding))))
      (draft:na-max (cadr left-lst) (cadr right-lst))   ; num-ancestors
      #t     ; single-level? after this step
      (or (cadddr left-lst) (cadddr right-lst))  ; position-required?
      ddo:type-boolean
      (append (list-ref left-lst 5)   ; deep-predicates
              (list-ref right-lst 5))))))
      
; {13} <RelationalExpr> ::= (<  <Expr> <Expr> )
;                           | (>  <Expr> <Expr> )
;                           | (<=  <Expr> <Expr> )
;                           | (>=  <Expr> <Expr> )
; NOTE: num-anc is dummy here, since it is always 0 for RelationalExpr
(define (ddo:ast-relational-expr op num-anc single-level? pred-nesting)
  (and-let*
   ((left-lst (ddo:ast-expr (cadr op) 0 single-level? pred-nesting))
    (right-lst (ddo:ast-expr (caddr op) 0 single-level? pred-nesting)))
   (let ((cmp-op
          (sxml:relational-cmp
           (cadr (assq (car op) `((< ,<) (> ,>) (<= ,<=) (>= ,>=))))))
         (left (car left-lst))
         (right (car right-lst)))
     (list
      (lambda (nodeset position+size var-binding)
        (cmp-op
         (draft:contextset->nodeset
          (left nodeset position+size var-binding))
         (draft:contextset->nodeset
          (right nodeset position+size var-binding))))
      (draft:na-max (cadr left-lst) (cadr right-lst))   ; num-ancestors
      #t     ; single-level? after this step
      (or (cadddr left-lst) (cadddr right-lst))  ; position-required?
      ddo:type-boolean
      (append (list-ref left-lst 5)   ; deep-predicates
              (list-ref right-lst 5))))))

; {14} <AdditiveExpr> ::= (+  <Expr> <Expr> )
;                         | (-  <Expr> <Expr>? )
; NOTE: num-anc is dummy here, since it is always 0 for AdditiveExpr
(define (ddo:ast-additive-expr op num-anc single-level? pred-nesting)
  (let ((expr-res-lst
         (map
          (lambda (expr) (ddo:ast-expr expr 0 single-level? pred-nesting))
          (cdr op))))
    (if
     (member #f expr-res-lst)  ; error detected
     #f
     (let ((add-op (cadr (assq (car op) `((+ ,+) (- ,-)))))
           (expr-impls (map car expr-res-lst)))
     (list
      (lambda (nodeset position+size var-binding)
        (apply
         add-op
         (map
          (lambda (expr)
            (sxml:number
             (draft:contextset->nodeset
              (expr nodeset position+size var-binding))))
          expr-impls)))
      (apply draft:na-max (map cadr expr-res-lst))  ; num-ancestors
      #t     ; single-level? after this step
      (apply ddo:or (map cadddr expr-res-lst))  ; position-required?
      ddo:type-number
      (apply append   ; deep-predicates
             (map
              (lambda (expr-res) (list-ref expr-res 5))
              expr-res-lst)))))))

; {15} <MultiplicativeExpr> ::= (*  <Expr> <Expr> )
;                               | (div  <Expr> <Expr> )
;                               | (mod  <Expr> <Expr> )
; NOTE: num-anc is dummy here, since it is always 0 for MultiplicativeExpr
(define (ddo:ast-multiplicative-expr op num-anc single-level? pred-nesting)
  (and-let*
   ((left-lst (ddo:ast-expr (cadr op) 0 single-level? pred-nesting))
    (right-lst (ddo:ast-expr (caddr op) 0 single-level? pred-nesting)))
   (let ((mul-op
          (sxml:relational-cmp
           (cadr (assq (car op) `((* ,*) (div ,/) (mod ,remainder))))))
         (left (car left-lst))
         (right (car right-lst)))
     (list
      (lambda (nodeset position+size var-binding)
        (mul-op
         (sxml:number
          (draft:contextset->nodeset
           (left nodeset position+size var-binding)))
         (sxml:number
          (draft:contextset->nodeset
           (right nodeset position+size var-binding)))))
      (draft:na-max (cadr left-lst) (cadr right-lst))   ; num-ancestors
      #t     ; single-level? after this step
      (or (cadddr left-lst) (cadddr right-lst))  ; position-required?
      ddo:type-number
      (append (list-ref left-lst 5)   ; deep-predicates
              (list-ref right-lst 5))))))

; {16} <UnionExpr> ::= (union-expr  <Expr> <Expr>+ )
; TECHNICAL NOTE: For implementing the union while supporting distinct document
; order, we need num-ancestors=#f for the arguments of the union-expr. This
; operation is time-consuming and should be avoided
(define (ddo:ast-union-expr op num-anc single-level? pred-nesting)
  (let ((expr-res-lst
         (map
          (lambda (expr na)
            (let ((expr-res
                   (ddo:ast-expr expr na single-level? pred-nesting)))
              (if
               (not (or (eq? (list-ref expr-res 4) ddo:type-nodeset)
                        (eq? (list-ref expr-res 4) ddo:type-any)))
               (draft:signal-semantic-error
                "expression to be unioned evaluates to a non-nodeset - "
                expr)
               expr-res)))
          (cdr op)
          (reverse  ; number of ancestors for each of the expressions unioned
           (cons
            num-anc  ; the last expr doesn't require num-ancestors=#f
            (map
             (lambda (dummy) #f)
             (cddr op)  ; (cdr op) is non-null
             ))))))
    (if
     (member #f expr-res-lst)  ; error detected
     #f
     (let ((expr-impls (map car expr-res-lst)))
     (list
      (lambda (nodeset position+size var-binding)
        (let rpt ((res '())
                  (fs expr-impls))
          (if
           (null? fs)
           res
           (let ((nset ((car fs) nodeset position+size var-binding)))
             (rpt
              (ddo:unite-2-contextsets 
               res
               (cond
                 ((not (nodeset? nset))
                  (sxml:xpointer-runtime-error
                   "expected - nodeset instead of " nset)
                  '())
                 (else nset)))
              (cdr fs))))))
      #f  ; num-ancestors
      #f     ; single-level? after this step
      (apply ddo:or (map cadddr expr-res-lst))  ; position-required?
      ddo:type-nodeset
      (apply append   ; deep-predicates
             (map
              (lambda (expr-res) (list-ref expr-res 5))
              expr-res-lst)))))))

; {17} <PathExpr> ::= (path-expr  <FilterExpr> <Step>+ )
; TECHNICAL NOTE: To calculate 'single-level?', we need to process components
; in straight orger. To calculate 'num-anc', we need to process steps in
; reverse order. It is too expensive to make the 2 passes, that's why we
; consider single-level?=#f for steps
(define (ddo:ast-path-expr op num-anc single-level? pred-nesting)
  (and-let*
    ((steps-res (ddo:ast-step-list
                 (cddr op) num-anc
                 #f  ; consider single-level?=#f after FilterExpr
                 pred-nesting))
     (filter-lst (ddo:ast-filter-expr
                  (cadr op)
                  (cadr steps-res)  ; num-ancestors
                  single-level?
                  pred-nesting)))
    (if
     (not (or (eq? (list-ref filter-lst 4) ddo:type-nodeset)
              (eq? (list-ref filter-lst 4) ddo:type-any)))
     (draft:signal-semantic-error
      "location steps are applied to a non-nodeset result - " (cadr op))
     (let ((init-impl (car filter-lst))
           (converters (car steps-res)))
       (list
        (lambda (nodeset position+size var-binding)
          (let ((nset
                 (init-impl nodeset position+size var-binding)))
            (let rpt ((nset
                       (cond
                         ((nodeset? nset) nset)
                         (else
                          (sxml:xpointer-runtime-error 
                           "expected - nodeset instead of " nset)
                          '())))
                      (fs converters))
              (if (null? fs)
                  nset
                  (rpt ((car fs) nset position+size var-binding)
                       (cdr fs))))))
        (cadr filter-lst)  ; num-ancestors
        (cadddr steps-res)     ; single-level?, =#f in our assumption
        (cadddr filter-lst)  ; position-required?
        ddo:type-nodeset
        (append (list-ref filter-lst 5)   ; deep-predicates
                (list-ref steps-res 5)))))))

; {18} <FilterExpr> ::= (filter-expr (primary-expr  <Expr> )
;                                    <Predicate>* )
(define (ddo:ast-filter-expr op num-anc single-level? pred-nesting)
  (cond
    ((not (eq? (car op) 'filter-expr))
     (draft:signal-semantic-error "not an FilterExpr - " op))
    ((not (eq? (caadr op) 'primary-expr))
     (draft:signal-semantic-error "not an PrimaryExpr - " (cadr op)))
    ((null? (cddr op))  ; no Predicates
     (ddo:ast-expr (cadadr op) num-anc single-level? pred-nesting))
    ((and (null? (cdddr op))   ; a single predicate
          (ddo:check-special-predicate (caddr op)))
     => (lambda (special-pred-impl)          
          (and-let*
           ((expr-lst (ddo:ast-expr
                       (cadadr op)
                       num-anc   ; special predicate doesn't require ancestors
                       single-level? pred-nesting)))
           (list
            (ddo:filter-expr-special-predicate
             (car expr-lst) special-pred-impl)
            (cadr expr-lst)  ; num-ancestors
            (caddr expr-lst)  ; single-level? after this step
            (cadddr expr-lst)  ; position-required?
            ddo:type-nodeset
            (list-ref expr-lst 5)  ; deep-predicates
            ))))
    (else   ; the general case
     (and-let*
       ((preds-res (ddo:ast-predicate-list (cddr op) 0 #t (+ pred-nesting 1)))
        (expr-lst (ddo:ast-expr
                   (cadadr op)
                   (draft:na-max num-anc (cadr preds-res))  ; num-anc
                   single-level? pred-nesting)))
       (if
        (not (or (eq? (list-ref expr-lst 4) ddo:type-nodeset)
                 (eq? (list-ref expr-lst 4) ddo:type-any)))
        (draft:signal-semantic-error
         "expression to be filtered evaluates to a non-nodeset - " (cadr op)) 
        (let ((expr-impl (car expr-lst))
              (pred-impl-lst (car preds-res)))
          (list
           (if
            (list-ref preds-res 3)  ; position required
            (ddo:filter-expr-general expr-impl pred-impl-lst)
            (ddo:filter-expr-non-pos expr-impl pred-impl-lst))
           (cadr expr-lst)  ; num-ancestors
           (caddr expr-lst)  ; single-level? after this step
           (cadddr expr-lst)  ; position-required?
           ddo:type-nodeset
           (append (list-ref expr-lst 5)   ; deep-predicates
                   (list-ref preds-res 5)))))))))

; {19} <VariableReference> ::= (variable-reference  <String> )
(define (ddo:ast-variable-reference op num-anc single-level? pred-nesting)
  (let ((name (string->symbol (cadr op))))
    (list
     (lambda (nodeset position+size var-binding)
       (cond
         ((assoc name var-binding)
          => cdr)
         (else
          (sxml:xpointer-runtime-error "unbound variable - " name)
          '())))
     0
     #t  ; ATTENTION: in is not generally on the single-level
     #f
     ddo:type-any  ; type cannot be statically determined
     '()  ; deep-predicates
     )))

; {20} <Literal> ::= (literal  <String> )
(define (ddo:ast-literal op num-anc single-level? pred-nesting)
  (let ((literal (cadr op)))
    (list
     (lambda (nodeset position+size var-binding) literal)
     0 #t #f ddo:type-string '())))

; {21} <Number> :: (number  <Number> )
(define (ddo:ast-number op num-anc single-level? pred-nesting)
  (let ((number (cadr op)))
    (list
     (lambda (nodeset position+size var-binding) number)
     0 #t #f ddo:type-number '())))

; {22} <FunctionCall> ::= (function-call (function-name  <String> )
;                                        (argument  <Expr> )* )
(define (ddo:ast-function-call op num-anc single-level? pred-nesting)
  (let ((core-alist
         ; (list fun-name min-num-args max-num-args na4res impl
         ;       single-level? requires-position? expr-type)         
         `((last 0 0 0 ,draft:core-last
                 #t #t ,ddo:type-number)
           (position 0 0 0 ,draft:core-position
                     #t #t ,ddo:type-number)
           (count 1 1 0 ,draft:core-count
                  #t #f ,ddo:type-number)
           (id 1 1 #f ,draft:core-id
               #f #f ,ddo:type-nodeset)
           (local-name 0 1 0 ,draft:core-local-name
                       #t #f ,ddo:type-string)
           (namespace-uri 0 1 0 ,draft:core-namespace-uri
                          #t #f ,ddo:type-string)
           (name 0 1 0 ,draft:core-name
                 #t #f ,ddo:type-string)
           (string 0 1 0 ,draft:core-string
                   #t #f ,ddo:type-string)
           (concat 2 -1 0 ,draft:core-concat
                   #t #f ,ddo:type-string)
           (starts-with 2 2 0 ,draft:core-starts-with
                        #t #f ,ddo:type-boolean)
           (contains 2 2 0 ,draft:core-contains
                     #t #f ,ddo:type-boolean)
           (substring-before 2 2 0 ,draft:core-substring-before
                             #t #f ,ddo:type-boolean)
           (substring-after 2 2 0 ,draft:core-substring-after
                            #t #f ,ddo:type-boolean)
           (substring 2 3 0 ,draft:core-substring
                      #t #f ,ddo:type-boolean)
           (string-length 0 1 0 ,draft:core-string-length
                          #t #f ,ddo:type-number)
           (normalize-space 0 1 0 ,draft:core-normalize-space
                            #t #f ,ddo:type-string)
           (translate 3 3 0 ,draft:core-translate
                      #t #f ,ddo:type-string)
           (boolean 1 1 0 ,draft:core-boolean
                    #t #f ,ddo:type-boolean)
           (not 1 1 0 ,draft:core-not
                #t #f ,ddo:type-boolean)
           (true 0 0 0 ,draft:core-true
                 #t #f ,ddo:type-boolean)
           (false 0 0 0 ,draft:core-false
                  #t #f ,ddo:type-boolean)
           (lang 1 1 #f ,draft:core-lang
                 #t #f ,ddo:type-boolean)
           (number 0 1 0 ,draft:core-number
                   #t #f ,ddo:type-number)
           (sum 1 1 0 ,draft:core-sum
                #t #f ,ddo:type-number)
           (floor 1 1 0 ,draft:core-floor
                  #t #f ,ddo:type-number)
           (ceiling 1 1 0 ,draft:core-ceiling
                    #t #f ,ddo:type-number)
           (round 1 1 0 ,draft:core-round
                  #t #f ,ddo:type-number))))
    (cond
      ((not (eq? (caadr op) 'function-name))
       (draft:signal-semantic-error "not an FunctionName - " (cadr op)))
      ((assq (string->symbol (cadadr op)) core-alist)       
       => (lambda (description)  ; Core function found
            (cond
              ((< (length (cddr op)) (cadr description))
               (draft:signal-semantic-error
                "too few arguments for the Core Function call - "
                (cadadr op)))
              ((and (>= (caddr description) 0)
                    (> (length (cddr op)) (caddr description)))
               (draft:signal-semantic-error
                "too many arguments for the Core Function call - "
                (cadadr op)))
              (else  ; correct number of arguments
               (and-let*
                ((args-impl-lst (ddo:ast-function-arguments
                                 (cddr op)  ; list of arguments
                                 single-level? pred-nesting)))
                (list
                 ; Producing a function implementation
                 (apply (list-ref description 4)
                        num-anc
                        (map car args-impl-lst))
                 (apply  ; num-ancestors required for function
                  draft:na-max
                  (cons
                   (list-ref description 3)  ; from function description
                   (map cadr args-impl-lst)  ; from arguments
                   ))                    
                 (list-ref description 5)  ; single-level?
                 (or (list-ref description 6)  ; position-required?
                     (not (null?
                           (filter cadddr args-impl-lst))))
                 (list-ref description 7)  ; return type
                 (apply append   ; deep-predicates
                        (map
                         (lambda (arg-res) (list-ref arg-res 5))
                         args-impl-lst))))))))
           (else  ; function definition not found
            (draft:signal-semantic-error
             "function call to an unknown function - " (cadadr op))))))

; {22a} ( (argument  <Expr> )* )
; na-lst - number of ancestors required for each of the arguments
; Returns:  #f  or
;  (listof 
;    (list expr-impl num-anc single-level? requires-position? expr-type
;          deep-predicates))
; NOTE: In XPath Core Function Library, none of the function arguments
; is required to save any ancestors in the context
(define (ddo:ast-function-arguments op-lst single-level? pred-nesting)
  (let ((arg-res-lst
         (map
          (lambda (op)
            (if
             (not (eq? (car op) 'argument))
             (draft:signal-semantic-error "not an Argument - " op)
             (ddo:ast-expr (cadr op) 0 single-level? pred-nesting)))
          op-lst)))
    (if
     (member #f arg-res-lst)  ; semantic error detected
     #f
     arg-res-lst)))


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
    (let-values*
        (((ns-binding num-anc) (draft:arglist->ns+na ns+na)))
      (and-let*
       ((ast (grammar-parser xpath-string ns-binding))
        (impl-lst (ast-parser ast num-anc
                              #t  ; we suppose single-level?=#t for src
                              0  ; predicate nesting is zero
                              )))
       (let ((impl-lambda
              (if
               (and num-anc (zero? num-anc))
               (let ((impl-car (car impl-lst)))
                 (lambda (node position+size var-binding)
                   (draft:contextset->nodeset
                    (impl-car node position+size var-binding))))                
               (car impl-lst))))
         (if
          (null? (list-ref impl-lst 5))   ; no deep predicates         
          (lambda (node . var-binding)   ; common implementation
            (impl-lambda
             (as-nodeset node)
             (cons 1 1)
             (if (null? var-binding) var-binding (car var-binding))))
          (let ((deep-predicates  ; NOTE: need to reverse
                 (reverse (list-ref impl-lst 5))))
            (lambda (node . var-binding)
              (impl-lambda
               (as-nodeset node)
               (cons 1 1)
               (ddo:evaluate-deep-predicates
                deep-predicates
                node
                (if (null? var-binding)
                    var-binding (car var-binding))))))))))))

(define ddo:txpath (ddo:api-helper txp:xpath->ast ddo:ast-location-path))
(define ddo:xpath-expr (ddo:api-helper txp:expr->ast ddo:ast-expr))

; Support for native sxpath syntax
(define ddo:sxpath (ddo:api-helper txp:sxpath->ast ddo:ast-expr))
