;; A tool for making functional-style modifications to SXML documents
; The basics of modification language design was inspired by Patrick Lehti
; and his data manipulation processor for XML Query Language:
;  http://www.ipsi.fraunhofer.de/~lehti/
; However, with functional techniques we can do this better...

;==========================================================================
; Modification core

; Displays an error to stderr and returns #f
(define (sxml:modification-error . text)
  (cerr "Modification error: ")
  (apply cerr text)
  (cerr nl)
  #f)

;  Separates the list into two lists with respect to the predicate
;  Returns:  (values  res-lst1  res-lst2)
; res-lst1 - contains all members from the input lst that satisfy the pred?
; res-lst2 - contains the remaining members of the input lst
(define (sxml:separate-list pred? lst)
  (let loop ((lst lst)
             (satisfy '())
             (rest '()))
    (cond
      ((null? lst)
       (values (reverse satisfy) (reverse rest)))
      ((pred? (car lst))   ; the first member satisfies the predicate
       (loop (cdr lst)
             (cons (car lst) satisfy) rest))
      (else
       (loop (cdr lst)
             satisfy (cons (car lst) rest))))))

;-------------------------------------------------
; Miscellaneous helpers

; Asserts that the given obj is a proper attribute node.
; If this is the case, returns #t. Otherwise, calls sxml:modification-error
; with the appropriate error message.
; Handles singular attributes correctly. In accordance with SXML 3.0, accepts
; aux lists as attribute nodes
(define (sxml:assert-proper-attribute obj)
  (if
   (or (and (pair? obj)   ; aux node - any content is acceptable
            (not (null? obj))
            (eq? (car obj) '@))
       (and (list? obj)   ; '() is not a list
            (symbol? (car obj))
            (or (null? (cdr obj))  ; singular attribute
                (null? (cddr obj)))))
   #t
   (sxml:modification-error
    "improper attribute node - " obj)))

;  Unites a list of annot-attributes into a single annot-attributes.
;  Ensures that every attribute is a proper one, and that there is no duplicate
;  attributes
; annot-attributes-lst ::= (listof  annot-attributes)
; In accordance with SXML specification, version 3.0:
; [3]  <annot-attributes> ::=  (@ <attribute>* <annotations>? )
;  In case of an error, returns #f.
;  In the correct case, returns:  annot-attributes
(define (sxml:unite-annot-attributes-lists . annot-attributes-lst)
  (if
   (null? annot-attributes-lst)  ; nothing to do
   '()
   (let iter-lst ((src annot-attributes-lst)
                  (attrs '())
                  (annotations '()))
     (if
      (null? src)  ; Recursion finished
      (if (null? annotations)
          (cons '@ (reverse attrs))
          `(@ ,@(reverse attrs) (@ ,@annotations)))
      (let iter-annot-attrs ((annot-attrs (cdar src))
                             (attrs attrs)
                             (annotations annotations))
        (if
         (null? annot-attrs)  ; proceed with the outer loop
         (iter-lst (cdr src) attrs annotations)
         (let ((curr (car annot-attrs)))
           (cond       
             ((and (pair? curr)
                   (not (null? curr))
                   (eq? (car curr) '@))
              ; an annotation node
              (iter-annot-attrs (cdr annot-attrs)
                                attrs
                                (append annotations (cdr curr))))
             ((sxml:assert-proper-attribute curr)
              (if
               (assq (car curr) attrs)  ; duplicate attribute detected
               (sxml:modification-error
                "duplicate attribute - " (car curr))
               (iter-annot-attrs (cdr annot-attrs)
                                 (cons curr attrs)
                                 annotations)))
             (else  ; improper attribute
              #f)))))))))

;-------------------------------------------------
; The core function of document transformation into a new document

; doc - a source SXML document
; update-targets ::= (listof  update-target)
; update-target ::= (list  context  handler  base-node)
; context - context of the node selected by the location path
; handler ::= (lambda (node context base-node) ...)
; handler - specifies the required transformation over the node selected
; base-node - the node with respect to which the location path was evaluated
;
;  Returns the new document. In case of a transformation that results to a
;  non-well-formed document, returns #f and the error message is displayed to
;  stderr as a side effect
(define (sxml:transform-document doc update-targets)  
  (letrec
      (; targets-alist ::= (listof  (cons  node-chain  update-target))
       ; node-chain - the chain of nodes, starting from the current node
       (tree-trans
        (lambda (curr-node targets-alist)
          (let-values*
           (((matched         ; handlers which match this node
              targets-alist   ; the rest
              )
             (sxml:separate-list
              (lambda (pair) (null? (car pair)))
              targets-alist)))
           (and-let*
            ((after-subnodes  ; curr-node after its subnodes are processed
              (if
               (or (not (pair? curr-node))  ; leaf node
                   (null? targets-alist)  ; no more handlers
                   )
               curr-node
               (let process-attrs ((targets-alist targets-alist)
                                   (src-attrs (sxml:attr-list curr-node))
                                   (res-attrs '()))
                 (if
                  (null? src-attrs)  ; all attributes processed
                  ; Go to proceed child elements
                  (if
                   (null? targets-alist)  ; children don't need to be processed
                   (cons  ; Constructing the result node
                    (car curr-node)  ; node name
                    ((lambda (kids)
                       (if (null? res-attrs)  ; no attributes
                           kids
                           (cons (cons '@ (reverse res-attrs))
                                 kids)))
                     ((if (and (not (null? (cdr curr-node)))
                               (pair? (cadr curr-node))
                               (eq? (caadr curr-node) '@))
                          cddr cdr)
                      curr-node)))
                   (let process-kids ((targets-alist targets-alist)
                                      (src-kids (cdr curr-node))
                                      (res-kids '()))
                     (cond
                       ((null? src-kids)  ; all kids processed
                        (let-values*
                         (((more-attrs kids)
                           (sxml:separate-list
                            (lambda (obj)
                              (and (pair? obj)
                                   (not (null? obj))
                                   (eq? (car obj) '@)))
                            res-kids)))
                         (cons  ; Constructing the result node
                          (car curr-node)  ; node name
                          (if
                           (and (null? res-attrs)
                                (null? more-attrs))
                           kids
                           (and-let*
                            ((overall-attrs
                              (apply
                               sxml:unite-annot-attributes-lists
                               (cons
                                (cons '@ (reverse res-attrs))
                                more-attrs))))
                            (cons overall-attrs kids))))))
                       ((and (pair? (car src-kids))
                             (eq? (caar src-kids) '@))
                        ; attribute node - already processed
                        (process-kids
                         targets-alist (cdr src-kids) res-kids))
                       (else
                        (let ((kid-templates
                               (filter
                                (lambda (pair)
                                  (eq? (caar pair) (car src-kids)))
                                targets-alist)))
                          (if
                           (null? kid-templates)
                           ; this child node remains as is
                           (process-kids
                            targets-alist
                            (cdr src-kids)
                            (append res-kids (list (car src-kids))))
                           (and-let*
                            ((new-kid
                              (tree-trans
                               (car src-kids)
                               (map
                                (lambda (pair)
                                  (cons (cdar pair)
                                        (cdr pair)))
                                kid-templates))))
                            (process-kids
                             (filter
                              (lambda (pair)
                                (not (eq? (caar pair) (car src-kids))))
                              targets-alist)
                             (cdr src-kids)
                             (append
                              res-kids
                              (if (nodeset? new-kid)
                                  new-kid
                                  (list new-kid)))))))))))
                  (let* ((curr-attr (car src-attrs))
                         (attr-templates
                          (filter
                           (lambda (pair)
                             (eq? (caar pair) curr-attr))
                           targets-alist)))
                    (if
                     (null? attr-templates)
                     ; this attribute remains as is
                     (process-attrs targets-alist
                                    (cdr src-attrs)
                                    (cons curr-attr res-attrs))
                     (let ((new-attr  ; cannot produce error for attrs
                            (tree-trans
                             curr-attr
                             (map
                              (lambda (pair)
                                (cons (cdar pair) (cdr pair)))
                              attr-templates))))
                       (process-attrs
                        (filter
                         (lambda (pair)
                           (not (eq? (caar pair) curr-attr)))
                         targets-alist)
                        (cdr src-attrs)
                        (if (nodeset? new-attr)
                            (append (reverse new-attr) res-attrs)
                            (cons new-attr res-attrs)))))))))))
            (let process-this ((new-curr-node after-subnodes)
                               (curr-handlers (map cdr matched)))
              (if
               (null? curr-handlers)
               new-curr-node
               (process-this
                (if (nodeset? new-curr-node)
                    (map-union
                     (lambda (node)
                       (as-nodeset
                        ((cadar curr-handlers)  ; lambda
                         node
                         (caar curr-handlers)  ; context
                         (caddar curr-handlers)  ; base-node
                         )))
                     new-curr-node)
                    ((cadar curr-handlers)  ; lambda
                     new-curr-node
                     (caar curr-handlers)  ; context
                     (caddar curr-handlers)  ; base-node
                     ))                      
                (cdr curr-handlers)))))))))
    (let ((targets-alist
           (map-union
            (lambda (triple)
              (let ((node-path (reverse (sxml:context->content (car triple)))))
                (if
                 (eq? (car node-path) doc)
                 (list (cons (cdr node-path) triple))
                 '())))
            update-targets)))
      (if (null? targets-alist)  ; nothing to do
          doc
          (tree-trans doc targets-alist)))))
            

;==========================================================================
; Processing update-specifiers

;  Evaluates lambda-upd-specifiers for the SXML document docþ
;  Returns:
; update-targets ::= (listof  update-target)
; update-target ::= (list  context  handler  base-node)
; context - context of the node selected by the location path
; handler ::= (lambda (node context base-node) ...)
; handler - specifies the required transformation over the node selected
; base-node - the node with respect to which the location path was evaluated
(define (sxml:lambdas-upd-specifiers->targets doc lambdas-upd-specifiers)
  (letrec
      ((construct-targets
        (lambda (base-context lambdas-upd-specifiers)
          (let* ((triple (car lambdas-upd-specifiers))
                 (context-set ((car triple)
                               (list base-context)
                               (cons 1 1)
                               '()  ; dummy var-binding
                               ))
                 (res (map
                       (lambda (context)
                         (list context
                               (caddr triple)  ; handler
                               (sxml:context->node base-context)))
                       context-set))
                 (next (cdr lambdas-upd-specifiers)))
            (if
             (null? next)  ; no more members
             res
             (append
              res
              (if
               (cadadr lambdas-upd-specifiers)  ; the following is relative
               (map-union
                (lambda (context)
                  (construct-targets context next))
                context-set)
               (construct-targets doc next))))))))
    (if
     (null? lambdas-upd-specifiers)  ; no transformation rules
     '()
     (construct-targets doc lambdas-upd-specifiers))))

;  "Precompiles" each of update-specifiers, by transforming location paths and
;  update actions into lambdas.
;  Returns:
; lambdas-upd-specifiers ::= (listof  lambdas-upd-specifier)
; lambdas-upd-specifier ::= (list  txpath-lambda  relative?  handler)
; txpath-lambda ::= (lambda (nodeset position+size var-binding) ...)
; txpath-lambda - full-argument implementation of a location path
; relative? - whether the txpath lambda is to be evaluated relatively to the
;  node selected by the previous lambdas-upd-specifier, or with respect to
;  the root of the document. For relative?=#t the base-node is the node
;  selected by the previous lambdas-upd-specifier, otherwise the base node is
;  the root of the document being transformed
; handler ::= (lambda (node context base-node) ...)
(define (sxml:update-specifiers->lambdas update-specifiers)
  (let iter ((src update-specifiers)
             (res '()))
    (if
     (null? src)  ; every specifier processed
     (reverse res)
     (let ((curr (car src)))
       (if
        (or (not (list? curr))
            (null? (cdr curr)))
        (sxml:modification-error "improper update-specifier: " curr)
        (and-let*
         ; Convert Location path to XPath AST
         ((ast (txp:xpath->ast (car curr))))
         (let-values*
          (((txpath-pair relative?)
            (if
             (eq? (car ast) 'absolute-location-path)
             (values
              (draft:ast-relative-location-path
               (cons 'relative-location-path (cdr ast))
               #f  ; keep all ancestors
               )
              #f)
             (values
              (draft:ast-relative-location-path ast #f)
              (not (null? res))   ; absolute for the first rule
              ))))
          (if
           (not txpath-pair)  ; semantic error
           txpath-pair  ; propagate the error
           (let ((txpath-lambda (car txpath-pair))
                 (action (cadr curr)))
             (if
              (procedure? action)  ; user-supplied handler
              (iter (cdr src)
                    (cons
                     (list txpath-lambda relative? action)
                     res))
              (case action
                ((delete delete-undeep)
                 (iter (cdr src)
                       (cons
                        (list
                         txpath-lambda
                         relative?
                         (cdr
                          (assq action
                                `((delete . ,modif:delete)
                                  (delete-undeep . ,modif:delete-undeep)))))
                        res)))
                ((insert-into insert-following insert-preceding)
                 (let ((params (cddr curr)))
                   (iter (cdr src)
                         (cons
                          (list
                           txpath-lambda
                           relative?
                           ((cdr
                             (assq
                              action
                              `((insert-into . ,modif:insert-into)
                                (insert-following . ,modif:insert-following)
                                (insert-preceding . ,modif:insert-preceding))))
                            (lambda (context base-node) params)))
                          res))))
                ((replace)
                 (let ((params (cddr curr)))
                   (iter (cdr src)
                         (cons
                          (list txpath-lambda relative?
                                (lambda (node context base-node) params))
                          res))))
                ((rename)
                 (if
                  (or (null? (cddr curr))  ; no parameter supplied
                      (not (symbol? (caddr curr))))
                  (sxml:modification-error
                   "improper new name for the node to be renamed: "
                   curr)                  
                  (iter
                   (cdr src)
                   (cons
                    (let ((new-name (caddr curr)))
                      (list txpath-lambda relative? (modif:rename new-name)))
                    res))))
                ((move-into move-following move-preceding)
                 (if
                  (or (null? (cddr curr))  ; no lpath supplied
                      (not (string? (caddr curr))))
                  (sxml:modification-error
                   "improper destination location path for move action: "
                   curr)
                  (and-let*
                   ((ast (txp:xpath->ast (caddr curr)))
                    (txpath-pair (draft:ast-location-path ast #f)))
                   (iter (cdr src)
                         (cons
                          (list
                           (car txpath-pair)
                           #t
                           ((cdr
                             (assq
                              action
                              `((move-into . ,modif:insert-into)
                                (move-following . ,modif:insert-following)
                                (move-preceding . ,modif:insert-preceding))))
                            (lambda (context base-node) base-node)))
                          (cons                                
                           (list txpath-lambda relative? modif:delete)
                           res))))))
                (else
                 (sxml:modification-error "unknown action: " curr)))))))))))))

;==========================================================================
; Several popular handlers

; Node insertion
;  node-specifier ::= (lambda (context base-node) ...)
; The lambda specifies the node to be inserted
(define (modif:insert-following node-specifier)
  (lambda (node context base-node)
    (cons node (as-nodeset (node-specifier context base-node)))))

(define (modif:insert-preceding node-specifier)
  (lambda (node context base-node)
    (let ((new (node-specifier context base-node)))
      (if (nodeset? new)
          (append new (list node))
          (list new node)))))

(define (modif:insert-into node-specifier)
  (lambda (node context base-node)
    (if (not (pair? node))  ; can't insert into
        node
        (append node (as-nodeset (node-specifier context base-node))))))
    
; Rename
(define (modif:rename new-name)
  (lambda (node context base-node)
    (if (pair? node)  ; named node
        (cons new-name (cdr node))
        node)))

; Delete
(define modif:delete
  (lambda (node context base-node) '()))

(define modif:delete-undeep
  (lambda (node context base-node)
    (if (pair? node) (cdr node) '())))


;==========================================================================
; Highest-level API function

; update-specifiers ::= (listof  update-specifier)
; update-specifier ::= (list  xpath-location-path  action  [action-parametes])
; xpath-location-path - addresses the node(s) to be transformed, in the form of
;  XPath location path. If the location path is absolute, it addresses the
;  node(s) with respect to the root of the document being transformed. If the
;  location path is relative, it addresses the node(s) with respect to the
;  node selected by the previous update-specifier. The location path in the
;  first update-specifier always addresses the node(s) with respect to the
;  root of the document. We'll further refer to the node with respect of which
;  the location path is evaluated as to the base-node for this location path.
; action - specifies the modification to be made over each of the node(s)
;  addressed by the location path. Possible actions are described below.
; action-parameters - additional parameters supplied for the action. The number
;  of parameters and their semantics depend on the definite action.
;
; action ::= 'delete | 'delete-undeep |
;            'insert-into | 'insert-following | 'insert-preceding |
;            'replace |
;            'move-into | 'move-following | 'move-preceding |
;            handler
; 'delete - deletes the node. Expects no action-parameters
; 'delete-undeep - deletes the node, but keeps all its content (which thus
;   moves to one level upwards in the document tree). Expects no
;   action-parameters
; 'insert-into - inserts the new node(s) as the last children of the given
;   node. The new node(s) are specified in SXML as action-parameters
; 'insert-following, 'insert-preceding - inserts the new node(s) after (before)
;   the given node. Action-parameters are the same as for 'insert-into
; 'replace - replaces the given node with the new node(s). Action-parameters
;   are the same as for 'insert-into
; 'rename - renames the given node. The node to be renamed must be a pair (i.e.
;   not a text node). A single action-parameter is expected, which is to be
;   a Scheme symbol to specify the new name of the given node
; 'move-into - moves the given node to a new location. The single
;   action-parameter is the location path, which addresses the new location
;   with respect to the given node as the base node. The given node becomes
;   the last child of the node selected by the parameter location path.
; 'move-following, 'move-preceding - the given node is moved to the location
;   respectively after (before) the node selected by the parameter location
;   path
; handler ::= (lambda (node context base-node) ...)
; handler - specifies the required transformation. It is an arbitrary lambda
;  that consumes the node and its context (the latter can be used for addressing
;  the other node of the source document relative to the given node). The hander
;  can return one of the following 2 things: a node or a nodeset.
;   1. If a node is returned, than it replaces the source node in the result
;  document
;   2. If a nodeset is returned, than the source node is replaced by (multiple)
;  nodes from this nodeset, in the same order in which they appear in the
;  nodeset. In particular, if the empty nodeset is returned by the handler, the
;  source node is removed from the result document and nothing is inserted
;  instead.
;
;  Returns either (lambda (doc) ...) or #f
;  The latter signals of an error, an the error message is printed into stderr
;  as a side effect. In the former case, the lambda can be applied to an SXML
;  document and produces the new SXML document being the result of the
;  modification specified.
(define (sxml:modify . update-specifiers)
  (and-let*
   ((lambdas-upd-specifiers
     (sxml:update-specifiers->lambdas update-specifiers)))
   (lambda (doc)
     (sxml:transform-document
      doc
      (sxml:lambdas-upd-specifiers->targets doc lambdas-upd-specifiers)))))
