;		XML/HTML processing in Scheme
;		SXML expression tree transformers
;
; $Id$

		; Output the 'fragments'
		; The fragments are a list of strings, characters,
		; numbers, thunks, #f -- and other fragments.
		; The function traverses the tree depth-first, writes out
		; strings and characters, executes thunks, and ignores
		; #f and '().
		; The function returns #t if anything was written at all;
		; otherwise the result is #f
(define (SRV:send-reply . fragments)
  (let loop ((fragments fragments) (result #f))
    (cond
      ((null? fragments) result)
      ((not (car fragments)) (loop (cdr fragments) result))
      ((null? (car fragments)) (loop (cdr fragments) result))
      ((pair? (car fragments))
        (loop (cdr fragments) (loop (car fragments) result)))
      ((procedure? (car fragments))
        ((car fragments))
        (loop (cdr fragments) #t))
      (else
        (display (car fragments))
        (loop (cdr fragments) #t)))))



;------------------------------------------------------------------------
;	          Traversal of an SXML tree or a grove:
;			a <Node> or a <Nodeset>
;
; A <Node> and a <Nodeset> are mutually-recursive datatypes that
; underlie the SXML tree:
;	<Node> ::= (name . <Nodeset>) | "text string"
; An (ordered) set of nodes is just a list of the constituent nodes:
; 	<Nodeset> ::= (<Node> ...)
; Nodesets, and Nodes other than text strings are both lists. A
; <Nodeset> however is either an empty list, or a list whose head is
; not a symbol (an atom in general). A symbol at the head of a node is
; either an XML name (in which case it's a tag of an XML element), or
; an administrative name such as '@'.
; See SXPath.scm and SSAX.scm for more information on SXML.

; Borrowed from SXPath.scm
(define (nodeset? x)
  (or (and (pair? x) (not (symbol? (car x)))) (null? x)))

	; Apply proc to each element of lst and return the list of results.
	; if proc returns a nodeset, splice it into the result
(define (map-union proc lst)
  (if (null? lst) lst
      (let ((proc-res (proc (car lst))))
	((if (nodeset? proc-res) append cons)
	 proc-res (map-union proc (cdr lst))))))

; Post-order traversal of a tree and creation of a new tree:
;	post-order:: <tree> x <bindings> -> <new-tree>
; where
; <bindings> ::= (<binding> ...)
; <binding> ::= (<trigger-symbol> <new-bindings> . <handler>) |
;		(<trigger-symbol> . <handler>)
; <trigger-symbol> ::= XMLname | *text* | *default*
; <handler> :: <trigger-symbol> x [<tree>] -> <new-tree>
;
; The post-order function visits the nodes and nodesets post-order
; (depth-first).  For each <Node> of the form (name <Node> ...) it
; looks up an association with the given name among its <bindings>. If
; failed, post-order tries to locate a *default* binding. It's an
; error if the latter attempt fails as well.  Having found a binding,
; the post-order function first calls itself recursively for each
; child of the current node, with <new-bindings> prepended to the
; <bindings> in effect. The result of these calls is passed to the
; <handler> (along with the head of the current <Node>). To be more
; precise, the handler is _applied_ to the head of the current node
; and its processed children. The result of the handler, which should
; also be a <tree>, replaces the current <Node>. If the current <Node>
; is a text string, a special binding with a symbol *text* is looked
; up.

(define (post-order tree bindings)
  (cond
   ((nodeset? tree)
    (map (lambda (a-tree) (post-order a-tree bindings)) tree))
   ((not (pair? tree))
    (let ((trigger '*text*))
      (cond
       ((or (assq trigger bindings) (assq '*default* bindings)) =>
	(lambda (binding)
	  ((if (procedure? (cdr binding)) (cdr binding) (cddr binding))
	   trigger tree)))
       (else
	(error "Unknown binding for " trigger " and no default")))
      ))
   (else
    (let ((trigger (car tree)))
      (cond
       ((or (assq trigger bindings) (assq '*default* bindings)) =>
	(lambda (binding)
	  (apply
	   (if (procedure? (cdr binding)) (cdr binding) (cddr binding))
	   (cons trigger 
		 (post-order (cdr tree) 
			     (if (pair? (cdr binding))
				 (append (cadr binding) bindings)
				 bindings))))))
       (else
	(error "Unknown binding for " trigger " and no default"))))))
)


; Pre-Post-order traversal of a tree and creation of a new tree:
;	pre-post-order:: <tree> x <bindings> -> <new-tree>
; where
; <bindings> ::= (<binding> ...)
; <binding> ::= (<trigger-symbol> *preorder* . <handler>) |
;		(<trigger-symbol> <new-bindings> . <handler>) |
;		(<trigger-symbol> . <handler>)
; <trigger-symbol> ::= XMLname | *text* | *default*
; <handler> :: <trigger-symbol> x [<tree>] -> <new-tree>
;
; The pre-post-order function visits the nodes and nodesets pre-post-order
; (depth-first).  For each <Node> of the form (name <Node> ...) it
; looks up an association with the given 'name' among its <bindings>. If
; failed, pre-post-order tries to locate a *default* binding. It's an
; error if the latter attempt fails as well.  Having found a binding,
; the pre-post-order function first checks to see if the binding is
; of the form
;	(<trigger-symbol> *preorder* . <handler>)
; If it is, the handler is 'applied' to the current node. Otherwise,
; the pre-post-order function first calls itself recursively for each
; child of the current node, with <new-bindings> prepended to the
; <bindings> in effect. The result of these calls is passed to the
; <handler> (along with the head of the current <Node>). To be more
; precise, the handler is _applied_ to the head of the current node
; and its processed children. The result of the handler, which should
; also be a <tree>, replaces the current <Node>. If the current <Node>
; is a text string, a special binding with a symbol *text* is looked
; up.

(define (pre-post-order tree bindings)
  (cond
   ((nodeset? tree)
    (map (lambda (a-tree) (pre-post-order a-tree bindings)) tree))
   ((not (pair? tree))
    (let ((trigger '*text*))
      (cond
       ((or (assq trigger bindings) (assq '*default* bindings)) =>
	(lambda (binding)
	  ((if (procedure? (cdr binding)) (cdr binding) (cddr binding))
	   trigger tree)))
       (else
	(error "Unknown binding for " trigger " and no default")))
      ))
   (else
    (let ((trigger (car tree)))
      (cond
       ((or (assq trigger bindings) (assq '*default* bindings)) =>
	(lambda (binding)
	  (if (and (pair? (cdr binding)) (eq? '*preorder* (cadr binding)))
	      (apply (cddr binding) tree)
	      (apply
	       (if (procedure? (cdr binding)) (cdr binding) (cddr binding))
	       (cons trigger 
		     (pre-post-order (cdr tree) 
				     (if (pair? (cdr binding))
					 (append (cadr binding) bindings)
					 bindings)))))))
       (else
	(error "Unknown binding for " trigger " and no default"))))))
)

