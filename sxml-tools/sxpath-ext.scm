;; W3C compliant extensions to SXPathlib
; $Id$:
;
; This software is in Public Domain.
; IT IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND.
;
; Please send bug reports and comments to:
;   lisovsky@acm.org      Kirill Lisovsky
;   lizorkin@hotbox.ru    Dmitry Lizorkin

;=========================================================================
; SXML counterparts to W3C XPath Core Functions Library

; The counterpart to XPath 'string' function (section 4.2 XPath Rec.)
; Converts a given object to a string
; NOTE:
;  1. When converting a nodeset - a document order is not preserved
;  2. number->string function returns the result in a form which is slightly
; different from XPath Rec. specification
(define (sxp:string object)
  (cond
    ((string? object) object)
    ((nodeset? object) (if (null? object)
			 ""
			 (sxp:string-value (car object))))
    ((number? object) (number->string object))
    ((boolean? object) (if object "true" "false"))
    (else "")))  ; Unknown type -> empty string. 
                 ; Option: write its value to string port?

; The counterpart to XPath 'boolean' function (section 4.3 XPath Rec.)
; Converts its argument to a boolean
(define (sxp:boolean object)
  (cond
    ((boolean? object) object)
    ((number? object) (not (= object 0)))
    ((string? object) (> (string-length object) 0))
    ((nodeset? object) (not (null? object)))
    (else #f)))  ; Not specified in XPath Rec.

; The counterpart to XPath 'number' function (section 4.4 XPath Rec.)
; Converts its argument to a number
; NOTE: 
;  1. The argument is not optional (yet?)
;  2. string->number conversion is not IEEE 754 round-to-nearest
;  3. NaN is represented as 0
(define (sxp:number obj)
  (cond
    ((number? obj) obj)
    ((string? obj)
     (let ((nmb (call-with-input-string obj read)))
       (if (number? nmb)
	 nmb
	 0))) ; NaN
    ((boolean? obj) (if obj 1 0))
    ((nodeset? obj) (sxp:number (sxp:string obj)))
    (else 0))) ; unknown datatype

; Returns a string value for a given node in accordance to
; XPath Rec. 5.1 - 5.7 
(define (sxp:string-value node)
  (if
    (not (pair? node))  ; a text node?
     (if (string? node)
        node
        "")  ; for XPath 1.0 it is an incorrect situation
  (apply
   string-append
   (cons ""
         (map
          sxp:string-value
          ((cond
	     ((null? (cdr node)) cdr)
             ((not (pair? (cadr node))) cdr)
             ((not (equal? (caadr node) '@)) cdr)
             (else cddr))
           node))))))

; Select SXML element by its unique IDs
; XPath Rec. 4.1
;  object - a nodeset or a datatype which can be converted to a string by means
; of a 'string' function
;  id-index = ( (id-value . element) (id-value . element) ... ) 
; This index is used for selection of an element by its unique ID. 
; The result is a nodeset
(define (sxp:id id-index)
  (lambda(object)
    (if (nodeset? object)
      (let loop ((str-lst (map sxp:string-value object))
		 (res '()))
	(if (null? str-lst)
	  (reverse res)
	  (let ((node (sxml:lookup (car str-lst) id-index)))
	    (if (not node)  ; no such element
	      (loop (cdr str-lst) res)
	      (loop (cdr str-lst) (cons node res))))))
      (let rpt ((lst (string->list (sxp:string object)))
		(tmp '())
		(res '()))
	(cond
	  ((null? lst)
	   (if (null? tmp) 
	     (reverse res)
	     (let ((node (sxml:lookup (list->string (reverse tmp)) id-index)))
	       (if (not node)
		 (reverse res)
		 (reverse (cons node res))))))
	  ((member (car lst) '(#\space #\return #\newline #\tab))
	   (if (null? tmp)
	     (rpt (cdr lst) tmp res)
	     (let ((node (sxml:lookup (list->string (reverse tmp)) id-index)))
	       (if (not node)
		 (rpt (cdr lst) '() res)
		 (rpt (cdr lst) '() (cons node res))))))
	  (else (rpt (cdr lst) (cons (car lst) tmp) res)))))))
                       
             
;=========================================================================
; Comparators for XPath objects 

; A helper for XPath equality operations: = , !=
;  'bool-op', 'number-op' and 'string-op' are comparison operations for 
; a pair of booleans,  numbers and strings respectively
(define (sxp:equality-cmp bool-op number-op string-op)
  (lambda (obj1 obj2)
    (cond
      ((and (not (nodeset? obj1)) (not (nodeset? obj2)))  
       ; neither object is a nodeset
       (cond
         ((boolean? obj1) (bool-op obj1 (sxp:boolean obj2)))
         ((boolean? obj2) (bool-op (sxp:boolean obj1) obj2))
         ((number? obj1) (number-op obj1 (sxp:number obj2)))
         ((number? obj2) (number-op (sxp:number obj1) obj2))
         (else  ; both objects are strings
          (string-op obj1 obj2))))
      ((and (nodeset? obj1) (nodeset? obj2))  ; both objects are nodesets
       (let first ((str-set1 (map sxp:string-value obj1))
                   (str-set2 (map sxp:string-value obj2)))
         (cond
           ((null? str-set1) #f)
           ((let second ((elem (car str-set1))
                         (set2 str-set2))
              (cond
                ((null? set2) #f)
                ((string-op elem (car set2)) #t)
                (else (second elem (cdr set2))))) #t)
           (else
            (first (cdr str-set1) str-set2)))))
      (else  ; one of the objects is a nodeset, another is not
       (let-values*
        (((nset elem)
          ; Equality operations are commutative
          (if (nodeset? obj1) (values obj1 obj2) (values obj2 obj1))))
        (cond
          ((boolean? elem) (bool-op elem (sxp:boolean nset)))
          ((number? elem)
           (let loop ((nset 
                       (map
                        (lambda (node) (sxp:number (sxp:string-value node)))
                        nset)))
             (cond
               ((null? nset) #f)
               ((number-op elem (car nset)) #t)
               (else (loop (cdr nset))))))
          ((string? elem)
           (let loop ((nset (map sxp:string-value nset)))
             (cond
               ((null? nset) #f)
               ((string-op elem (car nset)) #t)
               (else (loop (cdr nset))))))
          (else  ; unknown datatype
           (cerr "Unknown datatype: " elem nl)
           #f)))))))
                   
         
(define sxp:equal? (sxp:equality-cmp eq? = string=?))

(define sxp:not-equal?
  (sxp:equality-cmp
   (lambda (bool1 bool2) (not (eq? bool1 bool2)))
   (lambda (num1 num2) (not (= num1 num2)))
   (lambda (str1 str2) (not (string=? str1 str2)))))
         

; Relational operation ( < , > , <= , >= ) for two XPath objects
;  op is comparison procedure: < , > , <= or >=
(define (sxp:relational-cmp op)
  (lambda (obj1 obj2)
    (cond
      ((not (or (nodeset? obj1) (nodeset? obj2)))  ; neither obj is a nodeset
       (op (sxp:number obj1) (sxp:number obj2)))
      ((boolean? obj1)  ; 'obj1' is a boolean, 'obj2' is a nodeset
       (op (sxp:number obj1) (sxp:number (sxp:boolean obj2))))
      ((boolean? obj2)  ; 'obj1' is a nodeset, 'obj2' is a boolean
       (op (sxp:number (sxp:boolean obj1)) (sxp:number obj2)))
      ((or (null? obj1) (null? obj2)) ; one of the objects is an empty nodeset
       #f)
      (else  ; at least one object is a nodeset
       (op
        (cond
          ((nodeset? obj1)  ; 'obj1' is a (non-empty) nodeset
           (let ((nset1 (map
                         (lambda (node) (sxp:number (sxp:string-value node)))
                         obj1)))
             (let first ((num1 (car nset1))
                         (nset1 (cdr nset1)))
               (cond
                 ((null? nset1) num1)
                 ((op num1 (car nset1)) (first num1 (cdr nset1)))
                 (else (first (car nset1) (cdr nset1)))))))
          ((string? obj1) (sxp:number obj1))
          (else  ; 'obj1' is a number
           obj1))
        (cond
          ((nodeset? obj2)  ; 'obj2' is a (non-empty) nodeset
           (let ((nset2 (map
                         (lambda (node) (sxp:number (sxp:string-value node)))
                         obj2)))
             (let second ((num2 (car nset2))
                          (nset2 (cdr nset2)))
               (cond
                 ((null? nset2) num2)
                 ((op num2 (car nset2)) (second (car nset2) (cdr nset2)))
                 (else (second num2 (cdr nset2)))))))
          ((string? obj2) (sxp:number obj2))
          (else  ; 'obj2' is a number
           obj2)))))))
           
     
;=========================================================================
; Node tests

; According to XPath specification 2.3, this test is true for every node.
; For SXML: auxiliary lists and lists of attributes has to be excluded.
; See also sxml:node? function in sxml-tools
(define (sxp:node? node)
  (not (and 
	 (pair? node)
	 (memq (car node) '(@ @@)))))

;=========================================================================
; XPath axises
; An order in resulting nodeset is preserved

; Ancestor axis
(define (sxp:ancestor test-pred?)
  (lambda (root-node)   ; node or nodeset
    (lambda (node)      ; node or nodeset
      (if (nodeset? node)
	(map-union ((sxp:ancestor test-pred?) root-node) node)
	(let rpt ((paths (if (nodeset? root-node)
			   (map list root-node)
			   (list (list root-node)))))
	  (if (null? paths)
	    '()
	    (let ((path (car paths)))
	      (if (eq? (car path) node)
		((sxp:filter test-pred?) (cdr path))
		(rpt (append
		       (map
			 (lambda (arg) (cons arg path))
			 (append 
			   ((sxp:attribute (ntype?? '*)) (car path))
			   ((sxp:child sxp:node?) (car path))))
		       (cdr paths)))))))))))

; Ancestor-or-self axis
(define (sxp:ancestor-or-self test-pred?)
  (lambda (root-node)   ; node or nodeset
    (lambda (node)   ; node or nodeset
      (if (nodeset? node)
	(map-union ((sxp:ancestor-or-self test-pred?) root-node) node)
	(let rpt ((paths (if (nodeset? root-node)
			   (map list root-node)
			   (list (list root-node)))))
	  (if (null? paths)
	    ((sxp:filter test-pred?) (list node))
	    (let ((path (car paths)))
	      (if (eq? (car path) node)
		((sxp:filter test-pred?) path)
		(rpt (append
		       (map
			 (lambda (arg) (cons arg path))
			 (append 
			   ((sxp:attribute (ntype?? '*)) (car path))
			   ((sxp:child sxp:node?) (car path))))
		       (cdr paths)))))))))))
                                                                      
; Attribute axis
; 'sxml:attr-list' is used. Define it as sxml:attr-list-u for non-normalized SXML
(define (sxp:attribute test-pred?)
  (let ((fltr (sxp:filter test-pred?)))
    (lambda (node)
      (fltr
	(apply append
	  (filter-and-map
             sxml:element?
             sxml:attr-list
		(if (nodeset? node) node (list node))))))))
        
; Child axis
;  This function is similar to 'select-kids', but it returns an empty
;  child-list for PI, Comment and Entity nodes
(define (sxp:child test-pred?)
  (lambda (node)		; node or node-set
    (cond 
      ((null? node) node)
      ((not (pair? node)) '())   ; No children
      ((memq (car node) '(*PI* *COMMENT* *ENTITY*))   ; PI, Comment or Entity
       '())   ; No children
      ((symbol? (car node))    ; it's a single node       
       ((sxp:filter test-pred?) (cdr node)))
      (else (map-union (sxp:child test-pred?) node)))))

; Descendant axis
; It's similar to original 'node-closure' a resulting nodeset is 
; in depth-first order rather than breadth-first
; Fix: din't descend in non-element nodes!
(define (sxp:descendant test-pred?)
  (lambda (node)   ; node or nodeset
    (if (nodeset? node)
      (map-union (sxp:descendant test-pred?) node)
      (let rpt ((res '())
		(more ((sxp:child sxp:node?) node)))
	(if (null? more)
	  (reverse res)
	  (rpt (if (test-pred? (car more))
		 (cons (car more) res)
		 res)
	       (append ((sxp:child sxp:node?) (car more))
		       (cdr more))))))))

; Descendant-or-self axis
(define (sxp:descendant-or-self test-pred?)
  (lambda (node)   ; node or nodeset
    (if (nodeset? node)
      (map-union (sxp:descendant-or-self test-pred?) node)
      (let rpt ((res '())
		(more (list node)))
	(if (null? more)
	  (reverse res)
	  (rpt (if (test-pred? (car more))
		 (cons (car more) res)
		 res)
	       (append ((sxp:child sxp:node?) (car more))
		       ; sxml:node?
		       (cdr more))))))))

; Following axis
(define (sxp:following test-pred?)
  (lambda (root-node)   ; node or nodeset
    (lambda (node)      ; node or nodeset
      (if (nodeset? node)
	(map-union ((sxp:following test-pred?) root-node) node)
	(let loop ((seq (if (nodeset? root-node)
			  (list root-node)
			  (list (list root-node)))))
	  (cond
	    ((null? seq) '())
	    ((null? (car seq)) (loop (cdr seq)))
	    ((eq? (caar seq) node)
	     (let rpt ((seq (cdr (apply append seq)))
		       (res '()))
	       (if (null? seq)
		 res
		 (rpt (cdr seq)
		      (append 
			res
			((sxp:descendant-or-self test-pred?) (car seq)))))))
	    ((and (sxml:element? (caar seq))
		  (memq node (sxml:attr-list (caar seq))))
	     (let rpt ((sq (cdr (apply append seq)))
		       (res ((sxp:descendant test-pred?) (caar seq))))
	       (if (null? sq)
		 res
		 (rpt (cdr sq)
		      (append res
			      ((sxp:descendant-or-self test-pred?) (car sq)))))))
	    (else
	      (loop (cons 
		      ((sxp:child sxp:node?) (caar seq))
		      (cons (cdar seq) (cdr seq)))))))))))

; Following-sibling axis
(define (sxp:following-sibling test-pred?)
  (lambda (root-node)   ; node or nodeset
    (lambda (node)   ; node or nodeset
      (if (nodeset? node)
	(map-union ((sxp:following-sibling test-pred?) root-node) node)
	(let loop ((seqs (if (nodeset? root-node)
			   (list root-node)
			   (list (list root-node)))))
	  (if (null? seqs)
	    '()
	    (let rpt ((seq (car seqs)))
	      (cond
		((null? seq)
		 (loop (append
			 (map (sxp:child sxp:node?)
			      (car seqs))
			 (cdr seqs))))
		((eq? (car seq) node) ((sxp:filter test-pred?) (cdr seq)))
		(else (rpt (cdr seq)))))))))))

; Namespace axis
(define (sxp:namespace test-pred?)
  (lambda (node)   ; node or nodeset
    ((sxp:filter test-pred?) 
     (sxml:ns-list node))))

; Parent axis
; Faster counterpart to original 'node-parent' function 
; 'node-parent' calls node-closure from the root of the document, and
; the test-predicate for a node-closure looks like
; (node-or (node-reduce ...) (node-join ...)) 
; sxp:attribute?
; aux-list?
; comments, PI, etc ?
; ???
(define (sxp:parent test-pred?)
  (lambda (root-node)   ; node or nodeset
    (lambda (node)   ; node or nodeset
      (if (nodeset? node)
	(map-union ((sxp:parent test-pred?) root-node) node)
	(let rpt ((pairs 
		     (apply append
		       (map
			 (lambda (n)
			   (map
			     (lambda (arg) (cons arg n))
			     (append 
			       ((sxp:attribute (ntype?? '*)) n)
			       ((sxp:child sxp:node?) n))))                     
			 (if (nodeset? root-node)
			   root-node
			   (list root-node))))))
	  (if (null? pairs)
	    '()
	    (let ((pair (car pairs)))
	      (if (eq? (car pair) node)
		((sxp:filter test-pred?) (list (cdr pair)))
		(rpt (append
			(map
			  (lambda (arg) (cons arg (car pair)))
			  (append 
			    ((sxp:attribute (ntype?? '*)) (car pair))
			    ((sxp:child sxp:node?) (car pair))))
			(cdr pairs)))))))))))

; Preceding axis
(define (sxp:preceding test-pred?)
  (lambda (root-node)   ; node or nodeset
    (lambda (node)   ; node or nodeset
      (if (nodeset? node)
	(map-union ((sxp:preceding test-pred?) root-node) node)
	(let loop ((seq (if (nodeset? root-node)
			  (list (reverse root-node))
			  (list (list root-node)))))
	  (cond
	    ((null? seq) '())
	    ((null? (car seq)) (loop (cdr seq)))
	    ((or (eq? (caar seq) node)
		 (not (null? ((sxp:attribute 
				(lambda (n)
				  (eq? n node))) 
			      (caar seq)))))
	     (let rpt ((seq (cdr (apply append seq)))
		       (res '()))
	       (if (null? seq)
		 res
		 (rpt (cdr seq)
		      (append res
			      (reverse ((sxp:descendant-or-self test-pred?) 
					(car seq))))))))
	    (else (loop (cons (reverse ((sxp:child sxp:node?) (caar seq)))
			      (cons (cdar seq) (cdr seq)))))))))))

; Preceding-sibling axis
(define (sxp:preceding-sibling test-pred?)
  (lambda (root-node)   ; node or nodeset
    (lambda (node)   ; node or nodeset
      (if(nodeset? node)
	(map-union ((sxp:preceding-sibling test-pred?) root-node) node)
	(let loop ((seqs (if (nodeset? root-node)
			   (list root-node)
			   (list (list root-node)))))
	  (if (null? seqs)
	    '()
	    (let rpt ((seq (car seqs)))
	      (cond
		((null? seq)
		 (loop (append
			 (map
			   (lambda (n)
			     (reverse ((sxp:child sxp:node?) n)))
			   (car seqs))
			 (cdr seqs))))
		((eq? (car seq) node) ((sxp:filter test-pred?) (cdr seq)))
		(else (rpt (cdr seq)))))))))))
