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

; Select SXML element by their unique IDs
; XPath Rec. 4.1
;  object - a nodeset or a datatype which can be converted to a string by means
; of a 'string' function
;  id-index = ( (id-value . element) (id-value . element) ... ) 
; This index is used for selecting an element by its unique ID. 
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
; All the functions have an 'op' argument which is a symbolic representation
; for an operation: = != < > <= >=

; This function "inverts" an operation (example: < to >)
(define (sxp:invert-op op)
  (cond
    ((assq op '((< >) (> <) (<= >=) (>= <=)))
     => cdr)
    (else (cerr "sxp:invert-op:  unknown operation - " op nl)
     op)))

; Compare two booleans
(define (sxp:cmp-booleans op bool1 bool2)
  (cond 
    ((eq? op '=) (eq? bool1 bool2))
    ((eq? op '!=) (not (eq? bool1 bool2)))
    ((eq? op '<) (and (not bool1) bool2))
    ((eq? op '>) (and bool1 (not bool2)))
    ((eq? op '<=) (or (not bool1) bool2))
    ((eq? op '>=) (or bool1 (not bool2)))
    (else (cerr "sxp:cmp-booleans:  unknown operation - " op nl)
     #f)))

; Compare two numbers
(define (sxp:cmp-numbers op num1 num2)
  (cond
    ((eq? op '=) (= num1 num2))
    ((eq? op '!=) (not (= num1 num2)))
    ((eq? op '<) (< num1 num2))
    ((eq? op '>) (> num1 num2))
    ((eq? op '<=) (<= num1 num2))
    ((eq? op '>=) (>= num1 num2))
    (else (cerr "sxp:cmp-numbers:  unknown operation - " op nl)
     #f)))

; Compare two strings
(define (sxp:cmp-strings op str1 str2)
  (cond
    ((eq? op '=) (string=? str1 str2))
    ((eq? op '!=) (not (string=? str1 str2)))
    ((eq? op '<) (string<? str1 str2))
    ((eq? op '>) (string>? str1 str2))
    ((eq? op '<=) (string<=? str1 str2))
    ((eq? op '>=) (string>=? str1 str2))
    (else (cerr "sxp:cmp-strings:  unknown operation - " op nl)
     #f)))

; Compare a nodeset with an object
; Object can have the following type:  nodeset, boolean, number, string
(define (sxp:cmp-nodeset-object op nodeset object)
  (cond
    ((nodeset? object)
     (let rpt ((str-set1 (map sxp:string-value nodeset))
                (str-set2 (map sxp:string-value object)))
       (cond
         ((null? str-set1) #f)
         ((let loop ((set2 str-set2))
            (cond
	      ((null? set2) #f)
	      ((sxp:cmp-strings op (car str-set1) (car set2)) #t)
	      (else (loop (cdr set2))))) #t)
         (else
          (rpt (cdr str-set1) str-set2)))))
    ((number? object)
     (let rpt ((str-set (map sxp:string-value nodeset)))
       (cond
	 ((null? str-set) #f)
	 ((sxp:cmp-numbers op (sxp:number (car str-set)) object) #t)
	 (else (rpt (cdr str-set))))))
    ((string? object)
     (let rpt ((str-set (map sxp:string-value nodeset)))
       (cond
	 ((null? str-set) #f)
	 ((sxp:cmp-strings op (car str-set) object) #t)
	 (else (rpt (cdr str-set))))))
    ((boolean? object)
     (let rpt ((str-set (map sxp:string-value nodeset)))
       (cond
	 ((null? str-set) #f)
	 ((sxp:cmp-booleans op (sxp:boolean (car str-set)) object) #t)
	 (else (rpt (cdr str-set))))))
    (else #f)))
       
; Compare two object
; Object can have the following type:  nodeset, boolean, number, string
(define (sxp:cmp-objects op obj1 obj2)
  (cond
    ((nodeset? obj1) (sxp:cmp-nodeset-object op obj1 obj2))
    ((nodeset? obj2) (sxp:cmp-nodeset-object (sxp:invert-op op) obj2 obj1))
    ((memq op '(= !=))
     (cond
       ((boolean? obj1) (sxp:cmp-booleans op obj1 (sxp:boolean obj2)))
       ((boolean? obj2) (sxp:cmp-booleans op (sxp:boolean obj1) obj2))
       ((number? obj1) (sxp:cmp-numbers op obj1 (sxp:number obj2)))
       ((number? obj2) (sxp:cmp-numbers op (sxp:number obj1) obj2))
       (else (sxp:cmp-strings op (sxp:string obj1) (sxp:string obj2)))))
    (else (sxp:cmp-numbers op (sxp:number obj1) (sxp:number obj2)))))
     
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
