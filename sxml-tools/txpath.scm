;; SXPath processor for string representation of W3C XPath/XPointer expressions
; $Id$:
;
; This software is in Public Domain.
; IT IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND.
;
; Please send bug reports and comments to:
;   lisovsky@acm.org      Kirill Lisovsky
;   lizorkin@hotbox.ru    Dmitry Lizorkin
;
; 1. XPointer's points and ranges are NOT implemented
; 2. The following functions from an XPath Core Function Library are supported:
;    - last      (4.1 in XPath specification)
;    - position  (4.1 in XPath specification)
;    - count     (4.1 in XPath specification)
;    - id        (4.1 in XPath specification)
;    - string   (4.2 in XPath specification)
;    - boolean   (4.3 in XPath specification)
;    - not       (4.3 in XPath specification)
;    - true      (4.3 in XPath specification)
;    - false     (4.3 in XPath specification)
;    - number   (4.4 in XPath specification)
;    - floor    (4.4 in XPath specification)
;    - ceiling  (4.4 in XPath specification)
;    - round    (4.4 in XPath specification)

;=========================================================================
; Errors handling 

(define (sxp:xpointer-parse-error . text)
  (cerr nl "XPointer parser error: " text nl)
  #f)

; A warning message for grammar features which are not supported by this
; implementation
(define (sxp:xpointer-parse-warning . text)
  (cerr nl "XPointer parser warning: " text nl))

; Runtime errors handler (unbound variable, bad argument, etc).
; It may be re-defined (say, like a warning) without 'exit',  and evaluation will 
; be continued.
; In this case, a default value (usually empty nodeset or 0) is returned by 
; a sub-expression which caused an XPointer runtime error.
(define (sxp:xpointer-runtime-error . text)
  (cerr nl "XPointer runtime error: " text nl)
  (exit -1))

;=========================================================================
; Low level parsing functions
; The XPoiner path is represented as a list of chars

; A list of whitespace characters
(define sxp:whitespace '(#\space #\return #\newline #\tab))

; A sxp:whitespace or () <> [] : / + * , = | ! " ' @ $
(define sxp:delimiter (append sxp:whitespace
                              '(#\( #\) #\< #\> #\[ #\] #\: #\/ #\+ 
                                #\* #\, #\= #\| #\! #\" #\' #\@ #\$)))

; A list of characters a NCName cannot start with
(define (sxp:non-first? ch)
  (or (char-numeric? ch)
      (memv ch sxp:delimiter) 
      (memv ch '(#\. #\-))))

; The function reads a whitespace , production [3] (S) in XML Rec.
;  path - xpointer path string as a list of chars 
; It returns a new path
(define (sxp:skip-ws path)
  (if (or (null? path)
	  (not (memv (car path) sxp:whitespace)))
    path
    (sxp:skip-ws (cdr path))))

;------------------------------------------------
; These two functions read expected information from the path

; Whether the path begins with a 'str' (starting whitespaces are ignored)
;  str - a string to match
;  path - an xpointer path represented as a list of chars
;  char-list - an optional argument. If this argument is supplied, a 'str'
; pattern must be followed by a character from a 'char-list'
; If 'str' is really in the beginning of path, a new path is returned
; Otherwise, function returns #f (path remains unchanged)
(define (sxp:parse-check str path . char-list)
  (let loop ((lst (string->list str)) 
             (p (sxp:skip-ws path)))
    (cond
      ((null? lst)
       (if
        (or (null? p) (null? char-list) (memv (car p) (car char-list)))
        p
        #f))
      ((null? p) #f)
      ((char=? (car lst) (car p))
       (loop (cdr lst) (cdr p)))
      (else #f))))

; Similar to the 'parse-check' function. But this function also has a side
; effect. It displays an error message if the 'str' doesn't match the beginning
; of 'path'.
(define (sxp:parse-assert str path)
  (let loop ((lst (string->list str)) 
	     (p (sxp:skip-ws path)))
    (cond
      ((null? lst) p)
      ((null? p) 
       (sxp:xpointer-parse-error 
	 "unexpected end of XPointer path. Expected: " (car lst)))
      ((char=? (car lst) (car p)) (loop (cdr lst) (cdr p)))
      (else (sxp:xpointer-parse-error "unexpected symbol: " (car p) 
				      ". Expected: " (car lst))))))
             
;------------------------------------------------
; NCName readers

; Reads a NCName, taking into account that whitespaces and characters:
; ( ) < > [ ] : / + * , = | ! " ' @ $
; may not be used in it.
; Moreover, its first character can't be: . - or a digit
; The result:  (list  ncname  new-path)
;          or  #f
;  ncname - NCName represented as a string
; If there is no NCName in the current position of the path, then an error 
; message is displayed and #f is returned
(define (sxp:parse-ncname path)
  (let((path (sxp:skip-ws path)))
    (cond
      ((null? path) 
       (sxp:xpointer-parse-error
        "unexpected end of XPointer path. Expected - NCName"))
      ((sxp:non-first? (car path))
       (sxp:xpointer-parse-error
        "expected - NCName instead of " (car path)))
      (else
       (let loop ((ncname (list (car path)))
                  (path (cdr path)))
         (cond
           ((null? path) (list (list->string (reverse ncname)) path))
           ((memv (car path) sxp:delimiter)           
            (list (list->string (reverse ncname)) path))
           (else (loop (cons (car path) ncname) (cdr path)))))))))

; Reads a Name production. It is similar to a 'parse-ncname' function.
; The only difference is that #\: is allowed within a Name
(define (sxp:parse-name path)
  (let ((path (sxp:skip-ws path)))
    (cond
      ((null? path)
       (sxp:xpointer-parse-error
	 "unexpected end of XPointer path. Expected - Name"))
      ((and (sxp:non-first? (car path))
	    (not (char=? (car path) #\:)))
       (sxp:xpointer-parse-error "expected - Name instead of " (car path)))
      (else (let loop ((ncname (list (car path)))
		       (path (cdr path)))
	      (cond
		((null? path) 
		 (list (list->string (reverse ncname)) path))
		((and (memv (car path) sxp:delimiter)
		      (not (char=? (car path) #\:)))
		 (list (list->string (reverse ncname)) path))
		(else (loop (cons (car path) ncname) (cdr path)))))))))

; The function reads a qualified name (QName)
; Returns: ( (prefix . local-part) new-path )
;      or  ( local-part new-path )    if there is no prefix
;       if there is not QName in the beginning of the 'path' it calls 
;          sxp:xpointer-parse-error
;  prefix, local-part - strings
;  new-path - a list of characters
(define (sxp:parse-qname path)
  (and-let* ((r1 (sxp:parse-ncname path)))
	    (let ((first (car r1))
		  (path2 (cadr r1)))
	      (cond
		((null? path2) (list first path2))
		((not (char=? (car path2) #\:)) (list first path2))
		((null? (cdr path2))
		 (sxp:xpointer-parse-error "no local part of a qualified name"))
		((char=? (cadr path2) #\:) (list first path2))
		(else (and-let* ((r2 (sxp:parse-ncname (cdr path2))))
				(list (cons first (car r2)) (cadr r2)))
		      )))))
                   
;------------------------------------------------
; Parsers for data of basic types

; Reads a natural number:
; [1-9] [0-9]*
; The result:  (list  number  new-path)  or  #f
(define (sxp:parse-natural path)
  (let ((path (sxp:skip-ws path)))
    (cond
      ((null? path)
       (sxp:xpointer-parse-error
        "unexpected end of XPointer path. Expected - number"))
      ((or (char<? (car path) #\1) (char>? (car path) #\9))
       (sxp:xpointer-parse-error "expected - number instead of " (car path)))
      (else (let loop ((res (- (char->integer (car path))
			  48)) ; (char->integer #\0)
                  (path (cdr path)))
         (cond
           ((null? path) (list res path))
           ((char-numeric? (car path))
            (loop (+ (* res 10) (- (char->integer (car path)) 
				   48)) ; (char->integer #\0)
                  (cdr path)))
           (else (list res path))))))))

; Reads a Literal ([29] in XPath specification)
; [29]    Literal    ::=    '"' [^"]* '"'  
;                           | "'" [^']* "'"
; The result:  (string new-path)  or  #f
(define (sxp:parse-literal path)
  (let ((ch (if (sxp:parse-check "\"" path) #\" #\')))
    (let loop ((res '())
	       (path (sxp:parse-assert (if (char=? ch #\") "\"" "'") 
				       path)))
      (cond
	((not path) #f)
	((null? path)
	 (sxp:parse-assert (if (char=? ch #\") "\"" "'") 
			   path)
	 #f)
	((char=? (car path) ch)
	 (list (list->string (reverse res))
	       (cdr path)))
	(else (loop (cons (car path) res) (cdr path)))))))

; Reads a Number ([30]-[31] in XPath specification)
; [30]    Number    ::=    Digits ('.' Digits?)?  
;                          | '.' Digits  
; [31]    Digits    ::=    [0-9]+
; The result:  (number new-path)  or  #f
(define (sxp:parse-number path) 
  (define (digits path)
    (let loop ((n-lst '())
               (path path))
      (cond
        ((and (null? path) (null? n-lst))
         (sxp:xpointer-parse-error 
          "unexpected end of XPointer path. Expected - number"))
        ((null? path) (list n-lst path))
        ((and (or (char<? (car path) #\0) (char>? (car path) #\9))
              (null? n-lst))       
         (sxp:xpointer-parse-error "expected - number instead of " (car path)))
        ((or (char<? (car path) #\0) (char>? (car path) #\9))
         (list n-lst path))
        (else
         (loop (cons (- (char->integer (car path)) (char->integer #\0)) n-lst)
               (cdr path))))))
    
  (let ((path (sxp:skip-ws path)))
    (cond
      ((null? path)
       (sxp:xpointer-parse-error 
        "unexpected end of XPointer path. Expected - number"))
      ((char=? (car path) #\.)
       (and-let* ((lst (digits (cdr path))))
            (let rpt ((res 0)
                      (n-lst (car lst))
                      (path (cadr lst)))
              (if(null? n-lst)
                 (list (/ res 10) path)
                 (rpt (+ (/ res 10) (car n-lst))
                      (cdr n-lst) 
                      path)))))
      (else (and-let* ((lst (digits path)))
		      (let loop ((num1 0)
				 (n-lst (reverse (car lst)))
				 (path (cadr lst)))
			(if (null? n-lst)
			  (cond
			    ((null? path) (list num1 path))
			    ((not (char=? (car path) #\.)) (list num1 path))
			    (else
			      (and-let* ((lst2 (digits (cdr path))))
					(let rpt ((num2 0)
						  (n-lst (car lst2))
						  (path (cadr lst2)))
					  (if (null? n-lst)
					    (list (+ num1 (/ num2 10)) path)
					    (rpt (+ (/ num2 10) (car n-lst))
						 (cdr n-lst) 
						 path))))))
			  (loop (+ (* num1 10) (car n-lst))
				(cdr n-lst) 
				path))))))))

;=========================================================================
; Grammar parsing functions
; All these functions have similar arguments:
;  path - an xpointer path represented as a list of chars
;  ns-binding - declared namespace prefixes (not for all functions)
; ns-binding = (list (prefix . uri) (prefix . uri) ... )
; prefix, uri - strings

;------------------------------------------------
; Functions which parse XPath grammar

; Parses an AxisSpecifier production ([5],[6],[13] in XPath specification)
; [5]    AxisSpecifier    ::=    AxisName '::'  
;                                | AbbreviatedAxisSpecifier
; [6]    AxisName    ::=    'ancestor'  
;                           | 'ancestor-or-self'  
;                           | 'attribute'  
;                           | 'child'  
;                           | 'descendant'  
;                           | 'descendant-or-self'  
;                           | 'following'  
;                           | 'following-sibling'  
;                           | 'namespace'  
;                           | 'parent'  
;                           | 'preceding'  
;                           | 'preceding-sibling'  
;                           | 'self' 
; [13]    AbbreviatedAxisSpecifier    ::=    '@'? 
;
; The result is represented as a list
; (list  (lambda ...)  new-path  root-node-required)  
; or  #f in case of parse error
;  new-path - a list of chars. It represents the remainder of the source string
;  root-node-required - a boolean value
;  (lambda ...) one of the axis functions
; If root-node-required = #t, lambda's signature is
;  (lambda (test-pred?)
;   (lambda (root-node)
;    (lambda (nodeset) ... )))
; otherwise
;  (lambda (test-pred?)
;   (lambda (nodeset) ... ))
(define (sxp:parse-axis-specifier path)
  (cond
    ((sxp:parse-check "ancestor" path sxp:delimiter)
     (and-let* ((path (sxp:parse-assert "::" (sxp:parse-assert "ancestor" path))))
          (list sxp:ancestor path  #t)))
    ((sxp:parse-check "ancestor-or-self" path sxp:delimiter)
     (and-let* 
       ((path (sxp:parse-assert "::" (sxp:parse-assert "ancestor-or-self" path))))
          (list sxp:ancestor-or-self path #t)))
    ((sxp:parse-check "attribute" path sxp:delimiter)
     (and-let* 
       ((path (sxp:parse-assert "::" (sxp:parse-assert "attribute" path))))
          (list sxp:attribute path #f)))
    ((sxp:parse-check "child" path sxp:delimiter)
     (and-let*
       ((path (sxp:parse-assert "::" (sxp:parse-assert "child" path))))
          (list sxp:child path #f)))
    ((sxp:parse-check "descendant" path sxp:delimiter)
     (and-let* 
       ((path (sxp:parse-assert "::" (sxp:parse-assert "descendant" path))))
          (list sxp:descendant path #f)))
    ((sxp:parse-check "descendant-or-self" path sxp:delimiter)
     (and-let* ((path (sxp:parse-assert "::" (sxp:parse-assert 
					       "descendant-or-self" path))))
          (list sxp:descendant-or-self path #f)))
    ((sxp:parse-check "following" path sxp:delimiter)
     (and-let* ((path (sxp:parse-assert "::" (sxp:parse-assert "following" path))))
          (list sxp:following path #t)))
    ((sxp:parse-check "following-sibling" path sxp:delimiter)
     (and-let* ((path (sxp:parse-assert "::" (sxp:parse-assert 
					       "following-sibling" path))))
          (list sxp:following-sibling path #t)))
    ((sxp:parse-check "namespace" path sxp:delimiter)
     (and-let* ((path (sxp:parse-assert "::" (sxp:parse-assert "namespace" path))))
          (list sxp:namespace path #f)))
    ((sxp:parse-check "parent" path sxp:delimiter)
     (and-let* ((path (sxp:parse-assert "::" (sxp:parse-assert "parent" path))))
          (list sxp:parent path #t))) 
    ((sxp:parse-check "preceding" path sxp:delimiter)
     (and-let* 
       ((path (sxp:parse-assert "::" (sxp:parse-assert "preceding" path))))
          (list sxp:preceding path #t)))
    ((sxp:parse-check "preceding-sibling" path sxp:delimiter)
     (and-let*
       ((path (sxp:parse-assert "::" (sxp:parse-assert "preceding-sibling" path))))
          (list sxp:preceding-sibling path #t)))    
    ((sxp:parse-check "self" path sxp:delimiter)
     (and-let*
       ((path (sxp:parse-assert "::" (sxp:parse-assert "self" path))))
       (list sxp:filter path #f)))
    ((sxp:parse-check "@" path)
     (list sxp:attribute (sxp:parse-assert "@" path) #f))
    (else
     (list sxp:child path #f))))

; Parses a NodeTest production 
; ([7],[37] in XPath specification, [11] in XPointer specification)
; [7]    NodeTest    ::=    NameTest  
;                           | NodeType '(' ')'  
;                           | 'processing-instruction' '(' Literal ')' 
; [37]    NameTest    ::=    '*'  
;                            | NCName ':' '*'  
;                            | QName  
; [11]   NodeType   ::=   'comment'  
;                         | 'text'  
;                         | 'processing-instruction'  
;                         | 'node'
;                         | 'point'
;                         | 'range'
; The result is:   ( (lambda (node) ...)  new-path )    or  #f
; #f signals of an error
;  (lambda (node) ...)  - a node test function
(define (sxp:parse-node-test path ns-binding)
  
  (define (brackets path)
    (and-let* ((path (sxp:parse-assert "(" path)))
         (sxp:parse-assert ")" path)))
  (cond
    ((sxp:parse-check "comment" path sxp:delimiter)
     (and-let* ((path (brackets (sxp:parse-assert "comment" path))))
          (list (ntype?? '*COMMENT*) path)))
    ((sxp:parse-check "text" path sxp:delimiter)
     (and-let* ((path (brackets (sxp:parse-assert "text" path))))
          (list (ntype?? '*text*) path)))
    ((sxp:parse-check "node" path sxp:delimiter)
     (and-let* ((path (brackets (sxp:parse-assert "node" path))))
          (list sxp:node? path)))
    ((sxp:parse-check "processing-instruction" path sxp:delimiter)
     (and-let* ((path (sxp:parse-assert "(" 
                 (sxp:parse-assert "processing-instruction" path))))
       (cond
	 ((sxp:parse-check ")" path)
	  (list (lambda (node)
		  (and (pair? node)
		       (eq? (car node) '*PI*)))
		(sxp:parse-assert ")" path)))
	 (else (and-let* ((lst (sxp:parse-literal path))
			  (name (string->symbol (car lst)))
			  (path (sxp:parse-assert ")" (cadr lst))))
			 (list
			   (lambda (node)
			     (and (pair? node) 
				  (eq? (car node) '*PI*)
				  (equal? (cadr node) name)))
			   path))))))
    ((sxp:parse-check "point" path sxp:delimiter)
     (and-let* ((path (brackets (sxp:parse-assert "point" path))))
          (sxp:xpointer-parse-warning 
           "'point()' NodeTest is not supported by this implementation. "
           "It is defaulted to an predicate which is always false")
          (list (lambda (node) #f) path)))
    ((sxp:parse-check "range" path sxp:delimiter)
     (and-let* ((path (brackets (sxp:parse-assert "range" path))))
	   (sxp:xpointer-parse-warning 
	     "'range()' NodeTest is not supported by this implementation. "
	     "It is defaulted to an predicate which is always false")
	   (list (lambda (node) #f) path)))
    ((sxp:parse-check "*" path)
       (list (ntype?? '*) (sxp:parse-assert "*" path)))
    (else (and-let* ((lst (sxp:parse-ncname path)))
		    (let ((name (car lst))
			  (path (cadr lst)))            
		      (if
			(and (not (null? path)) (char=? (car path) #\:))
			(let ((path (sxp:parse-assert ":" path))
			      (pair (assoc name ns-binding)))
			  (cond
			    ((not pair) 
			     (sxp:xpointer-parse-error 
			       "unknown namespace prefix - " (car lst)))
			    ((and (not (null? path)) (char=? (car path) #\*))
			     (list
			       (ntype-namespace-id?? (cdr pair))
			       (sxp:parse-assert "*" path)))
			    (else
			      (and-let* ((lst (sxp:parse-ncname path)))
					(list
					  (ntype?? (string->symbol
						     (string-append (cdr pair) 
								    ":" 
								    (car lst))))
					  (cadr lst))))))
			(list (ntype?? (string->symbol name)) path)))))))
         
;------------------------------------------------
; Functions which parse XPath grammar, part 2
;
; All these functions return a value which has the same signature:
; (list (lambda (nodeset root-node context var-binding id-index) ...)
;       path 
;       index-required )
; or #f
; #f signals of a parse error (error message is printed as a side effect
; during parsing)
;
; (lambda (nodeset root-node context var-binding id-index) - an SXPath-like 
; function (it transforms a nodeset into a new nodeset)
;  nodeset - a current set of nodes
;  root-node - the root of a document (a singleton nodeset)
;  context - the context of the node; list of two elements - (position size)
;  position - context position (a number)
;  size - context size (a number)
;  id-index - is used for selecting an element by its unique ID
;  path - an XPointer path represented as the list of chars
;  index-required - boolean value: whether an id-index was required "deeper" 
; within the grammar

; Filter nodeset using preds-list as described in XPath rec. 2.4
; A helper for sxp:parse-step and sxp:parse-filter-expr
(define (sxp:xpath-nodeset-filter preds-list nodeset root-node 
				  var-binding id-index)
  (let rpt ((nodeset nodeset)
	    (ps (reverse preds-list)))
    (if (null? ps) 
      nodeset
      (let ((size (length nodeset)))
	(let lab ((nset nodeset)
		  (res '())
		  (pos 1)) 
	  (if (null? nset)
	    (rpt (reverse res) (cdr ps))
	    (let ((val ((car ps) 
			(list (car nset)) 
			root-node 
			(cons pos size) 
			var-binding 
			id-index)))
	      (lab (cdr nset)
		   (if (if (number? val)
			 (= val pos)
			 (sxp:boolean val))
		     (cons (car nset) res)
		     res)
		   (+ pos 1))
	      )))))))

; Parses a Step production 
; ([4xptr] in XPointer specification, [12] in XPath specification)
; [4xptr] Step ::= AxisSpecifier NodeTest Predicate*
;                  | AbbreviatedStep
;                  | 'range-to' '(' Expr ')' Predicate*
; [12]    AbbreviatedStep    ::=    '.'  
;                                   | '..' 
; ATTENTION: 'range-to' FUNCTION IS NOT IMPLEMENTED. INFORMATION REFERRING
; TO IT IS PARSED AND IGNORED. THIS FUNCTION CALL RESULTS TO AN EMPTY NODESET
(define (sxp:parse-step path ns-binding)
  (cond
    ((sxp:parse-check ".." path)
     (list
      (lambda (nodeset root-node context var-binding id-index)
        (((sxp:parent sxp:node?) root-node) nodeset))
      (sxp:parse-assert ".." path)
      #f))
    ((sxp:parse-check "." path)
     (list
      (lambda (nodeset root-node context var-binding id-index)
        ((sxp:filter sxp:node?) nodeset))
      (sxp:parse-assert "." path)
      #f))
    ((sxp:parse-check "range-to" path)
     (and-let*
       ((path0 (sxp:parse-assert "(" (sxp:parse-assert "range-to" path)))
        (lst (sxp:parse-expr path0 ns-binding))
        (path (sxp:parse-assert ")" (cadr lst)))) ; check again?
       (let loop ((path path))
	 (cond
	   ((sxp:parse-check "[" path)
	    (and-let* ((lst (sxp:parse-predicate path ns-binding)))
		      (loop (cadr lst))))
	   (else   ; Predicates are over
	     (sxp:xpointer-parse-warning
	       "range-to function not implemented. "
	       "Defaulting to an empty nodeset")
	     (list
	       (lambda (nodeset root-node context var-binding id-index)
		 '())
	       path
	       #f))))))
    (else (and-let* ((lst (sxp:parse-axis-specifier path)))
        (let ((axis (car lst))             
             (root-node-required (caddr lst)))
          (and-let* ((lst (sxp:parse-node-test (cadr lst) ns-binding)))
             (let ((test (car lst)))                 
               (let loop ((preds '())
                          (path (cadr lst))
                          (index-required #f))
                 (cond
                  ((sxp:parse-check "[" path)
                   (and-let* ((lst (sxp:parse-predicate path ns-binding)))
                        (loop (cons (car lst) preds)
                              (cadr lst)
                              (or index-required (caddr lst)))))
                  ; No more predicates 
                  ((null? preds)
                   (list
                    (lambda (nodeset root-node context var-binding id-index)
                      (if root-node-required
                          (((axis test) root-node) nodeset)
                          ((axis test) nodeset)))
                    path
                    index-required))
                  (else  ; More predicates 
		    (list  
		      (lambda (nodeset root-node context var-binding id-index)
			(sxp:xpath-nodeset-filter 
			  preds 
			  ((if root-node-required
			     ((axis test) root-node)
			     (axis test)) 
			   nodeset)
			  root-node var-binding id-index))
		      path
		      index-required))
		  )))))))))

; Parses a RelativeLocationPath production ([3],[11] in XPath specification)
; [3]  RelativeLocationPath  ::=  Step  
;                                 | RelativeLocationPath '/' Step  
;                                 | AbbreviatedRelativeLocationPath 
; [11]  AbbreviatedRelativeLocationPath  ::=  RelativeLocationPath '//' Step 
(define (sxp:parse-relative-location-path path ns-binding)
  (let loop ((funcs '())
             (path path)
             (index-required #f))
    (and-let* ((lst (sxp:parse-step path ns-binding)))
         (let ((func (car lst))
               (path (cadr lst)))
           (cond
             ((sxp:parse-check "//" path)
              (loop
               (cons 
                (lambda (nodeset root-node context var-binding id-index)
                  ((sxp:descendant-or-self sxp:node?) nodeset))
                (cons func funcs))
               (sxp:parse-assert "//" path)
               (or index-required (caddr lst))))
             ((sxp:parse-check "/" path)
              (loop (cons func funcs)
                    (sxp:parse-assert "/" path)
                    (or index-required (caddr lst))))
             ; no more steps
             ((null? funcs) lst) ; a current step is the only step
             (else   ; several steps   
              (list
               (lambda (nodeset root-node context var-binding id-index)
                 (let rpt ((nset nodeset)
                           (fs (reverse (cons func funcs))))
                   (if (null? fs)
                      nset
                      (rpt ((car fs)
                            nset root-node context var-binding id-index)
                           (cdr fs)))))
               path
               (or index-required (caddr lst)))))))))        
                   
; Parses a LocationPath production ([1],[2],[10] in XPath specification)
; [1]    LocationPath    ::=    RelativeLocationPath  
;                               | AbsoluteLocationPath  
; [2]    AbsoluteLocationPath    ::=    '/' RelativeLocationPath?  
;                                       | AbbreviatedAbsoluteLocationPath
; [10]    AbbreviatedAbsoluteLocationPath    ::=    '//' RelativeLocationPath
(define (sxp:parse-location-path path ns-binding) 
  (define (nothing? path)
    (let ((path (sxp:skip-ws path)))
      (cond
	((null? path) #t)
	((memv (car path) '(#\| #\+ #\- #\< #\> #\= #\) #\] #\,)) #t)
	((or (sxp:parse-check "mod" path sxp:delimiter)
	     (sxp:parse-check "div" path sxp:delimiter)
	     (sxp:parse-check "!=" path)
	     (sxp:parse-check "and" path sxp:delimiter)
	     (sxp:parse-check "or" path sxp:delimiter)) #t)
	(else #f))))
  (cond
    ((sxp:parse-check "//" path)
     (and-let* ((lst (sxp:parse-relative-location-path 
		       (sxp:parse-assert "//" path)
		       ns-binding)))
	       (let ((func (car lst))
		     (path (cadr lst))
		     (index-required (caddr lst)))
		 (list (lambda (nodeset root-node context var-binding id-index)
			 (func
			   ((sxp:descendant-or-self sxp:node?) root-node)
			   root-node context var-binding id-index))
		       path
		       index-required))))
    ((sxp:parse-check "/" path)
     (let ((path (sxp:parse-assert "/" path)))
       (if (nothing? path)
	 (list (lambda (nodeset root-node context var-binding id-index)
		 root-node)
	       path
	       #f)
	 (let ((lst (sxp:parse-relative-location-path path ns-binding)))
	   (if (null? lst)
	     #f
	     (let((func (car lst))
		  (path (cadr lst))
		  (index-required (caddr lst)))
	       (list (lambda (nodeset root-node context var-binding id-index)
		       (func root-node root-node context var-binding id-index))
		     path
		     index-required)))))))
    (else (sxp:parse-relative-location-path path ns-binding))))

; Parses a Predicate production ([8]-[9] in XPath specification)
; [8]    Predicate    ::=    '[' PredicateExpr ']'  
; [9]    PredicateExpr    ::=    Expr 
; Note that (according to specification) a Predicate must return a number or
; a boolean value. However, the return value type is not checked in this
; function. This is performed in functions that use 'parse-predicate'
(define (sxp:parse-predicate path ns-binding)
  (and-let* ((path0 (sxp:parse-assert "[" path))
             (lst (sxp:parse-expr path0 ns-binding))
             (path (sxp:parse-assert "]" (cadr lst))))
                 (list (car lst) ; func
		       path 
		       (caddr lst) ; index required
		       )))

; Parses a VariableReference production ([36] in XPath specification)
; [36]    VariableReference    ::=    '$' QName 
(define (sxp:parse-variable-reference path ns-binding)
  (and-let* ((path (sxp:parse-assert "$" path))
	     (lst (sxp:parse-qname path)))
	    (let ((name (car lst)))
	      (list (lambda (nodeset root-node context var-binding id-index)
		      (cond
			((assoc name var-binding)
			 => cdr)
			(else (sxp:xpointer-runtime-error "Unbound variable - "
				(if (pair? name)
				  (string-append (car name) ":" (cdr name))
				  name)
				". Defaulting to an empty nodeset")
			      '())))
		    (cadr lst)
		    #f))))
                         
; Parses a FunctionCall production ([16],[17],[35] in XPath specification)
; [16]    FunctionCall    ::=    FunctionName 
;                                '(' ( Argument ( ',' Argument )* )? ')'  
; [17]    Argument    ::=    Expr 
; [35]    FunctionName    ::=    QName - NodeType
; ATTENTION: SOME CORE FUNCTIONS ARE NOT SUPPORTED.
; SUCH A FUNCTION CALL IS DEFAULTED TO AN EMPTY NODESET
(define (sxp:parse-function-call path ns-binding)
  
  ; Parses a list of arguments
  ;  min - minimal number of arguments
  ;  max - maximal number of arguments. If max<0, then the number of 
  ; arguments is not limited from above
  ; Result:
  ;  (funcs  new-path  index-required)
  ;  funcs = (func func ...)  - functions which evaluate arguments    
  (define (sxp:parse-arguments min max path ns-binding)    
    (let ((path (sxp:parse-assert "(" path)))
      (cond
	((not path) #f)
	((= max 0)
	 (and-let* ((path (sxp:parse-assert ")" path)))
		   (list '() path #f)))
	((and (= min 0) (sxp:parse-check ")" path))
	 (list '() (sxp:parse-assert ")" path) #f))
	(else (let back ((n 1)
			 (funcs '())
			 (path path)
			 (index-required #f))
		(and-let* ((lst (sxp:parse-expr path ns-binding)))
			  (let ((func (car lst))
				(path (cadr lst))
				(bool (or index-required (caddr lst))))
			    (cond
			      ((= n max)
			       (and-let* ((path (sxp:parse-assert ")" path)))
					 (list (reverse (cons func funcs))
					       path
					       bool)))
			      ((and (>= n min) (sxp:parse-check ")" path))
			       (list (reverse (cons func funcs))
				     (sxp:parse-assert ")" path)
				     bool))
			      (else (and-let* ((path (sxp:parse-assert "," path)))
					      (back (+ n 1)
						    (cons func funcs)
						    path
						    bool)))))))))))
                 
  (and-let* ((lst (sxp:parse-qname path)))
      (let ((name (car lst))  ; can be a pair
	    (path (cadr lst)))       
	(cond
	  ((equal? name "last")  ; it is a 'last' function
	   (and-let* ((lst2 (sxp:parse-arguments 0 0 path ns-binding)))
	       (list (lambda (nodeset root-node context var-binding id-index) 
		   (cdr context))
		 (cadr lst2)
		 #f)))
         ((equal? name "position")  ; it is a 'position' function
	  (and-let* ((lst2 (sxp:parse-arguments 0 0 path ns-binding)))
	      (list (lambda (nodeset root-node context var-binding id-index) 
		  (car context))
		(cadr lst2)
		#f)))
	 ((equal? name "count")  ; it is a 'count' function
	  (and-let* ((lst2 (sxp:parse-arguments 1 1 path ns-binding)))
	      (let ((func (caar lst2))
		    (path (cadr lst2))
		    (index-required (caddr lst2)))
		(list (lambda (nodeset root-node context var-binding id-index)
		    (let ((res (func nodeset root-node context 
				     var-binding id-index)))
		      (cond
			((nodeset? res) (length res))
			(else
			  (sxp:xpointer-runtime-error
			    "'count' function - an argument is not a nodeset. "
			    "Returning zero")
			  0))))
		  path
		  index-required))))
	 ((equal? name "id")  ; it is an 'id' function
	  (and-let* ((lst2 (sxp:parse-arguments 1 1 path ns-binding)))
	      (let ((func (caar lst2))
		    (path (cadr lst2)))
		(list (lambda (nodeset root-node context var-binding id-index)
		    ((sxp:id id-index) 
		     (func nodeset root-node context var-binding id-index)))
		  path
		  #t))))
         ((equal? name "string")  ; it is a 'string' function
          (and-let* ((lst2 (sxp:parse-arguments 0 1 path ns-binding)))
             (let((funcs (car lst2))
                  (path (cadr lst2))
                  (index-required (caddr lst2)))
               (if (null? funcs)
                (list (lambda (nodeset root-node context var-binding id-index)
                   (sxp:string nodeset))
                 path #f)
                (list (lambda (nodeset root-node context var-binding id-index)
                   (sxp:string 
                    ((car funcs) 
                     nodeset root-node context var-binding id-index)))
                 path
                 index-required)))))
         ((equal? name "boolean")  ; it is a 'boolean' function
	  (and-let* ((lst2 (sxp:parse-arguments 1 1 path ns-binding)))
	      (let ((func (caar lst2))
		    (path (cadr lst2))
		    (index-required (caddr lst2)))
		(list (lambda (nodeset root-node context var-binding id-index)
		    (sxp:boolean 
		      (func nodeset root-node context var-binding id-index)))
		  path
		  index-required))))
         ((equal? name "not")  ; it is a 'not' function
	  (and-let* ((lst2 (sxp:parse-arguments 1 1 path ns-binding)))
	      (let ((func (caar lst2))
		    (path (cadr lst2))
		    (index-required (caddr lst2)))
		(list (lambda (nodeset root-node context var-binding id-index)
		    (not (sxp:boolean 
			   (func nodeset root-node context 
				 var-binding id-index))))
		  path
		  index-required))))
	 ((equal? name "true")  ; it is a 'true' function
	  (and-let* ((lst2 (sxp:parse-arguments 0 0 path ns-binding)))
	      (list (lambda (nodeset root-node context var-binding id-index) #t)
		(cadr lst2)
		#f)))
         ((equal? name "false")  ; it is a 'false' function
	  (and-let* ((lst2 (sxp:parse-arguments 0 0 path ns-binding)))
	      (list (lambda (nodeset root-node context var-binding id-index) #f)
		(cadr lst2)
		#f)))         
	 ((equal? name "number")  ; it is a 'number' function
	  (and-let* ((lst2 (sxp:parse-arguments 0 1 path ns-binding)))
		    (let ((funcs (car lst2))
			  (path (cadr lst2))
			  (index-required (caddr lst2)))
		      (if (null? funcs)
			(list
			  (lambda (nodeset root-node context var-binding id-index)
			    (sxp:number nodeset))
			  path
			  #f)
			(list                                                
			  (lambda (nodeset root-node context var-binding id-index)
			    (sxp:number 
			      ((car funcs) nodeset root-node context
					   var-binding id-index)))
			  path
			  index-required)))))
         ((equal? name "floor")  ; it is a 'floor' function
          (and-let* ((lst2 (sxp:parse-arguments 1 1 path ns-binding)))
               (let ((func (caar lst2))
                    (path (cadr lst2))
                    (index-required (caddr lst2)))
                 (list (lambda (nodeset root-node context var-binding id-index)
		    (inexact->exact
                    (floor (sxp:number 
                            (func nodeset root-node context
                                  var-binding id-index)))))
                  path
                  index-required))))
         ((equal? name "ceiling")  ; it is a 'ceiling' function
          (and-let* ((lst2 (sxp:parse-arguments 1 1 path ns-binding)))
               (let ((func (caar lst2))
                    (path (cadr lst2))
                    (index-required (caddr lst2)))
                 (list (lambda (nodeset root-node context var-binding id-index)
		    (inexact->exact
                    (ceiling (sxp:number
                              (func nodeset root-node context
                                    var-binding id-index)))))
                  path
                  index-required))))
         ((equal? name "round")  ; it is a 'round' function
	  (and-let* ((lst2 (sxp:parse-arguments 1 1 path ns-binding)))
	      (let ((func (caar lst2))
		    (path (cadr lst2))
		    (index-required (caddr lst2)))
		(list (lambda (nodeset root-node context var-binding id-index)
		    (inexact->exact
		      (round (sxp:number
			       (func nodeset root-node context
				     var-binding id-index)))))
		  path
		  index-required))))
         (else       ; unknown function
	   (and-let* ((lst (sxp:parse-arguments 0 -1 path ns-binding)))
	       (let ((path (cadr lst)))                           
		 (sxp:xpointer-parse-warning
		   "function not implemented - "
		   (if (pair? name)
		     (string-append (car name) ":" (cdr name))
		     name))
		 (list (lambda (nodeset root-node context var-binding id-index) '())
		   path
		   #f))))))))
     
; Parses a PrimaryExpr production ([15] in XPath specification)
; [15]    PrimaryExpr    ::=    VariableReference  
;                               | '(' Expr ')'  
;                               | Literal  
;                               | Number  
;                               | FunctionCall 
; [29]    Literal    ::=    '"' [^"]* '"'  
;                           | "'" [^']* "'"  
; [30]    Number    ::=    Digits ('.' Digits?)?  
;                          | '.' Digits  
; [31]    Digits    ::=    [0-9]+ 
(define (sxp:parse-primary-expr path ns-binding)
  (cond
    ((sxp:parse-check "$" path)  ; a VariableReference
     (sxp:parse-variable-reference path ns-binding))
    ((sxp:parse-check "(" path)  ; an '(' Expr ')'
     (and-let* ((lst (sxp:parse-expr (sxp:parse-assert "(" path) ns-binding))
	        (path (sxp:parse-assert ")" (cadr lst))))
	 (let ((func (car lst))
	       (index-required (caddr lst)))
	     (list func path index-required))))
    ((or (sxp:parse-check "\"" path) (sxp:parse-check "'" path))  ; a Literal
     (and-let* ((lst (sxp:parse-literal path)))
	 (list
	   (lambda (nodeset root-node context var-binding id-index) (car lst))
	   (cadr lst)
	   #f)))
    ((let ((p (sxp:skip-ws path)))
       (cond ((null? p) #f)
	     ((char=? (car p) #\.) #t)
	     ((and (char>=? (car p) #\0) (char<=? (car p) #\9)) #t)
	     (else #f)))   ; a Number
     (and-let* ((lst (sxp:parse-number path)))
	 (list
	   (lambda (nodeset root-node context var-binding id-index) (car lst))
	   (cadr lst)
	   #f)))
    (else   ; a Function call
      (sxp:parse-function-call path ns-binding))))


; Parses a FilterExpr production ([20] in XPath specification)
; [20]    FilterExpr    ::=    PrimaryExpr  
;                              | FilterExpr Predicate 
(define (sxp:parse-filter-expr path ns-binding)
  (and-let* ((lst (sxp:parse-primary-expr path ns-binding)))
      (let ((prim (car lst)))
	(let loop ((preds '())
		   (path (cadr lst))
		   (index-required (caddr lst)))
	  (if (sxp:parse-check "[" path)
	    (and-let* ((lst (sxp:parse-predicate path ns-binding)))
		(loop (cons (car lst) preds)
		      (cadr lst)
		      (or index-required (caddr lst))))
	    (if (null? preds) ; No more predicates
	      (list prim path index-required)
	      (list
		(lambda (nodeset root-node context var-binding id-index)
		  (let ((nodeset 
			  (prim nodeset root-node context var-binding id-index)))
		    (sxp:xpath-nodeset-filter 
		      preds 
		      (cond
			((nodeset? nodeset) nodeset)
			(else 
			  (sxp:xpointer-runtime-error 
			    "expected - nodeset instead of " nodeset 
			    ". Converting to an empty nodeset")
			  '()))
		      root-node var-binding id-index)))
		path
		index-required)))))))

; Parses a PathExpr production ([19] in XPath specification)
; [19]    PathExpr    ::=    LocationPath  
;                            | FilterExpr  
;                            | FilterExpr '/' RelativeLocationPath  
;                            | FilterExpr '//' RelativeLocationPath
(define (sxp:parse-path-expr path ns-binding)
  
  (define (filter-expr? path)
    (let ((path (sxp:skip-ws path)))
      (cond
        ((null? path) #f)
        ((member 
          (car path) 
          '(#\$ #\( #\" #\' #\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9)) #t)
        ((char=? (car path) #\.)
         (cond((null? (cdr path)) #f)
              ((member (cadr path)
                       '(#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9)) #t)
              (else #f)))
        ((member 
          (car path)
          '(#\) #\< #\> #\[ #\] #\/ #\+ #\* #\, #\= #\| #\! #\@ #\-)) #f)
        (else (let ((lst (sxp:parse-ncname path)))
		(cond
		  ((not lst) #f)
		  ((sxp:parse-check "::" (cadr lst)) #f)
		  (else (and-let* ((lst (sxp:parse-name path)))
				  (let ((name (car lst))
					(new-path (sxp:skip-ws (cadr lst))))
				    (cond
				      ((string=? name "range-to") #f)
				      ((string=? name "comment") #f)
				      ((string=? name "text") #f)
				      ((string=? name "processing-instruction") #f)
				      ((string=? name "node") #f)
				      ((string=? name "point") #f)
				      ((string=? name "range") #f)
				      ((null? new-path) #f)
				      ((char=? (car new-path) #\() #t)
				      (else #f))))))))))) 
  (if (not (filter-expr? path))
    (sxp:parse-location-path path ns-binding)
    (and-let* ((lst (sxp:parse-filter-expr path ns-binding)))
	      (let ((f-ex (car lst))
		    (path (cadr lst))
		    (index-required (caddr lst)))
		(cond
		  ((sxp:parse-check "//" path)
		   (and-let* ((lst2 (sxp:parse-relative-location-path 
				(sxp:parse-assert "//" path) ns-binding)))
		       (let ((f-rel (car lst2))
			    (path (cadr lst2))
			    (bool (or index-required (caddr lst2))))
			 (list
			   (lambda (nodeset root-node context var-binding id-index)
			     (let ((nset (f-ex nodeset root-node context
					      var-binding id-index)))
			       (let ((nset 
				      (cond
					((nodeset? nset) nset)
					(else 
					  (sxp:xpointer-runtime-error 
					    "expected - nodeset instead of " nset 
					    ". Converting to an empty nodeset")
					  '()))))
				 (let ((nset ((sxp:descendant-or-self sxp:node?) 
					     nset)))
				   (f-rel nset root-node context
					  var-binding id-index)))))
			   path
			   bool))))
		  ((sxp:parse-check "/" path)
		   (and-let* ((lst2 (sxp:parse-relative-location-path 
				      (sxp:parse-assert "/" path) ns-binding)))
			     (let ((f-rel (car lst2))
				   (path (cadr lst2))
				   (bool (or index-required (caddr lst2))))
			       (list
				 (lambda (nodeset root-node context var-binding 
						  id-index)
				   (let ((nset (f-ex nodeset root-node context
						     var-binding id-index)))
				     (let ((nset 
					     (cond
					       ((nodeset? nset) nset)
					       (else 
						 (sxp:xpointer-runtime-error 
						   "expected - nodeset instead of "
						   nset 
					   ". Converting to an empty nodeset")
						 '()))))                       
				       (f-rel nset root-node context var-binding 
					      id-index))))
				 path
				 bool))))
		  (else lst))))))
                   
; Parses a UnionExpr production ([18] in XPath specification)
; [18]    UnionExpr    ::=    PathExpr  
;                             | UnionExpr '|' PathExpr
(define (sxp:parse-union-expr path ns-binding)
  (let loop ((funcs '())
	     (path path)
	     (index-required #f))
    (and-let* ((lst (sxp:parse-path-expr path ns-binding)))
	      (let ((func (car lst))
		    (path (cadr lst))
		    (id-ind? (or index-required (caddr lst))))
		(let ((new-path (sxp:parse-check "|" path)))
		  (if (not new-path)  ; no more PathExprs
		    (if (null? funcs)  ; only one PathExpr
		      (list func path id-ind?)
		      (list
			(lambda (nodeset root-node context var-binding id-index)
			  (let rpt ((res '())
				    (fs (reverse (cons func funcs))))
			    (if (null? fs)
			      res
			      (let ((nset ((car fs) nodeset root-node context
						    var-binding id-index)))
				(rpt 
				  (append 
				    res
				    (cond
				      ((not (nodeset? nset))
				       (sxp:xpointer-runtime-error 
					 "expected - nodeset instead of "
					 nset ". Ignoring")
				       '())
				      (else nset)))
				  (cdr fs))))))
			path
			id-ind?))
		    (loop (cons func funcs) new-path id-ind?)))))))
 
; Parses a UnaryExpr production ([27] in XPath specification)
; [27]    UnaryExpr    ::=    UnionExpr  
;                             | '-' UnaryExpr 
; Note that the grammar allows multiple unary minuses
(define (sxp:parse-unary-expr path ns-binding)
  (if (not (sxp:parse-check "-" path))
    (sxp:parse-union-expr path ns-binding)
    (let loop ((minu #f) (path path))
      (let ((new-path (sxp:parse-check "-" path)))
	(if (not new-path)  ; minuses are over
	  (and-let* ((lst (sxp:parse-union-expr path ns-binding)))
		    (let ((func (car lst))
			  (path2 (cadr lst))
			  (index-required (caddr lst)))
		      (list
			(lambda (nodeset root-node context var-binding id-index)
			  (if minu
			    (- 0 (sxp:number 
				   (func nodeset root-node context
					 var-binding id-index)))
			    (sxp:number 
			      (func nodeset root-node context
				    var-binding id-index))))
			path2
			index-required)))
	  (loop (not minu) new-path))))))

; A helper for arithmetic parsers 
;   sxp:parse-additive-expr and sxp:parse-multiplicative-expr 
(define (sxp:no-more-exprs funcs func path id-ind? opers)
  (if (null? funcs) ; only one Expr
    (list func path id-ind?)
    (let ((flst (reverse (cons func funcs))))
      (list (lambda (nodeset root-node context var-binding id-index)
	  (let rpt ((res (sxp:number
			   ((car flst) nodeset root-node context
				       var-binding id-index)))
		    (fs (cdr flst))
		    (ops (reverse opers)))
	    (if (null? fs)
	      res
	      (rpt ((car ops)
		    res
		    (sxp:number 
		      ((car fs) nodeset root-node context
				var-binding id-index)))
		   (cdr fs)
		   (cdr ops)))))
	path
	id-ind?))))

; Parses a MultiplicativeExpr production ([26],[34] in XPath specification)
; [26] MultiplicativeExpr  ::=  UnaryExpr  
;                               | MultiplicativeExpr MultiplyOperator UnaryExpr
;                               | MultiplicativeExpr 'div' UnaryExpr  
;                               | MultiplicativeExpr 'mod' UnaryExpr 
; [34] MultiplyOperator  ::=  '*'
(define (sxp:parse-multiplicative-expr path ns-binding)
  (let loop ((funcs '())
             (opers '())
             (path path)
             (index-required #f))
    (and-let* ((lst (sxp:parse-unary-expr path ns-binding)))
         (let ((func (car lst))
              (path (cadr lst))
              (id-ind? (or index-required (caddr lst))))
           (cond
             ((sxp:parse-check "*" path)
              (loop (cons func funcs) (cons * opers)
                    (sxp:parse-assert "*" path) id-ind?))
             ((sxp:parse-check "div" path sxp:delimiter)
              (loop (cons func funcs) (cons / opers)
                    (sxp:parse-assert "div" path) id-ind?))
             ((sxp:parse-check "mod" path sxp:delimiter)
              (loop (cons func funcs) (cons remainder opers)
                    (sxp:parse-assert "mod" path) id-ind?))             
             (else  ; no more UnaryExprs
	       (sxp:no-more-exprs funcs func path id-ind? opers)
	      ))))))

; Parses a AdditiveExpr production ([25] in XPath specification)
; [25]    AdditiveExpr    ::=    MultiplicativeExpr  
;                                | AdditiveExpr '+' MultiplicativeExpr  
;                                | AdditiveExpr '-' MultiplicativeExpr 
(define (sxp:parse-additive-expr path ns-binding)
  (let loop ((funcs '())
             (opers '())
             (path path)
             (index-required #f))
    (and-let* ((lst (sxp:parse-multiplicative-expr path ns-binding)))
         (let ((func (car lst))
              (path (cadr lst))
              (id-ind? (or index-required (caddr lst))))
           (cond
             ((sxp:parse-check "+" path)
              (loop (cons func funcs) (cons + opers)
                    (sxp:parse-assert "+" path) id-ind?))
             ((sxp:parse-check "-" path)
              (loop (cons func funcs) (cons - opers)
                    (sxp:parse-assert "-" path) id-ind?))
             (else  ; no more MultiplicativeExprs
                (sxp:no-more-exprs funcs func path id-ind? opers)
	      ))))))

; Parses a RelationalExpr production ([24] in XPath specification)
; [24]    RelationalExpr    ::=    AdditiveExpr  
;                                  | RelationalExpr '<' AdditiveExpr  
;                                  | RelationalExpr '>' AdditiveExpr  
;                                  | RelationalExpr '<=' AdditiveExpr  
;                                  | RelationalExpr '>=' AdditiveExpr 
(define (sxp:parse-relational-expr path ns-binding)
  (let loop ((funcs '())
             (opers '())
             (path path)
             (index-required #f))
    (let ((lst (sxp:parse-additive-expr path ns-binding)))
      (if (not lst)
         #f
         (let ((func (car lst))
              (path (cadr lst))
              (bool (or index-required (caddr lst))))
           (cond
             ((sxp:parse-check "<=" path)
              (loop (cons func funcs) (cons '<= opers)                    
                    (sxp:parse-assert "<=" path) bool))
             ((sxp:parse-check ">=" path)
              (loop (cons func funcs) (cons '>= opers)
                    (sxp:parse-assert ">=" path) bool))             
             ((sxp:parse-check "<" path)
              (loop (cons func funcs) (cons '< opers)                    
                    (sxp:parse-assert "<" path) bool))
             ((sxp:parse-check ">" path)
              (loop (cons func funcs) (cons '> opers)                    
                    (sxp:parse-assert ">" path) bool))
             (else  ; no more AdditiveExprs
              (if (null? funcs) ; only one AdditiveExpr  ; sxp:no-more-helper ?
                 (list func path bool)
                 (let ((flst (reverse (cons func funcs))))
                   (list                  
                    (lambda (nodeset root-node context var-binding id-index)
                      (let rpt ((res ((car flst) nodeset root-node context
                                      var-binding id-index))
                                (fs (cdr flst))
                                (ops (reverse opers)))
                        (if (null? fs) 
                           res
                           (rpt (sxp:cmp-objects
                                 (car ops) 
                                 res 
                                 ((car fs) nodeset root-node context
                                           var-binding id-index))
                                (cdr fs)
                                (cdr ops)))))
                    path
                    bool)))
	      )))))))


; Parses an EqualityExpr production ([23] in XPath specification)
; [23]    EqualityExpr    ::=    RelationalExpr  
;                                | EqualityExpr '=' RelationalExpr  
;                                | EqualityExpr '!=' RelationalExpr 
(define (sxp:parse-equality-expr path ns-binding)
  (let loop ((funcs '())
	     (opers '())
	     (path path)
	     (index-required #f))
    (and-let* ((lst (sxp:parse-relational-expr path ns-binding)))
	      (let ((func (car lst))
		    (path (cadr lst))
		    (id-ind? (or index-required (caddr lst))))
		(cond
		  ((sxp:parse-check "=" path)
		   (loop (cons func funcs) (cons '= opers)                    
			 (sxp:parse-assert "=" path) id-ind?))
		  ((sxp:parse-check "!=" path)
		   (loop (cons func funcs) (cons '!= opers)                    
			 (sxp:parse-assert "!=" path) id-ind?))
		  (else  ; no more RelationalExprs
		    (if (null? funcs) ; only one RelationalExpr
		      (list func path id-ind?)
		      (let ((flst (reverse (cons func funcs))))
			(list                  
			  (lambda (nodeset root-node context var-binding id-index)
			    (let rpt ((res ((car flst) nodeset root-node context
						       var-binding id-index))
				      (fs (cdr flst))
				      (ops (reverse opers)))
			      (if (null? fs) 
				res
				(rpt (sxp:cmp-objects
				       (car ops) 
				       res 
				       ((car fs) nodeset root-node context
						 var-binding id-index))
				     (cdr fs)
				     (cdr ops)))))
			  path
			  id-ind?)))
		    ))))))

; Parses an AndExpr production ([22] in XPath specification)
; [22]    AndExpr    ::=    EqualityExpr  
;                           | AndExpr 'and' EqualityExpr 
; Note that according to 3.4 in XPath specification, the right operand is not
; evaluated if the left operand evaluates to false
(define (sxp:parse-and-expr path ns-binding)
  (let loop ((funcs '())
             (path path)
             (index-required #f))
    (and-let* ((lst (sxp:parse-equality-expr path ns-binding)))
         (let ((func (car lst))
               (path (cadr lst))
               (id-ind? (or index-required (caddr lst))))
           (let ((new-path (sxp:parse-check "and" path sxp:delimiter)))
             (if (not new-path)  ; no more EqualityExprs
                (if (null? funcs)  ; only one EqualityExpr
                   (list func path id-ind?)
                   (list
                    (lambda (nodeset root-node context var-binding id-index)
                      (let rpt ((fs (reverse (cons func funcs))))
                        (cond
                          ((null? fs) #t)
                          ((not (sxp:boolean
                                 ((car fs) nodeset root-node context
                                  var-binding id-index))) #f)
                          (else (rpt (cdr fs))))))
                    path
                    id-ind?))
                (loop (cons func funcs) new-path id-ind?)))))))


; Parses an Expr production ([14],[21] in XPath specification)
; [14]    Expr    ::=    OrExpr 
; [21]    OrExpr    ::=    AndExpr  
;                          | OrExpr 'or' AndExpr
; Note that according to 3.4 in XPath specification, the right operand is not
; evaluated if the left operand evaluates to true
(define (sxp:parse-expr path ns-binding)
  (let loop ((funcs '())
	     (path path)
	     (index-required #f))
    (and-let* ((lst (sxp:parse-and-expr path ns-binding)))
	      (let ((func (car lst))
		    (path (cadr lst))
		    (id-ind? (or index-required (caddr lst))))
		(let ((new-path (sxp:parse-check "or" path sxp:delimiter)))
		  (if (not new-path)  ; no more AndExprs
		    (if (null? funcs)  ; only one AndExpr
		      (list func path id-ind?)
		      (list
			(lambda (nodeset root-node context var-binding id-index)
			  (let rpt ((fs (reverse (cons func funcs))))
			    (cond
			      ((null? fs) #f)
			      ((sxp:boolean
				 ((car fs) nodeset root-node context
					   var-binding id-index)) #t)
			      (else (rpt (cdr fs))))))
			path
			id-ind?))
		    (loop (cons func funcs) new-path id-ind?)))))))
                           
;------------------------------------------------
; Functions which parse XPointer grammar
;
; The return value has the same signature for all these functions (and it
; coincides with the signature from the previous section)
; (list (lambda (nodeset root-node context var-binding id-index) ...)
;       path 
;       index-required )
; or #f
; #f signals of a parse error (error message is printed as a side effect
; during parsing)
;
; (lambda (nodeset root-node context var-binding id-index) - an SXPath-like 
; function (it transforms a nodeset into a new nodeset)
;  nodeset - a current set of nodes
;  root-node - the root of a document (a singleton nodeset)
;  context - the context of the node:  context = (cons position size)
;  position - context position (a number)
;  size - context size (a number)
;  id-index - is used for selecting an element by its unique ID
;  path - an XPointer path represented as the list of chars
;  index-required - boolean value: whether an id-index was required "deeper" 
; within the grammar


; Parses an FullXPtr production ([3]-[10] in XPointer specification)
; [3]    FullXPtr    ::=    XPtrPart (S? XPtrPart)* 
; [4]    XPtrPart    ::=    'xpointer' '(' XPtrExpr ')'
;                           | 'xmlns' '(' XPtrNsDecl? ')' 
;                           | Scheme '(' SchemeSpecificExpr ')' 
; [5]    Scheme    ::=    NCName 
; [6]    SchemeSpecificExpr    ::=    StringWithBalancedParens 
; [7]    StringWithBalancedParens    ::=
;                    [^()]* ('(' StringWithBalancedParens ')' [^()]*)*
; [8]    XPtrExpr    ::=    Expr
; [9]    XPtrNsDecl    ::=    NCName S? '=' S? XPtrNsURI 
; [10]    XPtrNsURI    ::=    Char*
(define (sxp:parse-full-xptr path ns-binding)
  (let loop ((funcs '())
             (ns-binding ns-binding)
             (path path)
             (index-required #f))
    (if (null? (sxp:skip-ws path))  ; the string is over
     (if (= (length funcs) 1)
      (list (car funcs) path index-required)
      (list (lambda (nodeset root-node context var-binding id-index)
         (let rpt ((fs (reverse funcs)))
           (if (null? fs)
              '()
              (let ((nset ((car fs) nodeset root-node context
                          var-binding id-index)))
                (if (null? nset)
                   (rpt (cdr fs))
                   nset)))))
       path
       index-required))
     (and-let* ((lst (sxp:parse-name path))
                (name (car lst))
                (path (cadr lst)))
          (cond
            ((string=? name "xpointer")
             (and-let* ((path (sxp:parse-assert "(" path))
                        (lst2 (sxp:parse-expr path ns-binding)))
                       (let ((func (car lst2))
                             (path (cadr lst2))
                             (bool (caddr lst2)))
                         (and-let* ((path (sxp:parse-assert ")" path)))
                              (loop (cons func funcs) ns-binding
                                    path (or index-required bool))))))
            ((string=? name "xmlns")
             (and-let* ((path0 (sxp:parse-assert "(" path))
                        (lst2 (sxp:parse-ncname path0))
                        (prefix (car lst2))                        
                        (path (sxp:parse-assert "=" (cadr lst2))))
                      (let rpt2 ((path (sxp:skip-ws path)) (uri '()))
                        (cond
                          ((null? path)
                           (sxp:parse-assert ")" path)
                           #f)
                          ((and (char=? (car path) #\)) (null? uri))
                           (sxp:xpointer-parse-error
                            "namespace URI cannot be empty"))
                          ((char=? (car path) #\))
                           (loop funcs 
                                 (cons 
                                  (cons prefix (list->string (reverse uri)))
                                  ns-binding)
                                 (cdr path)
                                 index-required))
                          (else (rpt2 (cdr path) (cons (car path) uri)))))))
            (else (and-let* ((path (sxp:parse-assert "(" path)))
                  (let rpt3 ((n 1) (path path))
                    (cond
                      ((= n 0)
                       (sxp:xpointer-parse-warning
                        "unknown xpointer schema - " name ". Ignoring")
                       (loop funcs ns-binding path index-required))
                      ((null? path)
                       (sxp:parse-assert ")" path)
                       #f)
                      ((char=? (car path) #\() (rpt3 (+ n 1) (cdr path)))
                      ((char=? (car path) #\)) (rpt3 (- n 1) (cdr path)))
                      (else (rpt3 n (cdr path))))))))))))

; Parses an ChildSeq production ([2] in XPointer specification)
; [2]    ChildSeq    ::=    Name? ('/' [1-9] [0-9]* )+ 
(define (sxp:parse-child-seq path)
       
  (define (helper path)
    (let loop ((funcs '()) (path path))
      (let ((path2 (sxp:parse-check "/" path)))
        (if (not path2)  ; no more #\/
           (if (null? (sxp:skip-ws path))  ; end of path
              (reverse funcs)
              (sxp:parse-assert "/" path))  ; this will cause an error message
           (and-let* ((lst (sxp:parse-natural path2)))
                (loop (cons 
                       (node-pos (car lst))
                       (cons (sxp:child (ntype?? '*)) funcs))
                      (cadr lst)))))))
  
  (let ((path2 (sxp:parse-check "/" path)))
    (if (not path2)
       (and-let* ((lst (sxp:parse-name path))
                  (name (car lst))
                  (funcs (helper (cadr lst))))
                 (list
                  (lambda (nodeset root-node context var-binding id-index)
                    (let ((nd (sxml:lookup name id-index)))
                      (if (not nd)
                          '()
                         (let rpt ((nset (list nd))
                                   (fs funcs))
                           (if (null? fs)
                              nset
                              (rpt ((car fs) nset) (cdr fs)))))))
                  '()
                  #t))
       (and-let* ((funcs (helper path)))
            (list (lambda (nodeset root-node context var-binding id-index)
               (if (nodeset? nodeset)
                  (let rpt ((nodeset nodeset) (res '()))
                    (if (null? nodeset)
                       res
                       (let rpt2 ((nset (list (car nodeset))) 
                                  (fs funcs))
                         (if (null? fs)
                            (rpt (cdr nodeset) (append res nset))
                            (rpt2 ((car fs) nset) (cdr fs))))))
                  (let rpt ((nodeset nodeset)
                            (fs funcs))
                      (if (null? fs)
                         nodeset
                         (rpt ((car fs) nodeset) (cdr fs))))))
             '()
             #f)))))
                                                                  
; Parses an XPointer production ([1] in XPointer specification)
; [1]    XPointer    ::=    Name | ChildSeq | FullXPtr 
(define (sxp:parse-xpointer path ns-binding)
  (if (sxp:parse-check "/" path)   ; #\/ is the first symbol => ChildSeq
    (sxp:parse-child-seq path)
    (and-let* ((lst (sxp:parse-name path))
	       (new-path (cadr lst)))
	      (if (sxp:parse-check "(" new-path)  ; FullXPtr production
		(sxp:parse-full-xptr path ns-binding)
		(sxp:parse-child-seq path)))))
      
;=========================================================================
; Highest level API functions

;------------------------------------------------
; 'sxp:xpath' and 'sxp:xpointer' functions
;
;  xpointer-string - an XPointer fragment identifier (a string)
;  ns-binding - declared namespace prefixes (an optional argument)
;  ns-binding = (list  (prefix . uri)
;                      (prefix . uri)
;                      ...)
;  prefix, uri - strings
;
; The returned result:   (lambda (node . id-index) ...)  
;                   or   #f
;  #f - signals of a parse error (error message is printed as a side effect
; during parsing) 
;  (lambda (node . id-index) ...)  - an SXPath-like function
;  node - a root node of the SXML document
;  id-index = (list  (id-value . element)
;                    (id-value . element)
;                    ...) 
; This index is used for selecting an element by its unique ID. 
; 'id-index' is an optional argument
; The default value of id-index will be an empty list

(define (sxp:xpointer xpointer-string . ns-binding)
  (and-let* ((res (sxp:parse-xpointer 
             (string->list xpointer-string)
             (if (null? ns-binding) ns-binding (car ns-binding))))
       (func (car res)))
         (lambda (node . id-index)
           (let((node (if (nodeset? node) node (list node)))
                (id-index (if (null? id-index) id-index (car id-index))))
             (func node node (cons 1 1) '() id-index)))))

    
(define (sxp:xpath xpath-string . ns-binding)
  (and-let* ((res (sxp:parse-location-path 
	       (string->list xpath-string)
	       (if (null? ns-binding) ns-binding (car ns-binding))))
	(func (car res))
	(path (sxp:skip-ws (cadr res))))
    (cond
      ((not (null? path))
       (sxp:xpointer-parse-error "unexpected symbol - " (car path)))
      (else (lambda (node . id-index)
	  (let ((node (if (nodeset? node) 
			node 
			(list node)))
		(id-index (if (null? id-index) 
			    id-index 
			    (car id-index))))
	    (func node node (cons 1 1) '() id-index)))))))

;------------------------------------------------
; 'sxp:xpath+index' and 'sxp:xpointer+index' functions
; 
;  xpointer-string - an XPointer fragment identifier (a string)
;  ns-binding - declared namespace prefixes (an optional argument)
;  ns-binding = (list  (prefix . uri)
;                      (prefix . uri)
;                      ...)
;  prefix, uri - strings
;
; The returned result:   (cons (lambda (node . id-index) ...)  
;                              index-required )
;                   or   #f
;  #f - signals of a parse error (error message is printed as a side effect
; during parsing) 
;  (lambda (node . id-index) ...)  - an SXPath-like function
;  node - a root node of the SXML document
;  id-index = (list  (id-value . element)
;                    (id-value . element)
;                    ...) 
; This index is used for selecting an element by its unique ID. 
; 'id-index' is an optional argument. It is not required when bool=#f. 
; The default value of id-index will be an empty list
;  index-required - a boolean value: whether an id-index is required
(define (sxp:xpointer+index xpointer-string . ns-binding)
  (and-let* 
    ((res (sxp:parse-xpointer 
	       (string->list xpointer-string)
	       (if (null? ns-binding) ns-binding (car ns-binding))))
      (func (car res)))
	(cons
	  (lambda (node . id-index)
	    (let ((node (if (nodeset? node) 
			  node 
			  (list node)))
		  (id-index (if (null? id-index) 
			      id-index 
			      (car id-index))))
	      (func node node (cons 1 1) '() id-index)))
	  (caddr res))))
            
(define (sxp:xpath+index xpath-string . ns-binding)
  (and-let* ((res (sxp:parse-location-path 
		    (string->list xpath-string)
		    (if (null? ns-binding) ns-binding (car ns-binding))))
	     (func (car res))
	     (path (sxp:skip-ws (cadr res))))
	    (cond
	      ((not (null? path))
	       (sxp:xpointer-parse-error "unexpected symbol - " (car path)))
	      (else (cons
		      (lambda (node . id-index)
			(let((node (if (nodeset? node) 
				     node 
				     (list node)))
			     (id-index (if (null? id-index) 
					 id-index 
					 (car id-index))))
			  (func node node (cons 1 1) '() id-index)))
		      (caddr res))))))

;------------------------------------------------
; 'sxp:xpath+root+vars' and 'sxp:xpointer+root+vars' functions
; 
;  xpointer-string - an XPointer fragment identifier (a string)
;  ns-binding - declared namespace prefixes (an optional argument)
;  ns-binding = (list  (prefix . uri)
;                      (prefix . uri)
;                      ...)
;  prefix, uri - strings
;
; The returned result:   (lambda (node root-node var-binding . id-index) ...)
;                   or   #f
;  #f - signals of a parse error (error message is printed as a side effect
; during parsing) 
;  (lambda (node root-node var-binding . id-index) ...)  - an SXPath-like
; function
;  node - a node (or a node-set) of the SXML document
;  root-node - the root of the document
;  var-binding = (list  (var-name . value)
;                       (var-name . value)
;                       ...)
;  var-name - (a string) a name of a variable
;  value - its value. The value can have the following type: boolean, number,
; string, nodeset. NOTE: a node must be represented as a singleton nodeset
;  id-index = (list  (id-value . element)
;                    (id-value . element)
;                    ...) 
; This index is used for selecting an element by its unique ID. 
; 'id-index' is an optional argument
; The default value of id-index will be an empty list
(define (sxp:xpointer+root+vars xpointer-string . ns-binding)
  (and-let* ((res (sxp:parse-xpointer 
	       (string->list xpointer-string)
	       (if (null? ns-binding) ns-binding (car ns-binding)))))
      (let ((func (car res)))
	(lambda (node root-node var-binding . id-index)
	  (let ((node (if (nodeset? node) node (list node)))
		(root-node (if (nodeset? root-node) 
			     root-node 
			     (list root-node)))
		(id-index (if (null? id-index) id-index (car id-index))))
	    (func node root-node (cons 1 1) var-binding id-index))))))       

;
(define (sxp:xpath+root+vars xpath-string . ns-binding)
  (and-let* 
    ((res (sxp:parse-location-path 
	    (string->list xpath-string)
	    (if (null? ns-binding) '() 
	      (car ns-binding)))))
    (let ((func (car res))
	  (path (sxp:skip-ws (cadr res))))
      (cond
	((not (null? path))
	 (sxp:xpointer-parse-error "unexpected symbol - " (car path)))
	(else   (lambda (node root-node var-binding . id-index)
		  (let ((node (if (nodeset? node) node (list node)))
			(root-node (if (nodeset? root-node)
				     root-node 
				     (list root-node)))
			(id-index (if (null? id-index) 
				    id-index 
				    (car id-index))))
		    (func node root-node (cons 1 1) var-binding id-index))))))))    

(define (sxp:xpath+root xpath-string . ns-binding)
  (and-let* 
    ((res (sxp:parse-location-path 
	    (string->list xpath-string)
	    (if (null? ns-binding) '() 
	      (car ns-binding)))))
    (let ((func (car res))
	  (path (sxp:skip-ws (cadr res))))
      (cond
	((not (null? path))
	 (sxp:xpointer-parse-error "unexpected symbol - " (car path)))
	(else   (lambda (node root-node  . id-index)
		  (let ((node (if (nodeset? node) node (list node)))
			(root-node (if (nodeset? root-node)
				     root-node 
				     (list root-node)))
			(id-index (if (null? id-index) 
				    id-index 
				    (car id-index))))
		    (func node root-node (cons 1 1) '() id-index))))))))    
