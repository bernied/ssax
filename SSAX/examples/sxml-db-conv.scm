;      An example of an interesting (S)XML transformation
;
; This example emphasizes how higher-order transformers in SXSLT
; elegantly solve a class of problems that often occurs in practice:
; transforming an XML document so that different occurrences of an
; element yield different results depending on that element's
; ancestors, siblings, or descendants.  We also highlight the
; treatment of XML Namespaces. Our example is an extended version of
; generating rudimentary English phrases from a database. The original
; problem was used in
;	Shriram Krishnamurthi, Gray, K.E. and Graunke, P.T.:
;       Transformation-by-Example for XML.
;       Practical Aspects of Declarative Languages, 2000.
;
; to demonstrate XT3D: Given a database of purchase records, the goal
; is to create a purchase summary such that multiple purchases are
; separated by commas, except for the last two, which are separated by
; the word 'and'. The transformation involves the restructuring of the
; original markup and the insertion of item delimiters: commas and
; 'and'.  We also add to the top-level 'text' element an attribute
; 'count' with the count of the descendant text elements. See 'doc1'
; and 'doc2' as two sample documents. Run this code to see the answer
; and the intermediate results.
;
; The desired transformation is done by a pre-post-order iterator
; taking instructions from a stylesheet. The stylesheets instructs
; pre-post-order to visit the nodes of the tree in post-order, the
; order of call-by-value applications. This depth-first traversal mode
; fits the problem.
;
; A Bigloo Scheme system is assumed. We extensively rely on the
; match-case form provided by Bigloo.
;
; $Id$


(module sxml-db-conversion
	(include "myenv-bigloo.scm")
	(include "util.scm")
	(include "look-for-str.scm")
	(include "input-parse.scm")
	(include "SSAX-code.scm")
	(include "SXML-tree-trans.scm")
	)

; The following two definitions satisfy the import requirement of SSAX
(define (parser-error port message . specialising-msgs)
  (apply cerr (cons message specialising-msgs))
  (cerr nl)
  (exit 4))
(define (ssax:warn port message . specialising-msgs)
  (apply cerr (cons message specialising-msgs))
  (cerr nl))


; Sample documents:
(define doc1
        "<db:purchase xmlns:db='http://internal.com/db'>
          <html:p xmlns:html='http://www.w3c.org/HTML/'>4 tinkers</html:p>
        </db:purchase>"
)
; The expected output is
;         <out:text acc:count='0'
;              xmlns:out='http://internal.com/out'
;              xmlns:acc='http://internal.com/accounting'
;         >4 tinkers</out:text> 

(define doc2
	"<db:purchase xmlns:db='http://internal.com/db'
             xmlns:html='http://www.w3c.org/HTML/'>
	   <html:p>4 tinkers</html:p>
	   <html:p>5 tailors</html:p>
	   <html:p>2 soldiers</html:p>
	   <html:p>1 spy</html:p>
	</db:purchase>")

; The tranlator, from a source XML document to the transformed document
; in the SXML form.
(define (convert-db doc-xml)
  (let ((doc 
	 ; The first step of the transformation is parsing the source
	 ; document into an abstract syntax tree (SXML) form. Using a SSAX XML
	 ; parser [SSAX], this step can be accomplished as follows.
	 ; NB: we instruct the parser to represent http://www.w3c.org/HTML/
	 ; by a ns-user-shortcut 'h'. See the SXML Specification
	 ; for a more detailed discussion of the namespaces in SXML.
	 (call-with-input-string doc-xml
	   (lambda (port) (ssax:xml->sxml port
			       '((h . "http://www.w3c.org/HTML/")))))))
    (cout nl 
     ">>>The following is the sample document to transform: " nl
     doc-xml nl nl
     ">> and its SXML form:" nl)
    (pp doc)

  (pre-post-order doc
   ; the conversion stylesheet
   `((h:p . ,(lambda (tag str)
       (lambda (count . args)  ; count can be #f: don't generate the attr
        (let ((att-list 
               (if count `((@ (acc:count ,count))) '())))
          (match-case args
           (() `(out:text ,@att-list ,str))
           ((?arg)
            `(out:text ,@att-list
               ,(string-append str " and")
               ,(arg #f)))
           ((?arg . ?rest)
            `(out:text ,@att-list ,(string-append str ",")
                   ,(apply arg (cons #f rest)))))))))

     (http://internal.com/db:purchase . ,(lambda (tag . procs)
	(if (null? procs) '()
	    (apply (car procs) (cons (length (cdr procs)) (cdr procs))))))
     (*text* . ,(lambda (trigger str) str))
     (*TOP* . ,(lambda x x))
     (@@ . ,(lambda x x))
     (*NAMESPACES* *preorder*
		   ; Replace namespace declarations of the source document
		   ; with the namespace declarations of the target document.
		   ; The target document uses two namespace-ids:
		   ; 'out:' and 'acc:'
	. ,(lambda (trigger ns)
	     `(,trigger (out "http://internal.com/out")
			(acc "http://internal.com/accounting"))))
     )))
)

; pretty-print the resulting SXML into XML, emitting the appropriate
; xmlns:xxx attributes
(define (SXML->XML sxml)
  (define this-ss
    `((@
      ((*default*       ; local override for attributes
        . ,(lambda (attr-key . value) ((enattr attr-key) value))))
      . ,(lambda (trigger . value) (list '@ value)))
     (*default* . ,(lambda (tag . elems) (apply (entag tag) elems)))
     (*text* . ,(lambda (trigger str) 
		  (if (string? str) (string->goodXML str) str)))
     (*TOP*       ; check for the namespaces and add xmlns:xxx attributes
      *preorder*  ; to the root element
      . ,(lambda (tag . elems)
	   (define (make-xmlns ns-assoc)
	     (list (string->symbol
		    (string-append "xmlns:" (symbol->string (car ns-assoc))))
		   (cadr ns-assoc)))
	   (pre-post-order
	    (match-case elems
			; the root element had its own attributes, add xmlns:
	      (((@@ ??- (*NAMESPACES* . ?ns) . ?_)
		 (?rootname (@ . ?attrs) . ?children))
	       `(,rootname (@ ,@(map make-xmlns ns) . ,attrs) . ,children))
			; the root element had no attr list: make one
	      (((@@ ??- (*NAMESPACES* . ?ns) . ?-)
		 (?rootname . ?children))
	      `(,rootname (@ . ,(map make-xmlns ns)) . ,children))
	      (else elems))
	    this-ss)))
     ))
 (SRV:send-reply
  (pre-post-order sxml this-ss)
  ))

(define (entag tag)
  (lambda elems
    (match-case elems
     (((@ . ?attrs) . ?children)
      (list #\< tag attrs 
	    (if (null? children) "/>"
		(list #\> children "</" tag #\>))))
     (() (list #\< tag "/>"))
     (else
      (list #\< tag #\> elems "</" tag #\>)))))

(define (enattr attr-key)
  (lambda (value)
    (if (null? value) (list #\space attr-key "='" attr-key "'")
        (list #\space attr-key "=\'" value #\'))))


; Given a string, check to make sure it does not contain characters
; such as '<' or '&' that require encoding. Return either the original
; string, or a list of string fragments with special characters
; replaced by appropriate character entities.

(define string->goodXML
  (make-char-quotator
   '((#\< . "&lt;") (#\> . "&gt;") (#\& . "&amp;") (#\" . "&quot;")
     (#\' . "&apos;"))))


(define (do-convert doc)
  (let ((transf-sxml (convert-db doc)))
    (cout nl ">>>The transformed SXML tree is as follows" nl)
    (pp transf-sxml)
    (cout nl ">>>Here is the result of the pretty printing of the transformed "
	  "SXML tree into XML" nl nl)
    (SXML->XML transf-sxml)
    (cout nl nl "====================================" nl)
    ))

(do-convert doc1)
(newline)
(do-convert doc2)
(cout nl nl "Done" nl)

