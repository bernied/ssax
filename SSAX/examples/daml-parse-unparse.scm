;      Pretty-printing from SXML to XML
;
; $Id$


(module sxml-to-xml
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
(define (SSAX:warn port message . specialising-msgs)
  (apply cerr (cons message specialising-msgs))
  (cerr nl))

; The file is downloaded from
; http://www.daml.org/services/daml-s/0.7/CongoProfile.daml
;(define daml-file "/tmp/aa.xml")
(define daml-file "xml/CongoProfile.daml")

; Internal entities declared in that file
(define DAML-entities
  '((rdf . "http://www.w3.org/1999/02/22-rdf-syntax-ns")
    (rdfs . "http://www.w3.org/2000/01/rdf-schema")
    (daml . "http://www.daml.org/2001/03/daml+oil")
    (process . "http://www.daml.org/services/daml-s/0.7/Process.daml")
    (service . "http://www.daml.org/services/daml-s/0.7/Service.daml")
    (profile . "http://www.daml.org/services/daml-s/0.7/Profile.daml")
    (profileTaxonomy . 
	"http://www.daml.org/services/daml-s/0.7/ProfileTaxonomy.daml")
    (country . "http://www.daml.ri.cmu.edu/ont/Country.daml")
    (concepts . "http://www.daml.ri.cmu.edu/ont/DAML-S/concepts.daml")
    (congoService . 
		  "http://www.daml.org/services/daml-s/0.7/CongoService.daml")
    (congoProcess . 
		  "http://www.daml.org/services/daml-s/0.7/CongoProcess.daml")
    (time . "http://www.ai.sri.com/daml/ontologies/time/Time.daml")
    (xsd . "http://www.w3.org/2000/10/XMLSchema.xsd")
    (DEFAULT . "http://www.daml.org/services/daml-s/0.7/CongoProfile.daml")
    ))

; The following is an instantiation variation of the SSAX parser
; to handle specified internal parsed entities.
; This code is identical to SSAX:XML->SXML except one line in
; the DOCTYPE handler:
;   (values #f DAML-entities namespaces seed)
; (The original SSAX:XML->SXML had '() in place of DAML-entities)
;
(define (SSAX:DAML->SXML port namespace-prefix-assig)
  (letrec
      ((namespaces
	(map (lambda (el)
	       (cons* #f (car el) (SSAX:uri-string->symbol (cdr el))))
	     namespace-prefix-assig))

       (RES-NAME->SXML
	(lambda (res-name)
	  (string->symbol
	   (string-append
	    (symbol->string (car res-name))
	    ":"
	    (symbol->string (cdr res-name))))))

       ; given the list of fragments (some of which are text strings)
       ; reverse the list and concatenate adjacent text strings
       (reverse-collect-str
	(lambda (fragments)
	  (if (null? fragments) '()	; a shortcut
	      (let loop ((fragments fragments) (result '()) (strs '()))
		(cond
		 ((null? fragments)
		  (if (null? strs) result
		      (cons (apply string-append strs) result)))
		 ((string? (car fragments))
		  (loop (cdr fragments) result (cons (car fragments) strs)))
		 (else
		  (loop (cdr fragments)
			(cons
			 (car fragments)
			 (if (null? strs) result
			     (cons (apply string-append strs) result)))
			'())))))))

       ; given the list of fragments (some of which are text strings)
       ; reverse the list and concatenate adjacent text strings
       ; We also drop "unsignificant" whitespace, that is, whitespace
       ; in front, behind and between elements. The whitespace that
       ; is included in character data is not affected.
       (reverse-collect-str-drop-ws
	(lambda (fragments)
	  (cond 
	   ((null? fragments) '())		; a shortcut
	   ((and (string? (car fragments))	; another shortcut
		 (null? (cdr fragments))	; remove trailing ws
		 (string-whitespace? (car fragments))) '())
	   (else
	    (let loop ((fragments fragments) (result '()) (strs '())
		       (all-whitespace? #t))
	      (cond
	       ((null? fragments)
		(if all-whitespace? result	; remove leading ws
		    (cons (apply string-append strs) result)))
	       ((string? (car fragments))
		(loop (cdr fragments) result (cons (car fragments) strs)
		      (and all-whitespace?
			   (string-whitespace? (car fragments)))))
	       (else
		(loop (cdr fragments)
		      (cons
		       (car fragments)
		       (if all-whitespace? result
			   (cons (apply string-append strs) result)))
		      '() #t))))))))
       )
    (let ((result
	   (reverse
	    ((SSAX:make-parser
	     NEW-LEVEL-SEED 
	     (lambda (elem-gi attributes namespaces
			      expected-content seed)
	       '())
   
	     FINISH-ELEMENT
	     (lambda (elem-gi attributes namespaces parent-seed seed)
	       (let ((seed (reverse-collect-str-drop-ws seed))
		     (attrs
		      (attlist-fold
		       (lambda (attr accum)
			 (cons (list 
				(if (symbol? (car attr)) (car attr)
				    (RES-NAME->SXML (car attr)))
				(cdr attr)) accum))
		       '() attributes)))
		 (cons
		  (cons 
		   (if (symbol? elem-gi) elem-gi
		       (RES-NAME->SXML elem-gi))
		   (if (null? attrs) seed
		       (cons (cons '@ attrs) seed)))
		  parent-seed)))

	     CHAR-DATA-HANDLER
	     (lambda (string1 string2 seed)
	       (if (string-null? string2) (cons string1 seed)
		   (cons* string2 string1 seed)))

	     DOCTYPE
	     (lambda (port docname systemid internal-subset? seed)
	       (when internal-subset?
		     (SSAX:warn port
			   "Internal DTD subset is not currently handled ")
		     (SSAX:skip-internal-dtd port))
	       (SSAX:warn port "DOCTYPE DECL " docname " "
		     systemid " found and skipped")
	       (values #f DAML-entities namespaces seed))

	     UNDECL-ROOT
	     (lambda (elem-gi seed)
	       (values #f '() namespaces seed))

	     PI
	     ((*DEFAULT* .
		(lambda (port pi-tag seed)
		  (cons
		   (list '*PI* pi-tag (SSAX:read-pi-body-as-string port))
		   seed))))
	     )
	    port '()))))
      (cons '*TOP*
	    (if (null? namespace-prefix-assig) result
		(cons (cons '*NAMESPACES* 
			    (map (lambda (ns) (list (car ns) (cdr ns)))
				 namespace-prefix-assig))
		      result)))
)))

(define daml-sxml
  (call-with-input-file daml-file
     (lambda (port) (SSAX:DAML->SXML port 
	 '((rdfs . "http://www.w3.org/2000/01/rdf-schema#")
	  (rdf . "http://www.w3.org/1999/02/22-rdf-syntax-ns#")
	  (daml . "http://www.daml.org/2001/03/daml+oil#")
	  (dc . "http://purl.org/dc/elements/1.1/")
	  (log . "http://www.w3.org/2000/10/swap/log#")
	  (service . "http://www.daml.org/services/daml-s/0.7/Service.daml")
	  (grounding . "http://www.daml.org/services/daml-s/0.7/Grounding.daml")
	  (congo_service . "http://www.daml.org/services/daml-s/0.7/CongoService.daml")
	  (congo_profile . "http://www.daml.org/services/daml-s/0.7/CongoProfile.daml")
	  (congo_process . "http://www.daml.org/services/daml-s/0.7/CongoProcess.daml")
	  (congo_grounding . "http://www.daml.org/services/daml-s/0.7/CongoGrounding.daml")
	  (congo_wsdl_grounding . "http://www.daml.org/services/daml-s/0.7/CongoGrounding.wsdl"))
	 ))))

(cout nl "Parsed result" nl)
(pp daml-sxml)
(newline)


; pretty-print the resulting SXML into XML, emitting the appropriate
; xmlns:xxx attributes
(define (SXML->XML sxml)
  ; Split the GE (entity name) into the namespace part and the local part
  ; Return both as strings
  ; If GE has no namespace part, return #f for the namespace part
  (define (split-ge ge)
    (let* ((ge-str (symbol->string ge))
	   (last-colon (string-rindex ge-str #\:))
	   )
      (if last-colon ; GE is indeed an extended name
	  (values
	   (substring ge-str 0 last-colon)
	   (substring ge-str (++ last-colon) (string-length ge-str)))
	  (values #f ge-str))))
  (define (make-symbol . pieces)
    (string->symbol (apply string-append pieces)))

  ; Find out the list of all namespaces 
  ; design the translation from SXML names to XML names
  ; Return both values.
  ; The list of all namespaces is a list of
  ;	(ns-id NS-URI prefix-str)
  ; The translation is an assoc list, a list of
  ;     (sxml-name prefix-str:local-name-str)
  ; A namespace association is converted into (ns-id NS-URI prefix)
  ; by setting prefix to be ns-id.
  ; If we come across an sxml name of the form ns-id1:local-name
  ; we check if we already know the translation for the name,
  ; if ns-id1 exists in the current list of namespaces,
  ; if ns-id1 is a NS-URI in the current list of namespaces.
  ; Otherwise, we add a new association to the list of namespaces
  ;    (ns-id1 ns-id1-string prefix1)
  ; where prefix1 is derived from ns-id1. We make it unique.

  (define (ns-normalize sxml)
    (let loop ((node sxml) (todo '())
	       (namespaces '()) (translation '()))
      (match-case node
        (()
	 (if (null? todo)
	     (values namespaces translation)  ; we're done
	     (loop (car todo) (cdr todo) namespaces translation)))
	((*NAMESPACES* . ?ns)
	 (loop '() todo
	       (append namespaces
		  (map
		   (lambda (ns-assoc)
		     (let ((ns-id (car ns-assoc))
			   (uri   (cadr ns-assoc))
			   )
		       (list ns-id uri (symbol->string ns-id))))
		   ns))
	      translation))
	((*PI* . ?-) (loop '() todo namespaces translation))
	(((and (? symbol?) ?ge) . ?children)  ; regular node
	 (if (assq ge translation)  ; if we have seen ge
	     (loop children todo namespaces translation)
	     (let-values*
	      (((ns-part local-name) (split-ge ge))
	       (ns-symbol (and ns-part (string->symbol ns-part))))
	      (cond
	       ((not ns-part) ; ge is a local, non-expanded name
		(loop children todo namespaces translation))
	       ((let find-loop ((ns-assocs namespaces))
		  (cond
		   ((null? ns-assocs) #f)
		   ((eq? (caar ns-assocs) ns-symbol) (car ns-assocs))
		   ((equal? (cadar ns-assocs) ns-part) (car ns-assocs))
		   (else (find-loop (cdr ns-assocs)))))
		=>
		(lambda (seen-assoc) ; add a new translation
		  (loop children todo
			namespaces
			(cons
			 (list ge (make-symbol
				   (caddr seen-assoc) ":" local-name))
			 translation))))
	       (else
		(let 
		    ((prefix (symbol->string (gensym)))) ; choose gensym
		  (loop 
		   children todo
		   (cons
		    (list ns-symbol ns-part prefix) namespaces)
		   (cons
		    (list ge (make-symbol prefix ":" local-name))
		    translation))))))))
	((?hd . ?tl)	; list of nodes, break them up
	 (loop hd (append tl todo) namespaces translation))
	(else
	 (loop '() todo namespaces translation))
	)))

  (define (this-ss namespaces translation)
    (define (translate name)
      (cond
       ((assq name translation) => cadr)
       (else name)))
    (define ns-attrs
      (map
       (lambda (ns-assoc)
	 (list (make-symbol "xmlns:" (caddr ns-assoc))
	       (cadr ns-assoc)))
       namespaces))
;     (pp namespaces)
;     (newline)
;     (pp translation)
;     (newline)
    `((@
      ((*default*       ; local override for attributes
        . ,(lambda (attr-key . value) ((enattr (translate attr-key)) value))))
      . ,(lambda (trigger . value) (list '@ value)))
     (*default* . ,(lambda (tag . elems) (apply 
					  (entag (translate tag)) elems)))
     (*text* . ,(lambda (trigger str) 
		  (if (string? str) (string->goodXML str) str)))
     (*NAMESPACES* *preorder* . ,(lambda _ '())) ; handled already
     (*PI*
      *preorder*
      . ,(lambda (tag target body)
	   (list "<?" target " " body "?>")))
     (*TOP*       ; check for the namespaces and add xmlns:xxx attributes
      *preorder*  ; to the root element
      . ,(lambda (tag . elems)
	   (let-values*
	    (((pis root-elem)
	      (let ((elems-rev (reverse elems)))
		(values (reverse (cdr elems-rev)) (car elems-rev))))
	     )
	    (pre-post-order
	     (match-case root-elem
			; the root element had its own attributes, add xmlns:
	      ((?rootname (@ . ?attrs) . ?children)
	       (list pis
		 `(,rootname 
		   (@ ,@ns-attrs . ,attrs) . ,children)))
			; the root element had no attr list: make one
	      ((?rootname . ?children)
	       (list pis
		     `(,rootname
		       (@ . ,ns-attrs) . ,children)))
	      (else (error "shouldn't happen")))
	    (this-ss namespaces translation)))))
     ))
 (SRV:send-reply
  (pre-post-order sxml
		  (call-with-values (lambda () (ns-normalize sxml)) this-ss)
  )))

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


(cout nl ">>>Here is the result of the pretty printing of the transformed "
      "SXML tree into XML" nl nl)
(SXML->XML daml-sxml)
(cout nl nl "====================================" nl)

(cout nl nl "Done" nl)

