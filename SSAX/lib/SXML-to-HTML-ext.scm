;	   HTML Authoring in SXML for my personal Web pages
;
; The present file defines several functions and higher-order
; SXML "tags" that are used to compose HTML pages on my web site.
; In LaTeX terms, this file is similar to article.cls.
;
; See http://pobox.com/~oleg/ftp/Scheme/xml.html#XML-authoring
; for more examples and explanation.
;
; $Id$

(include "SXML-to-HTML.scm")

; Look up a value associated with a symbolic key in alist (key value)
; and return (value)
; If failed, write a warning and return the default value, if non-#f
; A lookup failure is fatal if the default value is #f
(define (lookup-def key alist default-value)
  (cond
   ((assq key alist) => cdr)
   (default-value
     (cerr "Failed to find a binding for a key " key
	   ". The default value " default-value " will be used")
     default-value)
   (else
    (error "Failed to find a binding for a key " key))))


; Procedure make-header HEAD-PARMS
; Create the 'head' SXML/HTML tag. HEAD-PARMS is an assoc list of
; (h-key h-value), where h-value is a typically string;
; h-key is a symbol:
; title, description, AuthorAddress, keywords,
; Date-Revision-yyyymmdd, Date-Creation-yyyymmdd,
; long-title
; One of the h-key can be Links.
; In that case, h-value is a list of
;	(l-key l-href (attr value) ...)
; where l-key is one of the following:
;	start, contents, prev, next, top, home

(define (make-header head-parms)
  `(head
    (title ,(car (lookup-def 'title head-parms #f)))
    ,(map
      (lambda (key)
	(let ((val (car (lookup-def key head-parms '(#f)))))
	  (and val
	       `(meta (@ (name ,(symbol->string key)) (content ,val))))))
      '(description AuthorAddress keywords
	Date-Revision-yyyymmdd Date-Creation-yyyymmdd))
    ,(let ((links (lookup-def 'Links head-parms '())))
      (and (pair? links)
	   (map
	    (lambda (link-key)
	      (let ((val (lookup-def link-key links '())))
		(and (pair? val)
		     `(link (@ (rel ,(symbol->string link-key))
			       (href ,(car val))
			       ,@(cdr val))))))
	    '(start contents prev next)))))
)

; Create a navigational bar. The argument head-parms is the same
; as the one passed to make-header. We're only concerned with the
; h-value Links
(define (make-navbar head-parms)
  (let ((links (lookup-def 'Links head-parms '()))
	(nav-labels '((prev . "previous")
		      (next . "next")
		      (contents . "contents")
		      (top . "top"))))
    (and (pair? links)
      `(div (@ (align "center") (class "navbar"))
	 ,(let loop ((nav-labels nav-labels) (first? #t))
	    (if (null? nav-labels) '()
		(let ((val (car
			    (lookup-def (caar nav-labels) links '(#f)))))
		  (if (not val)
		      (loop (cdr nav-labels) first?)
		      (cons
		       (list " " (if first? #f '(n_)) " "
			     `(a (@ (href ,val)) ,(cdar nav-labels)))
		       (loop (cdr nav-labels) #f))))))
	 (hr)))
))
			      

; Create a footer. The argument head-parms is the same
; as passed to make-header.
(define (make-footer head-parms)
  `((br)
    (p (hr))
    (h3 "Last updated "
	,(let* ((date-revised
		 (car (lookup-def 'Date-Revision-yyyymmdd head-parms #f)))
		(tstamp (OS:string->time "%Y%m%d" date-revised)))
	   (OS:cftime "%B %e, %Y" tstamp)))
    ,(let ((links (lookup-def 'Links head-parms '())))
       (and (pair? links)
	    (let ((home (car (lookup-def 'home links '(#f)))))
	      (and home
		   `(p "This site's top page is "
		       (a (@ (href ,home)) (strong ,home)))))))
    (p (address "oleg@pobox.com or oleg@acm.org  or oleg@computer.org"
       (br)
       "Your comments, problem reports, questions are very welcome!"))
    (p (font (@ (size "-2")) "Converted from SXML by SXML->HTML"))
    ,(let ((rcs-id (lookup-def 'rcs-id head-parms '())))
       (and (pair? rcs-id)
	    `(h4 ,rcs-id)))
    ))

; Bindings for the post-order function, which traverses the SXML tree
; and converts it to a tree of fragments

; The universal transformation from SXML to HTML. The following rules
; work for every HTML, present and future
(define universal-conversion-rules
  `((@
     ((*default*       ; local override for attributes
       . ,(lambda (attr-key . value) ((enattr attr-key) value))))
     . ,(lambda (trigger . value) (list '@ value)))
    (*default* . ,(lambda (tag . elems) (apply (entag tag) elems)))
    (*text* . ,(lambda (trigger str) 
		 (if (string? str) (string->goodHTML str) str)))
    (n_		; a non-breaking space
     . ,(lambda (tag . elems)
	  (list "&nbsp;" elems)))))

; A variation of universal-conversion-rules which keeps '<', '>', '&'
; and similar characters intact. The universal-protected-rules are
; useful when the tree of fragments has to be traversed one more time.
(define universal-protected-rules
  `((@
     ((*default*       ; local override for attributes
       . ,(lambda (attr-key . value) ((enattr attr-key) value))))
     . ,(lambda (trigger . value) (list '@ value)))
    (*default* . ,(lambda (tag . elems) (apply (entag tag) elems)))
    (*text* . ,(lambda (trigger str) 
		 str))
    (n_		; a non-breaking space
     . ,(lambda (tag . elems)
	  (list "&nbsp;" elems)))))

; The following rules define the identity transformation
(define alist-conv-rules
  `((*default* . ,(lambda (tag . elems) (cons tag elems)))
    (*text* . ,(lambda (trigger str) str))))


; Transformation rules to drop out everything but the 'Header' node
(define search-Header-rules
  `((Header
     ,alist-conv-rules
     . ,(lambda (tag . elems) (cons tag elems)))
    (*default*
     . ,(lambda (attr-key . elems)
	  (let loop ((elems elems))
	    (cond
	     ((null? elems) '())
	     ((not (pair? (car elems))) (loop (cdr elems)))
	     ((eq? 'Header (caar elems)) (car elems))
	     (else (loop (cdr elems)))))))
    (*text* . ,(lambda (trigger str) '()))))


; Transformation rules that define a number of higher-order tags,
; which give "style" to all my pages.

(define generic-web-rules
  (append
   universal-conversion-rules
   `((html:begin 
      . ,(lambda (tag . elems)
	   (list
	    "<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.0 Transitional//EN\""
	    "\"http://www.w3.org/TR/REC-html40/loose.dtd\">\n"
	    "<html>\n"
	    elems
	    "</html>\n")))

     (Header
      ,alist-conv-rules
      . ,(lambda (tag . headers)
	   (post-order (make-header headers) universal-conversion-rules)
	   ))

     (body
      . ,(lambda (tag . elems)
	   (list "<body bgcolor=\"#FFFFFF\">\n" elems "</body>")))

     (navbar			; Find the Header in the Content
      . ,(lambda (tag)		; and create the navigation bar
	   (let ((header-parms
		  (lookup-def 'Header
			      (list (post-order Content
						search-Header-rules))
			      #f)))
	     (post-order (make-navbar header-parms)
			 universal-conversion-rules))))
     
     (footer			; Find the Header in the Content
      . ,(lambda (tag)		; and create the footer of the page
	   (let ((header-parms
		  (lookup-def 'Header
			      (list (post-order Content
						search-Header-rules))
			      #f)))
	     (post-order (make-footer header-parms)
			 universal-conversion-rules))))
     
     (page-title		; Find the Header in the Content
      . ,(lambda (tag)		; and create the page title rule
	   (let ((header-parms
		  (lookup-def 'Header
			      (list (post-order Content
					  search-Header-rules))
			      #f)))
	     (list "<h1 align=center>" 
		   (lookup-def 'long-title header-parms #f) "</h1>\n"))))


     (Section	; (Section level "content ...")
      . ,(lambda (tag level head-word . elems)
	   (list "<br>&nbsp;<a name=\"" head-word "\">&nbsp;</a>\n"
		 "<h" level ">"  head-word elems "</h" level ">\n")))

     (TOC	; Re-scan the Content for "Section" tags and generate
      . ,(lambda (tag)	; the Hierarchical Table of contents
	   (let ((sections
		  (post-order Content
		    `((Section	; (Section level "content ...")
		       ((*text* . ,(lambda (tag str) str)))
		       . ,(lambda (tag level head-word . elems)
			    (vector level
				    (list "<li><a href=\"#" head-word
					  "\">" head-word elems "</a>\n" ))))
		      (*default*
		       . ,(lambda (attr-key . elems) elems))
		      (*text* . ,(lambda (trigger str) '()))))))
	     ;(write sections ##stderr)
	     (list "<div>"
	      (let loop ((curr-level 1) (sections sections))
	       (cond
		((null? sections)
		 (let fill ((curr-level curr-level))
		   (if (> curr-level 1)
		       (cons "</ol>\n" (fill (-- curr-level)))
		       '())))
		((null? (car sections)) (loop curr-level (cdr sections)))
		((pair? (car sections)) (loop curr-level
					      (append (car sections)
						      (cdr sections))))
		((vector? (car sections))
		 (let ((new-level (vector-ref (car sections) 0)))
		   (cond
		    ((= new-level curr-level)
		     (cons (vector-ref (car sections) 1)
			   (loop curr-level (cdr sections))))
		    ((= (++ new-level) curr-level)
		     (cons "</ol>\n"
			   (cons (vector-ref (car sections) 1)
				 (loop new-level (cdr sections)))))
		    ((= new-level (++ curr-level))
		     (cons "\n<ol>"
			   (cons (vector-ref (car sections) 1)
				 (loop new-level (cdr sections)))))
		    (else 
		     (error "inconsistent levels: " curr-level new-level)))))
		(else "wrong item: " sections)))
	      "</div>\n"))))

     (bibitem
      . ,(lambda (tag label key . text)
	   (list "\n<p><a name=\"" key "\">[" label "]</a> " text)))

     (cite		; ought to locate the label and use the label!
      . ,(lambda (tag key)
	   (list "[<a href=\"#" key "\">" key "</a>]")))


     (trace		; A debugging aid
      . ,(lambda (tag . content)
	   (cerr tag content nl)
	   '()))

     (URL 
      . ,(lambda (tag url)
	   (list "<br>&lt;<a href=\"" url "\">" url "</a>&gt;")))

)))
