; Evaluation of this file yields an HTML document
; $Id$

(define Content
'(html:begin
  (Header
   (title "XML and Scheme")
   (description "Representing, authoring, querying and transforming
markup data in Scheme; XML notation for a programming language")
   (Date-Revision-yyyymmdd "20011012")
   (Date-Creation-yyyymmdd "20010706")
   (keywords "XML, XML parsing, XML Infoset, XPath, SAX, SXML, XSLT, Scheme, HTML composition, HTML authoring")
   (AuthorAddress "oleg@pobox.com")
   (long-title "SSAX and SXML at SourceForge")
   )

  (body
   (page-title)

   (a (@ (href "http://sourceforge.net")) " "
      (img (@ (src "http://sourceforge.net/sflogo.php?group_id=30687")
	      (width "88") (height "31") (border "0")
	      (alt "SourceForge Logo"))))

   (p
    "A SSAX functional XML parsing framework consists of a DOM/SXML
parser, a SAX parser, and a supporting library of lexing and parsing
procedures. The procedures in the package can be used separately to
tokenize or parse various pieces of XML documents. The framework
supports XML Namespaces, character, internal and external parsed
entities, xml:space, attribute value normalization, processing
instructions and CDATA sections.  The package includes a
semi-validating " (em "SXML parser") ": a DOM-mode parser that is an
instantiation of a SAX parser (called SSAX).")

   (p "SSAX is a full-featured, algorithmically optimal,
pure-functional parser, which can act as a stream processor. SSAX is
an efficient SAX parser that is " (em "easy to use") ". SSAX minimizes
the amount of application-specific state that has to be shared among
user-supplied event handlers. SSAX makes the maintenance of an
application-specific element stack unnecessary, which eliminates
several classes of common bugs. SSAX is written in a pure-functional
subset of Scheme. Therefore, the event handlers are referentially
transparent, which makes them easier for a programmer to write and to
reason about.  The more expressive, reliable and easier to use
application interface for the event-driven XML parsing is the outcome
of implementing the parsing engine as an enhanced tree fold
combinator, which fully captures the control pattern of the
depth-first tree traversal.")


   (p "Related to SSAX are SXPath queries and SXML transformations.")

   (p "Main SSAX/SXML page:"
      (URL "http://pobox.com/~oleg/ftp/Scheme/xml.html"))

   (p "SSAX project summary page at SourceForge"
      (URL "http://sourceforge.net/projects/ssax"))

   (dl

    (dt (a (@ (href "http://sf.net/project/showfiles.php?group_id=30687"))
	   "SSAX distributions"))
    (dd "The SSAX distributions for particular Scheme systems (PLT Scheme, Bigloo, Chicken, Guile, etc.) are created by Kirill Lisovsky, whose efforts are greatly appreciated. These distributions are also available from his web site " 
	(URL "http://pair.com/lisovsky/sxml/ssax/")
	(p "Kirill's web site has detailed instructions for compiling SSAX on different platforms. In particular, you can compile SSAX into " (em "Java bytecode") ", with a Bigloo compiler."))

    (dt (a (@ (href "http://lists.sourceforge.net/lists/listinfo/ssax-sxml"))
	   "SSAX-SXML Mailing list") (br) (n_))

    (dt (a (@ (href "http://sourceforge.net/cvs/?group_id=30687"))
	   "CVS Tree"))
    (dd "The CVS tree includes the complete SSAX/SXML code, some
documentation, validation tests, as well as several sample applications."
	(p "You can "
	   (a (@ (href "http://cvs.sourceforge.net/cgi-bin/viewcvs.cgi/ssax"))
	      "browse the files in the CVS tree")
	   " from any web browser."))


    (dt (a (@ (href  "http://sourceforge.net/tracker/?group_id=30687"))
	   "Trackers"))
    (dd "You can use a tracker to make a suggestion, to request a
feature, or to report a problem.")

    (dt (a (@ (href "http://sourceforge.net/forum/?group_id=30687"))
	   "Forums"))
    (dd "You can browse, search, and post messages related to SSAX and SXML"
	(br) (n_))

    (dt (a (@ (href "http://sourceforge.net/docman/?group_id=30687"))
	   "Doc Manager"))
    (dt (a (@ (href "http://sourceforge.net/pm/?group_id=30687"))
	   "Task Manager"))
    (dt (a (@ (href "http://sourceforge.net/survey/?group_id=30687"))
	   "Surveys"))
    )

   (footer)

)))

;(pp Content)

;========================================================================
;			HTML generation

(include "SXML-to-HTML-ext.scm")


(let* ((args (argv))
       (this-script (if (equal? "gsi" (car args)) (cadr args) (car args))))
  (OS:chdir (##path-directory this-script)))

; Generating HTML

(define (generate-HTML Content)
 (SRV:send-reply
  (post-order Content
   `(
     ,@generic-web-rules


     (verbatim	; set off pieces of code: one or several lines
      . ,(lambda (tag . lines)
	   (list "<pre>"
		 (map (lambda (line) (list "     " line "\n"))
		      lines)
		 "</pre>")))
		; (note . text-strings)
		; A note (remark), similar to a footnote
     (note
      . ,(lambda (tag . text-strings)
	   (list " <font size=\"-1\">[" text-strings "]</font>\n")))

		; A reference to a file
     (fileref
      . ,(lambda (tag pathname . descr-text)
	   (list "<a href=\"" pathname "\">"
		 (car (reverse (string-split pathname '(#\/))))
		 "</a> [" 
		 (let ((file-size (OS:file-length pathname)))
		   (if (not (positive? file-size))
		       (error "File not found: " pathname))
		   (cond
		    ((< file-size 1024) "&lt;1K")
		    (else (list (quotient (+ file-size 1023) 1024) "K"))))
		 "]<br>"
		 descr-text)))

		; A reference to a plain-text file (article)
     (textref
       . ,(lambda (tag pathname title . descr)
	    (let ((file-size (OS:file-length pathname)))
	      (if (not (positive? file-size))
		  (error "File not found: " pathname))
	      (list "<a href=\"" pathname "\">" title
		    "</a> <font size=\"-1\">[plain text file]</font><br>\n"
		    descr))))

		; Unit of a description for a piece of code
		; (Description-unit key title . elems)
		; where elems is one of the following:
		; headline, body, platforms, version
     (Description-unit
      ((headline
	. ,(lambda (tag . elems)
	     (list "<dt>" elems "</dt>\n")))
       (body
	. ,(lambda (tag . elems)
	     (list "<dd>" elems "<br>&nbsp;</dd>\n")))
       (platforms
	. ,(lambda (tag . elems)
	     (list "<dt><strong>Platforms</strong><dt><dd>"
		   elems "</dd>\n")))
       (version
	. ,(lambda (tag . elems)
	     (list "<dt><strong>Version</strong><dt><dd>"
		   "The current version is " elems ".</dd>\n")))
       (references
	. ,(lambda (tag . elems)
	     (list "<dt><strong>References</strong><dt><dd>"
		   elems "</dd>\n")))
       )
      . ,(lambda (tag key title . elems)
	   (post-order
	    `((a (@ (name ,key)) (n_))
	      (h2 ,title)
	      (dl (insert-elems))
	      )
	    `(,@universal-conversion-rules
	      (insert-elems
	       . ,(lambda (tag) elems))))))
))))

(generate-HTML Content)

