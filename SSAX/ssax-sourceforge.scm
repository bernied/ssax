; Evaluation of this file yields an HTML document
; $Id$

(define Content
'(html:begin
  (Header
   (title "XML and Scheme")
   (description "Representing, authoring, querying and transforming
markup data in Scheme; XML notation for a programming language")
   (Date-Revision-yyyymmdd "20021201")
   (Date-Creation-yyyymmdd "20010706")
   (keywords "XML, XML parsing, XML Infoset, XPath, XSLT, SAX, SXML, SXSLT, Scheme, HTML composition, HTML authoring")
   (AuthorAddress "oleg@okmij.org")
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
entities, attribute value normalization, processing
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

   (p "Related to SSAX are SXPath queries and SXML transformations (SXSLT).")

   (Section 3 "Availability")
   (p "The current version of SSAX is 4.9. The whole SSAX code is in public domain.")
   
   "SSAX has been tested on the following Scheme systems:"
   (br)
   "PLT Scheme (versions 103 and 200), Bigloo 2.4b, GambitC 3.0,
Chicken, Guile, SCM 5d2, MIT Scheme 7.5.2, Gauche 0.4.12."


   (Section 3 "Documentation")
   (p "Main SSAX/SXML page:"
      (URL "http://pobox.com/~oleg/ftp/Scheme/xml.html"))

   (p
    "Detailed introduction, motivation and real-life case-studies of SSAX, SXML, SXPath and SXSLT: the paper and the complementary talk presented at the International Lisp Conference 2002."
    (URL "http://pobox.com/~oleg/ftp/papers/SXs.pdf")
    (URL "http://pobox.com/~oleg/ftp/papers/SXs-talk.pdf")
    )

   (p
    "The derivation of the SSAX API and the comparison of SSAX with other
functional-style XML parsers and with the Expat were the topic of a
presentation at PADL2002, the 4th International Symposium on Practical
Aspects of Declarative Languages. The presentation (slides + the
transcript) is available online:"
    (URL "http://pobox.com/~oleg/ftp/papers/XML-parsing-talk.ps.gz"))

   (p "SSAX-SXML Mailing list:"
      (URL "http://lists.sourceforge.net/lists/listinfo/ssax-sxml"))

   (p "SSAX project summary page at SourceForge:"
      (URL "http://sourceforge.net/projects/ssax"))


   (Section 3 "SSAX distributions")
   (p "SSAX download site at SourceForge:"
      (URL "http://sf.net/project/showfiles.php?group_id=30687"))

   (p
    "The SSAX distributions for particular Scheme systems (PLT Scheme, Bigloo, Chicken, Guile, etc.) are created by Kirill Lisovsky, whose efforts are greatly appreciated. These distributions are also available from his web site " 
      (URL "http://pair.com/lisovsky/sxml/ssax/")
      (br)
      "Kirill's web site has detailed instructions for compiling SSAX on different platforms. In particular, you can compile SSAX into " (em "Java bytecode") ", with a Bigloo compiler.")
   (p
    (a (@ (href
	   "http://cvs.sourceforge.net/cgi-bin/viewcvs.cgi/gauche/SXML/"))
       "A Gauche distribution of SSAX") " is maintained by Shiro Kawai.")

  (Section 3 "SSAX CVS Tree")
  (p (a (@ (href "http://sourceforge.net/cvs/?group_id=30687"))
	"The CVS Tree")
     " includes the complete SSAX/SXML code, some
documentation, validation tests, as well as several sample applications.")
  (p "You can "
     (a (@ (href "http://cvs.sourceforge.net/cgi-bin/viewcvs.cgi/ssax"))
	"browse the files in the CVS tree")
     " from any web browser.")

;    (dl

;     (dt (a (@ (href  "http://sourceforge.net/tracker/?group_id=30687"))
; 	   "Trackers"))
;     (dd "You can use a tracker to make a suggestion, to request a
; feature, or to report a problem.")

;     (dt (a (@ (href "http://sourceforge.net/forum/?group_id=30687"))
; 	   "Forums"))
;     (dd "You can browse, search, and post messages related to SSAX and SXML"
; 	(br) (n_))

;     (dt (a (@ (href "http://sourceforge.net/docman/?group_id=30687"))
; 	   "Doc Manager"))
;     (dt (a (@ (href "http://sourceforge.net/pm/?group_id=30687"))
; 	   "Task Manager"))
;     (dt (a (@ (href "http://sourceforge.net/survey/?group_id=30687"))
; 	   "Surveys"))
;     )

   (footer)

)))

;(pp Content)

;========================================================================
;			HTML generation

; IMPORT
; SXML-to-HTML-ext.scm and all of its imports


; Generating HTML

(define (generate-HTML Content)
 (SRV:send-reply
  (pre-post-order Content
   (generic-web-rules Content '()))))

(generate-HTML Content)
