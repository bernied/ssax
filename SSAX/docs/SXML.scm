; Evaluation of this file yields an HTML document
; $Id$

(define Content '(
 (html:begin
  (Header
   (title "SXML")
   (description "Definition of SXML: an instance of XML Infoset as
S-expressions, an Abstract Syntax Tree of an XML document.")
   (Date-Revision-yyyymmdd "20010610")
   (Date-Creation-yyyymmdd "20010207")
   (keywords "XML, XML parsing, XML Infoset, XPath, SXML, Scheme")
   (AuthorAddress "oleg@pobox.com")
   (long-title "SXML")
   (Links
    (start "index.html" (title "Scheme Hash"))
    (contents "../README.html")
    (prev "xml.html")
    (next "web.html")
    (top "index.html")
    (home "http://pobox.com/~oleg/ftp/")))

  (body
   (navbar)
   (page-title)
   (p "SXML is an instance of XML Infoset as S-expressions. SXML is an
Abstract Syntax Tree of an XML document.")
   (p (b "Revision: 2.0"))

   (TOC)

   (Section 2 "Introduction")
   (p
    "An XML information set (Infoset) is an abstract data set that
describes information available in a well-formed XML document. 
Infoset is made of \"information items\", which denote components of
the document: elements, attributes, character data, processing
instructions, etc. Each information item has a number of associated
properties, e.g., name, namespace URI. Some properties -- for example,
'children' and 'attributes' -- are (ordered) collections of other
information items. Infoset describes only the information in an XML
document that is relevant to applications. Element declarations from
DTD, XML version, parameter entities, etc. data used merely for
parsing or validation are not included. XML Infoset is described in "
    (cite "XML Infoset") ". Although technically Infoset is specified for
XML, it largely applies to HTML as well.")
   (p
    "SXML is a concrete instance of the XML Infoset. Infoset's goal is
to present in some form all relevant pieces of data and their " (em
"abstract") ", container-slot relationships to each other.  SXML gives
the nest of containers a concrete implementation as S-expressions, and
provides means of accessing items and their properties. SXML is a
\"relative\" of XPath and DOM, whose data models are two other
instances of the XML Infoset. SXML is particularly suitable for
Scheme-based XML/HTML authoring, SXPath queries, and tree
transformations. In John Hughes' terminology, SXML is a term
implementation of evaluation of the XML document.")

   (Section 2 "Notation")
   (p
    "We will use both an Extended BNF Notation (EBNF) employed in the XML
Recommendation " (cite "XML") " and syntactic conventions of the
Revised Reports on Scheme. The following table summarizes the
differences:")
   (table (@ (align "center"))
    (tr (td (n_)) (th "EBNF notation") (th "R5RS notation"))
    (tr (td "An optional " (em "thing"))
	(td (code (ebnf-opt "thing")))
	(td (code "[thing]")))
    (tr (td "Zero or more " (em "thing") "s")
	(td (code (ebnf-* "thing")))
	(td (code "thing ...")))
    (tr (td "One or more " (em "thing") "s")
	(td (code (ebnf-+ "thing")))
	(td (code "thing thing ..."))))
   (p "Furthermore, "
      (ul
       (li (code (nonterm "thing")) (n_) " stands for a non-terminal of a grammar")
       (li (code (term-id "thing")) (n_) " denotes a terminal that is a Scheme identifier")
       (li (code (term-str "thing")) (n_) " is a terminal of the grammar that is a Scheme string")
       (li (code (term-lit "thing")) (n_) " is a literal Scheme symbol")))

   
   (Section 2 "Grammar")
   (productions
     (production 1
       (nonterm "TOP")
       ((term-lit "*TOP*")
	(ebnf-opt (nonterm "namespaces"))
	;(ebnf-opt (nonterm "decl-entities"))
	(ebnf-* (nonterm "PI"))
	(ebnf-* (nonterm "comment"))
	(nonterm "Element"))))
   (p
    "This S-expression stands for the root of the SXML tree, a
document information item of the Infoset. It contains the root element
of the XML document as its only child element.")

   (productions
    (production 2
      (nonterm "Element")
      ((nonterm "name")
       (ebnf-opt (nonterm "attributes-list"))
       (ebnf-opt (nonterm "namespaces"))
       (ebnf-* (nonterm "child-of-element"))))
    (production 3
      (nonterm "attributes-list")
      ((term-lit "@")
       (ebnf-* (nonterm "attribute"))))
    (production 4
      (nonterm "attribute")
      ((nonterm "name")
       (ebnf-opt (term-str "value"))))
    (production 5
      (nonterm "child-of-element")
      ((nonterm "Element") "|"
       (term-str "character data") "|"
       (nonterm "PI") "|"
       (nonterm "comment") "|"
       (nonterm "entity")))
    )

   (p "These are the basic constructs of SXML.")

   (productions
    (production 6
      (nonterm "PI")
      ((term-lit "*PI*")
       (term-id "pi-target")
       (term-str "processing instruction content string"))))
  (p "As the XML Recommendation mandates, processing instructions (PI) are
not part of document's character data, but must be passed through
to applications. PIs are therefore represented by SXML nodes of a
dedicated type " (code "*PI*") ". DOM Level 2 treats PIs in a similar
way.")

   (productions
    (production 7
      (nonterm "comment")
      ((term-lit "*COMMENT*")
       (term-str "comment string")))
    (production 8
      (nonterm "entity")
      ((term-lit "*ENTITY*")
       (term-str "public-id")
       (term-str "system-id"))))
   (p "Comments are mentioned for completeness only. A SSAX parser,
among others, transparently skips the comments -- as it should and as
the Infoset Recommendation says. However, the Recommendation also
permits retaining of the comments. DOM has a provision for
COMMENT nodes, too. The present SXML grammar allows comment nodes but does
not mandate them by any means.")


   (productions
    (production 9
      (nonterm "name")
      ((nonterm "LocalName") "|"
       (nonterm "ExpName"))
      "; An XML QName")
    (production 10
      (nonterm "LocalName")
      ((term-id "NCName")))
    (production 11
      (nonterm "ExpName")
      ((term-id "URI") ":" (nonterm "LocalName") "|"
       (term-id "user-ns-prefix") ":" (nonterm "LocalName"))
      "; An expanded name")
    )
   (p (code (term-id "NCName")) " is a Scheme symbol which conforms to
Prod. [4] of " (cite "XML Namespaces") ". " (code (nonterm "ExpName"))
" is also a Scheme symbol, with an embedded colon that joins a local
part of the name and a prefix. A " (code (term-id "URI")) " is a
Namespace URI converted to a Scheme symbol. As the URI specification
allows symbols in URI which are not permitted in Scheme identifiers,
the forbidden symbols should be %-quoted during the conversion from a
URI string to " (code (term-id "URI")) ". Universal resource
identifiers are normally long and unwieldy strings. Therefore, a SSAX
parser lets a user set his own prefixes to represent Namespace
URIs. Thus " (code (term-id "user-ns-prefix")) " is also a Scheme
identifier, which is typically far shorter than " (code (term-id "URI")) ".")

   (productions
    (production 12
      (nonterm "namespaces")
      ((term-lit "*NAMESPACES*")
       (ebnf-* (nonterm "namespace-assoc"))))
    (production 13
      (nonterm "namespace-assoc")
      ((term-id "URI") (term-str "URI") "|"
       (term-id "user-ns-prefix") (term-str "URI"))))


  #|
<Element> ::= (<name> [<attributes-coll>] <child-of-elem> ...)
<attributes-coll> ::= (@ <attrib> ...)
<attrib> ::= (<name> "value") | (<name>)
<COMMENT> ::= (*COMMENT* "comment string")
<NAMESPACES> ::= (*NAMESPACES* <namespace-assoc> ...)
<ENTITY> ::= (*ENTITY* "public-id" "system-id")
<namespace-assoc> ::= (<namespace-prefix> "URI") |
		      (<namespace-prefix> <URI>) |
		      (<user-prefix> "URI")
   |#


   (Section 2 "SXML Tree")
   (p
    "Infoset's information item is a sum of its properties. This makes
a list a particularly suitable data structure to represent an
item. The head of the list, a Scheme identifier, " (em "names") " the
item. For many items this is their (expanded) name, a core
property. For an information item that denotes an XML Element, the
corresponding list starts with element's expanded name, optionally
followed by collections of attributes and effective namespaces. The
rest of the element item list is an ordered sequence of element's
children -- character data, processing instructions, and other
elements. Every child is unique; items never share their children even
if the latter have the identical content.")
   (p
    "Just as XPath does and Infoset specification explicitly allows,
we group character information items into maximal text strings.  The
value of an attribute is normally a string; it may also be omitted (in
case of HTML) to denote a singular attribute, 
e.g., " (code "<option checked>") ".")
   (p
    "We consider a collection of attributes an information item in its
own right, tagged with a special name " (code "@") ". The '@'
character may not occur in a valid XML name; therefore
an " (code (nonterm "attributes-list"))
" can not be mistaken for a list that represents an element. An XML
document renders attributes, processing instructions, namespace
specifications and other meta-data differently from the element
markup. In contrast, SXML represents element content and meta-data
uniformly -- as tagged lists.  SXML was a beneficiary of the fact that
every XML name is also a valid Scheme identifier; but not every Scheme
identifier is a valid XML name. This observation let us introduce
administrative names such as " (code "@") ", " (code "*PI*")
", " (code "*NAMESPACES*") " without worrying about potential name
clashes. The observation also makes the relationship between XML and SXML
well-defined. An XML document converted to SXML can be reconstructed
into an equivalent (in terms of the Infoset) XML document. Moreover, due
to the implementation freedom given by the Infoset specification, SXML
itself is an instance of the Infoset.")
   (p
    "SXML grammar therefore can also be represented in the following, more uniform view, as an SXML tree:")

   (productions
    (production "N"
      (nonterm "Node")
      ((nonterm "Element") "|"
       (nonterm "attributes-list") "|"
       (nonterm "attribute") "|"
       (term-str "character data") "|"
       (nonterm "namespaces") "|"
       (nonterm "TOP") "|"
       (nonterm "PI") "|"
       (nonterm "comment") "|"
       (nonterm "entity"))))
   (p
    "We can represent this production by a set of two mutually-recursive datatypes:")

   (productions
    (production "N1"
      (nonterm "Node")
      ("(" (nonterm "name") "." (nonterm "Nodeset") ")" "|"
	(term-str "text string")))
    (production "N2"
      (nonterm "Nodeset")
      ("(" (nonterm "Node") "..." ")")
      ";; An ordered set of nodes")
    (production "N3"
      (nonterm "name")
      ((nonterm "LocalName") "|"
       (nonterm "ExpName") "|"
       (term-lit "@") "|"
       (term-lit "*TOP*") "|"
       (term-lit "*PI*") "|"
       (term-lit "*COMMENT*") "|"
       (term-lit "*ENTITY*") "|"
       (term-lit "*NAMESPACES*"))))

   (p
    "The uniformity of representation of elements, attributes, and
processing instructions in SXML simplifies its queries and
transformations. In our formulation, attributes and processing instructions
are just like regular elements but with a distinguished
name. Therefore, query and transformation functions dedicated to
attributes become redundant.")

   (p
    "A function SSAX:XML->SXML of a functional Scheme XML parsing
framework SSAX " (cite "SSAX") " can convert an XML document or a
well-formed part of it into the corresponding SXML form. The parser
supports namespaces, character and parsed entities, xml:space,
attribute value normalization, processing instructions and CDATA
sections.  The result of the that function is in conformance with the
XML Infoset.")


   (Section 2 "Namespaces")
   (p
    "Goals and the motivation for XML Namespaces are explained in an excellent article by James Clark " (cite "Clark1999") ". He says in part: ")
   (blockquote
    (p "The XML Namespaces Recommendation tries to improve this
situation by extending the data model to allow element type names and
attribute names to be qualified with a URI. Thus a document that
describes parts of cars can use " (code "part") " qualified by one URI; and a
document that describes parts of books can use " (code "part") " qualified by
another URI. I'll call the combination of a local name and a
qualifying URI a universal name. The role of the URI in a universal
name is purely to allow applications to recognize the name. There are
no guarantees about the resource identified by the URI. The XML
Namespaces Recommendation does not require element type names and
attribute names to be universal names; they are also allowed to be
local names."
    (br) "..." (br)
"The XML Namespaces Recommendation expresses universal names in an
indirect way that is compatible with XML 1.0. In effect the XML
Namespaces Recommendation defines a mapping from an XML 1.0 tree where
element type names and attribute names are local names into a tree
where element type names and attribute names can be universal
names. The mapping is based on the idea of a prefix. If an element
type name or attribute name contains a colon, then the mapping treats
the part of the name before the colon as a prefix, and the part of the
name after the colon as the local name. A prefix " (code "foo") "
refers to the URI specified in the value of the " (code "xmlns:foo") "
attribute. So, for example"
       (verbatim
	"<cars:part xmlns:cars='http://www.cars.com/xml'/>")
       "maps to"
       (verbatim
	"<{http://www.cars.com/xml}part/>")
       "Note that the " (code "xmlns:cars") " attribute has been removed by the mapping.")
    )

   (p
    "Using James Clark's terminology, SXML is precisely that tree where element
type names and attribute names can be universal names.  Production "
    (code (nonterm "name")) " above defines such a universal name, a
Scheme symbol. The universal name is either a local name (a symbol
with no embedded colons in its string) or an expanded name, "
    (code (nonterm "ExpName"))
    ". The latter is also a Scheme symbol, albeit a weird one. In
SXML, James Clark's example cited above will be represented as "
    (verbatim
     "(http://www.cars.com/xml:part)")
    "or, somewhat redundantly, "
    (verbatim
     "(http://www.cars.com/xml:part (@)"
     "   (*NAMESPACES* (cars \"http://www.cars.com/xml\")))"))

   (p
    "Such a representation also agrees with the Namespaces Recommendation "
    (cite "XML Namespaces") ", which says \"Note that the prefix
functions only as a placeholder for a namespace name. Applications
should use the namespace name, not the prefix, in constructing names
whose scope extends beyond the containing document.\"")

   (p "Of course, it is unwieldy to deal with identifiers
like " (code "http://www.cars.com/xml:part") ". Therefore, an application that
invokes the SSAX parser may tell the parser to map the
URI " (code "http://www.cars.com/xml") " to a prefix,
e.g., " (code "c") ". The parser will then produce"
   (verbatim
     "(c:part (*NAMESPACES* (c \"http://www.cars.com/xml\")))")
   "To be more precise, the parser will return just"
   (verbatim
    "(c:part)")
   "If an application told the parser how to map " (code "http://www.cars.com/xml") ", the application can keep this mapping in
its mind and will not need additional reminders.")


   (Section 2 "Case-sensitivity of SXML names")
   (p
    "XML is a case-sensitive language. The names of XML elements,
attributes, targets of PI, etc. may be distinguished solely by their
case. These names however are represented as Scheme identifiers in SXML. Although Scheme is traditionally a case-insensitive language, the use of Scheme symbols to represent XML names poses no contradictions. As R5RS says, "
    (blockquote
     "(symbol->string symbol) returns the name of symbol as a string. If
the symbol was part of an object returned as the value of a literal
expression (section 4.1.2) or by a call to the read procedure, and its
name contains alphabetic characters, then the string returned will
contain characters in the implementation's preferred standard
case -- some implementations will prefer upper case, others lower
case. If the symbol was returned by string->symbol, the case of
characters in the string returned will be the same as the case in the
string that was passed to string->symbol."))
   (p
    "Therefore, " (code "(string->symbol \"a\")") " is always
different from " (code "(string->symbol \"A\")") ". R5RS thus
explicitly permits case-sensitive symbols. Such symbols must be used
in SXML to represent XML names.  SXML-compliant XML parsers must
preserve the case of all names when converting them into symbols. A
parser may use the R5RS procedure " (code "string->symbol") " or other
available means.")
   (p "The DSSSL standard says:"
      (blockquote
       "7.3.1. Case Sensitivity. Upper- and lower-case forms of a
letter are always distinguished. NOTE 5: Traditionally Lisp systems
are case-insensitive."))
   (p "Therefore, any Scheme system that supports DSSSL should include
a case-sensitive reader. For example, Gambit has such a reader (which
is default), Bigloo also has one (albeit it is not default). On such a
system, using Scheme symbols for XML names is not only possible but
convenient as well. Entering SXML names on a case-insensitive system
requires the use of a bar notation, " (code "string->symbol") " or
other standard or non-standard way of producing case-sensitive
symbols. This is the only inconvenience with SXML applications on
case-insensitive Scheme systems. A web page " (cite "Scheme-case-sensitivity")
" discusses case sensitivity of various Scheme systems in detail.")

   (Section 2 "Normalized SXML")
   (p
    "For the sake of effective processing an SXML element may be
represented in a normalized form. A normalized SXML is a proper subset
of SXML; it follows the same grammar and additional restrictions. One
restriction concerns " (code (nonterm "attributes-list")) ". Although it
is declared optional in SXML grammar, production 2, it must always be
present in a normalized SXML document. If an element has no attributes,
" (code (nonterm "attributes-list")) " is then an s-expression " (code "(@)")
". Furthermore, in a normalized SXML, " (code (nonterm "comment"))
" nodes should be absent. " (code (nonterm "entity")) " nodes should be absent as well: parsed entities should be
expanded, even if they are external. " (code (nonterm "namespaces")) 
" nodes should only appear in a " (code "*TOP*") " element.")

   (p "SGML provides two equal forms for boolean attributes:"
      (ol
       (li "Minimized -  " (code "<OPTION checked>"))
       (li "Full      -  " (code "<OPTION checked=\"checked\">")))
      "XML mandates the full form only, whereas HTML allows both,
preferring the former. SXML supports the minimized form along with the
full one: " (code "(OPTION (@ (checked)))") " and " (code "(OPTION (@ (checked \"checked\")))") ". The normalized SXML however accepts only the full form.")


   (Section 2  "Examples")

   (p "Simple examples:"
      (verbatim
       "(some-name)                  ; An empty element without attributes"
       "(some-name (@))              ; The same but in the normalized form"
       ))

   (p "Progressively more complex examples:")
   (p (table (@ (border 1))
       (tr (th "XML") (th "SXML"))
       (tr
	(td (@ (align "left"))
	    (verbatim
	     "<WEIGHT unit=\"pound\">"
	     "  <NET certified=\"certified\"> 67 </NET>"
	     "  <GROSS> 95 </GROSS>"
	     "</WEIGHT>"))
	(td (@ (align "left"))
	    (verbatim
	     "(WEIGHT (@ (unit \"pound\"))"
	     "  (NET (@ (certified)) 67)"
	     "  (GROSS 95)"
             ")")))
       (tr
	(td (@ (align "left"))
	    (verbatim
	     " <BR/>"))
	(td (@ (align "left"))
	    (verbatim
	     "(BR)")))
       (tr
	(td (@ (align "left"))
	    (verbatim
	     " <BR></BR>"))
	(td (@ (align "left"))
	    (verbatim
	     "(BR)")))
       (tr
	(td (@ (align "left"))
	    (verbatim
	     "   <A HREF='URL'"
	     "      xml:space='preserve'>  link"
	     "<I xml:space='default'>"
	     "     itlink </I> &amp;amp;</A>"))
	(td (@ (align "left"))
	    (verbatim
	     "(*TOP*"
	     "  (A (@ (XML:space \"preserve\")"
	     "        (HREF \"URL\"))"
	     "  \"  link \""
	     "  (I (@ (XML:space \"default\")) "
	     "     \"itlink\") \" &amp;\"))")))

       (tr
	(td (@ (colspan "2") (align "center"))
	    (br) (n_)
	    "An example from the XML Namespaces Recommendation"))
       (tr
	(td (@ (align "left"))
	    (verbatim
	     "<!-- initially, the default"
	     "     namespace is 'books' -->"
	     "<book xmlns='urn:loc.gov:books'"
	     "  xmlns:isbn='urn:ISBN:0-395-36341-6'>"
	     "  <title>Cheaper by the Dozen</title>"
	     "  <isbn:number>1568491379</isbn:number>"
	     "  <notes>"
	     "  <!-- make HTML the default namespace "
	     "       for some commentary -->"
	     "     <p xmlns='urn:w3-org-ns:HTML'>"
	     "       This is a <i>funny</i> book!"
	     "     </p>"
	     "  </notes>"
	     "</book>"))
	(td (@ (align "left"))
	    (verbatim
	     "(*TOP*"
	     "  (urn:loc.gov:books:book"
	     "    (urn:loc.gov:books:title \"Cheaper by the Dozen\")"
	     "    (urn:ISBN:0-395-36341-6:number \"1568491379\")"
	     "    (urn:loc.gov:books:notes"
	     "      (urn:w3-org-ns:HTML:p"
	     "         \"This is a\" (urn:w3-org-ns:HTML:i \"funny\")"
	     "         \"book!\"))))")))

       (tr
	(td (@ (colspan "2") (align "center"))
	    (br) (n_)
	    "Another example from the XML Namespaces Recommendation"))
       (tr
	(td (@ (align "left"))
	    (verbatim
	     "<RESERVATION "
	     "  xmlns:HTML="
	     "   'http://www.w3.org/TR/REC-html40'>"
	     "<NAME HTML:CLASS=\"largeSansSerif\">"
	     "    Layman, A</NAME>"
	     "<SEAT CLASS='Y' "
	     "  HTML:CLASS=\"largeMonotype\">33B</SEAT>"
	     "<HTML:A HREF='/cgi-bin/ResStatus'>"
	     "    Check Status</HTML:A>"
	     "<DEPARTURE>1997-05-24T07:55:00+1"
	     "</DEPARTURE></RESERVATION>"))

	(td (@ (align "left"))
	    (verbatim
	     "(*TOP*"
	     "  (*NAMESPACES* (HTML \"http://www.w3.org/TR/REC-html40\"))"
	     "  (RESERVATION"
	     "    (NAME (@ (HTML:CLASS \"largeSansSerif\"))"
	     "      \"Layman, A\")"
	     "    (SEAT (@ (HTML:CLASS \"largeMonotype\")"
	     "             (CLASS \"Y\"))"
	     "       \"33B\")"
	     "    (HTML:A (@ (HREF \"/cgi-bin/ResStatus\"))"
	     "       \"Check Status\")"
	     "    (DEPARTURE \"1997-05-24T07:55:00+1\")))")))
	))


   (Section 2 "Acknowledgment")
   (p "Discussions with Kirill Lisovsky of MISA University are gratefully acknowledged. He shares the credit for this page. The errors are all mine.")


   (Section 2 "References")

   (bibitem "Clark1999" "Clark1999"
      "Jim Clark. XML Namespaces. Feb 4, 1999"
      (URL "http://www.jclark.com/xml/xmlns.htm"))

   (bibitem "DOM" "DOM"
     "World Wide Web Consortium. Document Object Model (DOM) Level 1
Specification. W3C Recommendation."
     (URL "http://www.w3.org/TR/REC-DOM-Level-1"))

   (bibitem "Scheme-case-sensitivity" "Scheme-case-sensitivity"
     "Kirill Lisovsky. Case sensitivity of Scheme systems."
     (URL "http://pair.com/lisovsky/scheme/case-sensitivity.html"))

   (bibitem "SSAX" "SSAX"
     "Functional XML parsing framework: SAX/DOM and SXML parsers
with support for XML Namespaces and validation."
     (URL "http://pobox.com/~oleg/ftp/Scheme/SSAX.scm"))

   (bibitem "SXML-short-paper" "SXML-short-paper"
      "XML and Scheme. An introduction to SXML and SXPath;
illustration of SXPath expressiveness and comparison with
XPath. September 17, 2000."
      (URL "SXML-short-paper.html"))

   (bibitem "XML" "XML"
      "World Wide Web Consortium. Extensible Markup Language (XML)
Version 1.0. W3C Recommendation 10 February 1998."
      (URL "http://www.w3.org/TR/1998/REC-xml-19980210"))

   (bibitem "XML Infoset" "XML Infoset"
      "World Wide Web Consortium. XML Information Set. W3C Working
Draft 2 February 2001"
      (URL "http://www.w3.org/TR/xml-infoset"))

   (bibitem "XML Namespaces" "XML Namespaces"
      "Namespaces in XML. W3C Recommendation. 14-January-1999"
      (URL "http://www.w3.org/TR/REC-xml-names/"))

   (bibitem "XPath" "XPath"
     "World Wide Web Consortium. XML Path Language (XPath).
Version 1.0. W3C Recommendation 16 November 1999."
     (URL "http://www.w3.org/TR/xpath"))

  (footer)


))))

;(pp Content)

;========================================================================
;			HTML generation

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
       "Your comments, problem reports, questions are very welcome!"))))



; Generating HTML

(define (generate-HTML Content)
  (let*
                ; Universal transformation rules. Work for every HTML,
                ; present and future
   ((universal-conversion-rules
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


		; Do the identical transformation for an alist
    (alist-conv-rules
     `((*default* . ,(lambda (tag . elems) (cons tag elems)))
       (*text* . ,(lambda (trigger str) str))))

		; Transformation rules to drop out everything but the
		; 'Header' node
    (search-Header-rules
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
    )

 (SRV:send-reply
  (post-order Content
   `(
     ,@universal-conversion-rules

     (html:begin 
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
			      (post-order Content
					  search-Header-rules)
			      #f)))
	     (post-order (make-navbar header-parms)
			 universal-conversion-rules))))
     
     (footer			; Find the Header in the Content
      . ,(lambda (tag)		; and create the footer of the page
	   (let ((header-parms
		  (lookup-def 'Header
			      (post-order Content
					  search-Header-rules)
			      #f)))
	     (post-order (make-footer header-parms)
			 universal-conversion-rules))))
     
     (page-title		; Find the Header in the Content
      . ,(lambda (tag)		; and create the page title rule
	   (let ((header-parms
		  (lookup-def 'Header
			      (post-order Content
					  search-Header-rules)
			      #f)))
	     (list "<h1 align=center>" 
		   (lookup-def 'long-title header-parms #f) "</h1>\n"))))


     (Section	; (Section level "content ...")
      . ,(lambda (tag level head-word . elems)
	   (list "<br>&nbsp;<a name=\"" head-word "\">&nbsp;</a>\n"
		 "<h" level ">"  head-word elems "</h" level ">\n")))

     (TOC	; Re-scan the Content for "Section" tags and generate
      . ,(lambda (tag)	; the Table of contents
	   (let ((sections
		  (post-order Content
		    `((Section	; (Section level "content ...")
		       ((*text* . ,(lambda (tag str) str)))
		       . ,(lambda (tag level head-word . elems)
			    (list "<li><a href=\"#" head-word
				  "\">" head-word elems "</a>\n" )))
		      (*default*
		       . ,(lambda (attr-key . elems) elems))
		      (*text* . ,(lambda (trigger str) '()))))))
	     ;(write sections ##stderr)
	     (list "<div><ol>" sections "</ol></div>\n"))))


     (verbatim	; set off pieces of code: one or several lines
      . ,(lambda (tag . lines)
	   (list "<pre>"
		 (map (lambda (line) (list "     " line "\n"))
		      lines)
		 "</pre>")))
     (URL 
      . ,(lambda (tag url)
	   (list "<br>&lt;<a href=\"" url "\">" url "</a>&gt;")))

     (bibitem
      . ,(lambda (tag label key . text)
	   (list "\n<p><a name=\"" key "\">[" label "]</a> " text)))

     (cite		; ought to locate the label and use the label!
      . ,(lambda (tag key)
	   (list "[<a href=\"#" key "\">" key "</a>]")))

     ; Grammatical terms
     (nonterm		; Non-terminal of a grammar
      . ,(lambda (tag term)
	   (list "&lt;" term "&gt;")))

     (term-id		; terminal that is a Scheme id
      . ,(lambda (tag term)
	   (list term )))

     (term-str		; terminal that is a Scheme string
      . ,(lambda (tag term)
	   (list "\"" term "\"")))

     (term-lit		; a literal Scheme symbol
      . ,(lambda (tag term)
	   (list "<em>" term "</em>")))

     (ebnf-opt		; An optional term
      . ,(lambda (tag term)
	   (list term "?")))

     (ebnf-*		; Zero or more repetitions
      . ,(lambda (tag term)
	   (list term "*")))

     (ebnf-+		; One or more repetitions
      . ,(lambda (tag term)
	   (list term "+")))

     (production
      . ,(lambda (tag number lhs rhs . comment)
	   (list "<tr valign=top><td align=right><a name=\"prod-" number
		 "\">[" number "]</a>&nbsp;</td><td align=right>"
		 "<code>" lhs "</code></td>"
		 "<td align=center><code> ::= </code></td>"
		 "<td align=left><code>" 
		 (if (pair? rhs)
		     (list-intersperse rhs " ")
		     rsh)
		 "</code> " comment
		 "</td></tr>\n")))

     (productions
      . ,(lambda (tag . prods)
	   (list "<p><table border=0 bgcolor=\"#f5dcb3\">\n" prods
		 "</table></p>\n")))
)))))

(generate-HTML Content)

