; Evaluation of this file yields an HTML document
; $Id$

(define Content
 '(html:begin
   (Header
    (title "SXML")
    (description "Definition of SXML: an instance of XML Infoset as
S-expressions, an Abstract Syntax Tree of an XML document.")
    (Date-Revision-yyyymmdd "20020218")
    (Date-Creation-yyyymmdd "20010207")
    (keywords "XML, XML parsing, XML Infoset, XML Namespaces, AST, SXML, Scheme")
    (AuthorAddress "oleg@pobox.com")
    (Revision "2.1")
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
   (abstract
    (Revision)
    "SXML is an abstract syntax tree of an XML document. SXML is also
a concrete representation of the XML Infoset in the form of
S-expressions. The generic tree structure of SXML lends itself to a
compact library of combinators for querying and transforming SXML."
    (prod-note (URL "http://pobox.com/~oleg/ftp/Scheme/xml.html"))
    (keywords))

   (TOC)

   (Section 2 "Introduction")
   (p
    "An XML document is essentially a tree structure. The start and the end
tags of the root element enclose the whole content of the document,
which may include other elements or arbitrary character data.  Text
with familiar angular brackets is an external representation of an XML
document. Applications ought to deal with its internalized form:
XML information set, or its specializations.  This form lets an
application locate specific data or transform an XML tree into another
tree, which can then be written out as an XML, HTML, PDF, etc.
document.")
   (p "XML information set (Infoset) " (cite "XML Infoset") " is an
abstract data set that describes information available in a
well-formed XML document.  Infoset is made of \"information items\",
which denote elements, attributes, character data, processing
instructions, and other components of the document. Each information
item has a number of associated properties, e.g., name, namespace
URI. Some properties -- for example, 'children' and 'attributes' --
are (ordered) collections of other information items. Infoset describes
only the information in an XML document that is relevant to
applications. The default value of attributes declared in the DTD, parameter
entities, the order of attributes within a start-tag, and other data used merely for parsing or validation are not included. Although technically 
Infoset is specified for XML it largely applies to HTML as well.")
   (p
    "The hierarchy of containers comprised of text strings and other
containers greatly lends itself to be described by
S-expressions. S-expressions " (cite "McCarthy") " are easy to parse into an internal
representation suitable for traversal.  They have a simple external
notation (albeit with many a parentheses), which is relatively easy to
compose even by hand.  S-expressions have another advantage: "
    (em "provided") 
    " an appropriate design, they can represent Scheme code to be evaluated.
 This code-data dualism is a distinguished feature of Lisp and Scheme. ")
   (p
    "SXML is a concrete instance of the XML Infoset. Infoset's goal is
to present in some form all relevant pieces of data and their " (em
"abstract") ", container-slot relationships to each other.  SXML gives
the nest of containers a concrete implementation as S-expressions, and
provides means of accessing items and their properties. SXML is a
\"relative\" of XPath " (cite "XPath") " and DOM " (cite "DOM") ", whose data models are two other
instances of the XML Infoset. SXML is particularly suitable for
Scheme-based XML/HTML authoring, SXPath queries, and tree
transformations. In John Hughes' terminology, SXML is a term
implementation of evaluation of the XML document.")

   (Section 2 "Notation")
   (p
    "We will use an Extended BNF Notation (EBNF) employed in the XML
Recommendation " (cite "XML") ". The following table summarizes the notation.")
   (dl
    (dt (code (ebnf-opt "thing")))
    (dd "An optional " (em "thing"))

    (dt (code (ebnf-* "thing")))
    (dd "Zero or more " (em "thing") "s")

    (dt (code (ebnf-+ "thing")))
    (dd "One or more " (em "thing") "s")

    (dt (code (ebnf-choice "thing1" "thing2" "thing3")))
    (dd "Choice of " (em "thing") "s")

    (dt (code (nonterm "thing")))
    (dd "A non-terminal of a grammar")

    (dt (code (term-id "thing")))
    (dd "A terminal of the grammar that is a Scheme identifier")

    (dt (code (term-str "thing")))
    (dd "A terminal of the grammar that is a Scheme string")

    (dt (code (term-lit "thing")))
    (dd "A literal Scheme symbol")

    (dt (code (sexp (nonterm "A") (ebnf-* (nonterm "B")))))
    (dd "An S-expression made of " (code (nonterm "A"))
	" followed by zero or more " (code (nonterm "B")))

    (dt (code (sexp-cons (nonterm "A") (nonterm "B"))))
    (dd "An S-expression that is made by prepending " (code (nonterm "A"))
	" to an S-expression denoted by " (code (nonterm "B")))

    (dt (code (sexp-symb (nonterm "A") ":" (nonterm "B"))))
    (dd "A symbol whose string representation consists of all
characters that spell "
	(code (nonterm "A")) " followed by the colon character and by the characters that spell " (code (nonterm "B")) ". The " (code (sexp-symb "" )) " notation can be regarded a meta-function that creates symbols." )
    )

   
   (Section 2 "Grammar")
   (productions
     (production 1
       (nonterm "TOP")
       ((sexp
	 (term-lit "*TOP*")
	 (ebnf-opt (nonterm "namespaces"))
	 ;(ebnf-opt (nonterm "decl-entities"))
	 (ebnf-* (nonterm "PI"))
	 (ebnf-* (nonterm "comment"))
	 (nonterm "Element")))))
   (p
    "This S-expression stands for the root of the SXML tree, a
document information item of the Infoset. It contains the root element
of the XML document as its only child element.")

   (productions
    (production 2
      (nonterm "Element")
      ((sexp
	(nonterm "name")
	(ebnf-opt (nonterm "attributes-list"))
	(ebnf-opt (nonterm "namespaces"))
	(ebnf-* (nonterm "child-of-element")))))
    (production 3
      (nonterm "attributes-list")
      ((sexp
	(term-lit "@")
	(ebnf-* (nonterm "attribute")))))
    (production 4
      (nonterm "attribute")
      ((sexp (nonterm "name")
	     (ebnf-opt (term-str "value")))))
    (production 5
      (nonterm "child-of-element")
      ((ebnf-choice 
	(nonterm "Element")
	(term-str "character data")
	(nonterm "PI")
	(nonterm "comment")
	(nonterm "entity"))))
    )

   (p "These are the basic constructs of SXML.")

   (productions
    (production 6
      (nonterm "PI")
      ((sexp (term-lit "*PI*")
       (term-id "pi-target")
       (term-str "processing instruction content string")))))
   (p "The XML Recommendation specifies that processing instructions (PI) are distinct from elements and character data; processing
instructions must be passed through to applications. In SXML, PIs are
therefore represented by nodes of a dedicated type " (code "*PI*")
". DOM Level 2 treats processing instructions in a similar way.")

   (productions
    (production 7
      (nonterm "comment")
      ((sexp (term-lit "*COMMENT*")
	     (term-str "comment string"))))
    (production 8
      (nonterm "entity")
      ((sexp (term-lit "*ENTITY*")
	     (term-str "public-id")
	     (term-str "system-id")))))
   (p "Comments are mentioned for completeness only. A SSAX XML parser
" (cite "SSAX") ", among others, transparently skips the comments.
The XML Recommendation permits the parser to pass the comments to
an application or to completely disregard them. The present SXML grammar
admits comment nodes but does not mandate them by any means." (br)
"An " (code (nonterm "entity")) " node represents a reference to an
unexpanded external entity. This node corresponds to an unexpanded
entity reference information item, defined in Section 2.5 of " (cite
"XML Infoset") ". Internal parsed entities are always expanded by the
XML processor at the point of their reference in the body of the
document.")


   (productions
    (production 9
      (nonterm "name")
      ((ebnf-choice (nonterm "LocalName")
		    (nonterm "ExpName"))))
    (production 10
      (nonterm "LocalName")
      ((term-id "NCName")))
    (production 11
      (nonterm "ExpName")
      ((sexp-symb (nonterm "namespace-id") ":" (nonterm "LocalName"))))
    (production 12
      (nonterm "namespace-id")
      ((ebnf-choice
	(term-id "URI") (term-id "user-ns-shortcut"))))
    (production 13
      (nonterm "namespaces")
      ((sexp
	(term-lit "*NAMESPACES*")
	(ebnf-* (nonterm "namespace-assoc")))))
    (production 14
      (nonterm "namespace-assoc")
      ((sexp (nonterm "namespace-id") (term-str "URI"))))

    )
   (p "An SXML " (code (nonterm "name")) " is a single symbol. It is generally an expanded
name " (cite "XML Namespaces") ", which conceptually consists of a
local name and a namespace URI. The latter part may be empty, in which
case
" (code (nonterm "name")) " is a " (code (term-id "NCName")) ": a Scheme
symbol whose spelling conforms to production [4] of the XML Namespaces
Recommendation " (cite "XML Namespaces") ". " (code (nonterm
"ExpName")) " is also a Scheme symbol, whose string representation
contains an embedded colon that joins a local and a namespace
parts of the name. A " (code (term-id "URI")) " is a Namespace URI
string converted to a Scheme symbol. Universal Resource Identifiers (URI)
may contain characters (e.g., parentheses) that are prohibited
in Scheme identifiers. Such characters must be %-quoted during the
conversion from a URI string to " (code (term-id "URI"))
". A " (code (term-id "user-ns-shortcut")) " is a Scheme symbol chosen by the user to represent a namespace URI in the application program. The SSAX parser offers a user to define (short and mnemonic) unique shortcuts for Namespace URIs, which are are often long and unwieldy
strings.")

   (productions
    (production "2e"
      (nonterm "Element")
      ((sexp
	(nonterm "name")
	(ebnf-opt (nonterm "attributes-list"))
	(ebnf-opt (nonterm "aux"))
	(ebnf-* (nonterm "child-of-element")))))
    (production "4e"
      (nonterm "attribute")
      ((sexp (nonterm "name")
	     (ebnf-opt (term-str "value"))
	     (ebnf-opt (nonterm "aux")))))
    (production "15e"
      (nonterm "aux")
      ((sexp
	(term-lit "@@")
	(ebnf-opt (nonterm "namespaces"))
	(ebnf-* (nonterm "aux-node")))))
    (production "16e"
      (nonterm "aux-node")
      ((n_))
      (em "To be defined in the future"))
    )

   (p "The XML Recommendation and related standards are not firmly
fixed, as the long list of errata and the proposed version 1.1 of XML
clearly show. Therefore, SXML has to be able to accommodate future
changes while guaranteeing backwards compatibility. SXML also ought to
permit applications to store various processing information (e.g.,
cached resolved IDREFs) in an SXML tree. To allow such extensibility, we
introduce two new node types, " (code (nonterm "aux")) " and " (code (nonterm "aux-node")) ". The semantics of the latter is to be
established in future versions of SXML. Other candidates for " (code (nonterm "aux-node")) " are the unique id of an element or the reference to element's parent. The structure and the semantics of the " (code (nonterm
"aux")) " node is similar to those of the attribute list. Applications that do not specifically look for auxiliary
nodes can transparently ignore any present and future extensions."  )


   (Section 2 "SXML Tree")
   (p
    "Infoset's information item is a sum of its properties. This makes
a list a particularly suitable data structure to represent an
item. The head of the list, a Scheme identifier, " (em "names") " the
item. For many items this is their (expanded) name. For an information
item that denotes an XML element, the corresponding list starts with
element's expanded name, optionally followed by collections of
attributes and effective namespaces. The rest of the element item list
is an ordered sequence of element's children -- character data,
processing instructions, and other elements. Every child is unique;
items never share their children even if the latter have the identical
content.")
   (p
    "Just as XPath does and the Infoset specification explicitly allows,
we group character information items into maximal text strings.  The
value of an attribute is normally a string; it may be omitted (in
case of HTML) for a boolean attribute, e.g., " (code "<option checked>") ".")
   (p
    "We consider a collection of attributes an information item in its
own right, tagged with a special name " (code "@") ". The character '@'
may not occur in a valid XML name; therefore
an " (code (nonterm "attributes-list"))
" cannot be mistaken for a list that represents an element. An XML
document renders attributes, processing instructions, namespace
specifications and other meta-data differently from the element
markup. In contrast, SXML represents element content and meta-data
uniformly -- as tagged lists.  SXML takes advantage of the fact that
every XML name is also a valid Scheme identifier, but not every Scheme
identifier is a valid XML name. This observation lets us introduce
administrative names such as " (code "@") ", " (code "*PI*")
", " (code "*NAMESPACES*") " without worrying about potential name
clashes. The observation also makes the relationship between XML and SXML
well-defined. An XML document converted to SXML can be reconstructed
into an equivalent (in terms of the Infoset) XML document. Moreover, due
to the implementation freedom given by the Infoset specification, SXML
itself is an instance of the Infoset.")
   (p
    "Since an SXML document is essentially a tree structure, the SXML grammar of Section 3 can be presented in the following, more uniform form:")

   (productions
    (production "N"
      (nonterm "Node")
      ((ebnf-choice
	(nonterm "Element")
	(nonterm "attributes-list")
	(nonterm "attribute")
	(term-str "character data")
	(nonterm "namespaces")
	(nonterm "TOP")
	(nonterm "PI")
	(nonterm "comment")
	(nonterm "entity")
	(nonterm "aux")))))
   (p
    "or as a set of two mutually-recursive datatypes, 
" (code "Node") " and " (code "Nodeset") ", where the latter is an ordered set of " (code "Node") "s: ")

   (productions
    (production "N1"
      (nonterm "Node")
      ((ebnf-choice
	(sexp-cons (nonterm "name") (nonterm "Nodeset"))
	(term-str "text string"))))
    (production "N2"
      (nonterm "Nodeset")
      ((sexp (nonterm "Node") (ebnf-* (nonterm "Node")))
      ))
    (production "N3"
      (nonterm "name")
      ((ebnf-choice
	(nonterm "LocalName")
	(nonterm "ExpName")
	(term-lit "@")
	(term-lit "*TOP*")
	(term-lit "*PI*")
	(term-lit "*COMMENT*")
	(term-lit "*ENTITY*")
	(term-lit "*NAMESPACES*")
	(term-lit "@@")))))

   (p
    "The uniformity of the SXML representation for elements,
attributes, and processing instructions simplifies queries and
transformations. In our formulation, attributes and processing
instructions look like regular elements with a distinguished
name. Therefore, query and transformation functions dedicated to
attributes become redundant.")

   (p
    "A function " (code "SSAX:XML->SXML") " of a functional Scheme XML parsing
framework SSAX " (cite "SSAX") " can convert an XML document or a
well-formed part of it into the corresponding SXML form. The parser
supports namespaces, character and parsed entities, 
attribute value normalization, processing instructions and CDATA
sections.")


   (Section 2 "Namespaces")
   (p
    "The motivation for XML Namespaces is explained in an excellent article by James Clark " (cite "Clark1999") ". He says in part: ")
   (blockquote
    "The XML Namespaces Recommendation tries to improve this
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
       "Note that the " (code "xmlns:cars") " attribute has been removed by the mapping."
    )

   (div
    "Using James Clark's terminology, SXML as defined by [N1] is
precisely that tree where element type names and attribute names can
be universal names.  According to productions [N3] and [9-12], a
universal name, "
    (code (nonterm "name")) ", is either a local name or an expanded
name. Both kinds of names are Scheme identifiers. A local name has no
colon characters in its spelling. An expanded name is spelled with at
least one colon, which may make the identifier look rather odd. In
SXML, James Clark's example will appear as follows:"
    (verbatim
     "(http://www.cars.com/xml:part)")
    "or, somewhat redundantly, "
    (verbatim
     "(http://www.cars.com/xml:part (@)"
     "   (*NAMESPACES* (cars \"http://www.cars.com/xml\")))"))

   (p
    "Such a representation also agrees with the Namespaces Recommendation "
    (cite "XML Namespaces") ", which says: \"Note that the prefix
functions only as a placeholder for a namespace name. Applications
should use the namespace name, not the prefix, in constructing names
whose scope extends beyond the containing document.\"")

   (div "It is clearly unwieldy to deal with identifiers such as "
	(code "http://www.cars.com/xml:part") ". Therefore, an application that
invokes the SSAX parser may tell the parser to map the URI "
	(code "http://www.cars.com/xml") " to an application-specific namespace shortcut " (code (term-id "user-ns-shortcut")) ", e.g., " (code "c") ". The parser will then produce"
	(verbatim
	 "(c:part (*NAMESPACES* (c \"http://www.cars.com/xml\")))")
	"To be more precise, the parser will return just"
	(verbatim
	 "(c:part)")
	"If an application told the parser how to map " (code "http://www.cars.com/xml") ", the application can keep this mapping in
its mind and will not need additional reminders."
	(p "We must note there is a 1-to-1 correspondence between " (code (term-id "user-ns-shortcut"))
"s and the corresponding namespace URIs. This is generally not true
for XML namespace prefixes and namespace URIs. A " (code (term-id
"user-ns-shortcut")) " uniquely represents the corresponding namespace
URI within the document, but an XML namespace prefix does not. For
example, different XML prefixes may specify the same namespace
URI; XML namespace prefixes may be redefined in children elements. The
other difference between " (code (term-id "user-ns-shortcut")) "s and
XML namespace prefixes is that the latter are at the whims of the author
of the document whereas the namespace shortcuts are defined by an
XML processing application."))



   (Section 2 "Case-sensitivity of SXML names")
   (div
    "XML is a case-sensitive language. The names of XML elements,
attributes, targets of processing instructions, etc. may be
distinguished solely by their case. These names however are
represented as Scheme identifiers in SXML. Although Scheme is
traditionally a case-insensitive language, the use of Scheme symbols
for XML names poses no contradictions. According to R5RS " (cite "R5RS") ", "
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
    "Thus, R5RS explicitly permits case-sensitive symbols: " (code
"(string->symbol \"a\")") " is always different from " (code
"(string->symbol \"A\")") ". SXML uses such case-sensitive symbols for
all XML names. SXML-compliant XML parsers must preserve the case of
all names when converting them into symbols. A parser may use the R5RS
procedure " (code "string->symbol") " or other available means.")
   (div
    "Reading and writing SXML documents may require special care. The cited R5RS section allows a Scheme system to alter the case of symbols found in a literal expression or read from input ports. Entering SXML names on case-insensitive systems requires the use of a bar notation, " (code "string->symbol") " or
other standard or non-standard ways of producing case-sensitive
symbols. A SSAX built-in test suite is a highly portable example of entering literal case-sensitive SXML names. Such workarounds, however simple, become unnecessary on Scheme systems that support DSSSL. According to the DSSSL standard,"
      (blockquote
       "7.3.1. Case Sensitivity. Upper- and lower-case forms of a
letter are always distinguished. NOTE 5: Traditionally Lisp systems
are case-insensitive.")
      "In addition, a great number of Scheme systems offer a
case-sensitive reader, which often has to be activated through a
compiler option or pragma. A web page " (cite
"Scheme-case-sensitivity") " discusses case sensitivity of various Scheme systems in detail.")

   (Section 2 "Normalized SXML")
   (p
    "Normalized SXML is a proper subset of SXML that permits efficient
processing. An SXML document in the normalized form must satisfy a
number of additional restrictions. The first restriction makes
" (code (nonterm "attributes-list")) " mandatory (cf. Section 3,
production [2]). If an element has no attributes, " (code (nonterm "attributes-list")) " shall be specified as " (code "(@)") ". In normalized SXML,
" (code (nonterm "comment")) " and " (code (nonterm "entity")) " nodes
must be absent. Parsed entities should be expanded, even if they are external. 
A node " (code (nonterm "namespaces"))  " may appear only in
a " (code "*TOP*") " element.")

   (div "SGML provides two equal forms for boolean attributes: minimized, e.g., " (code "<OPTION checked>") " and full, " (code "<OPTION checked=\"checked\">") ". XML mandates the full form only, whereas HTML allows both,
preferring the former. SXML supports the minimized form along with the
full one: " (code "(OPTION (@ (checked)))") " and " (code "(OPTION (@ (checked \"checked\")))") ". The normalized SXML however accepts only the full form.")


   (Section 2  "Examples")

   (div "Simple examples:"
      (verbatim
       "(some-name)           ; An empty element without attributes"
       "(some-name (@))       ; The same but in the normalized form"
       ))

   (p "The figure below shows progressively more complex examples.")
   (table (@ (border 1))
       (tr (th "XML") (th "SXML"))
       (tr
	(td (@ (align "left"))
	    (verbatim
	     "<WEIGHT unit=\"pound\">"
	     "  <NET certified=\"certified\">67</NET>"
	     "  <GROSS>95</GROSS>"
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
	     " <P>"
	     "<![CDATA[<BR>\\r\\n"
	     "<![CDATA[<BR>]]]]>&gt; </P>"))
	(td (@ (align "left"))
	    (verbatim
	     "(*TOP*"
	     "  (P \"<BR>\\n<![CDATA[<BR>]]> \"))")))
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
	     "    (urn:loc.gov:books:title"
	     "        \"Cheaper by the Dozen\")"
	     "    (urn:ISBN:0-395-36341-6:number \"1568491379\")"
	     "    (urn:loc.gov:books:notes"
	     "      (urn:w3-org-ns:HTML:p"
	     "         \"This is a \""
	     "          (urn:w3-org-ns:HTML:i \"funny\")"
	     "         \" book!\"))))")))

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
	     "  (*NAMESPACES* "
	     "       (HTML \"http://www.w3.org/TR/REC-html40\"))"
	     "  (RESERVATION"
	     "    (NAME (@ (HTML:CLASS \"largeSansSerif\"))"
	     "      \"Layman, A\")"
	     "    (SEAT (@ (HTML:CLASS \"largeMonotype\")"
	     "             (CLASS \"Y\"))"
	     "       \"33B\")"
	     "    (HTML:A (@ (HREF \"/cgi-bin/ResStatus\"))"
	     "       \"Check Status\")"
	     "    (DEPARTURE \"1997-05-24T07:55:00+1\")))")))
	)


   (Section 2 "Acknowledgment")
   (p "Discussions with Kirill Lisovsky of MISA University are gratefully acknowledged. He shares the credit for this page. The errors are all mine.")


   (References

    (bibitem "McCarthy" "McCarthy"
       "John McCarthy. Recursive Functions of Symbolic Expressions
and Their Computation by Machine, Part I. Comm. ACM, 3(4):184-195, April 1960."
       (URL "http://www-formal.stanford.edu/jmc/recursive/recursive.html"))

    (bibitem "Clark1999" "Clark1999"
       "Jim Clark. XML Namespaces. February 4, 1999."
       (URL "http://www.jclark.com/xml/xmlns.htm"))

    (bibitem "R5RS" "R5RS"
	     "R. Kelsey, W. Clinger, J. Rees (eds.), Revised5 Report on
                      the Algorithmic Language Scheme. Higher-Order and
                      Symbolic Computation, Vol. 11, No. 1, September, 1998
                      and
                      ACM SIGPLAN Notices, Vol. 33, No. 9, October, 1998."
	     (URL "http://www.schemers.org/Documents/Standards/R5RS/"))

    (bibitem "SSAX" "SSAX"
       "Oleg Kiselyov. Functional XML parsing framework: SAX/DOM and
SXML parsers with support for XML Namespaces and validation. September
5, 2001."
       (URL "http://pobox.com/~oleg/ftp/Scheme/SSAX.scm"))

    (bibitem "SXML-short-paper" "SXML-short-paper"
       "Oleg Kiselyov. XML and Scheme. An introduction to SXML and SXPath;
illustration of SXPath expressiveness and comparison with
XPath. September 17, 2000."
       (URL "http://pobox.com/~oleg/ftp/Scheme/SXML-short-paper.html"))

    (bibitem "Lisovsky" "Scheme-case-sensitivity"
       "Kirill Lisovsky. Case sensitivity of Scheme systems."
       (URL "http://pair.com/lisovsky/scheme/case-sensitivity.html"))

    (bibitem "DOM" "DOM"
       "World Wide Web Consortium. Document Object Model (DOM) Level 1
Specification. W3C Recommendation."
       (URL "http://www.w3.org/TR/REC-DOM-Level-1"))

    (bibitem "XML" "XML"
       "World Wide Web Consortium. Extensible Markup Language (XML)
1.0 (Second Edition). W3C Recommendation. October 6, 2000."
       (URL "http://www.w3.org/TR/REC-xml"))

    (bibitem "XML Infoset" "XML Infoset"
       "World Wide Web Consortium. XML Information Set.  W3C Recommendation. 24 October 2001."
       (URL "http://www.w3.org/TR/xml-infoset"))

    (bibitem "XML Namespaces" "XML Namespaces"
       "World Wide Web Consortium. Namespaces in XML. W3C Recommendation. January 14, 1999."
       (URL "http://www.w3.org/TR/REC-xml-names/"))

    (bibitem "XPath" "XPath"
       "World Wide Web Consortium. XML Path Language (XPath).
Version 1.0. W3C Recommendation. November 16, 1999."
       (URL "http://www.w3.org/TR/xpath"))
    )

  (footer)


)))

;(pp Content)

;========================================================================
;			HTML generation

(include "SXML-to-HTML-ext.scm")


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


     (abstract			; The abstract of the document
      ((Revision
	. ,(lambda (tag)	; Find the Header in the Content
				; and create the revision record
	     (let ((header-parms
		    (lookup-def 'Header
				(list (post-order Content
						  search-Header-rules))
				#f)))
	     (list "This page specifies <strong>revision "
		   (lookup-def 'Revision header-parms #f)
		   "</strong> of SXML. "))))
       (prod-note
	. ,(lambda (tag . master-url)
	     (list
	      "<blockquote><font size=\"-1\">The master SXML specification
file is written in SXML itself. The present web page is the result of
translating that SXML code using an appropriate \"stylesheet\". The
master file, its renditions in HTML and other formats, and the
corresponding stylesheets are available at " master-url ".</font></blockquote>")))
       (keywords . ,(lambda (tag) '()))
       )
      . ,(lambda (tag . abstract-body)
	   (list "<div>" abstract-body "</div>\n"))
      )
		 
     (Section	; (Section level "content ...")
      . ,(lambda (tag level head-word . elems)
	   (list "<br>&nbsp;<a name=\"" head-word "\">&nbsp;</a>\n"
		 "<h" level ">"  head-word elems "</h" level ">\n")))

     (References	; (References bibitem ...)
      . ,(lambda (tag . bib-items)
	   (let ((level 2) (head-word "References"))
	     (list "<br>&nbsp;<a name=\"" head-word "\">&nbsp;</a>\n"
		   "<h" level ">"  head-word "</h" level ">\n"
		   bib-items))))


     

     (TOC	; Re-scan the Content for "Section" tags and generate
      . ,(lambda (tag)	; the Table of contents
	   (let ((sections
		  (post-order Content
		    `((Section	; (Section level "content ...")
		       ((*text* . ,(lambda (tag str) str)))
		       . ,(lambda (tag level head-word . elems)
			    (list "<li><a href=\"#" head-word
				  "\">" head-word elems "</a>\n" )))
		      (References ; (References bibitem ...)
		       . ,(lambda (tag . bib-items)
			    (let ((head-word "References"))
			      (list "<li><a href=\"#" head-word
				    "\">" head-word "</a>\n" ))))
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

     (ebnf-choice	; Choice of terms
      . ,(lambda (tag . terms)
	   (list-intersperse terms " | ")))

     (sexp		; S-expression constructor
      . ,(lambda (tag . terms)
	   (list "<strong>(</strong> " (list-intersperse terms " ") 
		 " <strong>)</strong>")))

     (sexp-cons		; S-expression constructor, like cons
      . ,(lambda (tag car cdr)
	   (list "<strong>(</strong> " car "<strong> . </strong>" cdr  
		 " <strong>)</strong>")))

     (sexp-symb		; Symbol constructor
      . ,(lambda (tag . terms)
	   (list "<strong><em>MAKE-SYMBOL</em>(</strong>" 
		 terms "<strong>)</strong>")))

     (production
      . ,(lambda (tag number lhs rhs . comment)
	   (list "<tr valign=top><td align=right><a name=\"prod-" number
		 "\">[" number "]</a>&nbsp;</td><td align=right>"
		 "<code>" lhs "</code></td>"
		 "<td align=center><code> ::= </code></td>"
		 "<td align=left><code>" 
		 (if (pair? rhs)
		     (list-intersperse rhs " ")
		     rhs)
		 "</code> " comment
		 "</td></tr>\n")))

     (productions
      . ,(lambda (tag . prods)
	   (list "<table border=0 bgcolor=\"#f5dcb3\">\n" prods
		 "</table>\n")))
)))))

(generate-HTML Content)

