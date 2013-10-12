> *The authoratative version of* [SSAX][ssax] *is at* [Sourceforge][ssax].
<p>
> The following README is a slightly modified version of [Oleg Kiselyov's Sourceforge homepage for SSAX][ssax].
> [Oleg Kiselyov](http://okmij.org/ftp/Scheme/xml.html#XML-parser) was the primary developer with contributions from [Dmitry Lizorkin](http://modis.ispras.ru/Lizorkin/xml-functional.html), [Kirill Lisovsky](http://metapaper.net/xml/ssax/) and [Mike Sperber](http://www.deinprogramm.de/sperber/software/).

S-exp-based XML parsing/query/conversion
========================================

This project offers tools to inter-convert between an angular-bracket and a more efficient S-expression-based notations for markup documents, and to manipulate and query XML data in Scheme. The main components of the project are **SSAX**, **SXML**, **SXPath**, and **SXSLT**.

A SSAX functional XML parsing framework consists of a DOM/SXML parser, a SAX parser, and a supporting library of lexing and parsing procedures. The procedures in the package can be used separately to tokenize or parse various pieces of XML documents. The framework supports XML Namespaces, character, internal and external parsed entities, attribute value normalization, processing instructions and CDATA sections. The package includes a semi-validating *SXML parser:* a DOM-mode parser that is an instantiation of a SAX parser (called **SSAX**).

**SSAX** is a full-featured, algorithmically optimal, pure-functional parser, which can act as a stream processor. **SSAX** is an efficient SAX parser that is *easy to use*. **SSAX** minimizes the amount of application-specific state that has to be shared among user-supplied event handlers. **SSAX** makes the maintenance of an application-specific element stack unnecessary, which eliminates several classes of common bugs. **SSAX** is written in a pure-functional subset of Scheme. Therefore, the event handlers are referentially transparent, which makes them easier for a programmer to write and to reason about. The more expressive, reliable and easier to use application interface for the event-driven XML parsing is the outcome of implementing the parsing engine as an enhanced tree fold combinator, which fully captures the control pattern of the depth-first tree traversal.

**SXML** is an abstract syntax tree of an XML document. **SXML** is also a concrete representation of the XML Infoset in the form of S-expressions.

**SXSLT** is a manipulation language for XML and **SXPath** is an XPath-conforming XML query language. Both languages internally rely on **SXML** as a representation of the XML Infoset.

Documentation and tutorials
---------------------------

[SXML Tools Tutorial](http://modis.ispras.ru/Lizorkin/sxml-tutorial.html) by Dmitry Lizorkin

[Main SSAX/SXML page](http://okmij.org/ftp/Scheme/xml.html)

[XML Matters: Investigating SXML and SSAX: Manipulating XML in the Scheme programming language](http://www.ibm.com/developerworks/xml/library/x-matters31.html)
by David Mertz, Ph.D.<br>
[[Japanese translation](http://www.ibm.com/developerworks/jp/xml/library/x-matters31/)]

[Authoring dynamic websites with SXML](http://more-magic.net/docs/scheme/sxslt.pdf) by Peter Bex

[Detailed introduction, motivation and real-life case-studies of SSAX, SXML, SXPath and SXSLT.](http://okmij.org/ftp/papers/SXs.pdf)<br>
The [paper](http://okmij.org/ftp/papers/SXs.pdf) and the [complementary talk](http://okmij.org/ftp/papers/SXs-talk.pdf) presented at the International Lisp Conference 2002.

[The derivation of the SSAX API and the comparison of SSAX with other functional-style XML parsers and with the Expat](http://okmij.org/ftp/papers/XML-parsing-talk.ps.gz)<br>
A transcript of a presentation at PADL 2002, the 4th International Symposium on Practical Aspects of Declarative Languages.<br>
[Springer-Verlag © version](http://link.springer.com/chapter/10.1007/3-540-45587-6_14)<br>
[Paper version](http://okmij.org/ftp/papers/XML-parsing.ps.gz)

[SXML tutorial (in Japanese)](http://homepage1.nifty.com/blankspace/scheme/nsx.html)

[SXML, SSAX, and SXML Transforms. Presentation, notes, and exercises](http://schematics.sourceforge.net/scheme-london/jan-8-2002.html) by Noel Welsh and Matt Jadud<br>
4th December 2002 and 8th January 2003 meetings of the Scheme UK group

[Advanced SXLST tutorial](http://okmij.org/ftp/Scheme/sxslt-advanced.scm)<br>
This file describes common patterns of SXSLT on an interesting example. We demonstrate higher-order tags, pre-order and post-order transformations, re-writing of SXML elements in regular and special ways, context-sensitive applications of re-writing rules, and reflection. Although the file is technically a Scheme source code, it is actually a tutorial. Of 357 lines in the file, 241 are comments and 24 are just blank lines. August 2003.

[SXSLT: Manipulation Language for XML](http://okmij.org/ftp/papers/SXSLT-talk.pdf)<br>
A transcript of a presentation at PADL 2003. The talk introduces SXSLT and compares it with XSLT. Our experience and user comments show that SXSLT is expressive and easy to use. We argue that this outcome is a consequence of SXSLT providing right abstractions for XML transformations, of being higher-order, declarative and extensible.

SXML-related projects
---------------------

[Sedna - a Native XML DBMS](http://www.sedna.org/)<br>
Since version 0.2, Sedna provides the representation of query results in SXML

[SXPath library: an implementation of XPath. The SXML query library. SXPointer: an SXPath-based implementation of XPointer](http://metapaper.net/query/) by Kirill Lisovsky

[SXLink: An implementation of W3C XLink](http://metapaper.net/lisovsky/download/contrib/xlink/) by Dmitry Lizorkin

[STX: a compiler for a subset of XSLT and an embedding of XSLT into Scheme](http://metapaper.net/lisovsky/transform/stx/) by Kirill Lisovsky

[DataGuides: A descriptive database schema for XML/SXML data](http://metapaper.net/xml/dg/) by Kirill Lisovsky

[WebIt! - An XML Framework for Scheme](http://web.archive.org/web/20081216015143/http://celtic.benderweb.net/webit/) by Jim Bender<br>
[WebIt! at Sourceforge](http://sourceforge.net/projects/webit/)

[HtmlPrag: a permissive HTML parser that emits SXML](http://www.neilvandyke.org/htmlprag/) by Neil W. Van Dyke

[WebScraperHelper: simple generation of SXPath queries to extract data from (parsed) Web pages](http://www.neilvandyke.org/webscraperhelper/) by Neil W. Van Dyke

[sxmlcnv: XML <-> SXML SmartDoc-friendly conversion application](http://www.netfort.gr.jp/~kiyoka/sxmlcnv/) by Kiyoka Nishiyama

SXML for static and dynamic websites
------------------------------------

This page and all SXML-related pages are authored in SXML.

For more detailed explanation of these projects, see the [talk at the International Lisp Conference](http://okmij.org/ftp/papers/SXs-talk.pdf).

Availability
------------

The current released version of **SSAX** is 5.1. The whole **SSAX** code is in the *public domain*.

**SSAX** has been tested on the following Scheme systems:<br>
[PLT Scheme][plt], [Bigloo][bigloo], [GambitC 4.0][gambit], [Chicken][chicken], [Guile][guile], [SCM][scm], [MIT Scheme 7.5.2][mit], [Scheme48][s48], [SCSH][scsh], [Gauche][gauche], [SISC][sisc].

Distributions
-------------

[SSAX download site at SourceForge](http://sf.net/project/showfiles.php?group_id=30687).

Kirill Lisovsky's index of various [SSAX distributions](http://metapaper.net/xml/ssax/).<br>
Kirill has put together many of those distributions, in particular, the ones for PLT Scheme.

SSAX/SXML has been integrated into various Scheme systems and, in some cases, become part of the distribution for those systems:
+   [guile-lib](http://www.nongnu.org/guile-lib/), version 5.1 [by Andy Wingo]
+   [Gauche/SXML, SSAX version 4.9+](http://gauche.cvs.sourceforge.net/gauche/) [by Shiro Kawai]
+   [SCSH and Scheme48, version 4.9](http://www.scsh.net/resources/markup.html) [by Michael Sperber]
+   SISC, an extensible Java based interpreter, [SSAX version 4.9](http://sisc.cvs.sourceforge.net/viewvc/sisc/contrib/ssax/) [by Noel Welsh]

CVS Tree
--------

[The CVS Tree](http://sourceforge.net/cvs/?group_id=30687) includes the complete SSAX/SXML code, some documentation, validation tests, as well as several sample applications.

You can [browse the files](http://ssax.cvs.sourceforge.net/viewvc/ssax/) in the CVS tree from any web browser.

You can also use [this github repository](http://github.com/bernied/ssax) to browse the source.
<hr>

**Original Last updated March 9, 2013**

<tt>Duplicated from SourceForge [SSAX][ssax] repository on 2013/10/12.</tt>

[ssax]: http://ssax.sourceforge.net/  "Sourceforge SSAX"
[plt]: http://racket-lang.org/ "Racket"
[bigloo]: http://www-sop.inria.fr/indes/fp/Bigloo/ "Bigloo"
[gambit]: http://www.iro.umontreal.ca/~gambit/ "GambitC"
[chicken]: http://www.call-cc.org/ "Chicken Scheme"
[guile]: http://www.gnu.org/software/guile/ "GNU Guile"
[scm]: http://people.csail.mit.edu/jaffer/SCM.html "SCM"
[mit]: http://www.gnu.org/software/mit-scheme/ "MIT Scheme"
[s48]: http://s48.org/ "Scheme 48"
[scsh]: http://www.scsh.net/ "Scsh"
[gauche]: http://practical-scheme.net/gauche/ "Gauche"
[sisc]: http://sisc-scheme.org/ "SISC"
