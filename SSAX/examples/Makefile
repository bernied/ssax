#	    Examples for SSAX parsing and SXML processing
#
# $Id$

# Directory where all the "included" modules are located
LIBDIR=../lib

all-bigloo-i: run-html-parse-sample-bigloo-i \
	run-pull-punct-bigloo-i \
	run-apply-templates-bigloo-i \
	run-sxml-db-conv-i \
	run-daml-parse-unparse-i \
	run-parent-pointers-bigloo-i \
	run-validate-doctype-simple-bi \
	sxml-nesting-depth-label-bi \
	sxml-to-sxml-bi

all-bigloo-c: run-remove-markup-bigloo run-outline-bigloo run-sxml-bigloo

all-scm: test-remove-markup-scm test-outline-scm test-sxml-scm \
	run-sxslt-advanced-scm  \
	run-sxml-db-conv-scm \
	run-parent-pointers-scm \
	sxml-nesting-depth-label-scm \
	sxml-to-sxml-scm

all-petite: sxml-nesting-depth-label-petite \
	sxml-to-sxml-petite

clean:
	rm -f *_.c *.o core a.out \
	run-remove-markup-bigloo \
	run-outline-bigloo \
	run-sxml-bigloo \
	run-html-parse-sample-bigloo

# Bigloo-specific  part

BFLAGS=-s -I $(LIBDIR) -Obench -farithmetic -unsafe
BIGLOO=bigloo

run-remove-markup-bigloo: run-remove-markup-bigloo.scm \
		run-remove-markup.scm remove-markup.scm \
		$(LIBDIR)/SSAX-code.scm
	$(BIGLOO) $(BFLAGS) $@.scm -o $@

run-outline-bigloo: run-outline-bigloo.scm run-outline.scm outline.scm \
		$(LIBDIR)/SSAX-code.scm
	$(BIGLOO) $(BFLAGS) $@.scm -o $@

run-sxml-bigloo: run-sxml-bigloo.scm run-sxml.scm \
		$(LIBDIR)/SSAX-code.scm
	$(BIGLOO) $(BFLAGS) $@.scm -o $@


run-html-parse-sample-bigloo-i: html-parser.scm \
		$(LIBDIR)/SSAX-code.scm
	$(BIGLOO) $(BFLAGS) -i -eval '(module permissive-HTML-parser (include "myenv-bigloo.scm") (include "srfi-13-local.scm") (include "char-encoding.scm") (include "util.scm") (include "look-for-str.scm") (include "input-parse.scm") (include "SSAX-code.scm"))' html-parser.scm

run-pull-punct-bigloo-i: pull-punct-sxml.scm \
		$(LIBDIR)/SSAX-code.scm
	$(BIGLOO) $(BFLAGS) -i -eval '(module pull-punct-sxml (include "myenv-bigloo.scm") (include "srfi-13-local.scm") (include "char-encoding.scm") (include "util.scm") (include "look-for-str.scm") (include "input-parse.scm") (include "SSAX-code.scm") (include "SXML-tree-trans.scm")(include "SXML-to-HTML.scm"))' pull-punct-sxml.scm

run-apply-templates-bigloo-i: apply-templates.scm \
		$(LIBDIR)/SSAX-code.scm
	$(BIGLOO) $(BFLAGS) -i -eval '(module apply-templates-simple (include "myenv-bigloo.scm") (include "srfi-13-local.scm") (include "char-encoding.scm") (include "util.scm") (include "look-for-str.scm") (include "input-parse.scm") (include "SSAX-code.scm") (include "SXML-tree-trans.scm") (include "SXML-to-HTML.scm") (include "SXPath-old.scm"))' apply-templates.scm

run-sxml-db-conv-i: sxml-db-conv.scm \
		$(LIBDIR)/SSAX-code.scm
	$(BIGLOO) $(BFLAGS) -i -eval '(module sxml-db-conversion (include "myenv-bigloo.scm") (include "srfi-13-local.scm") (include "char-encoding.scm") (include "util.scm") (include "look-for-str.scm") (include "input-parse.scm") (include "SSAX-code.scm") (include "SXML-tree-trans.scm"))' sxml-db-conv.scm

run-daml-parse-unparse-i: daml-parse-unparse.scm \
		$(LIBDIR)/SSAX-code.scm
	$(BIGLOO) $(BFLAGS) -s -i daml-parse-unparse.scm

sxml-nesting-depth-label-bi: sxml-nesting-depth-label.scm
	$(BIGLOO) $(BFLAGS) -i -eval '(module sxslt-depth-numbering (include "myenv-bigloo.scm") (include "srfi-13-local.scm") (include "util.scm") (include "SXML-tree-trans.scm"))' sxml-nesting-depth-label.scm

sxml-to-sxml-bi: sxml-to-sxml.scm
	$(BIGLOO) $(BFLAGS) -i -eval '(module sxslt-depth-numbering (include "myenv-bigloo.scm") (include "srfi-13-local.scm") (include "util.scm"))' sxml-to-sxml.scm

run-sxslt-advanced-bigloo-i: sxslt-advanced.scm
	$(BIGLOO) $(BFLAGS) -i -eval '(module sxslt-advanced (include "myenv-bigloo.scm") (include "srfi-13-local.scm") (include "util.scm") (include "SXML-tree-trans.scm") (include "SXML-to-HTML.scm") (include "SXML-to-HTML-ext.scm"))' sxslt-advanced.scm

run-parent-pointers-bigloo-i: parent-pointers.scm
	$(BIGLOO) $(BFLAGS) -i -eval '(module parent-pointers (include "myenv-bigloo.scm") (include "srfi-13-local.scm") (include "char-encoding.scm") (include "util.scm") (include "look-for-str.scm") (include "input-parse.scm") (include "SSAX-code.scm") (include "SXML-tree-trans.scm") (include "SXPath-old.scm"))' parent-pointers.scm

run-validate-doctype-simple-bi: validate-doctype-simple.scm
	$(BIGLOO) $(BFLAGS) -i -eval '(module validate-xml (include "myenv-bigloo.scm") (include "srfi-13-local.scm") (include "char-encoding.scm") (include "util.scm") (include "look-for-str.scm") (include "input-parse.scm") (include "SSAX-code.scm") (include "catch-error.scm"))' validate-doctype-simple.scm

# SCM-specific part

test-remove-markup-scm:
	scm -r5 -b -l $(LIBDIR)/myenv-scm.scm  -l $(LIBDIR)/srfi-13-local.scm \
	-l $(LIBDIR)/char-encoding.scm -l $(LIBDIR)/util.scm \
	-l $(LIBDIR)/look-for-str.scm -l $(LIBDIR)/input-parse.scm \
	-l $(LIBDIR)/SSAX-code.scm \
	-l remove-markup.scm -l run-remove-markup.scm \
	-e '(main (list "myself" "xml/ddn.rdf"))'

test-outline-scm:
	scm -r5 -b -l $(LIBDIR)/myenv-scm.scm  -l $(LIBDIR)/srfi-13-local.scm \
	-l $(LIBDIR)/char-encoding.scm -l $(LIBDIR)/util.scm \
	-l $(LIBDIR)/look-for-str.scm -l $(LIBDIR)/input-parse.scm \
	-l $(LIBDIR)/SSAX-code.scm \
	-l outline.scm -l run-outline.scm \
	-e '(main (list "myself" "xml/total_weather.xsl"))'

test-sxml-scm:
	scm -r5 -b -l $(LIBDIR)/myenv-scm.scm  -l $(LIBDIR)/srfi-13-local.scm \
	-l $(LIBDIR)/char-encoding.scm -l $(LIBDIR)/util.scm \
	-l $(LIBDIR)/look-for-str.scm -l $(LIBDIR)/input-parse.scm \
	-l $(LIBDIR)/SSAX-code.scm \
	-l run-sxml.scm \
	-e '(main (list "myself" "xml/OMF-sample.xml"))'

run-sxslt-advanced-scm: sxslt-advanced.scm
	scm -r5 -b -l $(LIBDIR)/myenv-scm.scm  -l $(LIBDIR)/srfi-13-local.scm \
	-l $(LIBDIR)/util.scm \
	-l $(LIBDIR)/SXML-tree-trans.scm \
	-l $(LIBDIR)/SXML-to-HTML.scm -l $(LIBDIR)/SXML-to-HTML-ext.scm \
	-l sxslt-advanced.scm

run-sxml-db-conv-scm: sxml-db-conv.scm $(LIBDIR)/SSAX-code.scm
	scm -r5 -b -l $(LIBDIR)/myenv-scm.scm  -l $(LIBDIR)/srfi-13-local.scm \
	-l $(LIBDIR)/char-encoding.scm -l $(LIBDIR)/util.scm \
	-l $(LIBDIR)/look-for-str.scm -l $(LIBDIR)/input-parse.scm \
	-l $(LIBDIR)/SSAX-code.scm \
	-l $(LIBDIR)/SXML-tree-trans.scm \
	-l sxml-db-conv.scm

run-parent-pointers-scm: parent-pointers.scm
	scm -r5 -b -l $(LIBDIR)/myenv-scm.scm  -l $(LIBDIR)/srfi-13-local.scm \
	-l $(LIBDIR)/char-encoding.scm -l $(LIBDIR)/util.scm \
	-l $(LIBDIR)/look-for-str.scm -l $(LIBDIR)/input-parse.scm \
	-l $(LIBDIR)/SSAX-code.scm \
	-l $(LIBDIR)/SXML-tree-trans.scm \
	-l $(LIBDIR)/SXPath-old.scm \
	-l parent-pointers.scm

sxml-nesting-depth-label-scm: sxml-nesting-depth-label.scm
	scm -r5 -b -l $(LIBDIR)/myenv-scm.scm  -l $(LIBDIR)/srfi-13-local.scm \
	-l $(LIBDIR)/util.scm \
	-l $(LIBDIR)/SXML-tree-trans.scm \
	-l sxml-nesting-depth-label.scm

sxml-to-sxml-scm: sxml-to-sxml.scm
	scm -r5 -b -l $(LIBDIR)/myenv-scm.scm  -l $(LIBDIR)/srfi-13-local.scm \
	-l $(LIBDIR)/util.scm \
	-l sxml-to-sxml.scm

# Petite-Chez-specific  part

sxml-nesting-depth-label-petite: sxml-nesting-depth-label.scm
	echo "(exit 0)" | petite $(LIBDIR)/myenv-chez.scm  \
	$(LIBDIR)/srfi-13-local.scm \
	$(LIBDIR)/util.scm \
	$(LIBDIR)/SXML-tree-trans.scm \
	sxml-nesting-depth-label.scm

sxml-to-sxml-petite: sxml-to-sxml.scm
	echo "(exit 0)" | petite $(LIBDIR)/myenv-chez.scm  \
	$(LIBDIR)/srfi-13-local.scm \
	$(LIBDIR)/util.scm \
	sxml-to-sxml.scm
