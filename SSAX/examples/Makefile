#	    Examples for SSAX parsing and SXML processing
#
# $Id$

# Directory where all the "included" modules are located
LIBDIR=../lib

all-bigloo-i: run-html-parse-sample-bigloo-i \
	run-pull-punct-bigloo-i \
	run-apply-templates-bigloo-i \
	run-sxml-db-conv-i \
	run-daml-parse-unparse-i

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
	$(BIGLOO) $(BFLAGS) -i -eval '(module permissive-HTML-parser (include "myenv-bigloo.scm") (include "util.scm") (include "look-for-str.scm") (include "input-parse.scm") (include "SSAX-code.scm"))' html-parser.scm

run-pull-punct-bigloo-i: pull-punct-sxml.scm \
		$(LIBDIR)/SSAX-code.scm
	$(BIGLOO) $(BFLAGS) -i -eval '(module pull-punct-sxml (include "myenv-bigloo.scm") (include "util.scm") (include "look-for-str.scm") (include "input-parse.scm") (include "SSAX-code.scm") (include "SXML-tree-trans.scm")(include "SXML-to-HTML.scm"))' pull-punct-sxml.scm

run-apply-templates-bigloo-i: apply-templates.scm \
		$(LIBDIR)/SSAX-code.scm
	$(BIGLOO) $(BFLAGS) -i -eval '(module apply-templates-simple (include "myenv-bigloo.scm") (include "util.scm") (include "look-for-str.scm") (include "input-parse.scm") (include "SSAX-code.scm") (include "SXML-tree-trans.scm") (include "SXML-to-HTML.scm") (include "SXPath-old.scm"))' apply-templates.scm

run-sxml-db-conv-i: sxml-db-conv.scm \
		$(LIBDIR)/SSAX-code.scm
	$(BIGLOO) $(BFLAGS) -i sxml-db-conv.scm

run-daml-parse-unparse-i: daml-parse-unparse.scm \
		$(LIBDIR)/SSAX-code.scm
	$(BIGLOO) $(BFLAGS) -s -i daml-parse-unparse.scm

run-sxslt-advanced-bigloo-i: sxslt-advanced.scm
	$(BIGLOO) $(BFLAGS) -i -eval '(module sxslt-advanced (include "myenv-bigloo.scm") (include "util.scm") (include "SXML-tree-trans.scm") (include "SXML-to-HTML.scm") (include "SXML-to-HTML-ext.scm"))' sxslt-advanced.scm


# SCM-specific part

test-remove-markup-scm:
	scm -b -l $(LIBDIR)/myenv-scm.scm  -l $(LIBDIR)/util.scm \
	-l $(LIBDIR)/look-for-str.scm -l $(LIBDIR)/input-parse.scm \
	-l $(LIBDIR)/SSAX-code.scm \
	-l remove-markup.scm -l run-remove-markup.scm \
	-e '(main (list "myself" "xml/ddn.rdf"))'

test-outline-scm:
	scm -b -l $(LIBDIR)/myenv-scm.scm  -l $(LIBDIR)/util.scm \
	-l $(LIBDIR)/look-for-str.scm -l $(LIBDIR)/input-parse.scm \
	-l $(LIBDIR)/SSAX-code.scm \
	-l outline.scm -l run-outline.scm \
	-e '(main (list "myself" "xml/total_weather.xsl"))'

test-sxml-scm:
	scm -b -l $(LIBDIR)/myenv-scm.scm  -l $(LIBDIR)/util.scm \
	-l $(LIBDIR)/look-for-str.scm -l $(LIBDIR)/input-parse.scm \
	-l $(LIBDIR)/SSAX-code.scm \
	-l run-sxml.scm \
	-e '(main (list "myself" "xml/OMF-sample.xml"))'

run-sxslt-advanced-scm: sxslt-advanced.scm
	scm -b -l $(LIBDIR)/myenv-scm.scm  -l $(LIBDIR)/util.scm \
	-l $(LIBDIR)/SXML-tree-trans.scm \
	-l $(LIBDIR)/SXML-to-HTML.scm -l $(LIBDIR)/SXML-to-HTML-ext.scm \
	-l sxslt-advanced.scm
