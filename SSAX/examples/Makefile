#	    Examples for SSAX parsing and SXML processing
#
# $Id$

# Directory where all the "included" modules are located
LIBDIR=../lib

all-bigloo-i: run-html-parse-sample-bigloo-i \
	run-pull-punct-bigloo-i \
	run-apply-templates-bigloo-i \
	run-sxml-db-conv-i

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
	$(BIGLOO) $(BFLAGS) -i html-parser.scm

run-pull-punct-bigloo-i: pull-punct-sxml.scm \
		$(LIBDIR)/SSAX-code.scm
	$(BIGLOO) $(BFLAGS) -i pull-punct-sxml.scm

run-apply-templates-bigloo-i: apply-templates.scm \
		$(LIBDIR)/SSAX-code.scm
	$(BIGLOO) $(BFLAGS) -i apply-templates.scm

run-sxml-db-conv-i: sxml-db-conv.scm \
		$(LIBDIR)/SSAX-code.scm
	$(BIGLOO) $(BFLAGS) -i sxml-db-conv.scm


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
