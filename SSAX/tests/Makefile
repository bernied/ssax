#	    Testing the SSAX and the utility library
#
# The targets to make:
#	test-under-bigloo-i	Bigloo interpreter
#	test-under-bigloo-c	Bigloo compiler
#	test-under-scm		SCM interpreter
#	test-under-mit		MIT scheme interpreter
#	test-under-gambit-i	Gambit-C interpreter

# $Id$

# Directory where all the "included" modules are located
LIBDIR=../lib

clean:
	rm -f *_.c *.o core a.out \
	vmyenv-bigloo \
	vinput-parse-bigloo \
	vsrfi-12-bigloo


# Bigloo-specific  part

BFLAGS=-I $(LIBDIR) -call/cc -farithmetic -unsafea -fstack
BIGLOO=bigloo

# Bigloo interpreter
test-under-bigloo-i:
	$(BIGLOO) $(BFLAGS) -i vmyenv-bigloo.scm
	$(BIGLOO) $(BFLAGS) -i vinput-parse-bigloo.scm
	$(BIGLOO) $(BFLAGS) -i vsrfi-12-bigloo.scm
	echo '(module test (include "myenv-bigloo.scm") (include "srfi-12.scm") (include "vland.scm"))' | $(BIGLOO) $(BFLAGS) -i --


# Bigloo compiler
test-under-bigloo-c:
	$(BIGLOO) $(BFLAGS) vmyenv-bigloo.scm -o vmyenv-bigloo
	./vmyenv-bigloo
	$(BIGLOO) $(BFLAGS) vinput-parse-bigloo.scm -o vinput-parse-bigloo
	./vinput-parse-bigloo
	$(BIGLOO) $(BFLAGS) vsrfi-12-bigloo.scm -o vsrfi-12-bigloo
	./vsrfi-12-bigloo


# Other interpreters

test-under-scm:
	thisd=`pwd` && cd $(LIBDIR) && scm -b -l myenv-scm.scm -l util.scm \
	-l env.scm -l $$thisd/vmyenv.scm
	thisd=`pwd` && cd $(LIBDIR) && scm -b -l myenv-scm.scm \
	-l util.scm -l input-parse.scm -l look-for-str.scm \
	-l $$thisd/vinput-parse.scm
	thisd=`pwd` && cd $(LIBDIR) && scm -b -l myenv-scm.scm \
	-l util.scm -l input-parse.scm -l look-for-str.scm \
	-l SSAX.scm

test-under-mit:
	thisd=`pwd` && cd $(LIBDIR) && echo "" | scheme  -load myenv-mit.scm \
	util.scm env.scm $$thisd/vmyenv.scm
	thisd=`pwd` && cd $(LIBDIR) && echo "" | scheme  -load myenv-mit.scm \
	util.scm input-parse.scm look-for-str.scm  $$thisd/vinput-parse.scm
	thisd=`pwd` && cd $(LIBDIR) && echo "" | scheme  -load myenv-mit.scm \
	util.scm input-parse.scm look-for-str.scm  SSAX.scm

test-under-gambit-i:
	thisd=`pwd` && cd $(LIBDIR) && gsi myenv.scm util.scm \
	env.scm $$thisd/vmyenv.scm
	thisd=`pwd` && cd $(LIBDIR) && gsi myenv.scm util.scm \
	input-parse.scm look-for-str.scm $$thisd/vinput-parse.scm
	thisd=`pwd` && cd $(LIBDIR) && gsi myenv.scm util.scm \
	input-parse.scm look-for-str.scm  SSAX.scm
	thisd=`pwd` && cd $(LIBDIR) && gsi -e '(include "myenv.scm")' \
	srfi-12.scm $$thisd/vsrfi-12.scm
	thisd=`pwd` && cd $(LIBDIR) && gsi -e '(include "myenv.scm")' \
	srfi-12.scm $$thisd/vland.scm


