#	    Testing the SSAX and the utility library
#
# This Makefile assumes the use of GNU make, which is to be invoked
# as follows:
#    make PLATFORM=<platform> <target> ...
#
# where <platform> is one of the following:
#
#       gambi		Gambit-C interpreter
#       gambc		Gambit-C compiler
#       biglooi		Bigloo interpreter
#       biglooc		Bigloo compiler
#       scmi		SCM interpreter
# 
# and <target> is one of the following:
#       vmyenv
#	vinput-parse
#	vsrfi-12
#	vland
#       vSXML-tree-trans
#	vSXML-to-HTML
#	vSSAX
# 	all			All of the above
#	clean			Clean the working files and other garbage
#
# Not all of the targets are useful with all platforms.
# <target> may be omitted. In that case, all the above targets
# are assumed.
#
# $Id$

# Directory where all the "included" modules are located
LIBDIR=../lib

# Scheme library directory
LIBDIR=../lib
TMPSCM=/tmp/a.scm	# Used for Bigloo

PLATFORM = biglooi	# Default platform


#GAMBIT_LIBRARY=$(MYLIBRARY)

# Bigloo-specific  part

BFLAGS=-I $(LIBDIR) -call/cc -farithmetic -unsafea
BIGLOO=bigloo
#BFLAGS=-I $(LIBDIR) -Obench -farithmetic -unsafe
#BOPTFLAGS=-O3 -copt "-O3 -fomit-frame-pointer -mcpu=i686"

#------------------------------------------------------------------------
#	How to make the targets with different Scheme systems,
#		given the import list of "modules" (files)
#
# The 'functions' below take two parameters:
# the list of dependent files and the target (without .scm suffix)
# The rule must return a command to interpret or compile the target.
# The rules will be invoked as
#    $(call make-gambi,myenv.scm catch-error.scm,vmyenv)
# For more details, see Chapter 'Functions' from 'make' info pages

make-gambi= gsi -e "(include \"$(LIBDIR)/myenv.scm\")" \
	    $(foreach file,$(1),-e "(include \"$(LIBDIR)/$(file)\")") \
	    $(2).scm

make-biglooi=$(BIGLOO) $(BFLAGS) -i \
	 -eval "(module test (include \"myenv-bigloo.scm\") \
		$(foreach file,$(1),(include \"$(file)\")))" \
	$(2).scm

make-biglooc=echo "(module test (include \"myenv-bigloo.scm\") \
		$(foreach file,$(1),(include \"$(file)\")) \
	         (include \"$(strip $(2)).scm\"))" > $(TMPSCM); \
	     $(BIGLOO) $(BFLAGS) -o $(2) $(TMPSCM) && rm -f $(TMPSCM) && \
	     ./$(strip $(2))

make-scmi= scm -b -l $(LIBDIR)/myenv-scm.scm \
	   $(foreach file,$(1),-l $(LIBDIR)/$(file)) \
	   -l $(2).scm

#------------------------------------------------------------------------

gambi-targets=vmyenv vinput-parse vsrfi-12  \
	      vland \
	      vSXML-tree-trans vSXML-to-HTML vSSAX 

biglooi-targets=vmyenv vinput-parse vsrfi-12  \
	      vland \
	      vSXML-tree-trans vSXML-to-HTML vSSAX

biglooc-targets=vmyenv vinput-parse vsrfi-12  \
	      vSXML-tree-trans vSXML-to-HTML vSSAX

scmi-targets=vmyenv vinput-parse  \
	      vSXML-tree-trans vSXML-to-HTML vSSAX

all: $($(PLATFORM)-targets)
.PHONY: $($(PLATFORM)-targets)

clean:
	rm -f *_.c *.o core a.out $(TMPSCM) \
	$($(PLATFORM)-targets) \
	$(addsuffix .c,$($(PLATFORM)-targets)) \
	$(addsuffix .o,$($(PLATFORM)-targets))


#------------------------------------------------------------------------
#		Specific targets and their IMPORTS

vmyenv:
	$(call make-$(PLATFORM),\
	       catch-error.scm env.scm util.scm,\
	$@)


vinput-parse: 
	$(call make-$(PLATFORM),\
	       catch-error.scm util.scm look-for-str.scm input-parse.scm,\
	       $@)
vsrfi-12:
	$(call make-$(PLATFORM),\
	       srfi-12.scm,\
	       $@)

vtreap:
	$(call make-$(PLATFORM),\
	       catch-error.scm treap.scm,\
	       $@)

vmime:
	$(call make-$(PLATFORM),\
	       catch-error.scm util.scm input-parse.scm mime.scm,\
	       $@)

vhttp:
	$(call make-$(PLATFORM),\
	srfi-12.scm util.scm input-parse.scm http.scm mime.scm,\
	       $@)

vland:
	$(call make-$(PLATFORM),\
	       srfi-12.scm,\
	       $@)

vSXML-tree-trans:
	$(call make-$(PLATFORM),\
	       util.scm SXML-tree-trans.scm,\
	       $@)

vSXML-to-HTML:
	$(call make-$(PLATFORM),\
	       util.scm SXML-tree-trans.scm SXML-to-HTML.scm,\
	       $@)
vSSAX:
	$(call make-$(PLATFORM),\
	       catch-error.scm util.scm look-for-str.scm input-parse.scm,\
	       $(LIBDIR)/SSAX)

