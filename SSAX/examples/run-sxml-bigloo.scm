; Transform an XML document into SXML.
; Bigloo module declaration
;
; $Id$

(module xml-to-sxml
	(include "myenv-bigloo.scm")
	(include "util.scm")
	(include "look-for-str.scm")
	(include "input-parse.scm")
	(include "SSAX-code.scm")
	(include "run-sxml.scm")
	(main main))
