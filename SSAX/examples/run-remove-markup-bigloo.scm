; Given an XML document, remove all markup
; Bigloo module declaration
;
; $Id$

(module remove-markup
	(include "myenv-bigloo.scm")
	(include "util.scm")
	(include "look-for-str.scm")
	(include "input-parse.scm")
	(include "SSAX-code.scm")
	(include "remove-markup.scm")
	(include "run-remove-markup.scm")
	(main main))
