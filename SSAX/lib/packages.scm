(define-interface parser-error-interface
  (export parser-error))

(define-interface input-parse-interface
  (export peek-next-char
	  assert-curr-char
	  skip-until skip-while
	  next-token next-token-of
	  read-text-line
	  read-chars->string
	  input-parser-error))

(define-interface ssax-warn-interface
  (export SSAX:warn))

(define-interface run-test-interface
  (export run-test))

(define-interface assertions-interface
  (export assert assure))

(define-interface output-interface
  (export cout cerr nl))

(define-interface oleg-util-interface
  (export string-null?
	  any?
	  list-intersperse list-intersperse!
	  list-tail-diff
	  string-index string-rindex
	  substring?
	  string-prefix? string-prefix-ci?
	  string-suffix? string-suffix-ci?
	  string-downcase string-upcase
	  string-downcase! string-upcase!
	  string->integer
	  string-split
	  make-char-quotator))

(define-interface control-interface
  (export (when :syntax)
	  (begin0 :syntax)))

(define-interface look-for-str-interface
  (export find-string-from-port?))

(define-interface catch-error-interface
  (export (failed? :syntax)))

(define-interface sxml-tree-trans-interface
  (export SRV:send-reply
	  post-order pre-post-order replace-range))

(define-interface sxml-to-html-interface
  (export SXML->HTML
	  enattr
	  entag
	  string->goodHTML))

(define-interface sxml-to-html-ext-interface
  (export make-header
	  make-navbar
	  make-footer
	  universal-conversion-rules
	  universal-protected-rules
	  alist-conv-rules
	  search-Header-rules
	  make-generic-web-rules))

(define-interface ssax-interface
  (export xml-token? xml-token-kind xml-token-head
	  make-empty-attlist attlist-add
	  attlist-null?
	  attlist-remove-top
	  attlist->alist attlist-fold
	  SSAX:skip-internal-dtd
	  SSAX:read-markup-token
	  SSAX:read-CDATA-body
	  SSAX:read-char-ref
	  SSAX:read-attributes
	  SSAX:complete-start-tag
	  SSAX:read-external-ID
	  SSAX:read-char-data
	  SSAX:make-pi-parser SSAX:make-elem-parser SSAX:make-parser
	  SSAX:XML->SXML))

(define-structure define-opt (export (define-opt :syntax))
  (open scheme)
  (files define-opt))

(define-structure parser-error-vanilla parser-error-interface
  (open scheme signals)
  (begin
    (define (parser-error port message . rest)
      (apply error message rest))))

(define (make-input-parse parser-error-structure)
  (structure input-parse-interface
    (open scheme
	  define-opt
	  ascii
	  parser-error-structure)
    (files input-parse)))

(define input-parse-vanilla (make-input-parse parser-error-vanilla))

(define-structure run-test run-test-interface
  (open scheme)
  (files run-test))

(define-structure assertions assertions-interface
  (open scheme
	big-util)
  (files assert))

(define-structure output output-interface
  (open scheme i/o)
  (files output))

(define-structure oleg-util oleg-util-interface
  (open scheme
	big-util)
  (files util))

(define-structure sxml-tree-trans sxml-tree-trans-interface
  (open scheme
	assertions run-test
	signals srfi-11
	pp)
  (begin
    (define pp p))
  (files "SXML-tree-trans.scm"))

(define-structure with-output-to-string (export with-output-to-string
						call-with-input-string
						with-input-from-string)
  (open scheme extended-ports i/o-internal)
  (begin
    (define (with-output-to-string thunk)
      (call-with-string-output-port
       (lambda (port)
	 (call-with-current-output-port port thunk))))
    (define (call-with-input-string string proc)
      (proc (make-string-input-port string)))
    (define with-input-from-string call-with-input-string)))
	
(define-structure sxml-to-html sxml-to-html-interface
  (open scheme
	with-output-to-string
	output assertions run-test
	oleg-util
	sxml-tree-trans)
  (files "SXML-to-HTML.scm"))

(define-structure sxml-to-html-ext sxml-to-html-ext-interface
  (open scheme
	big-util
	output
	sxml-to-html
	sxml-tree-trans)
  (files "SXML-to-HTML-ext.scm"))

(define-structure control control-interface
  (open scheme)
  (files control))

(define-structure look-for-str look-for-str-interface
  (open scheme)
  (files look-for-str))

(define-structure catch-error catch-error-interface
  (open scheme handle)
  (begin
    (define-syntax failed?
      (syntax-rules ()
	((failed? stmts ...)
	 (thunk-failed? (lambda () stmts ...)))))
    (define (thunk-failed? thunk)
      (call-with-current-continuation
       (lambda (return)
	 (with-handler
	  (lambda (condition more)
	    (return #t))
	  (lambda ()
	    (thunk)
	    #f)))))))

(define (make-ssax input-parse-structure ssax-warn-structure)
  (structure ssax-interface
	     (open scheme
		   oleg-util control look-for-str
		   ascii big-util
		   assertions run-test
		   output catch-error
		   with-output-to-string
		   input-parse-structure
		   ssax-warn-structure
		   srfi-11
		   pp)
	     (begin
	       (define pp p))
	     (files "SSAX.scm")))

(define-structure ssax-warn-vanilla ssax-warn-interface
  (open scheme
	output)
  (files ssax-warn-vanilla))

(define ssax-vanilla (make-ssax input-parse-vanilla
				ssax-warn-vanilla))
