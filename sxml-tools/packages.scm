(define-interface sxml-errors-interface
  (export sxml:error))

(define-structure sxml-errors sxml-errors-interface
  (open scheme srfi-23)
  (begin
    (define sxml:error error)))

(define-interface sxpathlib-interface
  (export nodeset?
	  as-nodeset
	  sxml:element?
	  ntype-names??
	  ntype??
	  ntype-namespace-id??
	  sxml:complement
	  node-eq?
	  node-equal?
	  node-pos
	  sxml:filter
	  take-until take-after
	  map-union
	  node-reverse
	  node-trace
	  select-kids
	  node-self
	  node-join
	  node-reduce
	  node-or
	  node-closure
	  sxml:node?
	  sxml:attr-list
	  sxml:attribute
	  sxml:child
	  sxml:parent
	  node-parent
	  sxml:child-nodes
	  sxml:child-elements))

(define-structure sxpathlib sxpathlib-interface
  (open scheme
	(subset srfi-13 (string-prefix? string-index-right))
	assertions
	coutputs
	ppretty-prints)
  (files sxpathlib))

(define-interface sxml-tools-interface
  (export sxml:empty-element?
	  sxml:shallow-normalized?
	  sxml:normalized?
	  sxml:shallow-minimized?
	  sxml:minimized?
	  sxml:name
	  sxml:element-name
	  sxml:node-name
	  sxml:ncname
	  sxml:name->ns-id
	  sxml:content
	  sxml:text
	  sxml:content-raw
	  sxml:attr-list-u
	  sxml:aux-list
	  sxml:aux-list-u
	  sxml:aux-node
	  sxml:aux-nodes
	  sxml:attr
	  sxml:attr-from-list
	  sxml:num-attr
	  sxml:attr-u
	  sxml:ns-list
	  sxml:ns-id->nodes
	  sxml:ns-id->uri
	  sxml:ns-uri->nodes
	  sxml:ns-uri->id
	  sxml:ns-id sxml:ns-uri
	  sxml:ns-prefix
	  sxml:change-content!
	  sxml:change-content
	  sxml:change-attrlist
	  sxml:change-attrlist!
	  sxml:change-name!
	  sxml:change-name
	  sxml:add-attr
	  sxml:add-attr!
	  sxml:change-attr
	  sxml:change-attr!
	  sxml:set-attr
	  sxml:set-attr!
	  sxml:add-aux
	  sxml:add-aux!
	  sxml:squeeze!
	  sxml:squeeze
	  sxml:clean
	  select-first-kid
	  sxml:node-parent
	  sxml:add-parents
	  sxml:lookup
	  sxml:attr->xml
	  sxml:string->xml
	  sxml:sxml->xml
	  sxml:attr->html
	  sxml:string->html
	  sxml:non-terminated-html-tag?
	  sxml:sxml->html))

(define-structure sxml-tools sxml-tools-interface
  (open scheme
	(subset srfi-1 (filter))
	(subset srfi-13 (string-index-right))
	sxml-errors
	coutputs
	(subset oleg-utils (make-char-quotator))
	sxpathlib)
  (files sxml-tools))

(define-interface sxpath-ext-interface
  (export sxml:string
	  sxml:boolean
	  sxml:number
	  sxml:string-value
	  sxml:id
	  sxml:list-head
	  sxml:merge-sort
	  sxml:equality-cmp
	  sxml:equal?
	  sxml:not-equal?
	  sxml:relational-cmp
	  sxml:ancestor
	  sxml:ancestor-or-self
	  sxml:descendant
	  sxml:descendant-or-self
	  sxml:following sxml:following-sibling
	  sxml:namespace
	  sxml:preceding sxml:preceding-sibling))

(define-structure sxpath-ext sxpath-ext-interface
  (open scheme
	ascii
	coutputs
	oleg-string-ports
	srfi-11 ; LET-VALUES
	sxml-tools
	sxpathlib)
  (files sxpath-ext))

(define-interface xpath-parser-interface
  (export txp:parameterize-parser
	  sxml:whitespace
	  txp:signal-semantic-error
	  txp:error?))

(define-structure xpath-parser xpath-parser-interface
  (open scheme
	ascii
	(subset srfi-1 (filter))
	srfi-2 ; AND-LET*
	coutputs
	sxml-tools
	sxpath-ext
	sxpathlib)
  (files xpath-parser))

(define-interface txpath-interface
  (export sxml:core-last
	  sxml:core-position
	  sxml:core-count
	  sxml:core-id
	  sxml:core-local-name
	  sxml:core-namespace-uri
	  sxml:core-name
	  sxml:core-string
	  sxml:core-concat
	  sxml:core-starts-with
	  sxml:core-contains
	  sxml:core-substring-before sxml:core-substring-after
	  sxml:core-substring
	  sxml:core-string-length
	  sxml:core-normalize-space
	  sxml:core-translate
	  sxml:core-boolean
	  sxml:core-not
	  sxml:core-true
	  sxml:core-false
	  sxml:core-lang
	  sxml:core-number
	  sxml:core-sum
	  sxml:core-floor
	  sxml:core-ceiling
	  sxml:core-round
	  sxml:classic-params
	  sxml:xpath
	  sxml:xpointer
	  sxml:xpath-expr
	  sxml:xpath+index
	  sxml:xpointer+index
	  txpath))

(define-structure txpath txpath-interface
  (open scheme
	(subset srfi-13
		(string-prefix? string-prefix-ci? string-index-right))
	sxml-errors
	coutputs
	(subset oleg-utils (string-split substring?))
	sxml-tools
	sxpath-ext
	sxpathlib
	xpath-parser)
  (files txpath))

(define-interface sxpath-interface
  (export sxpath
	  if-sxpath
	  if-car-sxpath
	  car-sxpath
	  sxml:id-alist))

(define-structure sxpath sxpath-interface
  (open scheme
	srfi-2 ; AND-LET*
	coutputs
	txpath
	sxpathlib
	sxml-tools)
  (files sxpath))
