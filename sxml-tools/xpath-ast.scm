;; XPath/XPointer -> Abstract Syntax Tree parser
;
; This software is in Public Domain.
; IT IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND.
;
; Please send bug reports and comments to:
;   lisovsky@acm.org      Kirill Lisovsky
;   lizorkin@hotbox.ru    Dmitry Lizorkin


;==========================================================================
; Helper functions

; Writing operations as an S-expression in an infix notation
(define (txp:ast-operation-helper expr-lst op-lst add-on)
  (let ((rev-expr-lst (reverse expr-lst)))
    (let loop ((exprs (cdr rev-expr-lst))
               (ops (reverse op-lst))
               (res (car rev-expr-lst)))
      (if (null? ops)
          res
          (loop (cdr exprs) (cdr ops)
                (list (car ops) (car exprs) res))))))                              
  

;==========================================================================
; Parameters for TXPath -> AST implementation

(define txp:ast-params
  `(
    ; Axes
    (axis
     ((ancestor
       ,(lambda (add-on) 'ancestor))
      (ancestor-or-self
       ,(lambda (add-on) 'ancestor-or-self))
      (attribute
       ,(lambda (add-on) 'attribute))
      (child
       ,(lambda (add-on) 'child))
      (descendant
       ,(lambda (add-on) 'descendant))
      (descendant-or-self
       ,(lambda (add-on) 'descendant-or-self))
      (following
       ,(lambda (add-on) 'following))
      (following-sibling
       ,(lambda (add-on) 'following-sibling))
      (namespace
       ,(lambda (add-on) 'namespace))
      (parent
       ,(lambda (add-on) 'parent))
      (preceding
       ,(lambda (add-on) 'preceding))
      (preceding-sibling
       ,(lambda (add-on) 'preceding-sibling))
      (self
       ,(lambda (add-on) 'self))
      ; Addition by XLink
      (arc
       ,(lambda (add-on) 'arc))
      (traverse
       ,(lambda (add-on) 'traverse))
      (traverse-arc
       ,(lambda (add-on) 'traverse-arc))))
        
    ; Node test
    (node-test
     ((star
       ,(lambda (add-on) '((*))))
      (uri+star
       ,(lambda (uri add-on)
          `((namespace-uri ,uri))))
      (qname
       ,(lambda (uri local-name add-on)
          (if (not uri)
              `((local-name ,local-name))
              `((namespace-uri ,uri) (local-name ,local-name)))))      
      (comment
       ,(lambda (add-on) '((comment))))
      (text
       ,(lambda (add-on) '((text))))
      (processing-instruction
       ,(lambda (literal-string add-on)
          (if (not literal-string)  ; no literal provided
              '((pi))
              `((pi ,literal-string)))))
      (node
       ,(lambda (add-on) '((node))))
      (point
       ,(lambda (add-on) '((point))))
      (range
       ,(lambda (add-on) '((range))))))
            
    ; Location step
    (step
     ((common
       ,(lambda (axis-res node-test-res predicate-res-lst add-on)
          `(step
            (axis-specifier (,axis-res))
            (node-test ,@node-test-res)
            ,@predicate-res-lst)))
      (range-to
       ,(lambda (expr-res predicate-res-lst add-on)
          `(range-to
            (expr ,expr-res)
            ,@predicate-res-lst)))))
    
    ; Relative location path
    (relative-lpath
     ,(lambda (step-res-lst add-on)
        (cons 'relative-location-path step-res-lst)))
    
    ; Location path
    (location-path
     ((bare-slash
       ,(lambda (add-on) '(absolute-location-path)))
      (slash
       ,(lambda (relative-lpath-res add-on)
          (cons 'absolute-location-path (cdr relative-lpath-res))))                
      (double-slash
       ,(lambda (relative-lpath-res add-on)
          `(absolute-location-path
            (step
             (axis-specifier (descendant-or-self))
             (node-test (node)))
            ,@(cdr relative-lpath-res))))))
    
    ; Predicate
    (predicate
     ,(lambda (expr-res add-on)
        (list 'predicate expr-res)))
    
    ; Variable reference
    (variable-ref
     ,(lambda (var-name-string add-on)
        `(variable-reference ,var-name-string)))
    
    ; Function call
    (function-call
     ,(lambda (fun-name-string arg-res-lst add-on)
        `(function-call
          (function-name ,fun-name-string)
          ,@(map
             (lambda (arg-res) `(argument ,arg-res))
             arg-res-lst))))
                
    ; Primary expression
    (primary-expr
     ((literal
       ,(lambda (literal add-on)
          `(literal ,literal)))      
      (number
       ,(lambda (number add-on)
          `(number ,number)))))

    ; Filter expression
    (filter-expr
     ,(lambda (primary-expr-res predicate-res-lst add-on)
        `(filter-expr
          (primary-expr ,primary-expr-res)
          ,@predicate-res-lst)))
    
    ; Path expression
    (path-expr
     ((slash
       ,(lambda (filter-expr-res relative-lpath-res add-on)
          `(path-expr
            ,(if (eq? (car filter-expr-res) 'filter-expr)
                 filter-expr-res
                 `(filter-expr (primary-expr ,filter-expr-res)))
            ,@(cdr relative-lpath-res))))
      (double-slash
       ,(lambda (filter-expr-res relative-lpath-res add-on)
          `(path-expr
            ,(if (eq? (car filter-expr-res) 'filter-expr)
                 filter-expr-res
                 `(filter-expr (primary-expr ,filter-expr-res)))
            (step
             (axis-specifier (descendant-or-self))
             (node-test (node)))
            ,@(cdr relative-lpath-res))))))
    
    ; Union expression
    (union-expr
     ,(lambda (path-expr-res-lst add-on)
        (cons 'union-expr path-expr-res-lst)))            
    
    ; Unary expression
    (unary-expr
     ,(lambda (union-expr-res num-minuses add-on)
        (let loop ((n num-minuses)
                   (res union-expr-res))
          (if (= n 0) res
              (loop (- n 1) (list '- res))))))
    
    ; Different operations
    (operations
     ((* ,(lambda (add-on) '*))
      (div ,(lambda (add-on) 'div))
      (mod ,(lambda (add-on) 'mod))
      
      (+ ,(lambda (add-on) '+))
      (- ,(lambda (add-on) '-))
      (< ,(lambda (add-on) '<))
      (> ,(lambda (add-on) '>))
      (<= ,(lambda (add-on) '<=))
      (>= ,(lambda (add-on) '>=))
      (= ,(lambda (add-on) '=))
      (!= ,(lambda (add-on) '!=))))
    
    ; Additive and multiplicative expressions
    (mul-expr ,txp:ast-operation-helper)
    (add-expr ,txp:ast-operation-helper)
    
    ; Relational expression
    (relational-expr ,txp:ast-operation-helper)
    
    ; Equality expression
    (equality-expr ,txp:ast-operation-helper)
    
    ; And-expression
    (and-expr
     ,(lambda (equality-expr-res-lst add-on)
        (cons 'and equality-expr-res-lst)))
    
    ; Or-expression
    (or-expr
     ,(lambda (and-expr-res-lst add-on)
        (cons 'or and-expr-res-lst)))
    
    ; Full XPointer
    (full-xptr
     ,(lambda (expr-res-lst add-on)
        (cons 'full-xptr expr-res-lst)))
    
    ; XPointer child sequence
    (child-seq
     ((with-name
      ,(lambda (name-string number-lst add-on)
         `(child-seq
           (name ,name-string)
           ,@(map
              (lambda (num) (list 'number num))
              number-lst))))
      (without-name
       ,(lambda (number-lst add-on)
          (cons 'child-seq
                (map
                 (lambda (num) (list 'number num))
                 number-lst))))))
    ))
     
(define txp:ast-res (txp:parameterize-parser txp:ast-params))


;=========================================================================
; Highest level API functions
;
;  xpath-string - an XPath location path (a string)
;  ns-binding - declared namespace prefixes (an optional argument)
;  ns-binding = (list  (prefix . uri)
;                      (prefix . uri)
;                      ...)
;  prefix - a symbol
;  uri - a string
;
; The returned result:   abstract-syntax-tree   or   #f
;  abstract-syntax-tree - an S-expression
;  #f - signals of a parse error (error message is printed as a side effect
; during parsing)

(define (txp:ast-api-helper parse-proc)
  (lambda (xpath-string . ns-binding)
    (let ((res (parse-proc
                xpath-string
                (if (null? ns-binding) ns-binding (car ns-binding))
                '())))
      (if (txp:error? res)  ; error detected
          #f res))))
              
(define txp:xpath->ast
  (txp:ast-api-helper (cadr (assq 'xpath txp:ast-res))))
(define txp:xpointer->ast
  (txp:ast-api-helper (cadr (assq 'xpointer txp:ast-res))))
(define txp:expr->ast
  (txp:ast-api-helper (cadr (assq 'expr txp:ast-res))))
