;; sxpath+ - a macro implementation of the sxpath function
; $Id$
; 
; This macro was proposed by Jim Bender on SSAX-SXML mail list 30/07/2003
; http://sourceforge.net/mailarchive/forum.php?thread_id=2860201&forum_id=599
; 
; This is a fairly literal implementation of the rewrite rules 
; given in the comments for the sxpath function, and is based on 
; SXPath tagged r3_914 in SourceForge CVS (sxml20030712.plt package).
;
; The "look and feel" is similar to the normal sxpath function. Instead of 
;   (sxpath '(div // para))
; one writes 
;   (sxpath+ (div // para))
; 
; Please note that the location path list is not quoted any more!
;
; Like normal sxpath calls, one can unquote to embedded an expression result in 
; the path expression:
;   (sxpath+ (// (chapter (@ (equal? ,a-node)))))
; or to embedded a procedure:
;   (sxpath+ (,(lift (node-parent tree)) @ name))
; 
; Note: location step functions (in r3_914 version of SXPath) are required 
; to take 3 parameters (a node-or-nodeset, a root node, and an environment of 
; variable bindings). Thus a small convenience "lift" is used, in order to lift 
; a function of type nodeset -> nodeset into one that also accepts 
; (but does not use) 3 parameters: root node and environment.
;
; This code does use syntax-case, not just syntax-rules, in three
; places: to distinguish symbols and txpath strings in locations steps, to
; recognize numbers as abbreviations for (node-pos ...), and to match names
; which are bound-identifiers (eq? and equal?).
;

;--- PLT module declaration, remove/adjust for other Schemes ---------------
#cs
(module sxpath-plus mzscheme
  (require (lib "txpath.ss" "sxml"))
  (require (lib "sxpathlib.ss" "sxml"))  
  (provide sxpath+)
    
  ; dispatch on a number (node-pos) reducer pattern
  (define-syntax analyze-red1
    (lambda (stx)
      (syntax-case stx ()
        ((_ num nsenv root env)
         (number? (syntax-object->datum (syntax num)))
         (syntax (node-pos num))))))
  
  ; analyze a "reducer"
  (define-syntax analyze-reduce
    (syntax-rules ()
      ((_ () nsenv root env)
       (sxml:filter (analyze-path () nsenv root env)))
      ((_ (step ...) nsenv root env)
       (sxml:filter (analyze-path (step ...) nsenv root env)))
      ((_ item nsenv root env)
       (analyze-red1 item nsenv root env))))
  
  ; dispatch on string or symbol within a location step
  (define-syntax analyze-1
    (lambda (stx)
      (syntax-case stx ()
        ((_ sym nsenv root env)
         (identifier? (syntax sym))
         (syntax (select-kids (ntype?? 'sym))))
        ((_ str nsenv root env)
         (string? (syntax-object->datum (syntax str)))
         (syntax (lambda (nodeset)
                   ((txpath str nsenv) nodeset root env)))))))
  
  ; transform a single location step
  (define-syntax analyze-step
    (lambda (stx)
      (syntax-case stx (// *or* *not* ns-id:* unquote)
        ((_ // nsenv root env)
         (syntax (node-or (node-self (ntype?? '*any*))
                          (node-closure (ntype?? '*any*)))))
        ((_ (or@ item ...) nsenv root env)
         (syntax (select-kids (ntype-names?? '(item ...)))))
        ((_ (not@ item ...) nsenv root env)
         (syntax (select-kids (sxml:invert (ntype-names?? '(item ...))))))
        ; eq? and equal? must be matched in a non-hygienic way
        ; (in PLT, can use module-or-top-identifier=? for these comparisons)
        ((_ (sym-equal? (unquote x)) nsenv root env)
         (eq? 'equal? (syntax-object->datum (syntax sym-equal?)))
         (syntax (select-kids (node-equal? x))))
        ((_ (sym-equal? x) nsenv root env)
         (eq? 'equal? (syntax-object->datum (syntax sym-equal?)))
         (syntax (select-kids (node-equal? 'x))))
        ((_ (sym-eq? (unquote x)) nsenv root env)
         (eq? 'eq? (syntax-object->datum (syntax sym-eq?)))
         (syntax (select-kids (node-eq? x))))
        ((_ (sym-eq? x) nsenv root env)
         (eq? 'eq? (syntax-object->datum (syntax sym-eq?)))
         (syntax (select-kids (node-eq? 'x))))
        ((_ (ns-id:* x) nsenv root env)
         (syntax (select-kids (ntype-namespace-id?? x))))
        ; exp must evaluate to a procedure (nodeset -> root-node -> env) -> nodeset
        ; this is done to be consistent with current sxpath function
        ; in the past, this would have been a procedure nodeset -> nodeset
        ((_ (unquote exp) nsenv root env)
         (syntax (lambda (nodeset)
                   (exp nodeset root env))))
        ((_ ((step ...) reducer ...) nsenv root env)
         (syntax (node-reduce (analyze-path (step ...) nsenv root env)
                      (analyze-reduce reducer nsenv root env) ...)))
        ((_ (() reducer ...) nsenv root env)
         (syntax (node-reduce (analyze-path () nsenv root env)
                      (analyze-reduce reducer nsenv root env) ...)))
        ; this should actually verify that sym is an identifier!!!
        ((_ (sym reducer ...) nsenv root env)
         (syntax (node-reduce (analyze-1 sym nsenv root env)
                      (analyze-reduce reducer nsenv root env) ...)))
        ((_ item nsenv root env)
         (syntax (analyze-1 item nsenv root env))))))
  
  ; transform a location path
  (define-syntax analyze-path
    (syntax-rules ()
      ((_ () nsenv root env)
       (node-join))
      ((_ (step ...) nsenv root env)
       (node-join (analyze-step step nsenv root env) ...))
      ((_ str nsenv root env)
       (analyze-1 str nsenv root env))))
  
  (define-syntax sxpath+
    (syntax-rules ()
      ((_ path)
       (sxpath+ path '()))
      ((_ path ns-binding)
       (let ((nsenv ns-binding))
         (lambda (node . root-var-binding)
           (let ((root-node
                  (if (null? root-var-binding) node (car root-var-binding)))
                 (var-binding
                  (if
                   (or (null? root-var-binding) (null? (cdr 
root-var-binding)))
                   '() (cadr root-var-binding))))
             ((analyze-path path nsenv root-node var-binding) node)))))))
  
  ) ; module end


