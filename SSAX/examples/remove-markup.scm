; Given an XML document, remove all markup
; In other words, compute the string-value of a DOM tree of the document
;
; $Id$



; Given a port referring to an xml document, this procedure returns
; a string that contains all the character data from the document:
; that is, the body of the document without the markup.
; This function deliberately does not print anything. Because the function
; is free from a difficult-to-estimate overhead of writing data, the
; function is rather suitable for benchmarking of the SSAX parser.
;
; On the other hand, the file outline.scm in the present directory
; shows how to do an XML transformation on the fly, as we parse.

(define (remove-markup xml-port)
    ; Accumulate the text values of leaves in a seed, in reverse order
    (let ((result
	   ((ssax:make-parser
	     NEW-LEVEL-SEED 
	     (lambda (elem-gi attributes namespaces
			      expected-content seed)
	       seed)
   
	     FINISH-ELEMENT
	     (lambda (elem-gi attributes namespaces parent-seed seed)
	       seed)

	     CHAR-DATA-HANDLER
	     (lambda (string1 string2 seed)
	       (let* ((seed (cons string1 seed)))
		 (if (string-null? string2) seed
		     (cons string2 seed))))
	     )
	    xml-port '())))
      (string-concatenate-reverse result)
    ))

; Implementation of string-concatenate-reverse from SRFI-13
(define (string-concatenate-reverse string-list)
  (let* ((final-size
	 ; total the length of strings in string-list
	  (let loop ((size 0) (lst string-list))
	    (if (pair? lst)
		(loop (+ size (string-length (car lst))) (cdr lst))
		size)))
	 (final-str (make-string final-size))
	 (dest-i (-- final-size)))
    (let outer ((lst string-list))
      (if (pair? lst)
	  (let ((str (car lst)))
	    (do ((i (-- (string-length str)) (-- i)))
		((negative? i) (outer (cdr lst)))
	      (string-set! final-str dest-i (string-ref str i))
	      (--! dest-i)))
	  final-str))))

