; Validation code for SXML-to-HTML.scm
;
; IMPORT
; SXML-to-HTML.scm and all of its imports
;
; $Id$

; equal-strs? LIST-OF-PRINTABLES STRING
; Check to make sure that the result of writing out LIST-OF-PRINTABLES
; is the same as STRING
; LIST-OF-PRINTABLES can include strings, characters and numbers

(define (equal-strs?! strs expected-str)
   (let ((output-str
	  (with-output-to-string
	   (lambda ()
	     (for-each display strs)))))
     (assert (equal? output-str expected-str))))

(cout nl nl "Testing SXML-to-HTML.scm" nl nl)

(letrec ((gen (lambda (test-val)
		(with-output-to-string
		  (lambda ()
		    (SXML->HTML
		     `(p "par1" "par2" 
			 ,(and test-val (list "par3" "par4")))))))
	      ))
  (write (gen #t))
  (newline)
  (equal-strs?! '("<p>par1par2par3par4</p>" #\newline) (gen #t))
  (equal-strs?! '("<p>par1par2</p>" #\newline) (gen #f))
  )


(letrec ((gen (lambda (exp)
		(with-output-to-string
		  (lambda ()
		    (SXML->HTML exp))))))
  (equal-strs?! '("<p>&amp;</p>" #\newline) (gen '(p "&")))
  ;(write (gen '(p (@ (ALIGN "center")) "bad chars:" "<>&\"")))
  (equal-strs?! '("<p align=\"center\">bad chars:&lt;&gt;&amp;&quot;</p>"
		  #\newline)
		  (gen '(p (@ (align "center")) "bad chars:" "<>&\"")))
  (equal-strs?! '("<p align=\"center\" atr=\"&lt;value&gt;\">bad chars:<em>&lt;&gt;&amp;&quot;</em>" #\newline "</p>" #\newline)
		(gen '(p (@ (align "center") (atr "<value>"))
			 "bad chars:" (em "<>&\""))))
  (equal-strs?! '("<p align=\"center\" atr=\"&quot;text&quot;\"><br>"
		  #\newline "<ul compact><li>item 1</li>"
		  #\newline "</ul>" #\newline "</p>" #\newline)
		(gen '(p (@ (align "center") (atr "\"text\"")) (br)
			 (ul (@ (compact)) (li "item " 1)))))
  (equal-strs?!  '("<p><br>" #\newline "<ul compact><li>item 1</li>"
		   #\newline "</ul>" #\newline "</p>" #\newline)
		 (gen '(p (@) (br) (ul (@ (compact)) (li "item " 1)))))
  (equal-strs?! '("Content-type: text/html" #\newline #\newline
		  "<HTML><HEAD><TITLE>my title</TITLE></HEAD>"
		  #\newline "<body bgcolor=\"#ffffff\"><p>par1</p>"
		  #\newline "</body>" #\newline "</HTML>")
		(gen 
		 '(html:begin "my title" 
			      (body (@ (bgcolor "#ffffff")) (p "par1")))))
  )

(let ()
  (define (print-slide n max-count)
    (SXML->HTML
     `((h2 "Slide number:" ,n)     ; Note n is used in its native form
       ,(and (positive? n)
	     `(a (@ (href "base-url&slide=" ,(- n 1))) "prev"))
       ,(and (< (+ n 1) max-count)
	     `(a (@ (href "base-url&slide=" ,(+ n 1))) "next"))
       (p "the text of the slide"))))
  (equal-strs?! '("<h2>Slide number:0</h2>" #\newline
		  "<p>the text of the slide</p>" #\newline)
		(with-output-to-string (lambda () (print-slide 0 1))))
  (equal-strs?! '("<h2>Slide number:0</h2>" #\newline
		  "<a href=\"base-url&amp;slide=1\">next</a>"
		  #\newline "<p>the text of the slide</p>" #\newline)
		(with-output-to-string (lambda () (print-slide 0 3))))
  (equal-strs?! '("<h2>Slide number:1</h2>" #\newline
		  "<a href=\"base-url&amp;slide=0\">prev</a>"
		  #\newline "<a href=\"base-url&amp;slide=2\">next</a>"
		  #\newline "<p>the text of the slide</p>" #\newline)
		(with-output-to-string (lambda () (print-slide 1 3))))
  (equal-strs?! '("<h2>Slide number:2</h2>" #\newline
		  "<a href=\"base-url&amp;slide=1\">prev</a>"
		  #\newline "<p>the text of the slide</p>" #\newline)
		(with-output-to-string (lambda () (print-slide 2 3))))
  )

(SXML->HTML
 `(ul
   ,@(map (lambda (filename-title)
	    `(li (a (@ (href ,(car filename-title))))
		 ,(cdr filename-title)))
	  '(("slides/slide0001.gif" . "Introduction")
	    ("slides/slide0010.gif" . "Summary")))
   )
 )

(cout nl nl "All tests passed" nl)
