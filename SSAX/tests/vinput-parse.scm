;-----------------------------------------------------------------------------
;   This is a test driver for input parsing functions, to make sure they
;                       really work as intended
;
; IMPORT
; appropriate prelude: myenv.scm, myenv-bigloo.scm, myenv-scm.scm
;	depending on your system
; catch-error.scm -- for procedure, for-syntax
; util.scm
; look-for-str.scm
; input-parse.scm
;
; $Id$

	; To run this code under SCM, do
	; scm myenv-scm.scm catch-error.scm util.scm look-for-str.scm \
	;     input-parse.scm vinput-parse.scm

	; For Bigloo, you should evaluate or compile vinput-parse-bigloo.scm,
	; which contains a module declaration that includes the present file.


; This function is imported into the input-parse.scm module
(define (parser-error port msg . specializing-msgs)
  (apply error (cons msg specializing-msgs)))

		; make sure that the 'FORM' gave upon evaluation the
		; EXPECTED-RESULT
(define-macro (expect form expected-result)
  `(begin
    (display "evaluating ")
    (write ',form)
    (let ((real-result ,form))
     (if (equal? real-result ,expected-result)
       (cout "... gave the expected result: "
         (lambda () (write real-result)) nl)
       (error "... yielded: " real-result
        " which differs from the expected result: " ,expected-result)
      ))))

		; apply FORM to parse the input from the STR
		; and compare the result with the EXPECTED-RESULT
		; EXPECTED-RESULT is a pair: expected result from the
		; form and the expected next character from the stream
(define-macro (expect-parse-result str form expected-result)
  `(begin
    (display "applying ")
    (write ',form)
    (display " to the string ")
    (write ,str)
    (newline)
    (with-input-from-string ,str
      (lambda ()
        (let* ((real-result ,form) (real-next-char
				    (read-char (current-input-port))))
          (if (equal? (cons real-result real-next-char) ,expected-result)
            (cout "... gave the expected result: " real-result nl
              "    the next read character, " real-next-char
              " was expected as well" nl)
            (error "... yielded: " real-result " and the next char "
            real-next-char 
            " which differ from the expected result: " ,expected-result))
      )))))


; Build a string out of components
; A component can be a string, a character, a number
; (converted into a character), symbols cr and lf
; We could've used a notation like "abc\n\t"
; Unfortunately, not all Scheme systems support C-like notation
; of Scheme strings
(define (s . components)
  (apply string-append
	 (map (lambda (component)
		(cond
		 ((string? component) component)
		 ((char? component) (string component))
		 ((number? component) (string (integer->char component)))
		 ((eq? 'lf component) (string #\newline))
		 ((eq? 'cr component) (string #\return))
		 (else (error "bad component: " component))))
	      components)))


(cerr nl "Verifying string-index, string-rindex and substring? ..." nl)
(let ()
  (expect (string-index "" #\a) #f)
  (expect (string-index "cbda" #\a) 3)
  (expect (string-index "cbdal" #\a) 3)
  (expect (string-index "acbdal" #\a) 0)
  (expect (string-index "acbdal" #\space) #f)
  (expect (string-index "acbd al" #\space) 4)

  (expect (string-rindex "" #\a) #f)
  (expect (string-rindex "adbc" #\a) 0)
  (expect (string-rindex "ladbc" #\a) 1)
  (expect (string-rindex "ladbca" #\a) 5)
  (expect (string-rindex "ladbca" #\space) #f)
  (expect (string-rindex "la dbca" #\space) 2)
  
  (expect (substring? "rat" "pirate") 2)
  (expect (substring? "e" "pirate") 5)
  (expect (substring? "k" "pirate") #f)
  (expect (substring? "pi" "pirate") 0)
  (expect (substring? "te" "pirate") 4)
  (expect (substring? "rat" "outrage") #f)
  (expect (substring? "pit" "pirate") #f)
  (expect (substring? "rate" "pirate") 2)
  (expect (substring? "aa" "aaaaaaa") 0)
  (expect (substring? "pirate" "pirate") 0)
  (expect (substring? "pirates" "pirate") #f)
  (expect (substring? "pirate" "pirates") 0)
  (expect (substring? "ages" "outrage") #f)
  (expect (substring? "" "outrage") 0)
)

(cerr nl "Verifying string-prefix? and string-suffix? ..." nl)
(let ()
  (expect (string-prefix? "pir" "pirate") #t)
  (expect (string-prefix? "rat" "outrage") #f)
  (expect (string-prefix? "pir" (s "pirate " 'lf) ) #t)
  (expect (string-prefix? " pir" (s "pirate " 'lf)) #f)
  (expect (string-prefix? " pir" (s " pirate " 'lf)) #t)
  (expect (string-prefix? "pirate" "pirate") #t)
  (expect (string-prefix? "" "pirate") #t)
  (expect (string-prefix? "" "") #t)
  (expect (string-prefix? "pirate" "") #f)
  (expect (string-prefix? "pirat" "pirate") #t)
  (expect (string-prefix? "pirate" "pirat") #f)
  (expect (string-prefix? (s 'cr "Z!@~#$Ll*()") (s 'cr "Z!@~#$Ll*()def")) #t)

  (expect (string-prefix-ci? "pir" "pirate") #t)
  (expect (string-prefix-ci? "pIr" "pirate") #t)
  (expect (string-prefix? "pIr" "pirate") #f)
  (expect (string-prefix-ci? "rat" "outrage") #f)
  (expect (string-prefix-ci? "pir" (s "piratE " 'lf)) #t)
  (expect (string-prefix-ci? " pir" (s "pirate " 'lf)) #f)
  (expect (string-prefix-ci? " pir" (s " PIRate " 'lf)) #t)
  (expect (string-prefix-ci? "pirate" "pirate") #t)
  (expect (string-prefix-ci? "" "pirate") #t)
  (expect (string-prefix-ci? "" "") #t)
  (expect (string-prefix-ci? "pirate" "") #f)
  (expect (string-prefix-ci? "PiRaT" "pIrAte") #t)
  (expect (string-prefix-ci? "pIrAte" "PiRaT") #f)
  (expect (string-prefix-ci? (s 'cr "z!@~#$lL*()")
			     (s 'cr "Z!@~#$Ll*()def")) #t)
  (expect (string-prefix? (s 'cr "z!@~#$lL*()") (s 'cr "Z!@~#$Ll*()def")) #f)

  (expect (string-suffix? "ate" "pirate") #t)
  (expect (string-suffix? "rag" "outrage") #f)
  (expect (string-suffix? "rage" "outrage") #t)
  (expect (string-suffix? "rage" (s 'lf " outrage")) #t)
  (expect (string-suffix? "rage" (s 'lf " out\\rage" 'lf)) #f)
  (expect (string-suffix? (s "rage" 'lf) (s 'lf " out\\rage" 'lf)) #t)
  (expect (string-suffix? "pirate" "pirate") #t)
  (expect (string-suffix? "" "pirate") #t)
  (expect (string-suffix? "" "") #t)
  (expect (string-suffix? "pirate" "") #f)
  (expect (string-suffix? "pirat" "pirate") #f)
  (expect (string-suffix? "irate" "pirate") #t)
  (expect (string-suffix? "pirate" "irate") #f)
  (expect (string-suffix? (s 'cr "Z!@~#$Ll*()")
			  (s "def" 'cr "Z!@~#$Ll*()")) #t)

  (expect (string-suffix-ci? "ate" "pirate") #t)
  (expect (string-suffix-ci? "ATE" "pirate") #t)
  (expect (string-suffix? "ATE" "pirate") #f)
  (expect (string-suffix-ci? "rag" "outrage") #f)
  (expect (string-suffix-ci? "rage" "outraGE") #t)
  (expect (string-suffix-ci? "RAGE" (s 'lf " outrage")) #t)
  (expect (string-suffix-ci? "rage" (s 'lf " out\\rage" 'lf)) #f)
  (expect (string-suffix-ci? (s "rAge" 'lf)  (s 'lf " out\\raGe" 'lf)) #t)
  (expect (string-suffix-ci? "pirate" "pirate") #t)
  (expect (string-suffix-ci? "" "pirate") #t)
  (expect (string-suffix-ci? "" "") #t)
  (expect (string-suffix-ci? "pirate" "") #f)
  (expect (string-suffix-ci? "Pirat" "pirate") #f)
  (expect (string-suffix-ci? "iRATe" "piRATE") #t)
  (expect (string-suffix-ci? "piRATE" "iRATe") #f)
  (expect (string-suffix-ci? (s 'cr "z!@~#$lL*()")
			     (s "def" 'cr "Z!@~#$Ll*()")) #t)
  (expect (string-suffix? (s 'cr "z!@~#$lL*()")
			  (s "def" 'cr "Z!@~#$Ll*()")) #f)
)

(cerr nl "Verifying string case-changing functions ..." nl)
(let ((add-nl (lambda (str) (string-append str (string #\newline)))))
  (expect (string-downcase "") "")
  (expect (string-downcase (add-nl "1234abcde!")) (add-nl "1234abcde!"))
  (expect (string-downcase "XYZ\\,%^") "xyz\\,%^")
  (expect (string-downcase (string-append (string #\return) "Z!@~#$Ll*()def"))
	  (string-append (string #\return) "z!@~#$ll*()def"))

  (expect (string-upcase "") "")
  (expect (string-upcase (add-nl "1234abcde!")) (add-nl "1234ABCDE!"))
  (expect (string-upcase "XYZ\\,%^") "XYZ\\,%^")
  (expect (string-upcase (string-append (string #\return) "Z!@~#$Ll*()def"))
	  (string-append (string #\return) "Z!@~#$LL*()DEF"))

  (let* ((test-str (string-copy "a123456789.,Z"))
         (test-str-clone (string-copy test-str)))
       (assert (not (eq? test-str test-str-clone)))
       (assert (equal? test-str test-str-clone))
       (assert (not (eq? (string-downcase test-str) (string-downcase test-str))))
       (assert (equal? (string-downcase test-str) (string-downcase test-str)))
       (assert (not (eq? (string-upcase test-str) (string-upcase test-str))))
       (assert (equal? (string-upcase test-str) (string-upcase test-str)))
       (string-downcase! test-str)
       (assert (not (equal? test-str test-str-clone)))
       (assert (equal? test-str (string-downcase test-str-clone)))
       (assert (equal? test-str (string-downcase test-str)))
       (string-upcase! test-str)
       (assert (not (equal? test-str test-str-clone)))
       (assert (equal? test-str (string-upcase test-str-clone)))
       (assert (equal? test-str (string-upcase test-str)))
      )
)

; (cerr nl "Verifying promise-nice-xml-string ..." nl)
; (let ((test-p-xml
;       (lambda (str) (with-output-to-string (promise-nice-xml-string str)))))
;   (expect (test-p-xml "") "")
;   (expect (test-p-xml " ") " ")
;   (expect (test-p-xml "abcd 1234") "abcd 1234")
;   (expect (test-p-xml "'\"abcd '") "&apos;&quot;abcd &apos;")
;   (expect (test-p-xml "<tag>&amp;!</tag>") "&lt;tag&gt;&amp;amp;!&lt;/tag&gt;")
; )

(cerr nl "Verifying string-null? ..." nl)
(let ()
  (assert (string-null? ""))
  (assert (not (string-null? " ")))
  (assert (not (string-null? "1 ")))
  (assert (not (string-null? "\000")))
)

(cerr nl "Verifying string->integer ..." nl)
(let ()
  (expect (string->integer "" 0 0) #f)
  (expect (string->integer "" 0 1) #f)
  (expect (string->integer "" 1 0) #f)
  (expect (string->integer "1" 0 0) #f)
  (expect (string->integer "1" 0 1) 1)
  (expect (string->integer "1" 0 2) #f)
  (expect (string->integer "1" 1 1) #f)
  (expect (string->integer "1" 1 0) #f)
  (expect (string->integer "81234" 0 5) 81234)
  (expect (string->integer "81234" 1 5) 1234)
  (expect (string->integer "81234" -1 5) #f)
  (expect (string->integer "81234" 1 6) #f)
  (expect (string->integer "81234" 1 4) 123)
  (expect (string->integer "81234" 5 4) #f)
  (expect (string->integer "81234" 4 4) #f)
  (expect (string->integer "81234" 4 5) 4)
  (expect (string->integer "-1234" 4 5) 4)
  (expect (string->integer "-1234" 1 5) 1234)
  (expect (string->integer "-1234" 0 5) #f)
  (expect (string->integer "x12+4" 0 5) #f)
  (expect (string->integer "x12+4" 0 3) #f)
  (expect (string->integer "x12+4" 1 3) 12)
  (expect (string->integer "x12+4" 1 4) #f)
)

(cerr nl "Verifying string-split ..." nl)
(let ((tab (string #\tab)))
  (expect (string-split "") '())
  (expect (string-split "" '()) '())
  (expect (string-split "" '() 0) '())
  (expect (string-split "" '() 10) '())
  (expect (string-split " " '() 0) '())
  (expect (string-split " ") '())
  (expect (string-split (string #\tab #\space #\tab) '() 10) '())
  (expect (string-split "abcd" '() 10) '("abcd"))
  (expect (string-split "abcd") '("abcd"))
  (expect (string-split "  abcd   ") '("abcd"))
  (expect (string-split "  abcd   " '() -5) '())
  (expect (string-split "  abcd   " '() 1) '("abcd   "))
  (expect (string-split (string-append "  ab" tab "cd   ")) '("ab" "cd"))
  (expect (string-split (string-append "  ab" tab " cd   ")) '("ab" "cd"))
  (expect (string-split (string-append "  ab" tab " cd   ") '() 1)
	  (list (string-append "ab" tab " cd   ")))
  (expect (string-split (string-append "  ab" tab " cd   ") '() 2)
	  '("ab" "cd   "))
  (expect (string-split (string-append "  ab" tab " cd   ") '() 3)
	  '("ab" "cd"))
  (expect (string-split " abc d e f  ") '("abc" "d" "e" "f"))
  (expect (string-split " abc d e f  " '() 1) '("abc d e f  "))
  (expect (string-split " abc d e f  " '() 3) '("abc" "d" "e f  "))

  (expect (string-split "" '(#\: #\+)) '())
  (expect (string-split "" '(#\: #\+) 0) '())
  (expect (string-split "" '(#\: #\+) 10) '())
  (expect (string-split " " '(#\: #\+)) '(" "))
  (expect (string-split " " '(#\: #\+) 1) '(" "))
  (expect (string-split " " '(#\: #\+) 0) '())
  (expect (string-split ":" '(#\: #\+)) '("" ""))
  (expect (string-split "a:" '(#\: #\+)) '("a" ""))
  (expect (string-split "a:" '(#\: #\+) 1) '("a:"))
  (expect (string-split ":a" '(#\: #\+)) '("" "a"))
  (expect (string-split ":a" '(#\: #\+) 1) '(":a"))
  (expect (string-split ":" '(#\: #\+) 1) '(":"))
  (expect (string-split ":+" '(#\: #\+)) '("" "" ""))
  (expect (string-split ":+" '(#\: #\+) 1) '(":+"))
  (expect (string-split ":+" '(#\: #\+) 2) '("" "+"))
  (expect (string-split ":+" '(#\: #\+) 3) '("" "" ""))
  (expect (string-split ":+" '(#\: #\+) 4) '("" "" ""))
  (expect (string-split ":abc:d:e:f:" '(#\:)) '("" "abc" "d" "e" "f" ""))
  (expect (string-split ":abc:d:e::f:" '(#\:)) '("" "abc" "d" "e" "" "f" ""))
  (expect (string-split "root:x:0:0:Lord" '(#\:) 2) '("root" "x:0:0:Lord"))
  (expect (string-split "/usr/local/bin:/usr/bin:/usr/ucb/bin" '(#\:)) 
    '("/usr/local/bin" "/usr/bin" "/usr/ucb/bin"))
  (expect (string-split "/usr/local/bin" '(#\/)) '("" "usr" "local" "bin"))
)


(cerr nl "Verifying make-char-quotator ..." nl)
(let ((string->goodHTML
       (make-char-quotator
	'((#\< . "&lt;") (#\> . "&gt;") (#\& . "&amp;") (#\" . "&quot;")))))
  (expect (string->goodHTML "abc!def ") "abc!def ")
  (expect (string->goodHTML "") "")
  (expect (string->goodHTML "<") '("&lt;"))
  (expect (string->goodHTML "<a") '("&lt;" "a"))
  (expect (string->goodHTML "a&b") '("a" "&amp;" "b"))
  (expect (string->goodHTML "a b>") '("a b" "&gt;"))
  (expect (string->goodHTML "<>&\"") '("&lt;" "&gt;" "&amp;" "&quot;"))
  (expect (string->goodHTML " <>&\\\"")
	  '(" " "&lt;" "&gt;" "&amp;" "\\" "&quot;"))
  (expect (string->goodHTML "&amp;") '("&amp;" "amp;"))
)


(cerr nl "Verifying assert-curr-char ..." nl)
(let ()
  (define (test-assert-curr-char str char-list)
    (with-input-from-string str
      (lambda ()
        (assert-curr-char char-list "assert curr char" (current-input-port))
        )))
  
  (expect (test-assert-curr-char " abcd" '(#\a #\space)) #\space)
  (expect (test-assert-curr-char "a bcd" '(#\a #\space)) #\a)
  (assert (failed? (expect (test-assert-curr-char "bacd" '(#\a #\space)) #\a)))
)

(cerr nl "Verifying skipping of characters ..." nl)
(let (
      (eof (with-input-from-string "" read)))

  (expect-parse-result " abcd" (skip-until 1) '(#f . #\a))
  (assert (failed? (expect-parse-result " abcd" (skip-until 10) '(#f . #f))))
  (expect-parse-result " abcd" (skip-until 5) `(#f . ,eof))
  (expect-parse-result " abcd" (skip-until '(#\a #\space)) '(#\space . #\a))
  (expect-parse-result "xxxc bcd" (skip-until '(#\a #\space #\c))
    '(#\c . #\space))
  (expect-parse-result "xxxc" (skip-until '(#\a #\space #\c) (current-input-port))
    `(#\c . ,eof))
  (assert (failed? (expect-parse-result "xxxd"
        (skip-until '(#\a #\space #\c)) '(#f . #f))))
  (expect-parse-result "xxxd" (skip-until '(#\a #\space #\c *eof*))
    `(,eof . ,eof))
  (expect-parse-result "xxxc" (skip-until '(#\a #\space #\c *eof*))
    `(#\c . ,eof))

  (expect-parse-result "xxxd" (skip-while '(#\a #\space #\x))
    '(#\d . #\d))
  (expect-parse-result "yxxxd" (skip-while '(#\a #\space #\x))
    '(#\y . #\y))
  (expect-parse-result "xxx" (skip-while '(#\a #\space #\x) (current-input-port))
    `(,eof . ,eof))
  (expect-parse-result "xxa x" (skip-while '(#\a #\space #\x))
    `(,eof . ,eof))
)   


(cerr nl "Verifying tokenizing of the input stream ..." nl)
(let ((eof (with-input-from-string "" read)))

  (expect-parse-result "xxxd"
    (next-token '(#\a #\space #\x) '(#\d) "next token" (current-input-port))
    '("" . #\d))
  (expect-parse-result "xxx xa cccxd"
    (next-token '(#\a #\space #\x) '(#\d))
    '("cccx" . #\d))
  (expect-parse-result "xxx xa cccxdaa"
    (next-token '() '(#\d))
    '("xxx xa cccx" . #\d))
  (expect-parse-result "xxx xa cccxdaa"
    (next-token '() '(#\d #\a))
    '("xxx x" . #\a))
  (expect-parse-result "cccxd"
    (next-token '(#\a #\space #\x) '(#\d))
    '("cccx" . #\d))
  (assert (failed? (expect-parse-result "cccx"
    (next-token '(#\a #\space #\x) '(#\d) "next token")
    '(#f . #f))))
  (assert (failed? (expect-parse-result "cccx"
    (next-token '(#\a #\space #\x) '(#\d))
    '(#f . #f))))
  (expect-parse-result "cccx"
    (next-token '(#\a #\space #\x) '(#\d *eof*) "" (current-input-port))
    `("cccx" . ,eof))
  (assert (failed? (expect-parse-result "cccx"
    (next-token '(#\c #\space #\x) '(#\d))
    '(#f . #f))))
)

(cerr nl "Verifying tokenizing of the input stream: next-token-of ..." nl)
(let ((eof (with-input-from-string "" read)))

  (expect-parse-result "" (next-token-of '(#\a #\space #\x))
    `("" . ,eof))
  (expect-parse-result "d" (next-token-of '(#\a #\space #\x))
    '("" . #\d))
  (expect-parse-result "a   xx " (next-token-of '(#\a #\space #\x))
    `("a   xx " . ,eof))
  (expect-parse-result (s "a   xx " 'lf)
		       (next-token-of '(#\a #\space #\x) (current-input-port))
		       '("a   xx " . #\newline))
  (expect-parse-result (s "a  " 'cr " xx ") (next-token-of '(#\a #\space #\x))
    '("a  " . #\return))
  (expect-parse-result (s 'lf "a  " 'cr " xx ")
		       (next-token-of '(#\a #\space #\x))
    '("" . #\newline))

  (expect-parse-result ""
    (next-token-of (lambda (c) (and (not (eof-object? c)) c)))
    `("" . ,eof))
  (expect-parse-result (s "123" 'lf 'cr 0 "!")
    (next-token-of (lambda (c) (and (not (eof-object? c)) c)))
    `(,(s "123" 'lf 'cr 0 "!") . ,eof))

  (let ((down-pred
        (lambda (c)
          (cond ((eof-object? c) #f)
            ((char-alphabetic? c) (char-downcase c))
            (else #f)))))

    (expect-parse-result "" (next-token-of down-pred)
      `("" . ,eof))
    (expect-parse-result "12abc" (next-token-of down-pred)
      '("" . #\1))
    (expect-parse-result "abc12"
			 (next-token-of down-pred (current-input-port))
      '("abc" . #\1))
    (expect-parse-result "aB c12" (next-token-of down-pred)
      '("ab" . #\space))
    (expect-parse-result "XYZ" (next-token-of down-pred)
      `("xyz" . ,eof))
    )
)

(cerr nl "Verifying read-line ..." nl)
(let ((eof (with-input-from-string "" read)))

  (expect-parse-result "" (read-line)
    `(,eof . ,eof))
  (expect-parse-result "a 1 % xx" (read-line)
    `("a 1 % xx" . ,eof))
  (expect-parse-result (s 'lf) (read-line)
    `("" . ,eof))
  (expect-parse-result (s 'cr) (read-line)
    `("" . ,eof))
  (expect-parse-result (s 'cr 'lf) (read-line)
    `("" . ,eof))
  (expect-parse-result (s 'cr 'cr 'lf) (read-line (current-input-port))
    '("" . #\return))
  (expect-parse-result (s 'lf 'lf) (read-line)
    '("" . #\newline))
  (expect-parse-result (s #\space 'lf 'cr 'lf) (read-line)
    '(" " . #\return))
  (expect-parse-result (s " 12" 'lf "3" 'cr 'lf) (read-line)
    '(" 12" . #\3))
  (expect-parse-result (s " 12 " 'cr "3" 'cr 'lf) (read-line)
    '(" 12 " . #\3))
  (expect-parse-result (s " 12 " 'cr 'lf " 4" 'cr 'lf)
		       (read-line (current-input-port))
    '(" 12 " . #\space))
  (expect-parse-result (s " 12 " 'cr 'lf 'cr 'lf) (read-line)
    '(" 12 " . #\return))
)

(cerr nl "Verifying read-string ..." nl)
(let ((eof (with-input-from-string "" read)))

  (expect-parse-result "" (read-string 1)
    `("" . ,eof))
  (expect-parse-result "" (read-string 0)
    `("" . ,eof))
  (expect-parse-result "1234" (read-string 0)
    '("" . #\1))
  (expect-parse-result "1234" (read-string -10)
    '("" . #\1))
  (expect-parse-result (s 'lf "1234 " 'cr) 
		       (read-string 1 (current-input-port))
		       (cons (s 'lf) #\1))
  (expect-parse-result (s 'lf "1234 " 'cr) (read-string 3)
    (cons (s 'lf "12") #\3))
  (expect-parse-result (s 'lf "1234 " 'cr) (read-string 7)
    (cons (s 'lf "1234 " 'cr) eof))
  (expect-parse-result (s 'lf "1234 " 'cr)
		       (read-string 8 (current-input-port))
    (cons (s 'lf "1234 " 'cr) eof))
  (expect-parse-result (s 'lf "1234 " 'cr) (read-string 100)
    (cons (s 'lf "1234 " 'cr) eof))
)


(cerr nl "Verifying find-string-from-port? ..." nl)
(let ((eof (with-input-from-string "" read)))

  (expect-parse-result "bacacabd"
    (find-string-from-port? "acab" (current-input-port) 100)
    '(7 . #\d))
  (expect-parse-result "bacacabd"
    (find-string-from-port? "acab" (current-input-port))
    '(7 . #\d))
  (expect-parse-result "bacacabd"
    (find-string-from-port? "acad" (current-input-port) 100)
    `(#f . ,eof))
  (expect-parse-result "bacacabd"
    (find-string-from-port? "acad" (current-input-port))
    `(#f . ,eof))
  (expect-parse-result "bacacabd"
    (find-string-from-port? "bd" (current-input-port) 5)
    '(#f . #\a))
  (expect-parse-result "bacacabd"
    (find-string-from-port? "bd" (current-input-port) 9)
    `(8 . ,eof))
  (expect-parse-result "bacacabd"
    (find-string-from-port? "bd" (current-input-port))
    `(8 . ,eof))
  (expect-parse-result "bacacabd"
    (find-string-from-port? "bd" (current-input-port) 8)
    `(8 . ,eof))
  (expect-parse-result "bacacabd"
    (find-string-from-port? "be" (current-input-port) 20)
    `(#f . ,eof))
)

(cerr nl nl "All tests passed" nl)
