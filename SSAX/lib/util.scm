;****************************************************************************
;			My Scheme misc utility functions
;		(mainly dealing with string and list manipulations)
;
; myenv.scm, myenv-bigloo.scm or similar prelude is assumed.
; $Id$

(declare 			; Gambit-compiler optimization options
 (block)
 (standard-bindings)
 (fixnum)
)


;------------------------------------------------------------------------
;                               Iterator ANY?
;
; -- procedure+: any? PRED COLLECTION
;       Searches for the first element in the collection satisfying a
;       given predicate
;       That is, the procedure applies PRED to every element of the
;       COLLECTION in turn.
;       The first element for which PRED returns non-#f stops the iteration;
;       the value of the predicate is returned.
;       If none of the elements of the COLLECTION satisfy the predicate,
;       the return value from the procedure is #f
;       COLLECTION can be a list, a vector, a string, or an input port.
; See vmyenv.scm for validation tests.

(define (any? <pred?> coll)
  (cond
    ((list? coll)
      (let loop ((curr-l coll))
        (if (null? curr-l) #f
          (or (<pred?> (car curr-l)) (loop (cdr curr-l))))))
          
    ((vector? coll)
      (let ((len (vector-length coll)))
       (let loop ((i 0))
        (if (>= i len) #f
          (or (<pred?> (vector-ref coll i)) (loop (+ 1 i)))))))

    ((string? coll)
      (let ((len (string-length coll)))
       (let loop ((i 0))
        (if (>= i len) #f
          (or (<pred?> (string-ref coll i)) (loop (+ 1 i)))))))

    ((input-port? coll)
      (let loop ((c (read-char coll)))
        (if (eof-object? c) #f
          (or (<pred?> c) (loop (read-char coll))))))

    (else (error "any? on an invalid collection"))))


;------------------------------------------------------------------------
;		Some list manipulation functions

; -- procedure+: list-intersperse SRC-L ELEM
; inserts ELEM between elements of the SRC-L, returning a freshly allocated
; list (cells, that is)
      
(define (list-intersperse src-l elem)
  (if (null? src-l) src-l
    (let loop ((l (cdr src-l)) (dest (cons (car src-l) '())))
      (if (null? l) (reverse dest)
        (loop (cdr l) (cons (car l) (cons elem dest)))))))


; -- procedure+: list-intersperse! SRC-L ELEM
; inserts ELEM between elements of the SRC-L inplace
      
(define (list-intersperse! src-l elem)
  (if (null? src-l) src-l
    (let loop ((l src-l))
      (let ((next-l (cdr l)))
        (if (null? next-l) src-l
          (begin
            (set-cdr! l (cons elem next-l))
            (loop next-l)))))))


	; List-tail-difference: given two lists, list1 and list2 where
	; list2 is presumably a tail of list1, return 
	; a (freshly allocated) list which is a difference between list1 
	; and list2. If list2 is *not* a tail of list1, the entire list1
	; is returned.
(define (list-tail-diff list1 list2)
  (let loop ((l1-curr list1) (difference '()))
    (cond
      ((eq? l1-curr list2) (reverse difference))
      ((null? l1-curr) (reverse difference))
      (else (loop (cdr l1-curr) (cons (car l1-curr) difference))))))


;------------------------------------------------------------------------
;			String utilities

; Return the index of the first occurence of a-char in str, or #f

(define (string-index str a-char)
  (let loop ((pos 0))
    (cond
      ((>= pos (string-length str)) #f) ; whole string has been searched, in vain
      ((char=? a-char (string-ref str pos)) pos)
      (else (loop (+ 1 pos))))))

; Return the index of the last occurence of a-char in str, or #f
(define (string-rindex str a-char)
  (let loop ((pos (-- (string-length str))))
    (cond
      ((negative? pos) #f) 	; whole string has been searched, in vain
      ((char=? a-char (string-ref str pos)) pos)
      (else (loop (-- pos))))))



; -- procedure+: substring? PATTERN STRING
;     Searches STRING to see if it contains the substring PATTERN.
;     Returns the index of the first substring of STRING that is equal
;     to PATTERN; or `#f' if STRING does not contain PATTERN.
;
;          (substring? "rat" "pirate")             =>  2
;          (substring? "rat" "outrage")            =>  #f
;          (substring? "" any-string)              =>  0

(define (substring? pattern str)
  (let* ((pat-len (string-length pattern))
         (search-span (- (string-length str) pat-len))
         (c1 (if (zero? pat-len) #f (string-ref pattern 0)))
         (c2 (if (<= pat-len 1) #f (string-ref pattern 1))))
    (cond
     ((not c1) 0)           ; empty pattern, matches upfront
     ((not c2) (string-index str c1)) ; one-char pattern
     (else                  ; matching a pattern of at least two chars
	(let outer ((pos 0))
          (cond
	    ((> pos search-span) #f)	; nothing was found thru the whole str
            ((not (char=? c1 (string-ref str pos)))
                (outer (+ 1 pos)))	; keep looking for the right beginning
            ((not (char=? c2 (string-ref str (+ 1 pos))))
                (outer (+ 1 pos)))	; could've done pos+2 if c1 == c2....
            (else                  	; two char matched: high probability
				   	; the rest will match too
		(let inner ((i-pat 2) (i-str (+ 2 pos)))
                   (if (>= i-pat pat-len) pos ; whole pattern matched
                      (if (char=? (string-ref pattern i-pat)
                                  (string-ref str i-str))
                        (inner (+ 1 i-pat) (+ 1 i-str))
                        (outer (+ 1 pos))))))))))))	; mismatch after partial match

; Here are some specialized substring? functions
;
; -- procedure+: string-prefix? PATTERN STRING
; -- procedure+: string-prefix-ci? PATTERN STRING
; checks to make sure that PATTERN is a prefix of STRING
;
;          (string-prefix? "pir" "pirate")             =>  #t
;          (string-prefix? "rat" "outrage")            =>  #f
;          (string-prefix? "" any-string)              =>  #t
;          (string-prefix? any-string any-string)      =>  #t

(define (string-prefix? pattern str)
  (let loop ((i 0))
    (cond
      ((>= i (string-length pattern)) #t)
      ((>= i (string-length str)) #f)
      ((char=? (string-ref pattern i) (string-ref str i))
        (loop (++ i)))
      (else #f))))

(define (string-prefix-ci? pattern str)
  (let loop ((i 0))
    (cond
      ((>= i (string-length pattern)) #t)
      ((>= i (string-length str)) #f)
      ((char-ci=? (string-ref pattern i) (string-ref str i))
        (loop (++ i)))
      (else #f))))

; -- procedure+: string-suffix? PATTERN STRING
; -- procedure+: string-suffix-ci? PATTERN STRING
; checks to make sure that PATTERN is a suffix of STRING
;
;          (string-suffix? "ate" "pirate")             =>  #t
;          (string-suffix? "rag" "outrage")            =>  #f
;          (string-suffix? "" any-string)              =>  #t
;          (string-suffix? any-string any-string)      =>  #t

(define (string-suffix? pattern str)
  (let loop ((i (-- (string-length pattern))) (j (-- (string-length str))))
    (cond
      ((negative? i) #t)
      ((negative? j) #f)
      ((char=? (string-ref pattern i) (string-ref str j))
        (loop (-- i) (-- j)))
      (else #f))))

(define (string-suffix-ci? pattern str)
  (let loop ((i (-- (string-length pattern))) (j (-- (string-length str))))
    (cond
      ((negative? i) #t)
      ((negative? j) #f)
      ((char-ci=? (string-ref pattern i) (string-ref str j))
        (loop (-- i) (-- j)))
      (else #f))))


; 		String case modification functions

; Return a new string made of characters of the
; original string in the lower case
(define (string-downcase str)
  (do ((target-str (make-string (string-length str))) (i 0 (++ i)))
      ((>= i (string-length str)) target-str)
      (string-set! target-str i (char-downcase (string-ref str i)))))

; Return a new string made of characters of the
; original string in the upper case
(define (string-upcase str)
  (do ((target-str (make-string (string-length str))) (i 0 (++ i)))
      ((>= i (string-length str)) target-str)
      (string-set! target-str i (char-upcase (string-ref str i)))))

; Lower the case of string's characters inplace
(define (string-downcase! str)
  (do ((i 0 (++ i))) ((>= i (string-length str)))
    (string-set! str i (char-downcase (string-ref str i)))))

; Raise the case of string's characters inplace
(define (string-upcase! str)
  (do ((i 0 (++ i))) ((>= i (string-length str)))
    (string-set! str i (char-upcase (string-ref str i)))))


; 
; -- procedure+: string->integer STR START END
;
; Makes sure a substring of the STR from START (inclusive) till END
; (exclusive) is a representation of a non-negative integer in decimal
; notation. If so, this integer is returned. Otherwise -- when the
; substring contains non-decimal characters, or when the range from
; START till END is not within STR, the result is #f.
;
; This procedure is a simplification of the standard string->number.
; The latter is far more generic: for example, it will try to read
; strings like "1/2" "1S2" "1.34" and even "1/0" (the latter causing
; a zero-divide error). Note that to string->number,  "1S2" is a valid
; representation of an _inexact_ integer (100 to be precise).
; Oftentimes we want to be more restrictive about what we consider a
; number; we want merely to read an integral label.

(define (string->integer str start end)
  (and (< -1 start end (++ (string-length str)))
    (let loop ((pos start) (accum 0))
      (cond
        ((>= pos end) accum)
        ((char-numeric? (string-ref str pos))
          (loop (++ pos) (+ (char->integer (string-ref str pos)) 
              (- (char->integer #\0)) (* 10 accum))))
        (else #f)))))


; 
; -- procedure+: string-split STRING
; -- procedure+: string-split STRING '()
; -- procedure+: string-split STRING '() MAXSPLIT
;
; Returns a list of whitespace delimited words in STRING.
; If STRING is empty or contains only whitespace, then the empty list
; is returned. Leading and trailing whitespaces are trimmed.
; If MAXSPLIT is specified and positive, the resulting list will
; contain at most MAXSPLIT elements, the last of which is the string
; remaining after (MAXSPLIT - 1) splits. If MAXSPLIT is specified and
; non-positive, the empty list is returned. "In time critical
; applications it behooves you not to split into more fields than you
; really need."
;
; -- procedure+: string-split STRING CHARSET
; -- procedure+: string-split STRING CHARSET MAXSPLIT
;
; Returns a list of words delimited by the characters in CHARSET in
; STRING. CHARSET is a list of characters that are treated as delimiters.
; Leading or trailing delimeters are NOT trimmed. That is, the resulting
; list will have as many initial empty string elements as there are
; leading delimiters in STRING.
;
; If MAXSPLIT is specified and positive, the resulting list will
; contain at most MAXSPLIT elements, the last of which is the string
; remaining after (MAXSPLIT - 1) splits. If MAXSPLIT is specified and
; non-positive, the empty list is returned. "In time critical
; applications it behooves you not to split into more fields than you
; really need."
;
; This is based on the split function in Python/Perl
;
; (string-split " abc d e f  ") ==> ("abc" "d" "e" "f")
; (string-split " abc d e f  " '() 1) ==> ("abc d e f  ")
; (string-split " abc d e f  " '() 0) ==> ()
; (string-split ":abc:d:e::f:" '(#\:)) ==> ("" "abc" "d" "e" "" "f" "")
; (string-split ":" '(#\:)) ==> ("" "")
; (string-split "root:x:0:0:Lord" '(#\:) 2) ==> ("root" "x:0:0:Lord")
; (string-split "/usr/local/bin:/usr/bin:/usr/ucb/bin" '(#\:))
; ==> ("/usr/local/bin" "/usr/bin" "/usr/ucb/bin")
; (string-split "/usr/local/bin" '(#\/)) ==> ("" "usr" "local" "bin")

(define (string-split str . rest)
		; maxsplit is a positive number
  (define (split-by-whitespace str maxsplit)
    (define (skip-ws i yet-to-split-count)
      (cond
        ((>= i (string-length str)) '())
        ((char-whitespace? (string-ref str i))
          (skip-ws (++ i) yet-to-split-count))
        (else (scan-beg-word (++ i) i yet-to-split-count))))
    (define (scan-beg-word i from yet-to-split-count)
      (cond
        ((zero? yet-to-split-count)
          (cons (substring str from (string-length str)) '()))
        (else (scan-word i from yet-to-split-count))))
    (define (scan-word i from yet-to-split-count)
      (cond
        ((>= i (string-length str))
          (cons (substring str from i) '()))
        ((char-whitespace? (string-ref str i))
          (cons (substring str from i) 
            (skip-ws (++ i) (-- yet-to-split-count))))
        (else (scan-word (++ i) from yet-to-split-count))))
    (skip-ws 0 (-- maxsplit)))

		; maxsplit is a positive number
		; str is not empty
  (define (split-by-charset str delimeters maxsplit)
    (define (scan-beg-word from yet-to-split-count)
      (cond
        ((>= from (string-length str)) '(""))
        ((zero? yet-to-split-count)
          (cons (substring str from (string-length str)) '()))
        (else (scan-word from from yet-to-split-count))))
    (define (scan-word i from yet-to-split-count)
      (cond
        ((>= i (string-length str))
          (cons (substring str from i) '()))
        ((memq (string-ref str i) delimeters)
          (cons (substring str from i) 
            (scan-beg-word (++ i) (-- yet-to-split-count))))
        (else (scan-word (++ i) from yet-to-split-count))))
    (scan-beg-word 0 (-- maxsplit)))

			; resolver of overloading...
			; if omitted, maxsplit defaults to
			; (++ (string-length str))
  (if (string-null? str) '()
    (if (null? rest) 
      (split-by-whitespace str (++ (string-length str)))
      (let ((charset (car rest))
          (maxsplit
            (if (pair? (cdr rest)) (cadr rest) (++ (string-length str)))))
        (cond 
          ((not (positive? maxsplit)) '())
          ((null? charset) (split-by-whitespace str maxsplit))
          (else (split-by-charset str charset maxsplit))))))
)


; make-char-quotator QUOT-RULES
;
; Given QUOT-RULES, an assoc list of (char . string) pairs, return
; a quotation procedure. The returned quotation procedure takes a string
; and returns either a string or a list of strings. The quotation procedure
; check to see if its argument string contains any instance of a character
; that needs to be encoded (quoted). If the argument string is "clean",
; it is returned unchanged. Otherwise, the quotation procedure will
; return a list of string fragments. The input straing will be broken
; at the places where the special characters occur. The special character
; will be replaced by the corresponding encoding strings.
;
; For example, to make a procedure that quotes special HTML characters,
; do
;	(make-char-quotator
;	    '((#\< . "&lt;") (#\> . "&gt;") (#\& . "&amp;") (#\" . "&quot;")))

(define (make-char-quotator char-encoding)
  (let ((bad-chars (map car char-encoding)))

    ; Check to see if str contains one of the characters in charset,
    ; from the position i onward. If so, return that character's index.
    ; otherwise, return #f
    (define (index-cset str i charset)
      (let loop ((i i))
	(and (< i (string-length str))
	     (if (memv (string-ref str i) charset) i
		 (loop (++ i))))))

    ; The body of the function
    (lambda (str)
      (let ((bad-pos (index-cset str 0 bad-chars)))
	(if (not bad-pos) str	; str had all good chars
	    (let loop ((from 0) (to bad-pos))
	      (cond
	       ((>= from (string-length str)) '())
	       ((not to)
		(cons (substring str from (string-length str)) '()))
	       (else
		(let ((quoted-char
		       (cdr (assv (string-ref str to) char-encoding)))
		      (new-to 
		       (index-cset str (++ to) bad-chars)))
		  (if (< from to)
		      (cons
		       (substring str from to)
		       (cons quoted-char (loop (++ to) new-to)))
		      (cons quoted-char (loop (++ to) new-to))))))))))
))


;------------------------------------------------------------------------
;			EXEC-PATH facility
; The EXEC-PATH is the path used to load .scm files or run executable files
; with.
; 
; The path could be specified as an env-variable SCMPATH
; if this env variable isn't set, the path of the executable file
; is used

; (define EXEC-PATH:PATH 

;     	; extract the path from argv[0] and make it absolute
;   (letrec ((extract-argv0-path
;         (lambda ()
;           (let ((argv0-rel-path
;                 (let ((argv0 (vector-ref argv 0)))
;               	; reverse search for the last PATH-SEPARATOR-CHAR
;                   (let loop ((i (-- (string-length argv0))))
;                     (cond
;                       ((not (positive? i)) #f)
;                       ((char=? (string-ref argv0 i) PATH-SEPARATOR-CHAR)
;                         (substring argv0 0 (+ i 1)))
;                       (else (loop (-- i)))))))
;               (curr-path (OS:getcwd)))
;             (cond
;               ((not argv0-rel-path) curr-path)
;           		; check if argv0-rel-path was an absolute path
;               ((char=? PATH-SEPARATOR-CHAR (string-ref argv0-rel-path 0))
;                 argv0-rel-path)
;               (else (string-append curr-path (string PATH-SEPARATOR-CHAR)
;                   argv0-rel-path))))))
              
; 		; cached path
;       (path-itself #f)
;       		; returned the cached path
;       (get-path (lambda () path-itself))
      
;       		; Compute the cached path, runs only the first time around
;       (set-path
;         (lambda ()
;           (let ((path-being-computed 
;                 (or (OS:getenv "SCMPATH") (extract-argv0-path))))
;             (assert path-being-computed)
;             (set! path-itself
;               (if (char=? PATH-SEPARATOR-CHAR
;                   (string-ref path-being-computed
;                     (-- (string-length path-being-computed))))
;                 path-being-computed
;            ; make sure path-being-computed ends in PATH-SEPARATOR-CHAR
;                 (string-append path-being-computed (string PATH-SEPARATOR-CHAR))))
;             (cerr nl "EXEC PATH is set to " path-itself nl)
;             (set! EXEC-PATH:PATH get-path)
;             (get-path)))))
    
;     set-path))

    

; (define (EXEC-PATH:load scm-file-name)
;   (load (string-append (EXEC-PATH:PATH) scm-file-name)))

; (define (EXEC-PATH:help)
;   (cerr nl "Environment variable SCMPATH could be set up to point" nl)
;   (cerr "to a directory containing dynamically loadable dictionaries/conf files" nl)
;   (cerr "if this variable is not set, the directory where this executable resides" nl)
;   (cerr "will be used" nl nl)
;   (cerr "The current path is set to " (EXEC-PATH:PATH) nl nl))

; ; "shell-out" to run a separately compiled executable with given
; ; arguments, from the current EXEC-PATH
; ; all 'args' (which must be strings) are "concatenated" together
; ; to form a command-line for the executable
; (define (EXEC-PATH:system executable . args)
;   (let ((command-line 
;         (apply string-append
;           (cons (EXEC-PATH:PATH) (cons executable (cons " " args))))))
;     (cerr "about to execute: " command-line nl)
;     (OS:system command-line)))

