; Verification code for myenv.scm and other my standard "environments"

; For Bigloo, you should evaluate or compile vmyenv-bigloo.scm,
; which contains a module declaration that includes the present file.
; For SCM, (load "myenv-scm.scm") as well as env.scm and util.scm
; before evaluating this file

; $Id$

(include "myenv.scm")

(include "catch-error.scm")


(cerr nl "Verifying increment/decrement operators: ++, ++! etc..." nl)
(let
  ((x 0))
  (assert (= (++ x) 1))
  (assert (= (-- x) -1))
  (assert (begin (++! x) (= x 1)))
  (assert (begin (--! x) (--! x) (zero? (++ x))))
)

(cerr nl "Verifying begin0..." nl)
(let
  ((x 0))
  (assert (= x (begin0 x)))
  (assert (= 0 (begin0 x 1)))
  (assert (= 1 (begin x 1)))
  (assert (= 0 (begin0 x (++! x) x)))
  (assert (= 2 (begin x (++! x) x)))
)


(cerr nl "Verifying extended branching instructions..." nl)
(let
  ((x 0))

  (assert (= 2 (when (zero? x) (++! x) (++ x))))
  (assert (not (when (zero? x) (++! x))))
  (assert (= x 1))
  (whennot (zero? x) (--! x))
  (assert (zero? x))
  (assert (zero? (whennot (positive? x) (--! x) (++ x))))
  (assert (whennot (negative? x) (++ x)))
)

(cerr nl "Verifying assert..." nl)
(let
  ((x 1))

  (assert (eq? (positive? x) (assert (positive? x))))
  (assert (eq? x (assert x report: x)))
  (assert (eq? x (assert 0 x)))

  (assert (failed? (assert (zero? x))))
  (assert (failed? (assert (zero? x) report: "failure")))
  (assert (failed? (assert (zero? x) report: "failure" x (+ x 1) "!")))
  (assert (failed? 
	   (let ((y 2)) 
	     (assert (let ((z x)) (positive? z)) (positive? y) (zero? x)
		     report: "failure" x (+ x 1)))))
  (assert (failed? 
	   (let ((y 2)) 
	     (assert (let ((z x)) (positive? z)) (positive? y) (zero? x)
		     ))))
)

(cerr nl "Verifying values and let-values*" nl)
(let
  ()
  ; R5RS example
  (assert (= 5
	     (call-with-values (lambda () (values 4 5))
	       (lambda (a b) b))))
  (assert (= 4
	     (call-with-values (lambda () (values 4))
	       (lambda (b) b))))
  (assert (= 7
	     (call-with-values (lambda () (values))
	       (lambda () 7))))
  (assert (= 140
	     (call-with-values (lambda () (values 4 5 7)) *)))
  ; R5RS example
  ;(call-with-values * -)
  (assert (= -1
	     (call-with-values (lambda () (values (*))) -)))
  (pp
   (lambda () (let-values* ((a 1) (b 2)) (+ a b))))
  (assert (= 3
	     (let-values* ((a 1) (b 2)) (+ a b))))
  (pp
   (lambda () (let-values* ((a 1) ((b) 2)) (+ a b))))
  (assert (= 3
	     (let-values* ((a 1) (b 2)) (+ a b))))
  (pp
   (lambda () 
     (let-values* ((a 1) ((b) 2) ((c d) (values 3 4))) (+ a b (* c d)))))
  (assert (= 15
     (let-values* ((a 1) ((b) 2) ((c d) (values 3 4))) (+ a b (* c d)))))
  (pp
   (lambda () 
     (let-values* ((a 1) ((b) 2) ((c d e) (values 1 2 3))) (+ a b (* c d e)))))
  (assert (= 63
     (let-values* ((a 1) ((b) 2) ((c d e) (values 3 4 5))) (+ a b (* c d e)))))
  (pp
   (lambda () 
     (let-values* ((a 1) ((c d e) (values 3 4 5)) ((b) d)) (+ a b (* c d e)))))
  (assert (= 65
     (let-values* ((a 1) ((c d e) (values 3 4 5)) ((b) d)) (+ a b (* c d e)))))
)

(cerr nl "Verifying cond-expand: SRFI-0" nl)
(let
  ()
  (cond-expand
   (gambit (cout "Expanded in Gambit\n"))
   (else #f))
  (cond-expand
   (scm (cout "Expanded in SCM\n"))
   (else #f))
  (cond-expand
   (mit-scheme (cout "Expanded in MIT Scheme\n"))
   (else #f))
  (cond-expand
   (bigloo (cout "Expanded in Bigloo" nl))
   (else #f))

  (assert (= 1
	     (+
	      (cond-expand (gambit 1) (else 0))
	      (cond-expand (scm 1) (else 0))
	      (cond-expand (mit-scheme 1) (else 0))
	      (cond-expand (bigloo 1) (else 0)))))
  (cond-expand
   (gambit (assert (failed? (cond-expand ((not gambit) #t)))))
   (else #t))
  (assert (memv
	   (cond-expand
	    (gambit 0 1)
	    (scm 0 2)
	    (bigloo 0 3)
	    (mit-scheme 0 4)
	    (else 0))
	   '(1 2 3 4)))
  (assert (memv
	   (cond-expand
	    ((and bigloo srfi-0) 0 3)
	    ((and gambit srfi-0) 0 1)
	    ((and scm srfi-0) 0 2)
	    ((and mit-scheme srfi-0) 0 4)
	    (else 0))
	   '(1 2 3 4)))
  (assert (memv
	   (cond-expand
	    ((or xxx gambit zzz) 0 1)
	    ((or xxx scm zzz) 0 2)
	    ((or xxx bigloo zzz) 0 3)
	    ((or xxx mit-scheme zzz) 0 4)
	    (else 0))
	   '(1 2 3 4)))
  (assert (memv
	   (cond-expand
	    ((not gambit) 0 1)
	    ((not scm) 0 2)
	    ((not mit-scheme) 0 4)
	    ((not bigloo) 0 3)
	    (else 0))
	   '(1 2)))
  (assert (memv
	   (cond-expand
	    ((or (not gambit) (and gambit gambit)) 0 1)
	    ((or (not scm) (and scm scm)) 0 2)
	    ((or (not mit-scheme) (and mit-scheme mit-scheme)) 0 4)
	    ((or (not bigloo) (and bigloo bigloo)) 0 3)
	    (else 0))
	   '(1 2 3 4)))
  (assert (cond-expand (xxx (/ 1 0)) (else #t)))
  (assert (cond-expand ((not xxx) #t)))
  (cond-expand
   ((not bigloo)	; due to a cond-expand bug in Bigloo 2.2b
    (assert (cond-expand ((not (and gambit scm mit-scheme)) #t))))
   (else #t))
  (assert (cond-expand ((or xxx (not xxx)) #t)))
  (assert (cond-expand ((and (not xxx) xxx) (/ 1 0)) (else #t)))
  (assert
   (cond-expand
    (gambit (positive? +inf.))  ; works only in Gambit
    (scm (procedure? try-load)) ; works only in SCM
    (bigloo (<fx 1 2))		; works only in Bigloo
    (mit-scheme (fix:+ 1 2))	; works only in MIT-Scheme
    (else #f)))
  (assert
   (cond-expand
    ((or gambit scm mit-scheme) 
     (char=? #\newline (string-ref "\n" 0)))
    ((or scm bigloo mit-scheme) (eq? 'a 'A))
    (else #f)))
)

(cerr nl "Verifying assoc-functions with a default clause..." nl)
(let
  ((alist '((a 1) (b 2) (3 4) ("cde" f))))

  (assert (= 1 (cadr (assq-def 'a alist #f))))
  (assert (= 2 (cadr (assq-def 'b alist))))
  (assert (failed? (assq-def 'c alist)))	; would fail
  (assert (not (assq-def 'c alist #f)))
  (assq-def 'c alist (lambda () (cerr "message: key not found" nl)))

  (assert (= 1 (cadr (assv-def 'a alist #f))))
  (assert (= 2 (cadr (assv-def 'b alist))))
  (assert (= 4 (cadr (assv-def 3 alist))))
  (assert (failed? (assv-def 'c alist)))	; would fail
  (assert (not (assv-def 456 alist #f)))
  (assv-def 'c alist (lambda () (cerr "message: key not found" nl)))

  (assert (= 1 (cadr (assoc-def 'a alist #f))))
  (assert (= 2 (cadr (assoc-def 'b alist))))
  (assert (= 4 (cadr (assoc-def 3 alist))))
  (assert (eq? 'f (cadr (assoc-def "cde" alist))))
  (assert (failed? (assoc-def 'c alist)))	; would fail
  (assert (not (assoc-def 456 alist #f)))
  (assoc-def 'c alist (lambda () (cerr "message: key not found" nl)))
)

(cerr nl "Verifying list-intersperse and list-intersperse! ..." nl)
(let ((test-l '(4 5 "7" (9))))
  (assert (equal? '() (list-intersperse '() 1)))
  (assert (equal? '(4) (list-intersperse '(4) 1)))
  (assert (equal? '(4 1 5) (list-intersperse '(4 5) 1)))
  (assert (equal? '(4 #\, 5 #\, "7" #\, #\9)
      (list-intersperse '(4 5 "7" #\9) #\,)))
  (let ((test-clone (append test-l '())))
    (assert (equal? '(4 () 5 () "7" () (9))
        (list-intersperse test-clone '())))
    (assert (equal? test-l test-clone)))

  (assert (equal? '() (list-intersperse! '() 1)))
  (assert (equal? '(4) (list-intersperse! '(4) 1)))
  (assert (equal? '(4 1 5) (list-intersperse! '(4 5) 1)))
  (assert (equal? '(4 #\, 5 #\, "7" #\, #\9)
      (list-intersperse! '(4 5 "7" #\9) #\,)))
  (let ((test-clone (append test-l '()))
        (test-result '(4 () 5 () "7" () (9))))
    (assert (equal? test-result
        (list-intersperse! test-clone '())))
    (assert (not (equal? test-l test-clone)))
    (assert (equal? test-clone test-result)))
)

(cerr nl "Verifying list-tail-diff ..." nl)
(let ((test-l '(4 5 "7" (9))))
  (assert (equal? (list-tail-diff test-l '()) test-l))
  (assert (not (eq? (list-tail-diff test-l '()) test-l)))
  (assert (equal? (list-tail-diff test-l (append test-l '())) test-l))
  (assert (equal? (list-tail-diff test-l test-l) '()))
  (assert (equal? (list-tail-diff test-l (cdr test-l))  (list (car test-l))))
  (assert (equal? (list-tail-diff test-l (cddr test-l)) (list (car test-l) (cadr test-l))))
  (assert (equal? (list-tail-diff test-l (cdddr test-l)) 
        (list (car test-l) (cadr test-l) (caddr test-l))))
  (let ((test-l-copy (append test-l '())))
    (assert (equal? test-l-copy test-l))
    (assert (not (eq? test-l-copy test-l)))
    (set-car! (list-tail-diff test-l-copy (cdr test-l-copy)) "***")
    (assert (equal? test-l-copy test-l))
    )
)

(cerr nl "Verifying any? ..." nl)
(let ()
  (define (test-driver pred? coll expected-result)
    (let ((res (any? pred? coll)))
      (if (not (eqv? res expected-result))
	  (error "computed result " res "differs from the expected one "
		 expected-result))))
  (define (eq-a? x) (if (char=? x #\a) x #f))
  (define (gt1? x) (if (> x 1) x #f))
  
  (cerr "   finding an element in a list" nl)
  (test-driver gt1? '(1 2 3 4 5) 2)
  (test-driver gt1? '(1 1 1 1 1) #f)
  (test-driver gt1? '(4 1 1 1 1) 4)
  (test-driver gt1? '(4 5 6 1 9) 4)
  (test-driver gt1? '(-4 -5 -6 1 9) 9)
  (test-driver eq-a? '(#\b #\c #\a #\k) #\a)
  
  (cerr "   finding an element in a vector" nl)
  (test-driver gt1? '#(1 2 3 4 5) 2)
  (test-driver gt1? '#(1 1 1 1 1) #f)
  (test-driver gt1? '#(4 1 1 1 1) 4)
  (test-driver gt1? '#(4 5 6 1 9) 4)
  (test-driver gt1? '#(-4 -5 -6 1 9) 9)
  (test-driver eq-a? '#(#\b #\c #\a #\k) #\a)
)

(cerr nl "Verifying our environments..." nl)
(cerr nl nl "verifying environments")
(env.print "Initial environment")
(cerr "adding a few bindings..." nl)
(env.bind a 1)
(env.bind b-1 'c)
(env.bind cc "c c")
(env.bind dd '(1 3 5 (6) ()))
(cerr nl "The resulting environment. Now trying to get the stuff back..." nl)
(assert (= 1 (env.find a)))
(assert (not (env.find b)))
; (env.demand b)
(assert (failed? (env.demand b)))
(assert (eq? 'c (env.demand b-1)))
(assert (string=? "c c" (env.demand cc)))
(assert (equal? '(1 3 5 (6) ()) (env.find dd)))
(let ((alist (env.->alist)))
  (cerr "\nThe environment exported as an assoc-list\n")
  (pp alist)
  (assert (equal? alist '((dd 1 3 5 (6) ()) (cc . "c c") (b-1 . c) (a . 1))))
)
(let
  ((mark (env.mark)) (capture #f))
  (cerr "placing mark " mark nl)
  (env.bind* '((a . 3) (b . #(1 2 1/4))))
  (assert (= 3 (env.find a)))
  (assert (equal? '#(1 2 1/4) (env.demand b)))
  (env.print "after adding the mark")
  (let ((another-mark (env.mark)))
    (env.bind a 4)
    (env.print "after adding another mark " another-mark)
    (assert (= 4 (env.find a)))
    (cerr "capturing the env" nl)
    (set! capture (env.capture! another-mark "Captured Env"))
    (assert (= 3 (env.demand a)))
    (env.extend capture)
    (env.print "after putting the captured env back")
    )
  (assert (= 4 (env.find a)))
  (cerr "flushing through the mark " mark)
  (env.flush! mark)
  (env.print)
  (assert (= 1 (env.find a)))
  (assert (failed? (env.flush! mark)))
  (assert (= -1 
      (env.with capture
        (lambda ()
          (env.print "temporarily extended env")
          (assert (= 4 (env.find a)))
          (assert (failed? (env.demand b)))
          -1))))
  (assert (= 1 (env.find a)))
  (assert (= -3 
      (env.with-exclusive capture
        (lambda ()
          (env.print "temporarily replaced env")
          (assert (= 4 (env.find a)))
          (assert (failed? (env.demand dd)))
          -3))))
  (assert (= 1 (env.find a)))
  (assert (equal? '(1 3 5 (6) ()) (env.find dd)))
)

(cerr nl "All tests passed" nl)
