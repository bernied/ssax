#	    Building the library module for some systems
#
# $Id$

# A scheme interpreter to use
SCMI=gsi
# Other possible interpreters
#SCMI=scm
#SCMI=bigloo


clean:
	rm -f SSAX-code.scm *.o

# Strip away all the comments and built-in tests from SSAX.scm
SSAX-code.scm: SSAX.scm
	echo "\
	(call-with-input-file \"SSAX.scm\" 		\
	  (lambda (port) 		   		\
	   (call-with-output-file \"$@\"		\
	     (lambda (o-port)				\
	       (let loop ()		   		\
	         (let ((expr (read port)))	  	\
	           (cond 			   	\
		     ((eof-object? expr) #t)  		\
		     ((and (pair? expr) (eq? (car expr) 'run-test)) \
		      (loop))		   		\
		     (else (write expr o-port)		\
			   (newline o-port) (loop)))))))))" | $(SCMI)

