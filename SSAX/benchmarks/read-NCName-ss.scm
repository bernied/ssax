;*****************************************************************
; read-NCName-ss
; A version of SSAX:read-NCName that uses next-token-of-as-symb
;
; $Id$


(directives
	; Adding substring->symbol implemented in substring-symbol.c
	(extern
	 (macro substring->symbol::symbol (::string ::int ::int)    
				    "substring_to_symbol"))
	(pragma
	 (substring->symbol no-cfa-top nesting))
)

; The same as next-token-of with the exception that it returns its result
; as a symbol
; (define (next-token-of-as-symb incl-list/pred port)
;   (let* ((buffer (input-parse:init-buffer))
; 	 (curr-buf-len (string-length buffer)) (quantum 16))
;   (if (procedure? incl-list/pred)
;     (let loop ((i 0) (c (peek-char port)))
;       (cond
;         ((incl-list/pred c) =>
;           (lambda (c)
;             (if (>= i curr-buf-len)	; make space for i-th char in buffer
;               (begin			; -> grow the buffer by the quantum
;                 (set! buffer (string-append buffer (make-string quantum)))
;                 (set! quantum curr-buf-len)
;                 (set! curr-buf-len (string-length buffer))))
;             (string-set! buffer i c)
;             (read-char port)			; move to the next char
;             (loop (++ i) (peek-char port))))
;         (else (substring->symbol buffer 0 i))))
; 			; incl-list/pred is a list of allowed characters
;     (let loop ((i 0) (c (peek-char port)))
;       (cond
;         ((not (memq c incl-list/pred)) (substring->symbol buffer 0 i))
;     	(else
;     	  (if (>= i curr-buf-len)	; make space for i-th char in buffer
;     	    (begin			; -> grow the buffer by the quantum
;     	      (set! buffer (string-append buffer (make-string quantum)))
;     	      (set! quantum curr-buf-len)
;     	      (set! curr-buf-len (string-length buffer))))
;     	  (string-set! buffer i c)
;     	  (read-char port)			; move to the next char
;     	  (loop (++ i) (peek-char port))
;     	  ))))))

(define (next-token-of-as-symb incl-list/pred port)
  (let* ((buffer (input-parse:init-buffer))
	 (curr-buf-len (string-length buffer)) (quantum 16))
  (if (procedure? incl-list/pred)
    (let loop ((i 0) (c (peek-char port)))
      (cond
        ((incl-list/pred c) =>
          (lambda (c)
            (if (>= i curr-buf-len)	; make space for i-th char in buffer
              (begin			; -> grow the buffer by the quantum
                (set! buffer (string-append buffer (make-string quantum)))
                (set! quantum curr-buf-len)
                (set! curr-buf-len (string-length buffer))))
            (string-set! buffer i c)
            (read-char port)			; move to the next char
            (loop (++ i) (peek-char port))))
        (else (string->symbol (substring buffer 0 i)))))
			; incl-list/pred is a list of allowed characters
    (let loop ((i 0) (c (peek-char port)))
      (cond
        ((not (memq c incl-list/pred)) (string->symbol (substring buffer 0 i)))
    	(else
    	  (if (>= i curr-buf-len)	; make space for i-th char in buffer
    	    (begin			; -> grow the buffer by the quantum
    	      (set! buffer (string-append buffer (make-string quantum)))
    	      (set! quantum curr-buf-len)
    	      (set! curr-buf-len (string-length buffer))))
    	  (string-set! buffer i c)
    	  (read-char port)			; move to the next char
    	  (loop (++ i) (peek-char port))
    	  ))))))

; A version of SSAX:read-NCName that uses next-token-of-as-symb

; Read a NCName starting from the current position in the PORT and
; return it as a symbol.
(define (SSAX:read-NCName port)
  (let ((first-char (peek-char port)))
    (or (SSAX:ncname-starting-char? first-char)
      (parser-error port "XMLNS [4] for '" first-char "'")))
    (next-token-of-as-symb
      (lambda (c)
        (cond
          ((eof-object? c) #f)
          ((char-alphabetic? c) c)
          ((string-index "0123456789.-_" c) c)
          (else #f)))
      port))


