; Implementation of the structure parser-errors-vanilla
; for systems other than Scheme48 and PLT Scheme
; $Id$

(define (parser-error port message . rest)
  (apply error message rest))
