;; Implementation of time-apply

; (lambda f argv)
; Applies procedure `f' to its argument list `argv'
; Returns:  (cons f-result exec-time)
; f-result - result produced by applying procedure `f'
; exec-time - time in milliseconds taken by `f' to execute
(define sxml:time-apply
  (cond-expand
   (plt
    (lambda (f argv)
      (let-values
          (((f-res-lst t1 t2 t3) (time-apply f argv)))
        (cons (car f-res-lst) t2))))
   (gambit
    (lambda (f argv)
      (let ((t-begin (time->seconds (current-time))))
        (let ((f-res (apply f argv)))
          (let ((t-end (time->seconds (current-time))))
            (cons f-res (* (- t-end t-begin) 1000)))))))))
