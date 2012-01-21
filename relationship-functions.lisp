;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Romeo and Julia

; J(n+1) = J(n) + R(n) + s*(J'(n) - J(n)) + s*(R'(n) - R(n))
(defun j-n1 (n)
(+ (j n) (r n)
   (* *s* (- (jt n) (j n)))
   (* *s* (- (rt n) (r n)))))

; R(n+1) = R|(n) - J(n)
;(defun r-n1 (n) (* -1 (j n)))
; R(n+1) = -J(n) - p*( R'(n) - R(n) )
(defun r-n1 (n) 
(+ (* -1 (j n))
   (* -1
      *p* 
      (- (rt n) (r n)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Roberto and Juliette

; J'(n+1) = J'(n) + R'(n) + s*(J(n) - J'(n)) + s*(R(n) - R'(n))
(defun jt-n1 (n)
(+ (jt n) (rt n)
   (* *s* (- (j n) (jt n)))
   (* *s* (- (r n) (rt n)))))

; R'(n+1) = -J'(n)
;(defun rt-n1 (n) (* -1 (jt n)))
; R'(n+1) = -J(n) - p*( R(n) - R'(n) )
(defun rt-n1 (n)
(+ (* -1 (j n))
   (* -1
      *p*
      (- (r n) (rt n)))))

