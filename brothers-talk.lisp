; Julia next day:    J(n+1) = J(n) + R(n)
(defun j-n1 (n) (+ (j n) (r n)))

; Romeo next day:    R(n+1) = -J(n) - p*( R'(n) - R(n) )
(defun r-n1 (n) 
(+ (* -1 (j n))
   (* -1
      *p* 
      (- (rt n) (r n)))))

; Juliette next day: J'(n+1) = J'(n) + R'(n)
(defun jt-n1 (n) (+ (jt n) (rt n)))

; Roberto next day:  R'(n+1) = -J(n) - p*( R(n) - R'(n) )
(defun rt-n1 (n)
(+ (* -1 (j n))
   (* -1
      *p*
      (- (r n) (rt n)))))

; Julia & Juliette Averadge next day:   J+(n+1) = J+(n) + R+(n)
(defun j-jt-avrg-n1 (n) (+ (j-jt-avrg n) (r-rt-avrg n)))

; Romeo & Roberto Averadge next day:    R+(n+1) = - J+(n)
(defun r-rt-avrg-n1 (n) (* -1 (j-jt-avrg n)))

; Julia & Juliette Difference next day: J-(n+1) = J-(n) + R-(n)
(defun j-jt-diff-n1 (n) (+ (j-jt-diff n) (r-rt-diff n)))

; Romeo & Roberto Difference next day:  R-(n+1) = - (1 - 2*p) * J-(n)
(defun r-rt-diff-n1 (n)
(* (* -1
       (- 1
          (* 2 *p*))
   (j-jt-diff n))))

