; Julia next day:    J(n+1) = J(n) + R(n) + s*[J'(n) - J(n)] + s*[R(n) - R'(n)]
(defun j-n1 (n)
(+ (j n) (r n)
   (* *s*
      (- (jt n) (j n)))))

; Romeo next day:    R(n+1) = -J(n)
(defun r-n1 (n) (* -1 (j n)))

; Juliette next day: J'(n+1) = J'(n) + R'(n) + s*[J(n) - J'(n)] + s*[R(n) - R'(n)]
(defun jt-n1 (n)
(+ (jt n) (rt n))
   (* *s* (- (j n) (jt n))
   (* *s* (- (r n) (rt n)))))

; Roberto next day:  R'(n+1) = -J'(n)
(defun rt-n1 (n) (* -1 (jt n)))

; Julia & Juliette Averadge next day:   J+(n+1) = J+(n) + R+(n)
(defun j-jt-avrg-n1 (n) (+ (j-jt-avrg n) (r-rt-avrg n)))

; Romeo & Roberto Averadge next day:    R+(n+1) = - J+(n)
(defun r-rt-avrg-n1 (n) (* -1 (j-jt-avrg n)))

; Julia & Juliette Difference next day: J-(n+1) = (1 - 2*s) * [J-(n) + R-(n)]
(defun j-jt-diff-n1 (n)
(* (- 1 (* 2 *s*))
   (+ (j-jt-diff n) (r-rt-diff n))))

; Romeo & Roberto Difference next day:  R-(n+1) = - J-(n)
(defun r-rt-diff-n1 (n) (* -1 (j-jt-diff n)))

