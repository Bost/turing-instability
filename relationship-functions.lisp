; Julia's next day if not influenced by Juliette:
;      J(n+1) = J(n) + R(n);             (defun j-n1 (n) (+ (j n) (r n)))
;
; Julia's next day if influenced by Juliette by the factor of *s*:
;      J(n+1) = J(n) + R(n) + s*[J'(n) - J(n)] + s*[R(n) - R'(n)]
(defun j-n1 (n)
(+ (j n) (r n)
   (* *s*
      (- (jt n) (j n)))))

; Romeo's next day if not influenced by Roberto:
;      R(n+1) = -J(n);                   (defun r-n1 (n) (-(j n)))
;
; Romes's next day if influenced by Roberto by the factor of *p*:
;      R(n+1) = -J(n) - p*[R'(n) - R(n)]
(defun r-n1 (n) 
(- (-(j n))
   (* *p* 
      (- (rt n) (r n)))))

; Juliette next day if not influenced by Julia:
;      J'(n+1) = J'(n) + R'(n)           (defun jt-n1 (n) (+ (jt n) (rt n)))
;
; Juliette's next day if influenced by Julia by the factor of *s*:
;      J'(n+1) = J'(n) + R'(n) + s*[J(n) - J'(n)] + s*[R(n) - R'(n)]
(defun jt-n1 (n)
(+ (jt n) (rt n))
   (* *s* (- (j n) (jt n))
   (* *s* (- (r n) (rt n)))))

; Roberto's next day if not influenced by Romeo:
;      R'(n+1) = -J'(n)                  (defun rt-n1 (n) (-(jt n)))
;
; Roberto' next day if influenced by Romeo by the factor of *p*:
;      R'(n+1) = -J(n) - p*[R(n) - R'(n)]
(defun rt-n1 (n)
(- (-(j n))
   (* *p*
      (- (r n) (rt n)))))

; Julia & Juliette Difference next day if they influence each other by the factor of *s*:
;      J-(n+1) = J-(n) + R-(n)           (defun j-jt-diff-n1 (n) (+ (j-jt-diff n) (r-rt-diff n)))
; Julia & Juliette Difference next day if they don't influence each other:
;      J-(n+1) = (1 - 2*s) * [J-(n) + R-(n)]
(defun j-jt-diff-n1 (n)
(* (- 1 (* 2 *s*))
   (+ (j-jt-diff n) (r-rt-diff n))))

; Romeo & Roberto Difference next day if they influence each other:
;      R-(n+1) = -[J-(n)]                (defun r-rt-diff-n1 (n) (-(j-jt-diff n)))
; Romeo & Roberto Difference for the next day if they don't influence each other:
;      R-(n+1) = -(1 - 2*p) * J-(n)
(defun r-rt-diff-n1 (n)
(* (-(- 1 (* 2 *p*))
   (j-jt-diff n))))

; Julia & Juliette Averadge for the next day if they don't influence each other:
;      J+(n+1) = J+(n) + R+(n)
;(defun j-jt-avrg-n1 (n) (+ (j-jt-avrg n) (r-rt-avrg n)))

; Romeo & Roberto Averadge for the next day if they don't influence each other:
;      R+(n+1) = -[J+(n)]
;(defun r-rt-avrg-n1 (n) (-(j-jt-avrg n)))

