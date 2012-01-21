; current affection: 1 - positive ; 0 - neutral; -1 - negative
(defparameter *day-1-j* 1); In the begining Julia likes Romeo
(defparameter *day-1-r* 0); In the begining Romeno is neutral towards Julia

(defparameter *day-1-jt* 0); In the begining Juliette is neutral towars Roberto
(defparameter *day-1-rt* 1); In the begining Roberto

; Influence factor saying how much Julia and Juliette influence each other
; 0 <= *s* <= 1; 0: no influence, 1: total influence
(defparameter *s* 0.9)

; Influence factor saying how much Romeo and Roberto influence each other
; 0 <= *p* <= 1; 0: no influence, 1: total influence
(defparameter *p* 0.1)

; Basic function for Julia
(defun j (n) (if (= n 1) *day-1-j* (j-n1 (- n 1))))

; Basic function for Romeo
(defun r (n) (if (= n 1) *day-1-r* (r-n1 (- n 1))))

; Basic function for Juliette
(defun jt (n) (if (= n 1) *day-1-jt* (jt-n1 (- n 1))))

; Basic function for Roberto
(defun rt (n) (if (= n 1) *day-1-rt* (rt-n1 (- n 1))))


(load "relationship-functions.lisp")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Average and Difference functions for today

; J+(n) = (J(n) + J'(n)) / 2
(defun j-jt-avrg (n)  (/ (+ (j n) (jt n)) 2))

; J-(n) = (J(n) - J'(n)) / 2
(defun j-jt-diff (n)  (/ (- (j n) (jt n)) 2))

; R+(n) = (R(n) + R'(n)) / 2
(defun r-rt-avrg (n)  (/ (+ (r n) (rt n)) 2))

; R-(n) = (R(n) - R'(n)) / 2
(defun r-rt-diff (n)  (/ (- (r n) (rt n)) 2))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Average and Difference functions for the next day

; J+(n+1) = J+(n) + R+(n)
;(defun j-jt-avrg-n1 (n) (+ (j-jt-avrg-n1 (- n 1)) (r-rt-avrg-n1 (- n 1))))
 (defun j-jt-avrg-n1 (n) (+ (j-jt-avrg    (- n 1)) (r-rt-avrg    (- n 1))))

; R+(n+1) = -J+(n)
;(defun r-rt-avrg-n1 (n) (* -1 (j-jt-avrg-n1 (- n 1))))
 (defun r-rt-avrg-n1 (n) (* -1 (j-jt-avrg    (- n 1))))

; J-(n+1) = (1 -2*s) * (J-(n) + R-(n))
(defun j-jt-diff-n1 (n) 
(* (- 1 (* 2 *s*))
;  (+ (j-jt-diff-n1 (- n 1)) (r-rt-diff-n1 (- n 1)))))
   (+ (j-jt-diff    (- n 1)) (r-rt-diff    (- n 1)))))

; R-(n+1) = -J-(n)
;(defun r-rt-diff-n1 (n) (* -1 (j-jt-diff-n1 (- n 1))))
 (defun r-rt-diff-n1 (n) (* -1 (j-jt-diff    (- n 1))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun print-all (n)
  (print-j-jt n)
  (print-r-rt n)
  (print-abs-diffs n)
)

(defun print-j-jt (n)
(format t "Day: ~a; Julia, Juliette: ~a; ~a; Diff: ~a~%" n (j n) (jt n) (j-jt-diff n)))

(defun print-r-rt (n)
(format t "Day: ~a; Romeo; Roberto:  ~a; ~a; Diff: ~a~%" n (r n) (rt n) (r-rt-diff n)))

(defun print-abs-diffs (n)
(format t "Day: ~a; Romeo-Julia-Diff: ~a; Roberto-Juliette-Diff: ~a~%" 
	n
        (abs (- (r n) (j n)))
        (abs (- (rt n) (jt n)))))

