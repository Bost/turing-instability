; current affection: 1 - positive ; 0 - neutral; -1 - negative
(defparameter *day-1-j* 1); In the begining Julia likes Romeo
(defparameter *day-1-r* 0); In the begining Romeno is neutral towards Julia

(defparameter *day-1-jt* 0); In the begining Juliette is neutral towars Roberto
(defparameter *day-1-rt* 1); In the begining Roberto likes Juliette

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

; Julia & Juliette Averadge day n:   J+(n) = (J(n) + J'(n)) / 2
(defun j-jt-avrg (n)  (/ (+ (j n) (jt n)) 2))

; Julia & Juliette Difference day n: J-(n) = (J(n) - J'(n)) / 2
(defun j-jt-diff (n)  (/ (- (j n) (jt n)) 2))

; Romeo & Roberto Averadge day n:    R+(n) = (R(n) + R'(n)) / 2
(defun r-rt-avrg (n)  (/ (+ (r n) (rt n)) 2))

; Romeo & Roberto Difference day n:  R-(n) = (R(n) - R'(n)) / 2
(defun r-rt-diff (n)  (/ (- (r n) (rt n)) 2))

(load "relationship-functions.lisp")

(defun print-all (n)
  (print-j-jt n)
  (print-r-rt n)
  (print-abs-diffs n)
)

(defun print-j-jt (n)
(format t "Day: ~2d; Julia: ~13a, Juliette: ~13a; Diff: ~13a~%" n (j n) (jt n) (j-jt-diff n)))

(defun print-r-rt (n)
(format t "Day: ~2d; Romeo: ~13a; Roberto:  ~13a; Diff: ~13a~%" n (r n) (rt n) (r-rt-diff n)))

(defun print-abs-diffs (n)
(format t "Day: ~2d; Diff:  ~13a; Diff:     ~13a~%" 
	n
        (abs (- (r n) (j n)))
        (abs (- (rt n) (jt n)))))

; Translate the relationship value to words a la: like, dislike, ignore, hate, etc
; Note: This is propably the ugly imperative part of the program 
(defun qualify (val who whom)
(format t "~a ~a ~a ~%"
    who
    (cond ((<  val 0.5) "dislikes")
          ((>= val 0.5) "likes"))
    whom))
