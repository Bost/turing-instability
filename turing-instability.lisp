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

; relationship threshold to divide like/ignore/dislike areas
(defparameter *threshold* 0.333)
(defparameter *difference-threshold* 0.5)

; Translate the relationship value to words a la: like, dislike, ignore, hate, etc
; Note: This is propably the ugly imperative part of the program
(defun qualify (val subject-who object-whom)
  (format nil "~a (feelig-val: ~a) ~a ~a"
		  subject-who
		  val
		  (cond ((<  val (* -1 *threshold*)) "feels unhappy with")
				((>= val (*  1 *threshold*)) "feels happy with")
				(t                           "feels fine with"))
		  object-whom))

(defun qualify-diff (val)
  (format nil "~a"
		  (cond  ((<= val *difference-threshold*) "about the same")
				;((>  val *difference-threshold*) "almost the same")
				(t                                "different"))))

(defparameter *j-name*  "Julia")
(defparameter *jt-name* "Juliette")
(defparameter *r-name*  "Romeo")
(defparameter *rt-name* "Roberto")

;;;;;; human-prints
(defun human-print-general (n j jt j-name r-name jt-name rt-name)
  (format nil "~a and ~a. (Their) ~a"
		  (qualify (funcall j n)  j-name  r-name)
		  (qualify (funcall jt n) jt-name rt-name)
		  (human-print-abs-diffs-couple n j jt j-name jt-name)
		  ))

(defun human-print-all (n)
  (format t "Day: ~d:~%~a ~a ~a"
		  n
		  (human-print-general   n 'j 'jt *j-name* *r-name* *jt-name* *rt-name*)
		  (human-print-general   n 'r 'rt *r-name* *j-name* *rt-name* *jt-name*)
		  (human-print-abs-diffs n 'j 'r 'jt 'rt *j-name* *r-name* *rt-name* *jt-name*)))

(defun human-print-abs-diffs-couple (n j r j-name r-name)
  (format nil "Feelings of ~a and ~a are ~a (diff-val: ~a)."
			j-name
			r-name
		    (qualify-diff (abs (- (funcall j n) (funcall r n))))
		    (abs (- (funcall j n) (funcall r n)))))

(defun human-print-abs-diffs (n j r jt rt j-name r-name jt-name rt-name)
  (format nil "~a ~a"
		  (human-print-abs-diffs-couple n j r j-name r-name)
		  (human-print-abs-diffs-couple n jt rt jt-name rt-name)))

;;;;;; machine-prints

(defun machine-print-general (n j jt j-name r-name jt-name rt-name func-diff)
  (format t "Day: ~2d; ~a: ~13a, ~a: ~13a; Diff: ~13a~%"
		  n j-name (funcall j n) jt-name (funcall jt n) (funcall func-diff n)))

(defun machine-print-all (n)
  (machine-print-general   n 'j 'jt *j-name* *r-name* *jt-name* *rt-name* 'j-jt-diff)
  (machine-print-general   n 'r 'rt *r-name* *j-name* *rt-name* *jt-name* 'r-rt-diff)
  (machine-print-abs-diffs n 'j 'r 'jt 'rt))

(defun print-j-jt (n)
  (format t "Day: ~2d; Julia: ~13a, Juliette: ~13a; Diff: ~13a~%" n (j n) (jt n) (j-jt-diff n)))

(defun print-r-rt (n)
  (format t "Day: ~2d; Romeo: ~13a; Roberto:  ~13a; Diff: ~13a~%" n (r n) (rt n) (r-rt-diff n)))

(defun machine-print-abs-diffs (n r j rt jt)
  (format t "Day: ~2d; Diff:  ~13a; Diff:     ~13a~%"
		  n
		  (abs (- (funcall r n) (funcall j n)))
		  (abs (- (funcall rt n) (funcall jt n)))))

