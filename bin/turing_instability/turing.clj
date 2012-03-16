(ns turing)

; or try to 'include' clojure.contrib.math in order to get f.e. the abs function
;(ns turing 
;  (:require clojure.contrib.math))

;(defn abs [x]
;  (if (< x 0) (- x) x)) 

; current affection: 1 - positive ; 0 - neutral; -1 - negative
(def *day-1-j* 1); In the begining Julia likes Romeo
(def *day-1-r* 0); In the begining Romeno is neutral towards Julia

(def *day-1-jt* 0); In the begining Juliette is neutral towars Roberto
(def *day-1-rt* 1); In the begining Roberto likes Juliette

; Influence factor saying how much Julia and Juliette influence each other
; 0 <= *s* <= 1; 0: no influence, 1: total influence
(def *s* 0.9)

; Influence factor saying how much Romeo and Roberto influence each other
; 0 <= *p* <= 1; 0: no influence, 1: total influence
(def *p* 0.1)

; 'declare' defines the supplied var names with no bindings, useful for making forward declarations.
; (this is something not needed in a real lisp)
(declare j-n1)
(declare r-n1)
(declare jt-n1)
(declare rt-n1)

(load "relationshipFunc")

; Basic function for Julia
(defn j [n] (if (= n 1) *day-1-j* (j-n1 (- n 1))))

; Basic function for Romeo
(defn r [n] (if (= n 1) *day-1-r* (r-n1 (- n 1))))

; Basic function for Julia
(defn j [n] (if (= n 1) *day-1-j* (j-n1 (- n 1))))

; Basic function for Romeo
(defn r [n] (if (= n 1) *day-1-r* (r-n1 (- n 1))))

; Basic function for Juliette
(defn jt [n] (if (= n 1) *day-1-jt* (jt-n1 (- n 1))))

; Basic function for Roberto
(defn rt [n] (if (= n 1) *day-1-rt* (rt-n1 (- n 1))))

; Julia & Juliette Averadge day n:   J+[n] = (J[n] + J'[n]) / 2
(defn j-jt-avrg [n]  (/ (+ (j n) (jt n)) 2))

; Julia & Juliette Difference day n: J-[n] = (J[n] - J'[n]) / 2
(defn j-jt-diff [n]  (/ (- (j n) (jt n)) 2))

; Romeo & Roberto Averadge day n:    R+[n] = (R[n] + R'[n]) / 2
(defn r-rt-avrg [n]  (/ (+ (r n) (rt n)) 2))

; Romeo & Roberto Difference day n:  R-[n] = (R[n] - R'[n]) / 2
(defn r-rt-diff [n]  (/ (- (r n) (rt n)) 2))


; relationship threshold to divide like/ignore/dislike areas
(def *threshold* 0.333)
(def *difference-threshold* 0.5)

; Translate the relationship value to words a la: like, dislike, ignore, hate, etc
; Note: This is propably the ugly imperative part of the program
;(defn qualify [val subject-who object-whom]
;  (format nil "~a (feelig-val: ~a) ~a ~a"
;		  subject-who
;		  val
;		  (cond ((<  val (* -1 *threshold*)) "feels unhappy with")
;				((>= val (*  1 *threshold*)) "feels happy with")
;				(t                           "feels fine with"))
;		  object-whom))

;(defn qualify-diff [val]
;  (format nil "~a"
;		  (cond  ((<= val *difference-threshold*) "about the same")
;				;((>  val *difference-threshold*) "almost the same")
;				(t                                "different"))))

(def *j-name*  "Julia")
(def *jt-name* "Juliette")
(def *r-name*  "Romeo")
(def *rt-name* "Roberto")

;;;;;; human-prints
;(defn human-print-general [n j jt j-name r-name jt-name rt-name]
;  (format nil "~a and ~a. (Their) ~a"
;		  (qualify (funcall j n)  j-name  r-name)
;		  (qualify (funcall jt n) jt-name rt-name)
;		  (human-print-abs-diffs-couple n j jt j-name jt-name)
;		  ))

;(defn human-print-all [n]
;  (format t "Day: ~d:~%~a ~a ~a"
;		  n
;		  (human-print-general   n 'j 'jt *j-name* *r-name* *jt-name* *rt-name*)
;		  (human-print-general   n 'r 'rt *r-name* *j-name* *rt-name* *jt-name*)
;		  (human-print-abs-diffs n 'j 'r 'jt 'rt *j-name* *r-name* *rt-name* *jt-name*)))

;(defn human-print-abs-diffs-couple [n j r j-name r-name]
;  (format nil "Feelings of ~a and ~a are ~a (diff-val: ~a)."
;			j-name
;			r-name
;		    (qualify-diff (abs (- (funcall j n) (funcall r n))))
;		    (abs (- (funcall j n) (funcall r n)))))

;(defn human-print-abs-diffs [n j r jt rt j-name r-name jt-name rt-name]
;  (format nil "~a ~a"
;		  (human-print-abs-diffs-couple n j r j-name r-name)
;		  (human-print-abs-diffs-couple n jt rt jt-name rt-name)))

;;;;;; machine-prints

(defn machine-print-general [n j jt j-name r-name jt-name rt-name func-diff]
  (println "Day: " n "; " j-name ": " (j n) ", " jt-name ": " (jt n) "; Diff: " (func-diff n)))

(defn machine-print-all [n]
  (machine-print-general   n j jt *j-name* *r-name* *jt-name* *rt-name* j-jt-diff)
  (machine-print-general   n r rt *r-name* *j-name* *rt-name* *jt-name* r-rt-diff)
  (machine-print-abs-diffs n j r jt rt))

;(defn print-j-jt [n]
;  (format t "Day: ~2d; Julia: ~13a, Juliette: ~13a; Diff: ~13a~%" n (j n) (jt n) (j-jt-diff n)))

;(defn print-r-rt [n]
;  (format t "Day: ~2d; Romeo: ~13a; Roberto:  ~13a; Diff: ~13a~%" n (r n) (rt n) (r-rt-diff n)))

(defn machine-print-abs-diffs [n r j rt jt]
  (println "Day: " n 
         "; Diff:  " (abs (- (r n) (j n)))
         "; Diff:     " (abs (- (rt n) (jt n)))))




;(defn tag-circle [x y]
;usage: (tag-circle 20 30)
;<circle cx="20" cy="30" r="4" fill="blue"></circle>
;  (tag circle (cx x cy y r "4" fill "blue") ()))
;   (tag circle (cx x cy y r "4" fill "blue") ()))


;svg macro usage: (svg (tag-circle 50 40))
;<svg xmlns="http://www.w3.org/2000/svg" xmlns:xlink="http://www.w3.org/1999/xlink"><circle cx="50" cy="40" r="4" fill="blue"></circle></svg>

;(defn tag-circle-one-day [list-prm]
;Usage: (tag-circle-one-day '(1 2))
;  (tag-circle (car list-prm) (car (cdr list-prm))))

;(defn tag-circle-1st-n-days (n j-jt-diff-n1)
; TODO
;)

; TODO this method should evaluate to a two dimensional ?? (array or list or hashmap) for a given function: [day-i, value-of-diff-i]
;(defn calc-1st-n-days [n j-jt-diff-n1]
;example:
;CL-USER> (calc-1st-n-days 4 'j-jt-diff-n1)
;((1 -0.0) (2 -0.28399995) (3 0.68463194) (4 0.19869485))
;  (loop for i below n
     ;collect (list (+ i 1) (funcall j-jt-diff-n1 (+ i 1)))))
;     do (format t "~%"   ; this is here just to print every tag on a new line
;		(tag-circle (+ i 1) (funcall j-jt-diff-n1 (+ i 1))))))
