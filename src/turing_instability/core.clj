(ns ^{:author "The Bost"
      :doc "TODO doc for this namespace"}
  turing-instability.core
  (:use
    clojure.math.numeric-tower
    turing-instability.init
    turing-instability.relfuncs
    turing-instability.svg
    ))

; relationship threshold to divide like/ignore/dislike areas
(def threshold 0.333)
(def difference-threshold 0.5)

; Translate the relationship value to words a la: like, dislike, ignore, hate, etc
; Note: This is propably the ugly imperative part of the program
;(defn qualify [val subject-who object-whom]
;  (format nil "~a (feelig-val: ~a) ~a ~a"
;		  subject-who
;		  val
;		  (cond ((<  val (* -1 threshold)) "feels unhappy with")
;				((>= val (*  1 threshold)) "feels happy with")
;				(t                           "feels fine with"))
;		  object-whom))

;(defn qualify-diff [val]
;  (format nil "~a"
;		  (cond  ((<= val difference-threshold) "about the same")
;				;((>  val difference-threshold) "almost the same")
;				(t                                "different"))))

(def j-name  "Julia")
(def jt-name "Juliette")
(def r-name  "Romeo")
(def rt-name "Roberto")

;;;;;; human-prints
;(defn human-print-general [n j jt j-name r-name jt-name rt-name]
;  (format nil "~a and ~a. (Their) ~a"
;		  (qualify (j n)  j-name  r-name)
;		  (qualify (jt n) jt-name rt-name)
;		  (human-print-abs-diffs-couple n j jt j-name jt-name)
;		  ))

;(defn human-print-all [n]
;  (format t "Day: ~d:~%~a ~a ~a"
;		  n
;		  (human-print-general   n 'j 'jt j-name r-name jt-name rt-name)
;		  (human-print-general   n 'r 'rt r-name j-name rt-name jt-name)
;		  (human-print-abs-diffs n 'j 'r 'jt 'rt j-name r-name rt-name jt-name)))

;(defn human-print-abs-diffs-couple [n j r j-name r-name]
;  (format nil "Feelings of ~a and ~a are ~a (diff-val: ~a)."
;			j-name
;			r-name
;		    (qualify-diff (abs (- (j n) (r n))))
;		    (abs (- (j n) (r n)))))

;(defn human-print-abs-diffs [n j r jt rt j-name r-name jt-name rt-name]
;  (format nil "~a ~a"
;		  (human-print-abs-diffs-couple n j r j-name r-name)
;		  (human-print-abs-diffs-couple n jt rt jt-name rt-name)))

;;;;;; machine-prints

(defn machine-print-general [n j jt j-name r-name jt-name rt-name func-diff]
  (println "Day: " n "; " j-name ": " (j n) ", " jt-name ": " (jt n) "; Diff: " (func-diff n)
		      ))

(defn machine-print-abs-diffs [n r j rt jt]
  (println "Day: " n "; Diff:  " (abs (- (r n) (j n))) "; Diff:     " (abs (- (rt n) (jt n))))
		  )

(defn machine-print-all [n]
  (machine-print-general   n j jt j-name r-name jt-name rt-name j-jt-diff)
  (machine-print-general   n r rt r-name j-name rt-name jt-name r-rt-diff)
  (machine-print-abs-diffs n j r jt rt))

(defn print-j-jt [n]
  (println "Day: " n "; Julia: " (j n) ", Juliette: " (jt n) "; Diff: " (j-jt-diff n)))

(defn print-r-rt [n]
  (println "Day: " n "; Romeo: " (r n) "; Roberto:  " (rt n) "; Diff: "  (r-rt-diff n)))


(comment ;comment2

;svg macro usage: (svg (tag-circle 50 40))
;<svg xmlns="http://www.w3.org/2000/svg" xmlns:xlink="http://www.w3.org/1999/xlink"><circle cx="50" cy="40" r="4" fill="blue"></circle></svg>

(defn tag-circle-one-day [list-prm]
;Usage: (tag-circle-one-day '(1 2))
  (tag-circle (car list-prm) (car (cdr list-prm))))

;(defn tag-circle-1st-n-days (n j-jt-diff-n1)
; TODO
;)

; TODO this method should evaluate to a two dimensional ?? (array or list or hashmap) for a given function: [day-i, value-of-diff-i]
(defn calc-1st-n-days [n j-jt-diff-n1]
;example:
;CL-USER> (calc-1st-n-days 4 'j-jt-diff-n1)
;((1 -0.0) (2 -0.28399995) (3 0.68463194) (4 0.19869485))
  (loop for i below n
     ;collect (list (+ i 1) (j-jt-diff-n1 (+ i 1)))))
     do (format t "~%"   ; this is here just to print every tag on a new line
		(tag-circle (+ i 1) (j-jt-diff-n1 (+ i 1))))))

);comment2

;(defn -main [& args]
;  (webserver)
;  (println (str "Webserver started on http://localhost:" port webroute)))

(defn first-n-vals [n f]
  "TODO first-n-vals evaluates to lazy sequence - is it ok?"
  (for [i (range 1 n)]
    ;(dorun
      (int (* 100 (f i)))
     ; )
    ))
