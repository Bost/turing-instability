(ns ^{:author "The Bost"
      :doc "Relationship functions"}
  turing-instability.relfuncs
  (:use
    turing-instability.init
    ))

(comment
(load "../turing_instability/relfuncs")
)

(declare j-n1)
(declare r-n1)
(declare jt-n1)
(declare rt-n1)

; Basic function for Julia
(defn j [n] (if (= n 1) day-1-j (j-n1 (- n 1))))

; Basic function for Romeo
(defn r [n] (if (= n 1) day-1-r (r-n1 (- n 1))))

; Basic function for Juliette
(defn jt [n] (if (= n 1) day-1-jt (jt-n1 (- n 1))))

; Basic function for Roberto
(defn rt [n] (if (= n 1) day-1-rt (rt-n1 (- n 1))))

; Julia & Juliette Averadge day n:   J+[n] = (J[n] + J'[n]) / 2
(defn j-jt-avrg [n]  (/ (+ (j n) (jt n)) 2))

; Julia & Juliette Difference day n: J-[n] = (J[n] - J'[n]) / 2
(defn j-jt-diff [n]  (/ (- (j n) (jt n)) 2))

; Romeo & Roberto Averadge day n:    R+[n] = (R[n] + R'[n]) / 2
(defn r-rt-avrg [n]  (/ (+ (r n) (rt n)) 2))

; Romeo & Roberto Difference day n:  R-[n] = (R[n] - R'[n]) / 2
(defn r-rt-diff [n]  (/ (- (r n) (rt n)) 2))

; Julia's next day if not influenced by Juliette:
;      J(n+1) = J[n] + R[n];             (defn j-n1 [n] (+ (j n) (r n)))
;
; Julia's next day if influenced by Juliette by the factor of s:
;      J(n+1) = J[n] + R[n] + s*[J'[n] - J[n]] + s*[R[n] - R'[n]]
(defn j-n1 [n]
  (+ (j n) (r n)
	 (* s
		(- (jt n) (j n)))))

; Romeo's next day if not influenced by Roberto:
;      R(n+1) = -J[n];                   (defn r-n1 [n] (-(j n)))
;
; Romes's next day if influenced by Roberto by the factor of p:
;      R(n+1) = -J[n] - p*[R'[n] - R[n]]
(defn r-n1 [n]
  (- (-(j n))
	 (* p
		(- (rt n) (r n)))))

; Juliette next day if not influenced by Julia:
;      J'(n+1) = J'[n] + R'[n]           (defn jt-n1 [n] (+ (jt n) (rt n)))
;
; Juliette's next day if influenced by Julia by the factor of s:
;      J'(n+1) = J'[n] + R'[n] + s*[J[n] - J'[n]] + s*[R[n] - R'[n]]
(defn jt-n1 [n]
  (+ (jt n) (rt n))
  (* s (- (j n) (jt n))
	 (* s (- (r n) (rt n)))))

; Roberto's next day if not influenced by Romeo:
;      R'(n+1) = -J'[n]                  (defn rt-n1 [n] (-(jt n)))
;
; Roberto' next day if influenced by Romeo by the factor of p:
;      R'(n+1) = -J[n] - p*[R[n] - R'[n]]
(defn rt-n1 [n]
  (- (-(j n))
	 (* p
		(- (r n) (rt n)))))

; Julia & Juliette Difference next day if they influence each other by the factor of s:
;      J-(n+1) = J-[n] + R-[n]           (defn j-jt-diff-n1 [n] (+ (j-jt-diff n) (r-rt-diff n)))
; Julia & Juliette Difference next day if they don't influence each other:
;      J-(n+1) = (1 - 2*s) * [J-[n] + R-[n]]
(defn j-jt-diff-n1 [n]
  (* (- 1 (* 2 s))
	 (+ (j-jt-diff n) (r-rt-diff n))))

; Romeo & Roberto Difference next day if they influence each other:
;      R-(n+1) = -[J-[n]]                (defn r-rt-diff-n1 [n] (-(j-jt-diff n)))
; Romeo & Roberto Difference for the next day if they don't influence each other:
;      R-(n+1) = -(1 - 2*p) * J-[n]
(defn r-rt-diff-n1 [n]
  (* (-(- 1 (* 2 p))
	   (j-jt-diff n))))

; Julia & Juliette Averadge for the next day if they don't influence each other:
;      J+(n+1) = J+[n] + R+[n]
;(defn j-jt-avrg-n1 [n] (+ (j-jt-avrg n) (r-rt-avrg n)))

; Romeo & Roberto Averadge for the next day if they don't influence each other:
;      R+(n+1) = -[J+[n]]
;(defn r-rt-avrg-n1 [n] (-(j-jt-avrg n)))

