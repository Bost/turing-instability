(ns ^{:author "The Bost"
      :doc "Relationship functions"}
  turing-instability.relfuncs
  (:use
    [turing-instability.init]
    [clojure.math.numeric-tower]
    ))

(comment
(load "../turing_instability/relfuncs")
(in-ns 'turing-instability.relfuncs)
)

(defmacro dbg[x]
  "Print evaluated expression and return its result"
  `(let [x# ~x]
     (println '~x "=" x#) x#
     )
  )

(declare j-n1)
(declare r-n1)
(declare jt-n1)
(declare rt-n1)
(declare j-jt-diff-n1)
(declare r-rt-diff-n1)

; Basic function for Julia
(defn j  [n] (if (= n 1) day-1-j  (j-n1  (- n 1))))

; Basic function for Romeo
(defn r  [n] (if (= n 1) day-1-r  (r-n1  (- n 1))))

; Basic function for Juliette
(defn jt [n] (if (= n 1) day-1-jt (jt-n1 (- n 1))))

; Basic function for Roberto
(defn rt [n] (if (= n 1) day-1-rt (rt-n1 (- n 1))))

; Julia's next day if influenced by Juliette by the factor s:
;      J(n+1) = J[n] + R[n] + s*(J'[n] - J[n]) + s*(R'[n] - R[n])
(defn j-n1 [n]
  (+ (j n)
     (r n)
     (* s (- (jt n) (j n)))
     (* s (- (rt n) (r n)))))

; Romes's next day if influenced by Roberto by the factor p:
;      R(n+1) = -J[n] - p*(R'[n] - R[n]) = -1 * (J[n] + p * (R'[n] - R[n]))
(defn r-n1 [n]
( + (* -1 (j n))
    (* -1 (* p (- (rt n) (r n))))))

; Juliette's next day if influenced by Julia by the factor s:
;      J'(n+1) = J'[n] + R'[n] + s*(J[n] - J'[n]) + s*(R[n] - R'[n])
(defn jt-n1 [n]
  (+ (jt n)
     (rt n)
     (* s (- (j n) (jt n)))
     (* s (- (r n) (rt n)))))

; Roberto's next day if influenced by Romeo by the factor p:
;      R'(n+1) = -J'[n] - p*(R[n] - R'[n])
(defn rt-n1 [n]
( + (* -1 (jt n))
    (* -1 (* p (- (r n) (rt n))))))

; Julia & Juliette Averadge - day n:      J+[n] = (J[n] + J'[n]) / 2
(defn j-jt-avrg [n]  (/ (+ (j n) (jt n)) 2))

; Julia & Juliette Difference - day n:    J-[n] = |J[n] - J'[n]| / 2
(defn j-jt-diff [n]
  (if (= n 1)
     (/ (abs (- (j n) (jt n))) 2)
	 (j-jt-diff-n1 (- n 1))))

; Romeo & Roberto Averadge - day n:       R+[n] = (R[n] + R'[n]) / 2
(defn r-rt-avrg [n]  (/ (+ (r n) (rt n)) 2))

; Romeo & Roberto Difference - day n:     R-[n] = |R[n] - R'[n]| / 2
(defn r-rt-diff [n]
(if (= n 1)
   (/ (abs (- (r n) (rt n))) 2)
   (r-rt-diff-n1 (- n 1))))

; Julia & Juliette Difference - next day: J-(n+1) = (1 - 2*s) * (J-[n] + R-[n])
(defn j-jt-diff-n1 [n]
  (* (- 1 (* 2 s))
	 (+ (j-jt-diff n) (r-rt-diff n))))

; Romeo & Roberto Difference - next day:  R-(n+1) = -(1 - 2*p) * J-[n]
(defn r-rt-diff-n1 [n]
  (* (* -1 (- 1 (* 2 p)))
     (j-jt-diff n)))

;;;

(comment
; memoization example in clisp
; symbol-function returns the function definition!
(let ((old-j-n1 (symbol-function 'j-n1))
      (previous (make-hash-table)))
  (defun j-n1 (n)
    (or (gethash n previous)
	    (setf (gethash n previous) (funcall old-j-n1 n)))))
)


;; For versions of Clojure that don't have metadata on functions, this
;; allows you to reset the atom by passing the magic argument :reset!
(defn resetable-memoize [f]
  (let [mem (atom {})]
    (fn [& args]
      (if (= (first args) :reset!)
        (reset! mem {})
        (if-let [e (find @mem args)]
          (val e)
          (let [ret (apply f args)]
            (swap! mem assoc args ret)
            ret))))))

(def j-n1         (resetable-memoize j-n1))
(def r-n1         (resetable-memoize r-n1))
(def jt-n1        (resetable-memoize jt-n1))
(def rt-n1        (resetable-memoize rt-n1))
(def j-jt-diff-n1 (resetable-memoize j-jt-diff-n1))
(def r-rt-diff-n1 (resetable-memoize r-rt-diff-n1))

(defn reset []
 (j-n1 :reset!)
 (r-n1 :reset!)
 (jt-n1 :reset!)
 (rt-n1 :reset!)
 (j-jt-diff-n1 :reset!)
 (r-rt-diff-n1 :reset!)
 )


