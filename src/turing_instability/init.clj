(ns ^{:author "The Bost"
      :doc "Initial values"}
  turing-instability.init
;  (:use
;    turing-instability.svg
;    )
  )

; current affection: 1 - positive ; 0 - neutral; -1 - negative
(def day-1-j 1); In the begining Julia likes Romeo
(def day-1-r 0); In the begining Romeno is neutral towards Julia

(def day-1-jt 0); In the begining Juliette is neutral towars Roberto
(def day-1-rt 1); In the begining Roberto likes Juliette

; Influence factor saying how much Julia and Juliette influence each other
; 0 <= s <= 1; 0: no influence, 1: total influence
(def s 0.9)

; Influence factor saying how much Romeo and Roberto influence each other
; 0 <= p <= 1; 0: no influence, 1: total influence
(def p 0.1)
