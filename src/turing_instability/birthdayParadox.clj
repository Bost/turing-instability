;(ns birthdayParadox)

(defconstant +year-size+ 365)
(defconstant +desired-probability+ 0.4)
 
;; Calculate how many people need to meet on a party to have propabily of X that at least two of 
;; them have birthday on a same day  
(defun birthday-paradox (probability number-of-people)
  (let ((new-probability (* (/ (- +year-size+ number-of-people)
                               +year-size+)
                            probability)))
    (if (< new-probability +desired-probability+)
        (1+ number-of-people)
        (birthday-paradox new-probability (1+ number-of-people)))))