;(ns availableShells)

(defun list-matching-lines (file predicate)
  "Returns a list of lines in file, for which the predicate applied to
 the line returns T."
  (with-open-file (stream file)
    (loop for line = (read-line stream nil nil)
          while line
          when (funcall predicate line)
          collect it)))

(defun available-shells (&optional (file #p"/etc/shells"))
  (list-matching-lines
   file
   (lambda (line)
     (and (plusp (length line))
          (char= (char line 0) #\/)
          (pathname
           (string-right-trim '(#\space #\tab) line))))))

