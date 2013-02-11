;;;; Roman Numerals
;;;; --------------
;;;;
;;;; > (load "/Users/johanlindberg/Projects/doctest/doctest.lisp")
;;;; > (use-package :doctest)
;;;; > (test-function #'roman-numeral :output T)

;; Implementation
(defun roman-numeral (number)
  "Returns a string with the Roman numeral representing <number>.

  >> (mapcar #'roman-numeral '(1 2 3 5 6 7 8 4 10 9))
  (\"I\" \"II\" \"III\" \"V\" \"VI\" \"VII\" \"VIII\" \"IV\" \"X\" \"IX\")
  "
  (convert-roman-numeral number "X" "V" "I"))

(defun convert-roman-numeral (number s1 s2 s3)
  (do ((result "")
       (symbols (list s1 s2)) ; Roman symbols and
       (values  '( 10  5))    ; their values.
       (n number))
      ((eq n 0) result)
    (let ((v (pop values))
	  (s (pop symbols)))
      (when (>= n v)
	(setf result (format nil "~A~A" result s))
	(decf n v))
      (when (eq n (- v 1))
	(setf result (format nil "~A~A~A" result s3 s))
	(decf n (- v 1)))
      (when (<= n 3)
	(dotimes (i n)
	  (setf result (format nil "~A~A" result s3)))
	(setf n 0)))))
