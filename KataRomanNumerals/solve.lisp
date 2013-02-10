;;;; Roman Numerals
;;;; --------------
;;;;
;;;; > (load "/Users/johanlindberg/Projects/doctest/doctest.lisp")
;;;; > (use-package :doctest)
;;;; > (test-function #'roman-numeral :output T)

;; Implementation
(defun roman-numeral (n)
  "Returns a string with the Roman numeral representing n.

  >> (roman-numeral 1)
  \"I\"
  >> (roman-numeral 2)
  \"II\"
  >> (roman-numeral 3)
  \"III\"
  >> (roman-numeral 5)
  \"V\"
  >> (roman-numeral 6)
  \"VI\"
  >> (roman-numeral 7)
  \"VII\"
  >> (roman-numeral 8)
  \"VIII\"
  >> (roman-numeral 4)
  \"IV\"
  "
  (do ((result "")
       (i n))
      ((eq i 0) result)
    (if (>= i 5)
	(progn
	  (setf result (format nil "~A~A" result "V"))
	  (decf i 5))
	(progn
	  (setf result (format nil "~A~A" result "I"))
	  (decf i)))))