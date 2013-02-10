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
  "
  (do ((result "")
       (i n (decf i)))
      ((eq i 0) result)
    (setf result (format nil "~A~A" result "I"))))