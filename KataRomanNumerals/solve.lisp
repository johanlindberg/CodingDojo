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
  >> (roman-numeral 10)
  \"X\"
  >> (roman-numeral 9)
  \"IX\"
  "
  (do ((result "")
       (i n))
      ((eq i 0) result)
    (cond ((>= i 10)
	   (progn
	     (setf result (format nil "~A~A" result "X"))
	     (decf i 10)))
	  ((>= i 5)
	   (progn
	     (setf result (format nil "~A~A" result "V"))
	     (decf i 5)))
	  ((eq i 4)
	   (progn
	     (setf result (format nil "~A~A" result "IV"))
	     (decf i 4)))
	  ((<= i 3)
	   (progn
	     (setf result (format nil "~A~A" result "I"))
	     (decf i))))))
