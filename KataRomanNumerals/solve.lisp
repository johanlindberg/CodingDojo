;;;; Roman Numerals
;;;; --------------
;;;;
;;;; > (load "/Users/johanlindberg/Projects/doctest/doctest.lisp")
;;;; > (use-package :doctest)
;;;; > (test-function #'roman-numeral :output T)

;; Implementation
(defun roman-numeral (number)
  "Returns a string with the Roman numeral representing <number>.

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
       (symbols '("X" "V")) ; Roman symbols and
       (values  '( 10  5))  ; their values.
       (n number))
      ((eq n 0) result)
    (let ((v (pop values))
	  (s (pop symbols)))
      (when (>= n v)
	(setf result (format nil "~A~A" result s))
	(decf n v))
      (when (eq n (- v 1))
	(setf result (format nil "~A~A~A" result "I" s))
	(decf n (- v 1)))
      (when (<= n 3)
	(dotimes (i n)
	  (setf result (format nil "~A~A" result "I")))
	(setf n 0)))))
