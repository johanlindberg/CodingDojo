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

  >> (mapcar #'roman-numeral '(11 12 13 14 15 16 17 18 19 20))
  (\"XI\" \"XII\" \"XIII\" \"XIV\" \"XV\" \"XVI\" \"XVII\" \"XVIII\" \"XIX\" \"XX\")

  >> (mapcar #'roman-numeral '(21 32 43 54 65 76 87 98 99 100))
  (\"XXI\" \"XXXII\" \"XLIII\" \"LIV\" \"LXV\" \"LXXVI\" \"LXXXVII\" \"XCVIII\" \"XCIX\" \"C\")

  >> (mapcar #'roman-numeral '(121 232 343 454 565 676 787 898 999))
  (\"CXXI\" \"CCXXXII\" \"CCCXLIII\" \"CDLIV\" \"DLXV\" \"DCLXXVI\" \"DCCLXXXVII\" \"DCCCXCVIII\" \"CMXCIX\")

  >> (roman-numeral 1000)
  \"M\"

  "
  (let ((result ""))
    (dolist (numeral '((100 "M" "D" "C")
		       ( 10 "C" "L" "X")
		       (  1 "X" "V" "I")))
      (multiple-value-bind (n remainder)
	  (truncate number (first numeral))
	(setf number remainder)
	(setf result (format nil "~A~A" result
			     (convert-roman-numeral n
						    (second numeral)
						    (third numeral)
						    (fourth numeral))))))
    result))

(defun convert-roman-numeral (number s1 s2 s3)
  (let ((result ""))
    (when (eq number 9)
      (return-from convert-roman-numeral (format nil "~A~A" s3 s1)))

    (when (>= number 5)
      (setf result (format nil "~A~A" result s2))
      (decf number 5))

    (when (eq number 4)
      (return-from convert-roman-numeral (format nil "~A~A" s3 s2)))

    (dotimes (i number)
      (setf result (format nil "~A~A" result s3)))

    result))
