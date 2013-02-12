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

  >> (mapcar #'roman-numeral '(121 232 343 454 565 676 787 898 999 1000))
  (\"CXXI\" \"CCXXXII\" \"CCCXLIII\" \"CDLIV\" \"DLXV\" \"DCLXXVI\" \"DCCLXXXVII\" \"DCCCXCVIII\" \"CMXCIX\" \"M\")

  >> (mapcar #'roman-numeral '(1999 2000 2500 2499 3333 3999))
  (\"MCMXCIX\" \"MM\" \"MMD\" \"MMCDXCIX\" \"MMMCCCXXXIII\" \"MMMCMXCIX\")
  "
  (let ((result (make-array 1 
			    :adjustable t 
			    :element-type 'character
			    :fill-pointer 0)))
    (dolist (numeral '((1000 #\* #\* #\M)
		       ( 100 #\M #\D #\C)
		       (  10 #\C #\L #\X)
		       (   1 #\X #\V #\I)))
      (destructuring-bind (size sym1 sym2 sym3) numeral
	(multiple-value-bind (num remainder) (truncate number size)
	  (setq number remainder)
	  (when (eq num 9)
	    (vector-push-extend sym3 result)
	    (vector-push-extend sym1 result)
	    (decf num 9))
	  (when (>= num 5)
	    (vector-push-extend sym2 result)
	    (decf num 5))
	  (when (eq num 4)
	    (vector-push-extend sym3 result)
	    (vector-push-extend sym2 result)
	    (decf num 4))
	  (dotimes (i num)
	    (vector-push-extend sym3 result)
	    (decf num)))))

    result))
