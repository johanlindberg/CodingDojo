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

  >> (mapcar #'roman-numeral '(21 22 23 24 25 26 27 28 29 30))
  (\"XXI\" \"XXII\" \"XXIII\" \"XXIV\" \"XXV\" \"XXVI\" \"XXVII\" \"XXVIII\" \"XXIX\" \"XXX\")

  I'm expecting this test to fail which is why it's not part of the mapcar-bulk
  of tests.

  >> (mapcar #'roman-numeral '(31 32 33 34 35 36 37 38 39))
  (\"XXXI\" \"XXXII\" \"XXXIII\" \"XXXIV\" \"XXXV\" \"XXXVI\" \"XXXVII\" \"XXXVIII\" \"XXXIX\")

  >> (roman-numeral 40)
  \"XL\"
  "
  (do ((result "")
       (symbols '("X" "X" "X" "V")) ; Roman symbols and
       (values  '( 10  10  10  5))  ; their values.
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
