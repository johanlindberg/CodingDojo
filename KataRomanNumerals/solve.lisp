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
  (let ((result ""))
    (when (eq number 10)
      (return-from convert-roman-numeral s1))

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
