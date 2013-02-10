;;;; Roman Numerals
;;;; --------------
;;;;
;;;; > (load "/path/to/solve.lisp")
;;;; > (lisp-unit:run-tests '(tests))

;; Tests
(lisp-unit:define-test tests
  (lisp-unit:assert-equal "I" (roman-numeral 1)))

;; Implementation