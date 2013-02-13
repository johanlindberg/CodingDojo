;;;; Poker Hands
;;;; -----------

(defun rank (card1 card2)
  "Rank <cards> (using poker rules) and return which is the winner and why."
  t)

(defun same-suit-p (&rest cards)
  "Returns t if all <cards> are of the same suit, otherwise returns nil.

  >> (same-suit-p '2H '3H) t
  >> (same-suit-p '2H '2S) nil
  >> (same-suit-p '2H '3H '4H '5H '6H) t
  >> (same-suit-p '2H '3H '4H '5H '6H '2S) nil
  >> (same-suit-p '2S '2H '3H '4H '5H '6H) nil
  >> (same-suit-p '2S '2D '2H '2C) nil
  >> (same-suit-p '2D '3D) t

  "
  (let ((hearts '(2H 3H 4H 5H 6H 7H 8H 9H TH JH QH KH AH))
	(spades '(2S 3S 4S 5S 6S 7S 8S 9S TS JS QS KS AS)))
    (dolist (suit (list hearts spades))
      (let ((same-suit t))
	(dolist (card cards)
	  (unless (member card suit)
	    (setq same-suit nil)
	    (return)))
	(when same-suit
	  (return-from same-suit-p t))))))
