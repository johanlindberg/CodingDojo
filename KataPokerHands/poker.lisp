;;;; Poker Hands
;;;; -----------

(defun rank (card1 card2)
  "Rank <cards> (using poker rules) and return which is the winner and why."
  t)

(defun same-suit-p (hand)
  "Returns t if all cards in <hand> are of the same suit, otherwise returns nil.

  >> (same-suit-p '(2H 3H)) t
  >> (same-suit-p '(2H 2S)) nil
  >> (same-suit-p '(2H 3H 4H 5H 6H)) t
  >> (same-suit-p '(2H 3H 4H 5H 6H 2S)) nil
  >> (same-suit-p '(2S 2H 3H 4H 5H 6H)) nil
  >> (same-suit-p '(2S 2D 2H 2C)) nil
  >> (same-suit-p '(2D 3D)) t

  "
  (let ((clubs    '(2C 3C 4C 5C 6C 7C 8C 9C TC JC QC KC AC))
	(diamonds '(2D 3D 4D 5D 6D 7D 8D 9D TD JD QD KD AD))
	(hearts   '(2H 3H 4H 5H 6H 7H 8H 9H TH JH QH KH AH))
	(spades   '(2S 3S 4S 5S 6S 7S 8S 9S TS JS QS KS AS)))
    (dolist (suit (list clubs diamonds hearts spades))
      (let ((same-suit t))
	(dolist (card hand)
	  (unless (member card suit)
	    (setq same-suit nil)
	    (return)))
	(when same-suit
	  (return-from same-suit-p t))))))

(defun flush-p (hand)
  "Returns t if all 5 cards in <hand> are from the same suit.

  >> (flush-p '(2S 3S 4S 5S 6S))
  t
  "
  (and (eq 5 (length hand))
       (same-suit-p hand)))
