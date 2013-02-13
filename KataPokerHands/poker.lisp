;;;; Poker Hands
;;;; -----------

(defun rank (card1 card2)
  "Rank <cards> (using poker rules) and return which is the winner and why.

  >> (rank '2H '3H)
  t

  >> (rank '2H '2S)
  nil
  "
  (let ((hearts '(2H 3H))
	(spades '(2S)))
    (dolist (suit (list hearts spades))
      (when (and (member card1 suit)
		 (member card2 suit))
	(return-from rank t)))))