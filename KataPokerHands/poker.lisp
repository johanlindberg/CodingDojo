;;;; Poker Hands
;;;; -----------


(defparameter *clubs*    '(2C 3C 4C 5C 6C 7C 8C 9C TC JC QC KC AC))
(defparameter *diamonds* '(2D 3D 4D 5D 6D 7D 8D 9D TD JD QD KD AD))
(defparameter *hearts*   '(2H 3H 4H 5H 6H 7H 8H 9H TH JH QH KH AH))
(defparameter *spades*   '(2S 3S 4S 5S 6S 7S 8S 9S TS JS QS KS AS))

(defparameter *values*   '(2C 2D 2H 2S 3C 3D 3H 3S 4C 4D 4H 4S 5C 5D 5H 5S
			   6C 6D 6H 6S 7C 7D 7H 7S 8C 8D 8H 8S 9C 9D 9H 9S
			   TC TD TH TS JC JD JH JS QC QD QH QS KC KD KH KS
			   AC AD AH AS))

(defun rank (hand)
  "Rank <hand> (using poker rules) and returns the score and a description."
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
  (dolist (suit (list *clubs* *diamonds* *hearts* *spades*))
    (let ((same-suit t))
      (dolist (card hand)
	(unless (member card suit)
	  (setq same-suit nil)
	  (return)))
      (when same-suit
	(return-from same-suit-p t)))))

(defun flush-p (hand)
  "Returns t if all 5 cards in <hand> are from the same suit.

  >> (flush-p '(2S 3S 4S 5S 6S)) t
  >> (flush-p '(2S 3S 4S 5S)) nil
  >> (flush-p '(2H 3S 4S 5S 6S)) nil
  >> (flush-p '(2S 3S 4S 5S 6H)) nil
  >> (flush-p '(2H 3H 4H 5H 6H)) t
  "
  (and (eq 5 (length hand))
       (same-suit-p hand)))

(defun high-card (hand)
  "Returns a list with the score and the symbol of the highest card in, <hand>.

  >> (high-card '(2S 3S))
  (3 3S)

  >> (high-card '(AS 3H AH))
  (14 AS)
  "
  (let ((highest-card '2C))
    (dolist (card hand)
      (when (< (length (member card *values*))
	       (length (member highest-card *values*)))
	(setq highest-card card)))
    highest-card))
