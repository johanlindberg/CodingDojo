;;;; Poker Hands
;;;; -----------


(defparameter *clubs*    '(2C 3C 4C 5C 6C 7C 8C 9C TC JC QC KC AC))
(defparameter *diamonds* '(2D 3D 4D 5D 6D 7D 8D 9D TD JD QD KD AD))
(defparameter *hearts*   '(2H 3H 4H 5H 6H 7H 8H 9H TH JH QH KH AH))
(defparameter *spades*   '(2S 3S 4S 5S 6S 7S 8S 9S TS JS QS KS AS))

(defparameter *values*   '(AS AH AD AC KS KH KD KC QS QH QD QC
			   JS JH JD JC TS TH TD TC 9S 9H 9D 9C
			   8S 8H 8D 8C 7S 7H 7D 7C 6S 6H 6D 6C
			   5S 5H 5D 5C 4S 4H 4D 4C 3S 3H 3D 3C
			   2S 2H 2D 2C))

(defun rank (hand)
  "Rank <hand> (using poker rules) and returns the score and a description."
  t)

;; Scoring functions

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

(defun pair-p (hand)
  "Returns the score of the pair cards if <hand> contains a pair, otherwise nil.

  >> (pair-p '(2H 2S 3S))
  2
  "
  (let ((score 1))
    (print (sort (mapcar #'score hand) #'>))
    (dolist (s (sort (mapcar #'score hand) #'>))
      (if (eql s score)
	  (return-from pair-p score))
	  (setq score s))))

(defun high-card (hand)
  "Returns a list with the score and the symbol of the highest card in, <hand>.

  >> (mapcar #'high-card '((2S 3S) (AS 3H AH)(3H AH)))
  (3S AS AH)

  "
  (let ((highest-card '2C)
	(highest-score 0))
    (dolist (card hand)
      (let ((s (score card)))
	(when (> s highest-score)
	  (setq highest-score s
		highest-card card))))

    highest-card))

;; Helper functions

(defun score (card)
  "Returns the score for <card>.
  
  >> (mapcar #'score '(2C 3H 4S 5D 6C 7H 8S 9D TC JH QS KD AC))
  (2 3 4 5 6 7 8 9 10 11 12 13 14)
  "
  (+ 1 (ceiling (/ (length (member card *values*)) 4))))

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

