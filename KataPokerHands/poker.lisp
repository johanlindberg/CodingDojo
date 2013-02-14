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

(defun straight-flush-p (hand)
  "Returns the score of the highest card if <hand> contains a straight flush,
   otherwise nil.

  >> (straight-flush-p '(2S 3S 4S 5S 6S))
  6
  "
  (let ((score (straight-p hand)))
    (when (and score
	       (flush-p hand))
      (return-from straight-flush-p score))))

(defun four-of-a-kind-p (hand)
  "Returns the score and the symbols of the cards, if <hand> contains three
   of a kind, otherwise nil.

  >> (multiple-value-list (four-of-a-kind-p '(2S 2D 2C 2H 4D)))
  (2 (2S 2D 2C 2H))
  "
  (n-of-a-kind-p 4 hand))

(defun full-house-p (hand)
  "Returns the score of the three-of-a-kind if <hand> contains a full house,
   otherwise nil.

  >> (full-house-p '(2S 2H 2D 3D 3S))
  2

  >> (full-house-p '(3S 2H 2D 3D 2S))
  2

  >> (full-house-p '(2S 2H 2D 2C 3S))
  nil
  "
  (multiple-value-bind (score cards)
      (n-of-a-kind 3 hand)
    (when (and score
	       (pair-p (remove-if #'(lambda (card)
				      (member card cards)) hand)))
      (return-from full-house-p score))))
      

(defun flush-p (hand)
  "Returns the score of the highest card if all 5 cards in <hand> are from
   the same suit.

  >> (flush-p '(2S 3S 4S 5S 6S)) 6
  >> (flush-p '(2S 3S 4S 5S)) nil
  >> (flush-p '(2H 3S 4S 5S 6S)) nil
  >> (flush-p '(2S 3S 4S 5S 6H)) nil
  >> (flush-p '(2H 3H 4H 5H 6H)) 6
  "
  (when (and (eq 5 (length hand))
	     (same-suit-p hand))
    (score (high-card hand))))

(defun straight-p (hand)
  "Returns the score of the highest card if <hand> contains 5 cards with
   consecutive values, otherwise nil.

  >> (mapcar #'straight-p '((2S 3S 4S 5S 6S) (2S 4S 4D 5S 6S)
                            (6S 5C 4C 3D 2S) (5D 7S 4H 8C 6S)))
  (6 nil 6 8)
  "
  (let* ((scored-hand (sort (mapcar #'score hand) #'>))
	 (prev (car scored-hand)))
    (dolist (score (cdr scored-hand))
      (if (eq prev (+ 1 score))
	  (setq prev score)
	  (return-from straight-p)))
    (car scored-hand)))

(defun three-of-a-kind-p (hand)
  "Returns the score and the symbols of the cards, if <hand> contains three
   of a kind, otherwise nil.

  >> (multiple-value-list (three-of-a-kind-p '(2S 2D 2C)))
  (2 (2S 2D 2C))

  >> (multiple-value-list (three-of-a-kind-p '(2S 3D 4C)))
  (nil)

  >> (multiple-value-list (three-of-a-kind-p '(4C 2S 4H 3D 4D)))
  (4 (4C 4H 4D))
  "
  (n-of-a-kind-p 3 hand))

(defun two-pair-p (hand)
  "Returns a list with the scores from the pairs if <hand> holds two pairs,
   otherwise nil.

  >> (multiple-value-list (two-pair-p '(2H 3H 2S 3S AH)))
  ((3 2) ((3H 3S) (2H 2S)))
  "
  (multiple-value-bind (first-score first-pair)
      (pair-p hand)
    (when first-pair
      (multiple-value-bind (second-score second-pair)
	  (pair-p (remove-if #'(lambda (card)
				 (eq first-score (score card))) hand))
	(when second-pair
	  (return-from two-pair-p (values (list first-score second-score)
					  (list first-pair second-pair))))))))

(defun pair-p (hand)
  "Returns the score and the symbols of the pair cards, if <hand> contains
   a pair, otherwise nil.

  >> (mapcar #'pair-p '((2H 2S 3S) (2H 2S 3S 3H) (3H 3S 2S 2H) (AH AS AC KH KS)
                        (2H 3H 4H 5H 6H)))
  (2 3 3 14 nil)

  XXX Should I do something about this one? It's not really a problem the way
  I see it, but perhaps it's a bit unexpected.
  >> (multiple-value-list (pair-p '(3S 3H 3D)))
  (3 (3S 3H 3D))

  >> (multiple-value-list (pair-p '(3S 4H 5D)))
  (nil)  
  "
  (n-of-a-kind-p 2 hand))

(defun high-card (hand)
  "Returns a list with the score and the symbol of the highest card in <hand>.

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

(defun n-of-a-kind-p (n hand)
  "Returns the score and the symbols of the cards, if <hand> contains <n>
   of a kind, otherwise nil.

  >> (multiple-value-list (n-of-a-kind-p 3 '(2S 2D 2C)))
  (2 (2S 2D 2C))

  >> (multiple-value-list (n-of-a-kind-p 3 '(2S 3D 4C)))
  (nil)

  >> (multiple-value-list (n-of-a-kind-p 3 '(4C 2S 4H 3D 4D)))
  (4 (4C 4H 4D))
  "
  (let ((score 0)
	(count 1))
    (dolist (s (sort (mapcar #'score hand) #'>))
      (if (eql s score)
	  (progn
	    (incf count)
	    (when (eq count n)
	      (return-from n-of-a-kind-p
		(values score
			(remove-if-not #'(lambda (card)
					   (eq score (score card))) hand)))))
	  (setq score s count 1)))))

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

