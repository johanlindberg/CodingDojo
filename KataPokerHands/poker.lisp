;;;; Poker Hands
;;;; -----------

(defparameter *clubs*    '(AC KC QC JC TC 9C 8C 7C 6C 5C 4C 3C 2C))
(defparameter *diamonds* '(AD KD QD JD TD 9D 8D 7D 6D 5D 4D 3D 2D))
(defparameter *hearts*   '(AH KH QH JH TH 9H 8H 7H 6H 5H 4H 3H 2H))
(defparameter *spades*   '(AS KS QS JS TS 9S 8S 7S 6S 5S 4S 3S 2S))

(defun compare (&rest hands)
  "Compares all <hands> and returns the winning one.

  >> (compare '(Black 2H 3D 5S 9C KD) '(White 2C 3H 4S 8C AH))
  White

  >> (compare '(Black 2H 4S 4C 2D 4H) '(White 2S 8S AS QS 3S))
  Black ; XXX According to CodingDojo.org description White wins this one!

  >> (compare '(Black 2H 3D 5S 9C KD) '(White 2C 3H 4S 8C KH))
  Black

  >> (compare '(Black 2H 3D 5S 9C KD) '(White 2D 3H 5C 9S KH))
  Tie
  "
  (let ((high-score (rank (cdar hands)))
        (winner (caar hands))
        (tie? nil))
    (dolist (hand (cdr hands))
      (let ((scores (rank (cdr hand))))
        (setq tie? (tree-equal high-score scores :test #'eq))
        (tree-equal high-score
                    (rank (cdr hand))
                    :test #'(lambda (hs s)
                              (if (eq s hs)
                                  t
                                  (when (> s hs)
                                    (setq high-score (rank (cdr hand)))
                                    (setq winner (car hand))
                                    nil))))))
    (if tie?
        'tie
        winner)))

(defun rank (hand)
  "Rank <hand> (using poker rules) and returns a list with scores.

   Score lists are compared from left to right until a difference is found.
   Whichever hand has the highest score in that position wins. If both lists
   are equal, it's a tie.

  >> (rank '(JS 6H 4C TD 6S))
  (2 6 11 10 4) ; The 2 is for the pair. The 6 is for the score of the pair.
                ; The rest of the scores are the rest of the cards high to low.

  >> (rank '(JS 6H 4C 6D 4S))
  (3 6 4 11) ; See above. 3 is for the two pairs. 6 and 4 are the scores for
             ; each of the pairs.

  >> (rank '(JS 6H 4C 6D 6S))
  (4 6 11 4) ; See above. 4 is for the three-of-a-kind.

  >> (rank '(4C 5D 6D 7H 8S))
  (5 8) ; 5 is for the straight, 8 is for the high-card.

  >> (rank '(4C TC 3C 9C 8C))
  (6 10 9 8 4 3) ; See above. 6 is for flush.

  >> (rank '(4C 8D 8S 4D 4S))
  (7 4) ; 7 is for full-house, 4 is for the three-of-a-kind.
        ; XXX Should I also include 8 in the score?

  >> (rank '(5C 5D 8C 5H 5S))
  (8 5 8) ; 8 is for four-of-a-kind. 5 is the score of the cards
          ; and 8 is the remaining card's score.

  >> (rank '(4C 5C 6C 7C 8C))
  (9 8) ; 9 is the score for a straight flush. 8 is for the
        ; high-card.

  >> (rank '(2H 3D 5S 9C KD))
  (1 13 9 5 3 2)
  "
  (cond ((straight-flush-p hand)
         (cons 9 (list (straight-flush-p hand))))

        ((four-of-a-kind-p hand)
         (multiple-value-bind (score cards) (four-of-a-kind-p hand)
           (cons 8 (cons score (scores (remaining hand cards))))))

        ((full-house-p hand)
         (cons 7 (list (full-house-p hand))))

        ((flush-p hand)
         (cons 6 (scores hand)))

        ((straight-p hand)
         (cons 5 (list (straight-p hand))))

        ((three-of-a-kind-p hand)
         (multiple-value-bind (score cards) (three-of-a-kind-p hand)
           (cons 4 (cons score (scores (remaining hand cards))))))

        ((two-pair-p hand)
         (multiple-value-bind (score cards) (two-pair-p hand)
           (cons 3 (append score (scores (remaining hand cards))))))

        ((pair-p hand)
         (multiple-value-bind (score cards) (pair-p hand)
           (cons 2 (cons score (scores (remaining hand cards))))))

        (t
         (cons 1 (scores hand)))))			

;; Scoring functions

(defun straight-flush-p (hand)
  "Returns the score of the highest card if <hand> contains a straight flush,
   otherwise nil.

  >> (mapcar #'straight-flush-p '((2S 3S 4S 5S 6S) (2S 3S 4S 5S 6D) (2S 3S 4S 5S 7S)))
  (6 nil nil)
  "
  (when (flush-p hand)
    (straight-p hand)))

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

  >> (mapcar #'full-house-p '((2S 2H 2D 3D 3S) (3S 2H 2D 3D 2S) (2S 2H 2D 2C 3S)))
  (2 2 nil)
  "
  (multiple-value-bind (score cards)
      (three-of-a-kind-p hand)
    (when (and score
	       (pair-p (remaining hand cards)))
      (return-from full-house-p score))))
      

(defun flush-p (hand)
  "Returns the score of the highest card if all 5 cards in <hand> are from
   the same suit.

  >> (mapcar #'flush-p '((2S 3S 4S 5S 6S) (2S 3S 4S 5S 6C) (2H 3S 4S 5S 6S)
                         (2S 3S 4S 5S 6H) (2H 3H 4H 5H 6H)))
  ((6 5 4 3 2) nil nil nil (6 5 4 3 2))
  "
  (let ((suit (suit (car hand))))
    (dolist (card (cdr hand))
      (unless (eq suit (suit card))
        (return-from flush-p)))
    (scores hand)))

(defun straight-p (hand)
  "Returns the score of the highest card if <hand> contains 5 cards with
   consecutive values, otherwise nil.

  >> (mapcar #'straight-p '((2S 3S 4S 5S 6S) (2S 4S 4D 5S 6S)
                            (6S 5C 4C 3D 2S) (5D 7S 4H 8C 6S)))
  (6 nil 6 8)
  "
  (let* ((scored-hand (scores hand))
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
  ((3 2) (3H 3S 2H 2S))
  "
  (multiple-value-bind (first-score first-pair)
      (pair-p hand)
    (when first-pair
      (multiple-value-bind (second-score second-pair)
	  (pair-p (remaining hand first-pair))
	(when second-pair
	  (return-from two-pair-p (values (list first-score second-score)
					  (append first-pair second-pair))))))))

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
    (dolist (s (scores hand))
      (if (eql s score)
	  (progn
	    (incf count)
	    (when (eq count n)
	      (return-from n-of-a-kind-p
		(values score
			(remove-if-not #'(lambda (card)
					   (eq score (score card))) hand)))))
	  (setq score s count 1)))))

(defun scores (hand)
  (sort (mapcar #'score hand) #'>))

(defun remaining (hand cards)
  "Returns a list of the cards in <hand> that are not in <cards>."
  (remove-if #'(lambda (card)
                 (member card cards))
             hand))

(defun suit (card)
  "Returns the suit for <card>."
  (cond ((member card *clubs*) *clubs*)
        ((member card *diamonds*) *diamonds*)
        ((member card *hearts*) *hearts*)
        ((member card *spades*) *spades*)))

(defun score (card)
  "Returns the score for <card>.
  
  >> (mapcar #'score '(2C 3H 4S 5D 6C 7H 8S 9D TC JH QS KD AC))
  (2 3 4 5 6 7 8 9 10 11 12 13 14)
  "
  (+ 1 (length (member card (suit card)))))
