;;;; Poker Hands
;;;; -----------

(defun rank (&rest cards)
  "Rank <cards> (using poker rules) and return which is the winner and why.

  >> (rank '2H)
  -> |2 Hearts|
  NIL
  "
  t)