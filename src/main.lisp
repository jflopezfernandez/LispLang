
; Grammar
; Sentence := Noun-Phrase + Verb-Phrase
; Noun-Phrase := Article + Noun
; Verb-Phrase := Verb + Noun-Phrase
; Article := the, a, ...
; Noun := man, ball, woman, table, ...
; Verb := hit, took, saw, liked, ...

(defparameter *ARTICLES* '(the a '(a different)))

(defparameter *NOUNS* '(man woman dog ball cat table))

(defparameter *VERBS* '(hit took saw liked poked))

(defun sentence ()
    (append (noun-phrase) (verb-phrase)))

(defun noun-phrase ()
    (append (Article) (Noun)))

(defun verb-phrase ()
    (append (Verb) (noun-phrase)))

(defun Article ()
    (one-of *ARTICLES*))

(defun Noun ()
    (one-of *NOUNS*))

(defun Verb ()
    (one-of *VERBS*))

; Auxiliary Functions

(defun one-of (set)
    "Pick one element of a set, and return it as a one-element list."
    (list (random-elt set)))

(defun random-elt (choices)
    "Choose an element from a list at random."
    (elt choices (random (length choices))))
