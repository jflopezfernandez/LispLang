
; Grammar
; Sentence := Noun-Phrase + Verb-Phrase
; Noun-Phrase := Article + Noun
; Verb-Phrase := Verb + Noun-Phrase
; Article := the, a, ...
; Noun := man, ball, woman, table, ...
; Verb := hit, took, saw, liked, ...

; List definitions

(defparameter *ARTICLES* '(the a '(a different)))

(defparameter *NOUNS* '(man woman dog ball cat table))

(defparameter *VERBS* '(hit took saw liked poked))

; Grammar rules

(defparameter *SIMPLE-GRAMMAR* '(
    (sentence -> (noun-phrase verb-phrase))
    (noun-phrase -> (Article Noun))
    (verb-phrase -> (Verb noun-phrase))
    (Article -> the a)
    (Noun -> man ball woman table)
    (Verb -> hit took saw liked))
    "A grammar for a trivial subset of English.")

(defparameter *BIGGER-GRAMMAR* '(
    (sentence -> (noun-phrase verb-phrase))
    (noun-phrase -> (Article Adj* Noun PP*) (Name) (Pronoun))
    (verb-phrase -> (Verb noun-phrase PP*))
    (PP* -> () (PP PP*))
    (Adj* -> () (Adj Adj*))
    (PP -> (Prep noun-phrase))
    (Prep -> to in by with on)
    (Adj -> big little blue green adiabatic)
    (Article -> the a)
    (Name -> Pat Kim Lee Terry Robin)
    (Noun -> man ball woman table)
    (Verb -> hit took saw liked)
    (Pronoun -> he she it these those that)))

(defvar *GRAMMAR* *BIGGER-GRAMMAR*
    "The grammar used by the generate function.")

; Grammar auxiliary functions

(defun rule-lhs (rule)
    "The left-hand side of a rule."
    (first rule))

(defun rule-rhs (rule)
    "The right-hand side of a rule."
    (rest (rest rule)))

(defun rewrites (category)
    "Return a list of the possible rewrites of this category."
    (rule-rhs (assoc category *GRAMMAR*)))

(defun generate-tree (phrase)
    "Generate a random sentence or phrase, with a complete parse tree."
    (cond ((listp phrase)
           (mapcar #'generate-tree phrase))
          ((rewrites phrase)
           (cons phrase
                (generate-tree (random-elt (rewrites phrase)))))
          (t (list phrase))))

; Generator functions

(defun generate (phrase)
    "Generate a random sentence or phrase."
    (cond ((listp phrase)
            (mappend #'generate phrase))
            ((rewrites phrase)
             (generate (random-elt (rewrites phrase))))
            (t (list phrase))))

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

; Generator auxiliary Functions

(defun mappend (fn the-list)
    "Apply fn to each element in the specified list and return a list of 
    the results."
    (if (null the-list)
        nil
        (append (funcall fn (first the-list))
                (mappend fn (rest the-list)))))

(defun one-of (set)
    "Pick one element of a set, and return it as a one-element list."
    (list (random-elt set)))

(defun random-elt (choices)
    "Choose an element from a list at random."
    (elt choices (random (length choices))))
