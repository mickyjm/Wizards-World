;;;; -*- Mode: LISP; Syntax: Common-lisp; Package: USER; Base: 10 -*-
;;;; Name: Michael Mangrobang
;;;; Date:  03/09/2018
;;;; Course: ICS313
;;;; Assignment: #4
;;;; File: mjm44wizardspart2.lisp

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;------- wizards_game part 2 -------;;;;
;;; wizards game part 2 code below here ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; defines the function to continuously read commands
(defun game-repl ()
    (let ((cmd (game-read)))
        (unless (eq (car cmd) 'quit)
            (game-print (game-eval cmd))
            (game-repl)
        )
    )
)

;;; defines function to concatenate the parenthesis and execute functions in LISP
(defun game-read ()
    (let ((cmd (read-from-string (concatenate 'string "(" (read-line) ")"))))
        (flet ((quote-it (x)
            (list 'quote x)))
            (cons (car cmd) (mapcar #'quote-it (cdr cmd)))
        )
    )
)

;;; defines the list of commands allowed within game-repl
(defparameter *allowed-commands* '(look walk pickup inventory smelt talk help h ?))

;;; defines function to check if command exists
(defun game-eval (sexp)
    (if (member (car sexp) *allowed-commands*)
        (eval sexp)
        (help)    ;'(i do not know that command.)
    )
)

;;; defines function to modify text with sentence casing and removing the tacs
(defun tweak-text (lst caps lit)
    (when lst
        (let ((item (car lst))
            (rest (cdr lst)))
            (cond
                ((eql item #\space)
                    (cons item (tweak-text rest caps lit))
                )
                ((member item '(#\! #\? #\.))
                    (cons item (tweak-text rest t lit))
                )
                ((eql item #\")
                    (tweak-text rest caps (not lit))
                )
                (lit
                    (cons item (tweak-text rest nil lit))
                )
                (caps
                    (cons (char-upcase item) (tweak-text rest nil lit))
                )
                (t
                    (cons (char-downcase item) (tweak-text rest nil nil))
                )
            )
        )
    )
)

;;; defines function to print lines
(defun game-print (lst)
    (princ
        (coerce
            (tweak-text (coerce (string-trim "() " (prin1-to-string lst)) 'list) t nil) 'string
        )
    )
    (fresh-line)
)
