;;;; -*- Mode: LISP; Syntax: Common-lisp; Package: USER; Base: 10 -*-
;;;; Name: Michael Mangrobang
;;;; Date:  03/09/2018
;;;; Course: ICS313
;;;; Assignment: #4
;;;; File: mjm44b.lisp

(setf *sword-smelted* nil)    ; sword-smelted boolean

;;; macro: game-action
;;; parameters: smelt, the command; hilt, the first object; iron-bar, the second object; smelting-chamber, the location
;;; return: a smelted sword in the game
;;; description: the macro to smelt a sword
(game-action smelt hilt iron-bar smelting-chamber
    (if (and (have 'hilt) (not *sword-smelted*))
        (progn (setf *sword-smelted* t)
               (setq *objects* (remove 'hilt *objects*))
               (setq *objects* (remove 'iron-bar *objects*))
               (pushnew '(sword smelting-chamber) *object-locations*)
              '(you have smelted a sword.))
        '(you do not have a hilt)
    )
)

(setf *sword-given* nil)    ; sword-given boolean

;;; macro: game-spel
;;; parameters: cmd*, the command; subj*, the subject to interact; place*, the location to be in; body, the body of the macro
;;; return: the macro
;;; description: the macro for talking to somebody
(defmacro game-spel (cmd* subj* place* &body body)
    `(progn (defun ,cmd* (subject*)
        (if (and (eq *location* ',place*)
            (eq subject* ',subj*))
            ,@body
            '(i cant ,cmd* to them.)))
        (pushnew ',cmd* *allowed-commands*)
    )
)

;;; macro: game-spel
;;; parameters: talk, the command; knight, the subject;, smelting-chamber, the location
;;; return: the knight's gratitude for making him a sword
;;; description: talks to the knight to give him a sword
(game-spel talk knight smelting-chamber
    (if (and (have 'sword) (not *sword-given*))
        (progn (setf *sword-given* t)
               (setq *objects* (remove 'sword *objects*))
               (pushnew '(999999999gold smelting-chamber) *object-locations*)
              '(i like that sword you have. may I have it? i can? thank you! have some gold.))
        (if (not *sword-given*)
            '(i am in need of a sword.)
            '(thank you for the sword!)
        )
    )
)
