;;;; -*- Mode: LISP; Syntax: Common-lisp; Package: USER; Base: 10 -*-
;;;; Name: Michael Mangrobang
;;;; Date:  03/09/2018
;;;; Course: ICS313
;;;; Assignment: #4
;;;; File: mjm44specialactions.lisp

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;------- wizard special actions -------;;;;
;;; wizard special actions code below here ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; macro: game-action
;;; parameters: command, the command; subj, the subject to interact with; obj, the object to interact with; place, the location to be in; body the body of the maro
;;; return: the macro
;;; description: game action macro that handles majority of the game inceractions
(defmacro game-action (command subj obj place &body body)
    `(progn (defun ,command (subject object)
        (if (and (eq *location* ',place)
            (eq subject ',subj)
            (eq object ',obj)
            (have ',subj))
            ,@body
            '(i cant ,command like that.)))
        (pushnew ',command *allowed-commands*)
    )
)

(defparameter *chain-welded* nil)     ; chain-wield boolean

;;; macro: game-action
;;; parameters: weld, the command; chain, the subject; bucket, the object; attic, the location
;;; return: a bucket with a chain welded to it
;;; description: the macro to weld a bucket to a chain
(game-action weld chain bucket attic
    (if (and (have 'bucket) (not *chain-welded*))
        (progn (setf *chain-welded* 't)
            '(the chain is now securely welded to the bucket.))
            '(you do not have a bucket.)
    )
)

(defparameter *bucket-filled* nil)    ; bucket-filled boolean

;;; macro: game-action
;;; parameters: dunk, the command; bucket, the subject; well, the object; garden, the location
;;; return: a bucket filled with water
;;; description: game macro to fill the bucket with water
(game-action dunk bucket well garden
    (if *chain-welded*
        (progn (setf *bucket-filled* 't)
        '(the bucket is now full of water))
        '(the water level is too low to reach.)
    )
)

;;; macro: game-action
;;; parameters: splash, the command; bucket, the subject; wizard, the object; living-room, the location
;;; return: the ending of the game
;;; the macro that "ends" the game
(game-action splash bucket wizard living-room
    (cond
        ((not *bucket-filled*)
            '(the bucket has nothing in it.)
        )
        ((have 'frog)
            '(the wizard awakens and sees that you stole his frog. he is so upset he banishes you to the netherworlds- you lose! the end.)
        )
        (t
            '(the wizard awakens from his slumber and greets you warmly. he hands you the magic low-carb donut- you win! the end.)
        )
    )
)
