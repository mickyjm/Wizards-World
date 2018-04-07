;;;; -*- Mode: LISP; Syntax: Common-lisp; Package: USER; Base: 10 -*-
;;;; Name: Michael Mangrobang
;;;; Date:  03/09/2018
;;;; Course: ICS313
;;;; Assignment: #4
;;;; File: mjm44c.lisp

;;; function: help
;;; parameters: none
;;; return: *allowed-commands*
;;; description: prints out the only commands that are allowed in the game
(defun help ()
    (format t "These are the only commands that I know: ")
    (terpri)
    *allowed-commands*
)

;;; function: h
;;; parameters: none
;;; return: help function
;;; description: calls th help function to print the only commands allowed in the game
(defun h ()
    (help)
)

;;; function: ?
;;; parameters: none
;;; return: help function
;;; description: calls th help function to print the only commands allowed in the game
(defun ? ()
    (help)
)
