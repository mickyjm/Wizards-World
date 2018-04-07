;;;; -*- Mode: LISP; Syntax: Common-lisp; Package: USER; Base: 10 -*-
;;;; Name: Michael Mangrobang
;;;; Date:  03/09/2018
;;;; Course: ICS313
;;;; Assignment: #4
;;;; File: mjm44wizardspart1.lisp

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;------- wizards_game part 1 -------;;;;
;;; wizards game part 1 code below here ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; defines parameter *nodes* that holds all the areas of wizard worlds along with their descriptions
(defparameter *nodes* '((living-room (you are in the living-room.
                            a wizard is snoring loudly on the couch.))
                        (garden (you are in a beautiful garden.
                            there is a well in front of you.))
                        (attic (you are in the attic.
                            there is a giant welding torch in the corner.))
                        (water-well (you are near a water-well.
                            the well is dry.))
                        (smelting-chamber (you are in the smelting-chamber.
                            there is a swordless knight near the entrance and a smelter at the far end of the room.))))

;;; defines function that helps print out the description of each location
(defun describe-location (location nodes)
   (cadr (assoc location nodes)))

;;; defines parameter *edges* that sets what each location is connected to
(defparameter *edges* '((living-room (garden west door)
                                     (attic upstairs ladder))
                        (garden (living-room east door)
                                (water-well south walkway)
                                (smelting-chamber west walkway))
                        (attic (living-room downstairs ladder))
                        (water-well (garden north walkway))
                        (smelting-chamber (garden east door))))

;;; defines function that helps descrbe to the path to connected areas
(defun describe-path (edge)
  `(there is a ,(caddr edge) going ,(cadr edge) from here.))

;;; defines function that describes the locations along with their edges
(defun describe-paths (location edges)
  (apply #'append (mapcar #'describe-path (cdr (assoc location edges)))))

;;; defines *objects* which is the list of objects in the game world
(defparameter *objects* '(whiskey bucket frog chain hilt iron-bar sword 999999999gold))

;;; defines *object-locations* which is where certain items are located
(defparameter *object-locations* '((whiskey living-room)
                                   (bucket living-room)
                                   (chain garden)
                                   (frog garden)
                                   (hilt water-well)
                                   (iron-bar smelting-chamber)))

;;; defines function that finds the object in the location
(defun objects-at (loc objs obj-loc)
   (labels ((is-at (obj)
              (eq (cadr (assoc obj obj-loc)) loc)))
       (remove-if-not #'is-at objs)))

;;; defines function which helps describes the object in the area
(defun describe-objects (loc objs obj-loc)
   (labels ((describe-obj (obj)
                `(you see a ,obj on the floor.)))
      (apply #'append (mapcar #'describe-obj (objects-at loc objs obj-loc)))))

;;; defines default starting position
(defparameter *location* 'living-room)

;;; defines game action, to look around the current area
(defun look ()
  (append (describe-location *location* *nodes*)
          (describe-paths *location* *edges*)
          (describe-objects *location* *objects* *object-locations*)))

;;; defines game action to walk around game world
(defun walk (direction)
  (labels ((correct-way (edge)
             (eq (cadr edge) direction)))
    (let ((next (find-if #'correct-way (cdr (assoc *location* *edges*)))))
      (if next
          (progn (setf *location* (car next))
                 (look))
          '(you cannot go that way.)))))

;;; defines function to pickup objects in game world
(defun pickup (object)
  (cond ((member object (objects-at *location* *objects* *object-locations*))
         (push (list object 'body) *object-locations*)
         `(you are now carrying the ,object))
	  (t '(you cannot get that.))))

;;; defines function to display current items in your inventory
(defun inventory ()
  (cons 'items- (objects-at 'body *objects* *object-locations*)))

;;; defines function to check if you have the item in your inventory
(defun have (object)
    (member object (cdr (inventory))))
