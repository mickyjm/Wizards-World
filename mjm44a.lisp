;;;; -*- Mode: LISP; Syntax: Common-lisp; Package: USER; Base: 10 -*-
;;;; Name: Michael Mangrobang
;;;; Date:  03/09/2018
;;;; Course: ICS313
;;;; Assignment: #4
;;;; File: mjm44a.lisp

(defparameter +ID+ "Michael Mangrobang")    ; define parameter +ID+ with the value of my name

;;; function: id
;;; paramaters: course*, the class course number (should be 313); assignment*, the assignment number (should be 3)
;;; return: nil
;;; description: prints my name, course number and assignment number
(defun id (course* assignment*)
    (cond
        ((not (eq course* 313))                           ; checks if course given is 313
            (princ "Invalid Course Number")               ; prints error
        )
        ((not (eq assignment* 4))                         ; checks if assignment given is 4
            (princ "Invalid Assignment Number")           ; prints error
        )
        (t (princ "Name: ")                               ; prints my name
            (format t +ID+)
            (terpri)                                      ; prints new line
            (format t "Course: ~D~%" course*)             ; prints course number
            (format t "Assignment # ~D~%" assignment*)    ; prints assignment number
        )
    )
    (return-from id nil)                                  ; returns nil
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;-------------------- PART A --------------------;;;;
;;; The macros and helper functions are located here ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; function: location-exist
;;; paramaeters: name*, the name of the location to find in locations*; locations*, the list of locations
;;; return: t if the location already exists, nil if it does not exist
;;; description: checks if the new location to add in exists
(defun location-exist (name* locations*)
    (cond
        ((null locations*)                              ; check if locations is null
            nil                                         ; returns nil
        )
        ((eq name* (caar locations*))                   ; check if the name is the same as the first item in the list of the list
            t                                           ; returns t
        )
        (t
            (location-exist name* (cdr locations*))     ; recursive cal to check next item in list
        )
    )
)

;;; function: object-exist
;;; paramaeters: name*, the name of the object to find in objects*; objects*, the list of objects
;;; return: t if the object already exists, nil if it does not exist
;;; description: checks if the new object to add in exists
(defun object-exist (name* objects*)
    (cond
        ((null objects*)                            ; check if objects is null
            nil                                     ; return nil
        )
        ((eq name* (car objects*))                  ; check if the name is the same as the first item in the list
            t                                       ; returns t
        )
        (t
            (object-exist name* (cdr objects*))     ; recursive call to check next item in list
        )
    )
)

;;; function: path-exist
;;; paramaeters: path*, the path to find in locations*; locations*, the list of locations
;;; return: t if the path already exists, nil if it does not exist
;;; description: checks if the new path to add in exists
(defun path-exist (path* locations*)
    (cond
        ((null locations*)                          ; checks if locations is null
            nil                                     ; returns nil
        )
        ((eq path* (caar locations*))               ; check if path exists in the first item of the list in a list
            t                                       ; returns t
        )
        (t
            (path-exist path* (cdr locations*))     ; recursive call to check next item in list
        )
    )
)

;;; function: not-exists
;;; parameters: object*, the game object that does not exist
;;; return: nil
;;; description: prints the the object to find does not exist
(defun not-exists (object*)
    (format t "Error, ~S, does not exist in the game world." object*)
)

;;; function: exists
;;; parameters: object*, the game object that does already exists
;;; return: nil
;;; description: prints the the object to find does already exists
(defun exists (object*)
    (format t "Error, ~S, does already exists in the game world." object*)
)

;;; function: path-exists
;;; parameters: location01*, the first area; location02*, the second area
;;; return nil
;;; description: prints that the path from location01 to location 02 exists
(defun path-exists (location01* location02*)
    (format t "Error, there already exists a path from ~S to ~S." location01* location02*)
)

;;; function: symbol-error
;;; parameters: name*, name of the function with error; param*, the argument passed in
;;; return: nil
;;; description: prints the error message
(defun symbol-error (param*)
    (format t "Error, ~S, is not a symbol." param*)
)

;;; macro: new-location
;;; parameters: name*, the name of the location to add; descr*, the description of the location; extra*, catches any extra arguments typed in, so there is no error
;;; return: the list of locations with the new inserted location
;;; description: a macro that adds a new location to the game world
(defmacro new-location (name* descr* &rest extra*)
    `(cond
          ((not (symbolp ',name*))                                              ; check if the name is a symbol
              (symbol-error ',name*)                                            ; print error
          )
          ((not (listp ',descr*))                                               ; check if the description is a list
              (format t "Error, the description needs to be in a list")         ; print error
          )
          ((not (null (location-exist ',name* *nodes*)))                        ; check if the location exists
              (exists ',name*)                                                  ; print error
          )
          (t
              (push (list ',name*) *edges*)                                     ; push in the new location to edges
              (push (list ',name* ',descr*) *nodes*)                            ; push in the new location
          )
     )
)

;;; macro: new-object
;;; parameters: name*, the name of the location to add; descr*, the description of the location;
;;; return: the list of objects with the new inserted object
;;; description: a macro that adds a new object to the game world
(defmacro new-object(name* location*)
    `(cond
          ((not (symbolp ',name*))                                    ; check if the name is a symbol
              (symbol-error ',name*)                                  ; print error
          )
          ((not (symbolp ',location*))                                ; check if location is a symbol
              (symbol-error ',location*)                              ; print error
          )
          ((not (null (object-exist ',name* *objects*)))              ; check if the object already exists
              (exists ',name*)                                        ; print error
          )
          ((null (location-exist ',location* *nodes*))                ; check if the location does not exists
              (not-exists ',location*)                                ; print error
          )
          (t
              (push ',name* *objects*)                                ; push the new object into the list
              (push (list ',name* ',location*) *object-locations*)    ; push the new object into the game world
          )
     )
)

;;; macro: new-path
;;; parameters: location01*, the name of the first location to connect to location02*; location02*, the name of the second location to connect to location01*; direction01*, the direction to travel from location01* to location02*; pathway*, the path that connects location01* to location02*; direction02*, the direction to travel from location02* to location01*
;;; return: the list of paths with the new inserted path(s)
;;; description: a macro that adds a new path to the game world
(defmacro new-path (location01* location02* direction01* pathway* &optional direction02*)
    `(cond
        ((not(symbolp ',location01*))                                       ; check if location01* is a symbol
            (symbol-error ',location01*)                                    ; print error
        )
        ((not(symbolp ',location02*))                                       ; check if location02* is a symbol
            (symbol-error ',location02*)                                    ; print error
        )
        ((not(symbolp ',direction01*))                                      ; check if direction01* is a symbol
            (symbol-error ',direction01*)                                   ; print error
        )
        ((not(symbolp ',pathway*))                                          ; check if pathway* is a symbol
            (symbol-error ',pathway*)                                       ; print error
        )
        ((not(symbolp ',direction02*))                                      ; check if direction02 is a symbol
            (symbol-error ',direction02*)                                   ; print error
        )
        ((null (location-exist ',location01* *nodes*))                      ; check if location01 exists
            (not-exists ',location01*)                                      ; print error
        )
        ((null (location-exist ',location02* *nodes*))                      ; checks if location02 extsts
            (not-exists ',location02*)                                      ; print error
        )
        ((path-exist ',location01* (cdr (assoc ',location02* *edges*)))     ; check if the path from location01 to location02 exists
            (path-exists ',location01* ',location02*)    ; print error
        )
        ((path-exist ',location02* (cdr (assoc ',location01* *edges*)))     ; check if the path from location02 to location01 exists
            (path-exists ',location02* ',location01*)                       ; print error
        )
        ((not (null ',direction02*))                                        ; check if direction02* is not null
            (push
                (list ',location01* ',direction02* ',pathway*)              ; push path from location02* to location01*
                (cdr (assoc ',location02* *edges*))
            )
            (push
                (list ',location02* ',direction01* ',pathway*)              ; push path from location01* to location02*
                (cdr (assoc ',location01* *edges*))
            )
        )
        (t
            (push
                (list ',location02* ',direction01* ',pathway*)              ; push path from location01* to location02*
                (cdr (assoc ',location01* *edges*))
            )
        )
    )
)

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


(load "mjm44b.lisp")                  ; load the part b file

(load "mjm44c.lisp")                  ; load the part c file
