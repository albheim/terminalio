;(defpackage :terminalio (:use :cl :hunchentoot))

;(in-package :terminalio)


(defparameter *objects* '(whiskey-bottle bucket frog chain))

(defparameter *map* '((living-room (you are in the living-room of a wizards house. there is a wizard snoring loudly on the couch.)
               (west door garden)  
               (upstairs stairway attic))
              (garden (you are in a beautiful garden. there is a well in front of you.)
               (east door living-room))
              (attic (you are in the attic of the wizards house. there is a giant welding torch in the corner.)
               (downstairs stairway living-room))))

(defparameter *object-locations* '((whiskey-bottle living-room)
                           (bucket living-room)
                           (chain garden)
                           (frog garden)))

(defparameter *location* 'living-room)

(defparameter *doors* '((living-room attic nil 15723)
                        (living-room garden nil 516o235)))

(defparameter *chain-welded* nil)

(defparameter *bucket-filled* nil)

;(defparameter *actions* '(look take use inventory help))
(defparameter *actions* '(help look inventory walk pickup))

(defun describe-location (location map)
  (second (assoc location map)))

(defun describe-path (path)
  `(there is a ,(second path) going ,(first path) from here.))

(defun describe-paths (location map)
  (apply #'append (mapcar #'describe-path (cddr (assoc location map)))))

(defun is-at (obj loc obj-loc)
  (eq (second (assoc obj obj-loc)) loc))

(defun describe-floor (loc objs obj-loc)
  (apply #'append (mapcar (lambda (x)
                            `(you see a ,x on the floor.))
                          (remove-if-not (lambda (x)
                                           (is-at x loc obj-loc))
                                         objs))))

(defun look ()
  (append (describe-location *location* *map*)
          (describe-paths *location* *map*)
          (describe-floor *location* *objects* *object-locations*)))

(defun walk-direction (direction)
  (let ((next (assoc direction (cddr (assoc *location* *map*)))))
    (cond (next (setf *location* (third next)) (look))
          (t '(you cannot go that way.)))))

(defmacro defspel (&rest rest) `(defmacro ,@rest))

(defspel walk (direction)
  `(walk-direction ',direction))

(defun pickup-object (object)
  (cond ((is-at object *location* *object-locations*)
         (push (list object 'body) *object-locations*)
         `(you are now carrying the ,object))
        (t '(you cannot get that.))))

(defspel pickup (object)
  `(pickup-object ',object))

(defun inventory ()
  (remove-if-not (lambda (x)
                   (is-at x 'body *object-locations*))
                 *objects*))

(defun have (object)
  (member object (inventory)))

(defspel game-action (command subj obj place &rest rest)
  `(defspel ,command (subject object)
     `(cond ((and (eq *location* ',',place)
                  (eq ',subject ',',subj)
                  (eq ',object ',',obj)
                  (have ',',subj))
             ,@',rest)
            (t '(i cant ,',command like that)))))


(game-action weld chain bucket attic
             (cond ((and (have 'bucket) (setf *chain-welded* 't))
                    '(the chain is now securely welded to the bucket.))
                   (t '(you do not have a bucket))))


(game-action dunk bucket will garden (cond (*chain-welded* (setf *bucket-filled* 't)
                                                           '(the bucket is nor full of water))
                                           (t '(the water level is too low to reach.))))

(game-action splash bucket wizard living-room
             (cond ((not *bucket-filled*) '(the bucket has nothing in it.))
                   ((have 'frog) '(the wizard awakens and sees that you stole his frog.
                                   he is so upset he banishes you to the
                                   netherworlds- you lose! the end.))
                   (t  '(the wizard awakens from his slumber and greets you warmly.
                         he hands you the magic low-carb donut- you win! the end.))))


(defun help (&optional cmd)
  (cond ((eq (car cmd) 'help) "help shows you general help, help <command> shows you specific help about that command")
        ((eq (car cmd) 'look) "look around in the room you are currently in")
        (t (string-downcase (format nil "available commands are: ~{~A~^, ~}" (mapcar #'string *actions*))))))

(defun string-to-symbols (s)
  (mapcar #'intern (split-sequence:split-sequence #\Space (string-upcase s))))

(defun symbols-to-string (s)
  (string-downcase (format nil "~{~A~^ ~}" (mapcar #'string s))))

(defun run-cmd (cmd)
  (let ((a (string-to-symbols cmd)))
    (cond ((eq (car a) 'help)
           (help (cdr a)))
          ((eq (car a) 'echo)
           (symbols-to-string a))
          ((member (car a) *actions*) 
           (symbols-to-string (eval a)))
          (t "invalid command"))))


;;;; WEB SERVER


(ql:quickload '(hunchentoot))

(defvar *server* (make-instance 'hunchentoot:easy-acceptor :port 4242 :document-root #p"c:/Users/Albin Heimerson/Dropbox/Programmering/terminalio/"))

(hunchentoot:start *server*)

(hunchentoot:define-easy-handler (send-cmd :uri "/cmd") (cmd)
  (setf (hunchentoot:content-type*) "text/plain")
  (run-cmd cmd))


