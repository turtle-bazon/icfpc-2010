;;;
;;; car.lisp --- Technical Specification of Cars.
;;;              or
;;;              CLOS/S-CAR-FORMs representations of CARs.
;;;

(in-package :icfpc)

;;; CLOS representation:

(defclass section ()
  ((fuel-tank  :reader   fuel-tank  :initarg  :fuel-tank :type symbol)
   (fuel-value :accessor fuel-value :initform 0          :type integer)))

(defclass pipe ()
  ((sections :reader sections :initarg :sections :type list)))

(defclass chamber ()
  ((upper-pipe :reader upper-pipe :initarg :upper-pipe :type pipe)
   (lower-pipe :reader lower-pipe :initarg :lower-pipe :type pipe)))

(defclass car-engine ()
  ((chambers :reader chambers :initarg :chambers :type list)))

;;; Representation in S-CAR-FORMs:
;;;   Each CAR parametrizated with list of two lists of fuel-tank names.

;;; S-CAR-FORM -> CLOS convertion:

(defun make-section (name)
  (make-instance 'section
                 :fuel-tank name))

(defun make-pipe (names)
  (make-instance 'pipe
                 :sections (mapcar #'make-section names)))

(defun make-chamber (upper-names/lower-names)
  (make-instance 'chamber
                 :upper-pipe (make-pipe (first  upper-names/lower-names))
                 :lower-pipe (make-pipe (second upper-names/lower-names))))

(defun make-car-engine (s-car-form)
  (make-instance 'car-engine
                 :chambers (mapcar #'make-chamber s-car-form)))

;;; CLOS -> S-CAR-FORM with keys convertion:

(defmethod s-section ((section section))
  (list :section (fuel-tank section) (fuel-value section)))

(defmethod s-pipe ((pipe pipe))
  (cons :pipe (mapcar #'s-section (sections pipe))))

(defmethod s-chamber ((chamber chamber))
  (list (cons :upper-pipe (s-pipe (upper-pipe chamber)))
        (cons :lower-pipe (s-pipe (lower-pipe chamber)))))

(defmethod s-car-engine ((car-engine car-engine))
  (cons :car-engine (mapcar #'s-chamber (chambers car-engine))))

;;; PRINT-OBJECT for CAR objects:

(defmethod print-object ((section section) stream)
  (format stream "<:section to fuel-tank ~A>" (fuel-tank section)))

(defmethod print-object ((pipe pipe) stream)
  (mapcar #'(lambda (e) (print e stream))
          (cons :pipe (sections pipe))))

(defmethod print-object ((chamber chamber) stream)
  (mapcar #'(lambda (e) (print e stream))
          (list :chamber (upper-pipe chamber) (lower-pipe chamber))))

(defmethod print-object ((car-engine car-engine) stream)
  (mapcar #'(lambda (e) (print e stream))
          (cons :car-engine (chambers car-engine))))

;;; Examples:

#|

  CAR with one CHAMBER.

      upper pipe:  -> section -> section ------------.
    /                   /          /                  \
   /         fuel 0  --<----------'--------.      difference      positive
 air                    \                   \       engine  --->  energy
   \                     \                   \        /
    \ lower pipe:  -> section -> section -> section -'
                                    /
             fuel 1 ---------------'

(make-car-engine           ;; one chamber
 '(((fuel0 fuel0)          ;; <<-- first  list of fuel-tank names.
    (fuel0 fuel1 fuel0)))) ;; <<-- second list of fuel-tank names.

;; =>

:CAR-ENGINE 

:CHAMBER 

:PIPE 
<:section to fuel-tank FUEL0> 
<:section to fuel-tank FUEL0>  

:PIPE 
<:section to fuel-tank FUEL0> 
<:section to fuel-tank FUEL1> 
<:section to fuel-tank FUEL0>

(s-car-engine
 (make-car-engine
  '(((fuel0 fuel0)
     (fuel0 fuel1 fuel0)))))

;; =>

(:CAR-ENGINE
 ((:UPPER-PIPE :PIPE (:SECTION FUEL0 0) (:SECTION FUEL0 0))
  (:LOWER-PIPE :PIPE (:SECTION FUEL0 0) (:SECTION FUEL1 0) (:SECTION FUEL0 0))))

|#
