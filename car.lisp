;;;
;;; car.lisp --- CAR specification.
;;;

;;; CAR CLOS representation:

(defclass fuel-tank ()
  ((name :reader name :initarg :name :type symbol)))

(defclass section ()
  ((fuel-tank :reader fuel-tank :initarg :fuel-tank :type fuel-tank)))

(defclass pipe ()
  ((sections :reader sections :initarg :sections :type list)))

(defclass chamber ()
  ((upper-pipe :reader upper-pipe :initarg :upper-pipe :type pipe)
   (lower-pipe :reader lower-pipe :initarg :lower-pipe :type pipe)))

(defclass car-engine ()
  ((chambers :reader chambers :initarg :chambers :type list)))

;;; Generate CAR representation.

(defmethod make-fuel-tank (name)
  (make-instance 'fuel-tank
                 :name name))

(defmethod make-section (name)
  (make-instance 'section
                 :fuel-tank (make-fuel-tank name)))

(defmethod make-pipe (names)
  (make-instance 'pipe
                 :sections (mapcar #'make-section names)))

(defmethod make-chamber (upper-names/lower-names)
  (make-instance 'chamber
                 :upper-pipe (make-pipe (first  upper-names/lower-names))
                 :lower-pipe (make-pipe (second upper-names/lower-names))))

(defmethod make-car-engine (chambers-names)
  (make-instance 'car-engine
                 :chambers (mapcar #'make-chamber chambers-names)))

;;; Theorem

#|

  Each CAR parametrizated with list of two lists of fuel-tank names.

|#

;;; Example

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

|#

(make-car-engine           ;; one chamber
 '(((fuel0 fuel0)          ;; <<-- first  list of fuel-tank names.
    (fuel0 fuel1 fuel0)))) ;; <<-- second list of fuel-tank names.
