(in-package :consistent-hashing)

(defclass finder ()
  ((data-structure
    :accessor finder-data-structure
    :initarg :data-structure
    :documentation "Consistent hashing search data structure.")))

(defgeneric finder-insert (finder virtual-node)
  (:documentation "Inserts virtual node into the consistent hashing search data structure."))

(defgeneric finder-delete (finder virtual-node)
  (:documentation "Deletes virtual node from the consistent hashing search data structure."))

(defgeneric finder-greater-or-equal (finder value)
  (:documentation "Returns a virtual node whitch key is greater or equal to the value."))

(defgeneric finder-minimum (finder)
  (:documentation "Returns the virtual node with the minimum key from the consistent hashing search data structure."))

(defun make-finder (&key data-structure)
  (make-instance 'finder :data-structure data-structure))
