(in-package :consistent-hashing)

(defclass virtual-node ()
  ((key
    :accessor virtual-node-key
    :initarg :key
    :documentation "Consistent hashing virtual node key.")
   (hostname
    :accessor virtual-node-hostname
    :initarg :hostname
    :type string
    :documentation "Consistent hashing virtual node hostname represented as a string.")))

(defun make-virtual-node (&key hostname key)
  (make-instance 'virtual-node :hostname hostname :key key))
