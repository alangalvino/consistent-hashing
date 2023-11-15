(in-package :consistent-hashing)

(defparameter *replication-factor* 20)
(defparameter *hash-function* 'sxhash)
(defparameter *virtual-node-key-format-string* "~s:~s")

(defclass ring ()
  ((replication-factor
    :accessor ring-replication-factor
    :initarg :replication-factor
    :documentation "Replication factor used to build the virtual nodes.")
   (hash-function
    :accessor ring-hash-function
    :initarg :hash-function
    :documentation "Hash function used to define the key for each virtual node and for the search value.")
   (nodes-set
    :accessor ring-nodes-set
    :initarg :nodes-set
    :initform ())
   (finder
    :accessor ring-finder
    :initarg :finder
    :documentation "Search data structure that is used to store and query the virtual nodes.")))

(defmethod make-ring (&key (replication-factor *replication-factor*) (hash-function 'sxhash) finder)
  (make-instance 'ring :replication-factor replication-factor
                       :hash-function hash-function
                       :finder finder))

(defmethod %ring-insert-node ((ring ring) node-hostname)
  (let ((nodes-set (ring-nodes-set ring)))
    (unless (member node-hostname nodes-set :key #'string :test #'equal)
      (setf (ring-nodes-set ring) (push node-hostname nodes-set)))))

(defmethod %ring-delete-node ((ring ring) node-hostname)
  (let ((nodes-set (ring-nodes-set ring)))
    (if (member node-hostname nodes-set :key #'string :test #'equal)
        (setf (ring-nodes-set ring) (remove node-hostname nodes-set :key #'string :test #'equal)))))

(defmethod %ring-hash-value ((ring ring) value)
  (let ((hash-function (ring-hash-function ring)))
    (funcall hash-function value)))

(defmethod %ring-hash-virtual-node ((ring ring) node-hostname index)
  (let ((hash-function (ring-hash-function ring)))
    (funcall hash-function (format nil *virtual-node-key-format-string* node-hostname index))))

(defmethod ring-insert ((ring ring) node-hostname)
  (let* ((replication-factor (ring-replication-factor ring))
         (data-structure (ring-finder ring)))
    (%ring-insert-node ring node-hostname)
    (dotimes (replica-index replication-factor)
      (finder-insert
       data-structure
       (make-virtual-node :hostname node-hostname
                          :key (%ring-hash-virtual-node ring node-hostname replica-index))))))

(defmethod ring-delete ((ring ring) node-hostname)
  (let* ((replication-factor (ring-replication-factor ring))
         (data-structure (ring-finder ring)))
    (%ring-delete-node ring node-hostname)
    (dotimes (replica-index replication-factor)
      (finder-delete
       data-structure
       (make-virtual-node :hostname node-hostname
                          :key (%ring-hash-virtual-node ring node-hostname replica-index))))))

(defmethod ring-find-virtual-node-for ((ring ring) value)
  (let* ((hashed-value (%ring-hash-value ring value))
         (data-structure (ring-finder ring))
         (virtual-node-for-value (or (finder-greater-or-equal data-structure hashed-value)
                                     (finder-minimum data-structure))))
    virtual-node-for-value))

(defmethod ring-nodes ((ring ring))
  (ring-nodes-set ring))
