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

(defmethod %ring-node-member-p ((ring ring) node-hostname)
  (let ((nodes-set (ring-nodes-set ring)))
    (member node-hostname nodes-set :key #'string :test #'equal)))

(defmethod %ring-insert-node ((ring ring) node-hostname)
  (unless (%ring-node-member-p ring node-hostname)
    (setf (ring-nodes-set ring)
          (push node-hostname (ring-nodes-set ring)))))

(defmethod %ring-delete-node ((ring ring) node-hostname)
  (when (%ring-node-member-p ring node-hostname)
    (setf (ring-nodes-set ring)
          (remove node-hostname (ring-nodes-set ring) :key #'string :test #'equal))))

(defmethod %ring-hash-value ((ring ring) value)
  (let ((hash-function (ring-hash-function ring)))
    (funcall hash-function value)))

(defmethod %ring-hash-virtual-node ((ring ring) node-hostname index)
  (let ((hash-function (ring-hash-function ring)))
    (funcall hash-function (format nil *virtual-node-key-format-string* node-hostname index))))

(defmacro %for-each-virtual-node (ring node-hostname &body body)
  `(let* ((replication-factor (ring-replication-factor ,ring))
          (finder (ring-finder ,ring)))
     (dotimes (replica-index replication-factor)
       (let ((virtual-node (make-virtual-node :hostname ,node-hostname
                                              :key (%ring-hash-virtual-node ,ring
                                                                            ,node-hostname
                                                                            replica-index))))
         ,@body))))

(defmethod ring-insert ((ring ring) node-hostname)
  (%ring-insert-node ring node-hostname)
  (%for-each-virtual-node ring node-hostname
    (finder-insert finder virtual-node)))

(defmethod ring-delete ((ring ring) node-hostname)
  (%ring-delete-node ring node-hostname)
  (%for-each-virtual-node ring node-hostname
    (finder-delete finder virtual-node)))

(defmethod ring-find-virtual-node-for ((ring ring) value)
  (let* ((hashed-value (%ring-hash-value ring value))
         (finder (ring-finder ring))
         (virtual-node-for-value (or (finder-greater-or-equal finder hashed-value)
                                     (finder-minimum finder))))
    virtual-node-for-value))

(defmethod ring-nodes ((ring ring))
  (ring-nodes-set ring))
