(defpackage cl-consistent-hashing
  (:use :cl))
(in-package :cl-consistent-hashing)

(defparameter *replicas* 4)

(defparameter *ring*
  (trees:make-binary-tree :avl #'(lambda (x y)
                                   (< x y))
                          :key #'car))

(defun ring-insert-node (node)
  (dotimes (replica-index 4)
    (trees:insert (list (%ring-hash-key (format nil "~s:~s" node replica-index)) node)
                  *ring*)))

(defun ring-delete-node (node)
  (dotimes (replica-index 4)
    (trees:delete (%ring-hash-key (format nil "~s:~s" node replica-index))
                  *ring*)))

(defun ring-find-node-for (any-object)
  (let* ((hashed-object (%ring-hash-key any-object))
         (node-for-object (or (trees:upper-bound hashed-object *ring*) (trees:minimum *ring*))))
    node-for-object))

(defun ring-nodes ()
  (let ((unique-nodes '()))
    (trees:dotree (virtual-node *ring*)
      (let ((virtual-node-host (cadr virtual-node)))
        (unless (member virtual-node-host unique-nodes)
          (push virtual-node-host unique-nodes))))
    unique-nodes))

(defun %ring-hash-key (key)
  (sxhash key))
