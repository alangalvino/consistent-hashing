(in-package :consistent-hashing)

(defclass finder-tree (finder) ()
  (:documentation "A finder implemented using a avl binary tree."))

(defun make-finder-tree ()
  (make-finder :data-structure
               (trees:make-binary-tree :avl #'(lambda (x y)
                                                (< x y))
                                       :key #'virtual-node-key)))

(defmethod finder-insert ((finder finder) (virtual-node virtual-node))
  (with-slots ((tree data-structure)) finder
    (trees:insert virtual-node tree)))

(defmethod finder-delete ((finder finder) (virtual-node virtual-node))
  (with-slots ((tree data-structure)) finder
    (trees:delete (virtual-node-key virtual-node) tree)))

(defmethod finder-greater-or-equal ((finder finder) value)
  (with-slots ((tree data-structure)) finder
    (trees:upper-bound value tree)))

(defmethod finder-minimum ((finder finder))
  (with-slots ((tree data-structure)) finder
    (trees:minimum tree)))
