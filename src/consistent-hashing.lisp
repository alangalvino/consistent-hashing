(defpackage consistent-hashing
  (:nicknames :ch)
  (:use :cl)
  (:export
   
   :make-ring
   :ring-insert
   :ring-delete
   :ring-find-virtual-node-for
   :ring-nodes

   :make-virtual-node
   :virtual-node-key
   :virtual-node-hostname

   :make-finder
   :finder-insert
   :finder-delete
   :finder-greater-or-equal
   :finder-minimum

   :make-finder-tree))

#+nil
(defvar my-ring (ch:make-ring :replication-factor 4
                              :finder (ch:make-finder-tree)
                              :hash-function 'sxhash))

#+nil
(ch:ring-insert my-ring "192.168.0.1")

#+nil
(ch:virtual-node-hostname
 (ch:ring-find-virtual-node-for my-ring "www.image.com"))

#+nil
(ch:ring-nodes my-ring)
