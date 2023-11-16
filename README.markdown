# Consistent Hashing

Common Lisp consistent hashing implementation.

## Usage

```common-lisp
(defvar my-ring (ch:make-ring :replication-factor 4
                              :finder (ch:make-finder-tree)
                              :hash-function 'sxhash))

(ch:ring-insert my-ring "192.168.0.1")

(ch:virtual-node-hostname
 (ch:ring-find-virtual-node-for my-ring "www.image.com"))

(ch:virtual-node-hostname
 (ch:ring-find-virtual-node-for my-ring "www.image.com"))
```

## Author

* Alan Gomes

## Copyright
Copyright (c) 2023 Alan Gomes

## License

Licensed under the MIT License.
