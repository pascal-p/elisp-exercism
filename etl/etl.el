;;; etl.el --- etl Exercise (exercism)

;;; Commentary:

;;; Code:
(defun etl (htable)
  (if (= 0 (hash-table-count htable)) htable
    (let ((n-htable (make-hash-table :test 'equal)))
      (progn
        (maphash (lambda (k v)
                   (if (< k 0) (throw 'Error "negative key not supported")
                     (transform k v n-htable))) htable)
        n-htable)
      )))

(defun transform (key values nhtable)
  "values are becoming new keys whose value are the initial key"
  (mapcar (lambda(v)
            (if (not (string-match "^[A-Za-z]$" v))
                (throw 'Error "only alphabetical values ares supported")
              (puthash (downcase v) key nhtable)))
          values))

(provide 'etl)
;;; etl.el ends here
