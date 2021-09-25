;;; acronym.el --- Acronym (exercism)

;;; Commentary:

;;; Code:

(require 'cl-lib)

(defun acronym (str)
  (if (= 0 (length str)) ""
    (mapconcat 'identity
               (mapcar 'str-to-1stchar (str-split str))
               "")
    )
  )

(defun str-split (str)
  "Split a string on \"-\" then on \" \"."
  (let ((sep "-")
        (spc " "))
    (split-string (mapconcat 'identity
                             (split-string str sep)
                             spc)
                  spc))
  )

(defun str-to-1stchar (str)
  "Return first character of word (upcase)"
  (if (= 0 (length str)) ""
    (upcase (substring str 0 1)))
  )

(provide 'acronym)
;;; acronym.el ends here
