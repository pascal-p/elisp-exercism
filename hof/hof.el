;;; hof.el --- higher order function

;;; Commentary:

;;; Code:
(require 'cl-lib)

(defun ya:odd-pred (x)
  (= 1 (% x 2)))

(defmacro ya:remove-if-not (pred-fn &rest args)
  "build the complementary function of remove-if"
  `(cl-remove-if (lambda (x) (not (funcall ,pred-fn x))) ,@args))

(defmacro ya:complement (fn)
  "defines the complement of a given function"
  `(lambda (&rest args) (not (apply ,fn args))))

(defalias 'ya:~ 'ya:complement)

;; now we can rewrite remove-if-not in term of complement
(defmacro ya:remove-if-not-b (pred-fn &rest args)
  `(cl-remove-if (ya:~ ,pred-fn) ,@args)
  )

;; build an operator to return destrcutive counter-part of a non destructive function
;; non destructive | destructive
;;  cl-remove-if   | cl-delete-if
;;  reverse        | nreverse
;;  append         | nconc

(defvar *!store-fns* (make-hash-table))

(defmacro ! (fn)
  "retrieve destructive alias from *!store-fns*"
  `(or (gethash ,fn *!store-fns*) ,fn))

(defmacro def! (fn fn!)
  `(setf (gethash ,fn *!store-fns*) ,fn!))

(def! 'cl-remove-if 'cl-delete-if)  ;; now we can use (! cl-remove-if) (as destructive equivalnet of cl-remove-if instead of ...)
(def! 'reverse 'nreverse)
(def! 'append 'nconc)

(defalias 'ya:remove-if! (! 'cl-remove-if))
(defalias 'ya:reverse! (! 'reverse))
(defalias 'ya:append! (! 'append))

(provide 'hof)
;;; hof.el ends here
