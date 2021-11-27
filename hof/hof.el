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

(provide 'hof)
;;; hof.el ends here
