;;; hof.el --- higher order function

;;; Commentary:

;;; Code:
(require 'cl-lib)

(defun odd-pred (x)
  (= 1 (% x 2)))

(defmacro remove-if-not (pred-fn &rest args)
  "build the complementary function of remove-if"
  `(cl-remove-if (lambda (x) (not (funcall ,pred-fn x))) ,@args))

(provide 'hof)
;;; hof.el ends here
