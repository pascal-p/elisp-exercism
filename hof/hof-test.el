;;; hof-test.el --- Tests for hof

;;; Commentary:

;;; Code:

;; (require 'cl-lib)
(load-file "hof.el")

;; (defun odd-pred (x)
;;   (= 1 (% x 2)))

(ert-deftest test-remove-if ()
  (should (equal (cl-remove-if 'ya:odd-pred '(10 11 20 21 30 31 40 41)) ;; remove odd numbers
                 '(10 20 30 40))))

(ert-deftest test-expansion-remove-if-not ()
    (should (equal (macroexpand '(ya:remove-if-not 'ya:odd-pred '(10 11 20 21 30 31 40 41)))
                   '(cl-remove-if (lambda (x) (not (funcall 'ya:odd-pred x))) '(10 11 20 21 30 31 40 41))
                   )))

(ert-deftest test-remove-if-not ()
 (should (equal (ya:remove-if-not 'ya:odd-pred '(10 11 20 21 30 31 40 41)) ;; remove not odd numbers ie. remove even numbers
                '(11 21 31 41))))

(ert-deftest test-remove-if-not-b ()
 (should (equal (ya:remove-if-not-b 'ya:odd-pred '(10 11 20 21 30 31 40 41)) ;; remove not odd numbers ie. remove even numbers
                '(11 21 31 41))))


(provide 'hof-test)
;;; hof-test.el ends here
