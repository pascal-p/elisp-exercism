;;; trinary.el --- Trinary (exercism)

;;; Commentary:

;;; Code:

;; (require 'cl-lib)
(setq BASE 3)

(defun trinary-to-decimal (nstr)
  (cond
   ((not (string-match "^[012]+$" nstr)) 0)
   ((= 1 (length nstr)) (string-to-number nstr))
   (t (convert (ya-str-to-list nstr)))))

;; this part is taken from binary.el and adjusted...
(defun convert (lst)
  "Use a FP approach with reducer-fn and hof ya-reduce"
  (let ((reducer-fn (lambda (d b)
                      (* (+ (string-to-number d) b) BASE))))
    (/ (ya-reduce reducer-fn lst 0) BASE)))

(defun ya-reduce (reducer-fn lst init)
  (if (ya-empty? lst) init
    (ya-reduce reducer-fn
               (cdr lst)
               (funcall reducer-fn (car lst) init))))

(defun ya-empty? (lst)
  (= 0 (length lst)))

(defun ya-str-to-list (str)
  (delete "" (split-string str "")))

(provide 'trinary)
;;; trinary.el ends here
