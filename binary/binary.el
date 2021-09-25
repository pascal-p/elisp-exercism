;;; binary.el --- Binary exercise (exercism)

;;; Commentary:

;;; Code:
(defun to-decimal (bstr)
  (cond
   ((= 0 (length bstr)) 0)
   ((not (all-binary-digits? bstr)) 0)
   (t (convert bstr))))

(defun convert (bstr)
  "Use a FP approach with reducer-fn and hof ya-reduce"
  (let ((b-lst (to-binary-list bstr))
        (reducer-fn (lambda (d b)
                      (* (+ (string-to-number d) b) 2))))
    (/ (ya-reduce reducer-fn b-lst 0) 2))
  )

(defun ya-reduce (reducer-fn lst init)
  (if (ya-empty? lst) init
    (ya-reduce reducer-fn
               (cdr lst)
               (funcall reducer-fn (car lst) init))))

(defun ya-empty? (lst)
  (= 0 (length lst)))

(defun all-binary-digits? (bstr)
  (let ((b-lst (to-binary-list bstr)))
    (all-binary-p b-lst t)))

(defun all-binary-p (blst r)
  (if (= 0 (length blst)) r
    (all-binary-p (cdr blst) (and (binary-digit? (car blst)) r))
    ))

(defun binary-digit? (bstr)
  "Predicate to check whether a given string is a binary digit \"0\" or \"1\""
  (or (string= "0" bstr) (string= "1" bstr)))

(defun to-binary-list (bstr)
  (delete "" (split-string bstr "")))

(provide 'binary)
;;; binary.el ends here
