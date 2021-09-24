;;; armstrong-numbers.el --- armstrong-numbers Exercise (exercism)

;;; Commentary:

;;; Code:
(defun armstrong-p (num)
  (cond
   ((not (numberp num)) nil)
   ((and (>= num 0) (<= num 9)) t)
   ((= (armstrong-dec num) num) t)
   (t nil)))

(defun armstrong-dec (num)
  "153 = 1^3 + 5^3 + 3^3 = 1 + 125 + 27 = 153 / thus 153 is an amsrtong number
10 != 1^2 + 0^2 = 1 / 10 is not an armstromg number
"
  (let ((n (length (number-to-string num))))
    (calc-sum n num 0)))

(defun calc-sum (n num res)
  (if (= num 0) res
    (calc-sum n (/ num 10) (+ res (pow (% num 10) n)))))

(defun pow (n p)
  "calculates n^p if p>=0 and n != 0 when p =0"
  (cond
   ((and (= p 0) (not (= n 0))) 1)
   ((and (= p 0) (= n 0)) (throw 'Error "undefined form"))
   ((< p 0) (throw 'Error "not yet defined"))
   (t (calc-pow n p 1))
   ))

(defun calc-pow (n p r)
  (if (= p 0) r (calc-pow n (1- p) (* r n)))
  )

(provide 'armstrong-numbers)
;;; armstrong-numbers.el ends here
