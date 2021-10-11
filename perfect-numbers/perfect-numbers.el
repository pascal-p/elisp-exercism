;;; perfect-numbers.el --- Perfect Numbers (exercism)

;;; Commentary:

;;; Code:
(defun classify (n)
  (cond
   ((< n 1) (error "Classification is only possible for natural numbers"))
   ((= n 1) 'deficient)
   (t (let ((sum-pdiv (sum-proper-divisors n)))
        (cond
         ((= sum-pdiv n) 'perfect)
         ((< sum-pdiv n) 'deficient)
         (t 'abundant))
        ))
    ))

(defun sum-proper-divisors (n)
  "Calculate the sum of the proper divisors of n.
Example: Σ divisor 6 is 1 + 2 + 3 == 6"
  (let ((d 2)
        (sum 1))
    (while (<= (* d d) n)
      (let* ((pair (divrem n d))
             (m (car pair))
             (r (cdr pair)))
        (if (= 0 r)
            (if (= m d) (setq sum (+ sum d))
              (setq sum (+ sum d m)))
          )
        (setq d (1+ d))))
    sum
    ))

(defun divrem (n d)
  (if (= 0 d) (throw 'Error "Division by 0")
    (let ((q (/ n d))
          (r (% n d)))
      (cons q r))
    ))

(provide 'perfect-numbers)
;;; perfect-numbers.el ends here
