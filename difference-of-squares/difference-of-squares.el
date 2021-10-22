;;; difference-of-squares.el --- Difference of Squares (exercism)

;;; Commentary:

;;; Code:
(defun difference (n)
  (- (square-of-sum n) (sum-of-squares n)))

(defun square-of-sum (n)
  "Σ (1:n)² ≡ (n × (n + 1) / 2)²"
  (let* ((x1 n)
         (x2 (1+ n))
         (p (lsh (* x1 x2) -1)))
    (* p p)))

(defun sum-of-squares (n)
  "Sum the squaes of the first `n` positive integers
1² + 2² + 3² + ... + n² ≡ (n × (n + 1) × (2n + 1)) / 6"
  (let* ((x1 n)
         (x2 (1+ n))
         (x3 (1+ (lsh n 1)))
         (x (lsh (* x1 x2 x3) -1)))
    (/ x 3)))

(provide 'difference-of-squares)
;;; difference-of-squares.el ends here
