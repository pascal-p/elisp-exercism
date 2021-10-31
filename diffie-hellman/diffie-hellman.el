;;; diffie-hellman.el --- Affine-Cipher

;;; Commentary:

;;; Code:
(defun private-key (p)
  (progn
    (check-factor p)
    (random-wrapper 3 (1- p))))

(defun public-key (p g priv)
  (progn
    (check-key priv p)
    (% (expt g priv) p))) ;; can and will overflow!

;;
;; Function: expt x y
;;   This function returns x raised to power y.
;;   If both arguments are integers and y is nonnegative, the result is an integer; in this case, overflow signals an error, so watch out.
;;   If x is a finite negative number and y is a finite non-integer, expt returns a NaN.

(defun secret (p pub priv)
  (progn
    (check-key priv p)
    (check-key pub p)
    (% (expt pub priv) p))) ;; can and will overflow!

(defun check-factor (p)
  (if (or (< p 2) (not (is-prime? p))) (throw 'Error "p must prime > 2")
    t)
  )

(defun check-key (key p)
  (if (or (< p 2) (>= key p)) (throw 'Error "not a valid key"))
  )

(defun is-prime? (p)
  (cond ((or (<= p 1) (and (> p 2) (= 0 (% p 2)))) nil)
        ((<= p 3) t)
        ((= 0 (% p 3)) nil)
        (t (let ((n (floor (sqrt p)))
                 (prime t)
                 (cp 5))
             (while (and prime (< cp n))
               (if (or (= 0 (% p cp)) (= 0 (% p (+ p 2)))) (setq prime nil))
               (setq cp (+ 6 cp)))
             prime))))

(defun random-wrapper (from to)
  "use random() lib function to pick randomly a number r such that:
from ≤ r ≤ to"
  (if (or (< from 0) (>= from to)) (throw 'Error "0 ≤ from < to")
    (progn
      (random t) ;; set random seed
      (let ((r (random to)))
        (while (or (< r from) (> r to))
          (setq r (random to)))
        r)
      )))

(provide 'diffie-hellman)
;;; diffie-helman.el ends here
