;;; raindrops.el --- Raindrops (exercism)

;;; Commentary:

;;; Code:

;; (require 'cl-lib)

(setq RAIN-BASE
      '((7 . "Plong")
        (5 . "Plang")
        (3 . "Pling")))

(setq FACTORS
      (mapcar 'car RAIN-BASE))

(defun convert (n)
  "Convert integer N to its raindrops string."
  (cond
   ((<= n 1) (number-to-string n))
   ((in n FACTORS) (cdr (assoc n RAIN-BASE)))
   (t (let ((res (decomp n)))
        (if (= (length res) 0)
            (number-to-string n)
          (mapconcat 'identity res ""))))
   ))

(defun decomp (n)
  (let* ((res '())
         (np n)
         (f (car FACTORS))
         (factors (cdr FACTORS)))
    (progn
      (while (> (length factors) 0)
        (setq res-l (while-body np f res))
        (setq np (car res-l)
              f (cadr res-l)
              res (caddr res-l)
              f (car factors)
              factors (cdr factors)))
      (setq res-l (while-body np f res))
      (setq np (car res-l)
            f (cadr res-l)
            res (caddr res-l))
      res)))

(defun while-body (np f res)
  (progn
    (if (= 0 (% np f))
      (progn
        (setq res (cons (cdr (assoc f RAIN-BASE)) res))
        (while (= 0 (% np f))
          (setq np (/ np f))))
      )
    (list np f res)
    ))

(defun in (e lst)
  (or (= e (car lst))
      (= e (cadr lst))
      (= e (caddr lst))))

(provide 'raindrops)
;;; raindrops.el ends here
