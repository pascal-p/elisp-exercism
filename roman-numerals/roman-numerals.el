;;; roman-numerals.el --- roman-numerals Exercise (exercism)

;;; Commentary:

;;; Code:
(setq ya-roman-map
      '(
        (1 . "I") (2 . "II") (3 . "III") (4 . "IV") (5 . "V")
        (6 . "VI") (7 . "VII") (8 . "VIII") (9 . "IX") (10 . "X")
        (50 . "L") (100 . "C") (500 . "D") (1000 . "M")))

;; limit 3000 to test
(defun to-roman (num)
  (if (or (<= num 0) (> num 3000))
      (throw 'Error "0 <= num <= 3000")
    (to-roman-fn num)))

(defun to-roman-fn (num)
  (let* ((p 1000)
         (pair (divrem num p))
         (num (car pair))
         (rem (cdr pair))
         (roman ""))

    (if (= 0 rem)
        (update-roman-value roman p num)
      ;; else
      (while (> rem 0)
        (if (> num 0)
            (setq roman (update-roman-value roman p num))
          ;; no else
          )
        (setq p (/ p 10)
              num rem
              pair (divrem num p)
              num (car pair)
              rem (cdr pair)))
      (concat roman (roman-fn num p)))
    ))

(defmacro cdr-assoc (key)
  `(cdr (assoc ,key ya-roman-map)))

(defun update-roman-value (roman p num)
  (cond
   ((= p 1000)
    (concat roman (repeat-string (cdr-assoc p) num)))
   ((or (= p 100) (= p 10))
    (concat roman (roman-fn num p)))
   ((= p 1)
    (concat roman (cdr-assoc p)))
   (t roman)))

(defun roman-fn (num p)
  (cond
   ((<= num 3)  ;; ex. 30 => XXX
    (repeat-string (cdr-assoc p) num))
   ((= num 4)   ;; ex. 40 => XL
    (concat (cdr-assoc p) (cdr-assoc (* 5 p))))
   ((< num 9)   ;; ex. 70 => LXX
    (concat (cdr-assoc (* 5 p)) (repeat-string (cdr-assoc p) (- num 5))))
   (t           ;; 90 => XC
    (concat (cdr-assoc p) (cdr-assoc (* 10 p))))
   ))

(defsubst divrem (n d)
  (if (= 0 d) (throw 'Error "Division by 0")
    (cons (/ n d) (% n d))))

(defsubst repeat-string (str num)
  "input string str must be mono character
(repeat-string \"X\" 3) => \"XXX\"
"
  (make-string num (string-to-char str)))

(provide 'roman-numerals)
;;; roman-numerals.el ends here
