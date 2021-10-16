;;; binary.el --- Binary exercise (exercism)

;;; Commentary:

;;; Code:
(defun all-your-base (digits base-in base-out)
  (cond
   ((= 0 (length digits)) '(0))
   ((are-all-digits-0? digits) '(0))
   (t (all-your-base-fn digits base-in base-out))))

(defun all-your-base-fn (digits base-in base-out)
  (progn
    (check-valid-bases? base-in base-out)
    (check-valid_digits? digits base-in)
    (to-target-base (to-base-10 digits base-in) base-out)
    ))

(defun to-target-base (num base)
  (if (< num base) (list num)
    (let* ((pair (divrem num base))
           (num (car pair))
           (rem (cdr pair))
           (digits (list rem)))
      (while (>= num base)
        (setq pair (divrem num base)
              num (car pair)
              rem (cdr pair)
              digits (cons rem digits))
        )
      (cons num digits))))

(defun divrem (n q)
  "returns n div by q and the rest: n mod q as pair - assuming q != 0"
  (let ((r (% n q))
        (n (/ n q)))
    (cons n r)))

(defun to-base-10 (digits base)
   (let* ((init 0)
           (reducer-fn (lambda (acc d) (* (+ acc d) base))))
      (/ (ya-reduce reducer-fn digits init) base)))

(defun check-valid_digits? (digits base)
  (let ((d (car digits))
        (r-digits (cdr digits)))
    (while (> (length r-digits) 0)
      (if (or (>= d base) (< d 0)) (throw 'Error "digit < 0 xor >= input base"))
      (setq d (car r-digits))
      (setq r-digits (cdr r-digits))
      )
    t))

(defun check-valid-bases? (base-in base-out)
  (if (or (<= base-in 1) (<= base-out 1)) (throw 'Error "bases mut be > 1")
    t)
  )

(defun are-all-digits-0? (digits)
  (if (= 0 (length digits)) nil
    (let ((d (car digits))
          (l-digits (cdr digits))
          (witness t))
      (while (and (not (ya-empty? l-digits)) (= 0 d))
        (setq d (car l-digits)
              l-digits (cdr l-digits))
        )
      (and (ya-empty? l-digits) (= 0 d))
      )))

(defun ya-reduce (reducer-fn lst acc)
  (if (ya-empty? lst) acc
    (ya-reduce reducer-fn
               (cdr lst)
               (funcall reducer-fn (car lst) acc))))

(defun ya-empty? (lst)
  (= 0 (length lst)))

(provide 'all-your-base)
;;; all-your-base.el ends here
