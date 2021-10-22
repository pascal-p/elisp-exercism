;;; allergies.el --- Allergies Exercise (exercism)

;;; Commentary:

;;; Code:
(setq ALLERGIES '((1 . "eggs")
                  (2 . "peanuts")
                  (4 . "shellfish")
                  (8 . "strawberries")
                  (16 . "tomatoes")
                  (32 . "chocolate")
                  (64 . "pollen")
                  (128 . "cats")))
(setq KEYS
      (sort (mapcar 'car ALLERGIES) '<))

(setq N (length KEYS))


(defun allergen-list (n)
  (if (<= n 0) '()
    (allergen-list-fn n)))

(defun allergic-to-p (n allergen)
  (in? allergen (allergen-list n)))

(defun in? (e lst)
  "Check whether e is in list or not"
  (in-fn e lst nil))

(defun allergen-list-fn (n)
  (let* ((s '())
         (score n)
         (ix (next-power score)))
    (while (> score 0)
      (setq kx (nth ix KEYS)
            s (set-add s (cdr (assoc kx ALLERGIES)))
            score (- score kx)
            ix (next-power score))
      ;; (setq  l (while-body s score ix)
      ;;        s (car l)
      ;;        score (cadr l)
      ;;        ix (caddr l))
      )
    s)
  )

;; (defun while-body (s score ix)
;;   (let* ((kx (nth ix KEYS))
;;          (s (set-add s (cdr (assoc kx ALLERGIES))))
;;          (score (- score kx))
;;          (ix (next-power score)))
;;         (list s score ix)
;;     ))

(defun next-power (n)
  "Find closest power of 2 (out of KEYS) from n (lower bound)
Examples:
  n = 32 => 2 as KEYS is (128 64 32 16 8 4 2 1) and 32 is at index 2
  n = 70 => 1 as 64 is the closest power of 2 (as lower bound)
"
  (cond
   ((<= n 0) -1)
   ((> n (nth (1- N) KEYS)) (1- N))
   ((in? n KEYS) (find-index n KEYS 0))
   (t (let* ((x (ceiling (log n 2)))
             (ix (if (> x 0) (1- x) x)))
          (if (>= ix N) (1- N) ix)))
  ))

(defun find-index (n lst ix)
  (cond
   ((= 0 (length lst)) ix)
   ((equal n (car lst)) ix)
   (t (find-index n (cdr lst) (1+ ix))))
  )

(defun in-fn (e lst res)
  (cond
   ((= 0 (length lst)) res)
   ((equal e (car lst)) t)
   (t (in-fn e (cdr lst) res)))
  )

(defun set-add (s e)
  "Add element e in s only if it is not already in (thus treating s as a set)
"
  (if (in? e s) s
    (cons e s) ;; (append s (list e))
    )
  )

(provide 'allergies)
;;; allergies.el ends here
