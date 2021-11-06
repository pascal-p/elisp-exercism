;;; sieve.el --- Sieve of Eratosthenes

;;; Commentary:

;;; Code:
(setq max-lisp-eval-depth 10000)

(defun sieve (limit)
  (let* ((candidates (generate-candidates limit))
         (r-limit (ceiling (sqrt limit)))
         (cand 3))
    (while (< cand r-limit)
      (setq candidates (filter-multiples-of cand candidates (last-of candidates))
            cand (select-next-prime-candidate cand candidates r-limit))
      )
    candidates
    ))

(defun filter-multiples-of (prime cand-list limit)
  (let ((x (* 2 prime)))
    (while (<= x limit)
      (setq cand-lst (delete x cand-list)
            x (+ x prime))
    )
   cand-list
   ))

(defun select-next-prime-candidate (cand candidates limit)
  (cond ((>= cand limit) limit)
        ((= (car candidates) cand) (cadr candidates))
        (t (select-next-prime-candidate cand (cdr candidates) limit)))
 )

;; tail-recursion
;; (defun last-of (lst)
;;   (last-fn-tr lst nil))
;;
;; (defun last-fn-tr (lst elt)
;;   (if (= 0 (length (cdr lst))) elt
;;     (last-fn-tr (cdr lst) (car lst))))

;; iterative version
(defun last-of (lst)
  (let* ((l lst)
         (e (car l)))
    (while (> (length (cdr l)) 0)
      (setq l (cdr l)
            e (car l)))
    e
  ))

;; non-terminal recursion
;; (defun last-of (lst)
;;   (if (= 0 (length (cdr lst))) (car lst)
;;     (last-of (cdr lst)))
;;   )

(defun generate-candidates (limit)
  (let* ((c-lst '(2))
         (x 3))
    (while (< x limit)
      (setq c-lst (append c-lst (list x))
            x (+ 2 x))
      )
    (if (<= x limit) (append c-lst (list x))
      c-lst
      )))

(provide 'sieve)
;;; sieve.el ends here
