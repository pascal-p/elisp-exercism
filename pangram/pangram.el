;;; pangram.el --- Pangram (exercism)

;;; Commentary:

;;; Code:

(setq ALPHA_LEN 26)

(defun all-letters ()
  "returns a copy which can then be modified"
  (let (alst '())
    (progn
      (dotimes (x ALPHA_LEN)
        (setq alst (cons (cons (char-to-string (+ ?a x)) 0) alst)))
      (copy-alist alst)
      )))

(defun is-pangram (str)
  (if (= (length str) 0) nil
    (pangram? (delete "" (split-string (normalize str) "")) (all-letters)))
  )

(defun pangram? (lst letter-map)
  (if (= 0 (length lst)) (check-map letter-map)
    (pangram? (cdr lst) (update-map (car lst) letter-map)))
  )

(defun check-map (letter-map)
  (let ((ix 0)
        (continue t)
        (cnt -1))
    (while (and continue (< ix ALPHA_LEN))
      (setq cnt (cdr (assoc (char-to-string (+ ?a ix)) letter-map)))
      (if (= 0 cnt)
          (setq continue nil))
      (setq ix (1+ ix))
      )
    continue))

(defun update-map (letter letter-map)
  "update letter map count"
  (let* ((cnt (cdr (assoc letter letter-map)))
         (n-cnt (1+ cnt))
         (n-pair (cons letter n-cnt)))
    (cons n-pair (assoc-delete-all letter letter-map))
    )
  )

(defun normalize (str)
  "Suppress non-alpha char and downcase..."
  (let* ((nosp-str (replace-regexp-in-string "[^[:alpha:]]+" "" str))
         (norm-str (downcase nosp-str)))
    norm-str))


(provide 'pangram)
;;; pangram.el ends here
