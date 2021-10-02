;;; atbash-cipher.el --- Atbash-Cipher (exercism)

;;; Commentary:




;;; Code:
(setq ENCODING-MAP
      (let (lst '())
        (dotimes (x 26)
          (setq lst (cons (cons (char-to-string (+ ?a x)) (char-to-string (- ?z x))) lst)))
        lst))

(setq GROUPING 5)

(defun encode (plaintext)
  "Encode PLAINTEXT to atbash-cipher encoding."
  (if (= 0 (length plaintext)) plaintext
    (group-by (_encode (ya-str-to-list plaintext))))
  )


(defun _encode (clst)
  (let ((n-lst '())
        (x nil))
    (while clst
      (setq x (assoc (car clst) ENCODING-MAP))

      (if (not x) ;; then
          (progn
            (setq x (car clst))
            (if (not (string-match "^[0-9]$" x)) (setq x "")))
        ;; else
        (setq x (cdr x)))
      ;; careful building in reverse order to get encoding in right order
      (setq n-lst (append n-lst (list x)))
      (setq clst (cdr clst))
      )
    (apply 'concat n-lst)
    )
  )

(defun group-by (str)
  (let* ((from 0)
        (n (length str))
        (to (min n GROUPING))
        (enc '()))
    (while (<= to n)
      (setq enc (append enc (list (substring str from to))))
      (setq from (+ GROUPING from))
      (setq to (+ GROUPING to))
      (if (> to n) (setq to n))
      (if (> from n)
          (setq to (1+ n))) ;; make sure to > n
      )
    ;; (substring enc 0 (max 0 (1- (length enc))))
    ;; (string-join enc " ")
    (mapconcat 'identity (delete "" enc) " ")
   )
  )

(defun ya-str-to-list (str)
  "from string to list (of downcase character)"
  (delete ""
          (split-string (downcase (replace-regexp-in-string "\s+" "" str)) ""))
  )

(provide 'atbash-cipher)
;;; atbash-cipher.el ends here
