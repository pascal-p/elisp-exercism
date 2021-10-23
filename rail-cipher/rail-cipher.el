;;; rail-cipher.el --- Atbash-Cipher (exercism)

;;; Commentary:


;;; Code:
(defun encode (n src)
  "Encode src (clear text) using rail-fence cipher using n rails
"
  (cond
   ((= (length src) 0) src)
   ((or (< n 1) (>= n (length src)))
    (throw 'Error "1 < n < length(src)")) ;; slightly incorrect as the space between word does not count
   ((= 1 n) src)
   (t (mapconcat 'identity
                 (cat-rails (encode-fn n src)) "")))
  )

(defun encode-fn (n src)
  (let* ((txt-list (delete ""
                           (split-string (replace-regexp-in-string "[^[:alpha:]]" "" src) "")))
         (rails (init-rails n))
         (rix 0)
         (desc t)
         (op-fn '+)
         (distribute (lambda (l)
                       (progn
                         (setq rails (add l rix rails)  ;; add letter to rix-th rails
                               rix (update-ix rix op-fn))
                         (cond
                          ((and (= n rix) desc) (setq desc nil
                                                      rix (- rix 2)
                                                      op-fn '-))
                          ((and (= -1 rix) (not desc)) (setq desc t
                                                             rix 1
                                                             op-fn '+))
                          (t t))))))
    (progn
      (mapcar distribute txt-list)
      rails)
    )
  )

(defun init-rails (n)
  (let ((rails '())
        (ix 1))
    (while (<= ix n)
      (setq rails (cons '() rails)
            ix (1+ ix)))
    rails)
  )

(defun update-ix (ix op)
  "op is assumed to be either '- or '+"
  (apply op (list ix 1))
  )

(defun add (l ix rails)
  (let ((jx 0)
        (n (length rails))
        (n-rails '()))

    (if (>= ix n) rails
      ;; else
      (while (< jx n)
        (if (= ix jx)
            (setq nl (append (car rails) (list l))
                  n-rails (append n-rails (list nl)))
          (setq n-rails (append n-rails (list (car rails)))))
        (setq jx (1+ jx)
              rails (cdr rails))
        )
      n-rails)
      )
  )

(defun cat-rails (rails)
  (let ((n-rails '()))
    (while (> (length rails) 0)
      (setq n-rails (append n-rails (car rails))
            rails (cdr rails)))
    n-rails))

;; TODO
(defun decode (l ix rails)
  )

(provide 'rail-cipher)
;;; rail-cipher.el ends here
