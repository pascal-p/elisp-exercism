;;; word-count.el --- word-count Exercise (exercism)

;;; Commentary:
;; input == a string containing words and punctuation
;; output assoc list with count for each distinct word
;;  split by token using single space as separator
;;  make sure each token is a word => remove any punctuation (even signle quote)
;;  then iterate over each word (case insensitive) updating the assoc list

;; string ref: http://ergoemacs.org/emacs/elisp_string_functions.html
;; regexp ref: https://www.emacswiki.org/emacs/RegularExpression

;;; Code:

(defun word-count (input)
  "given an input string, return the count of words
(case insensitive/punctuation ignored)"
  (if (= 0 (length input)) nil
         (wc (split-string input))))

(defun wc (wlst)
  "given a list of token, exclude non word ones and return an
assoc list word / count for the rest"
  (let* ((words (sort (down-case (to-word-list wlst)) 'string<))
         (wc-assoc '()))
    (progn
      (mapc (lambda (w)
              (setq wc-assoc (update-wc w wc-assoc))) words)
      wc-assoc)
    )
  )

(defun update-wc (word wc-assoc)
  "no side-effect on wc-assoc here..."
  (let* ((cnt (or (cdr (assoc word wc-assoc)) 0))
         (ncnt (1+ cnt)))
    (cons (cons word ncnt) (assoc-delete-all word wc-assoc))
    ))

(defun to-word-list (lst)
  "lst is a list of token.
Remove any non-word character from each token
Remove empty tokens"
  (delete ""
          (mapcar (lambda (w) (replace-regexp-in-string "[^[:alnum:]]+$" ""  w))
                  lst))
  )

(defun down-case (lst)
  (mapcar 'downcase lst)
  )

;; we need a filter hof (higher order function)
(defun ya-filter (pred lst)
  (delete ":x_d_e_l"              ;; a special token to remove from resulting list
          (mapcar (lambda (x)
                    (if (funcall pred x) x (symbol-name ':x_d_e_l))) lst))
  )

;; (ya-filter (lambda (x) (= 1 (% x 2))) '(1 2 3 4 5))   ; => (1 3 5)
;; (ya-filter (lambda (w) (string-match "^[[:word:]]+$" w)) '("foo" "bar" "bar!")) ; => ("foo" "bar")
;; (sort '(2 1 3) '>)             ;; => (3 2 1)
;; (sort '("foo" "bar") 'string<) ;; => ("bar" "foo")
;; (downcase "FOO") ;; => "foo"

(provide 'word-count)
;;; word-count.el ends here
