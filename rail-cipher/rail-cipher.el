;;; rail-cipher.el --- Atbash-Cipher (exercism)

;;; Commentary:


;;; Code:
(setq QMARK "?"
      DMARK ".")

(defun encode (n src)
  "Encode src (clear text) using rail-fence cipher using n rails"
  (cond
   ((= (length src) 0) src)
   ((or (< n 1) (>= n (length src)))
    (throw 'Error "1 < n < length(src)")) ;; slightly incorrect as the space between word does not count
   ((= 1 n) src)
   (t (mapconcat 'identity
                 (cat-rails (encode-fn n src)) "")))
  )

(defun decode (n src)
  "Decode src (ciphered text) using n rails"
  (cond ((= 0 (length src)) src)
        ((< n 1) (throw 'Error "n must be such that 1 < n < length(src)"))
        ((= n 1) src)
        (t (decode-fn n src)))
  )

(defun encode-fn (n src &optional switch)
  "
encode-fn can be used to encode a clear text to its encoded form, but it can also be used
to expand the special mark ? for the decoding portion.

Optional argument switch is used to toggle the behaviour of encode-fn:
  - if nil => encoding
  - otherwise spread \"?\"
"
  (let* ((txt-list (word-to-list src))
         (rails (init-rails n))
         (rix 0)
         (desc t)
         (op-fn '+)
         ;; closure:
         (distribute (lambda (l)
                       (progn
                         (and (not (equal switch nil)) (setq l QMARK)) ;; overwrite l
                         (setq rails (add l rix rails)                 ;; add letter to rix-th rails, dot to remaining rails
                               triplet (update-ix rix n op-fn desc)    ;; update rix and change op and direction accordingly
                               rix (car triplet)
                               op-fn (cadr triplet)
                               desc (caddr triplet)))
                       )))
    (progn
      (mapcar distribute txt-list)
      rails)
    )
  )

(defun decode-fn (n src)
  (let* ((l-src (word-to-list src))
         (rails (encode-fn n src t))
         (n-rails (replace-mark rails l-src)))
    ;;
    (mapconcat 'identity
               (mapcar 'car
                       (mapcar 'squeeze-rail
                               (transpose n-rails)))
               "")
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

(defun update-ix (ix n op desc)
  "op is assumed to be either '- or '+
return triplet <updated ix, op and desc>
"
  (let ((rix (apply op (list ix 1))))
    (cond
     ((and (= n rix) desc) (list (- rix 2) '- nil))
     ((and (= -1 rix) (not desc)) (list 1 '+ t))
     ;; no change
     (t (list rix op desc)))
  ))

(defun add (l ix rails)
  "Add letter l to ix-th rails, while filling the others with \".\""
  (let ((jx 0)
        (n (length rails))
        (n-rails '()))
    (if (>= ix n) rails
      ;; else
      (while (< jx n)
        (if (= ix jx)
            (setq nl (append (car rails) (list l)))
          ;; else
          (setq nl (append (car rails) (list DMARK))))
        (setq n-rails (append n-rails (list nl))
              jx (1+ jx)
              rails (cdr rails))
        )
      n-rails)
      )
  )

(defun squeeze-rail (rail &optional char)
  "Delete all occurences of char (default is \".\") in rails"
  (progn
    (and (equal nil char) (setq char DMARK))
    (delete char rail)))

(defun cat-rails (rails)
  (let ((n-rails '()))
    (while (> (length rails) 0)
      (setq n-rails (append n-rails (squeeze-rail (car rails)) )
            rails (cdr rails)))
    n-rails))

(defun replace-mark (rails src)
  "Replace mark on each given rails by assigning the letters from src in order...
Returns the updated rails
"
  (let ((ix 0)
        (n (length rails))
        (n-rails '()))
    (while (< ix n)
      (setq tuple (replace-mark-fn (nth ix rails) src '()) ;; (replace-mark-fn (nth ix rails) src '() '())
            src (car tuple)
            n-rails (append n-rails (cdr tuple))
            ix (1+ ix)))
    n-rails
    ))

(defun replace-mark-fn (rail src n-rail)
  "Returns a list of 2 sub-lists"
  (cond ((= 0 (length rail)) (list src n-rail))
        ;; match
        ((equal QMARK (car rail))
         (replace-mark-fn (cdr rail)
                          (cdr src)
                          (append n-rail (list (car src)))))
        ;; non-match
        (t (replace-mark-fn (cdr rail)
                            src
                            (append n-rail (list DMARK)))))
  )

;; (defun replace-mark-fn (rail src n-rail n-src)
;;   "Returns a list of 2 sub-lists"
;;   (cond ((= 0 (length rail)) (list n-src n-rail))
;;         ;; match
;;         ((equal QMARK (car rail))
;;          (replace-mark-fn (cdr rail)
;;                           (cdr src)
;;                           (append n-rail (list (car src)))
;;                           (append n-src (list DMARK))))
;;         ;; non-match
;;         (t (replace-mark-fn (cdr rail)
;;                             (cdr src)
;;                             (append n-rail (list DMARK))
;;                             (append n-src (list (car src))))))
;;   )

(defun word-to-list (src)
  (delete ""
          (split-string (replace-regexp-in-string "[^[:alnum:]]" "" src) ""))
  )

(defun transpose (llst)
  "From '((\"F\" \".\" \"O\" \".\" \"A\" \".\")
          (\".\" \"O\" \".\" \"B\" \".\" \"R\")))
To:
   ((\"F\" \".\")
    (\".\" \"O\")
    (\"O\" \".\")
    (\".\" \"B\")
    (\"A\" \".\")
    (\".\" \"R\"))
"
  (if (= 0 (length llst)) llst
    (let* ((n (length (car llst))) ;; all sublists of llst assumed to be of same length
          (ix 0)
          (r-lst '())
          (n-th (lambda (lst)
                  (nth ix lst))))
      (while (< ix n)
        (setq s-lst (mapcar n-th llst)
              ix (1+ ix)
              r-lst (append r-lst (list s-lst)))
        )
      r-lst
      )))

(provide 'rail-cipher)
;;; rail-cipher.el ends here
