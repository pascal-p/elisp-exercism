;;; simple-cipher.el --- Simple-Cipher

;;; Commentary:

;;; Code:
(defun gen-alist ()
  "Return pair
 - l->ix letter to index,
 - ix->l index to letter"
  (gen-alist-fn 0 '() '()))

(defun gen-alist-fn (ix al r-al)
  (cond ((= LEN-ALPHA ix) (list al r-al))
        (t (gen-alist-fn (1+ ix)
                         (append al (list (cons (char-to-string (+ ?a ix)) ix)))
                         (append r-al (list (cons ix (char-to-string (+ ?a ix)))))
                         )))
  )

(setq LEN-ALPHA 26   ;; length of western alphabet
      LEN-KEY 100    ;; length of (pseudo) random generated key
      _pair (gen-alist)                                                                 
      L->IX (car _pair)                                                                 
      IX->L (cadr _pair))                                                               
                                                                                        
;; encode / decode 

(defun encode (txt &optional key)
  "Returns a pair (encoded message . key)"
  (if (= 0 (length txt)) txt
    ;; else
    (progn
      (if (equal key nil) (setq key (init-key)))
      (let* ((len-key (length key))
             (cl-encode-fn (lambda (pair)
                             (let* ((curr-ch (car pair))
                                    (ix (cdr pair)))
                               (encode-fn ix curr-ch key len-key))
                             )))

        (list->pair
         (mapcar cl-encode-fn (enum-filtered-text txt))
         key)
        )
      )
    )
  )

(defun decode (txt &optional key)
  ;; TODO
  )

;; helpers

(defun init-key ()
  "Generate a (pseudo) random key of len LEN-KEY"
  (let ((ix 0)
        (key ""))
    (while (< ix LEN-KEY)
      (setq key (concat key (cdr (assoc (mod (random) LEN-ALPHA) IX->L)))
            ix (1+ ix))
      )
    key
    )
  )
(defun list->pair (ltxt key)
  (cons (mapconcat 'identity
                   ltxt
                   "")
        key))

(defun enum-filtered-text (txt)
  "Enumerate all letters from txt discarding non-letters and non-digits, along with an index
Returns a list of pairs ((letter . 0) (letter' . 1) (letter'' . 2) ....)
"
  (let* ((ix 0)
         (ltxt (filter->list txt))
         (enum-fn (lambda (l)
                    (progn
                      (setq ix (1+ ix))
                      (cons l (1- ix))))))
    (mapcar enum-fn ltxt)
    )
  )

(defun filter->list (src)
  (delete ""
          (split-string (replace-regexp-in-string "[^[:alnum:]]" "" src) ""))
  )

(defun decode-fn (ix curr-ch key len-key)
  (let* ((ik (% ix len-key))
         (jx (next-index curr-ch key ik '-)))
    (cdr (assoc (mod jx LEN-ALPHA) IX->L)))  ;; jx can be < 0
  )

(defun encode-fn (ix curr-ch key len-key)
  (let* ((ik (% ix len-key))
         (jx (next-index curr-ch key ik '+)))
    (cdr (assoc (% jx LEN-ALPHA) IX->L)))
  )

(defun next-index (curr-ch key ik op)
  (funcall op
           (cdr (assoc (downcase curr-ch) L->IX))
           (cdr (assoc (substring key ik (+ 1 ik)) L->IX)))
  )

(provide 'simple-cipher)
;;; simple-cipher.el ends here
