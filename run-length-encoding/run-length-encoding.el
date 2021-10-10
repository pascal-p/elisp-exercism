;;; run-length-encoding.el --- run-length-encoding Exercise (exercism)

;;; Commentary:

;;; Code:
(defun run-length-encode (str)
  (cond
   ((= 0 (length str)) str)
   ((= 1 (length str)) str)
   (t (rle () (delete "" (split-string str "")) ()))
   ))

(defun run-length-decode (str)
  (rld (delete "" (split-string str "")) ())
  )

(defun rle (curr lst nlst)
  (cond
   ((= 0 (length lst)) (join (append nlst (list (enc-char curr))) ""))
   ((string= (car curr) (car lst)) (rle (update-pair curr) (cdr lst) nlst))
   ((equal nil curr) (rle (cons (car lst) 1) (cdr lst) nlst))
   (t (rle (cons (car lst) 1) (cdr lst) (append nlst (list (enc-char curr)))))
   ))

(defun rld (lst nlst)
  (cond
   ((= 0 (length lst)) (join nlst ""))
   ((= 1 (length lst)) (join (append nlst (list (car lst))) ""))
   ;; at least length 2
   (t (let* ((pair (build-list-num lst))
             (num (string-to-number (cdr pair)))
             (c-lst (car pair)))
        (if (<= num 1)
            (rld (cdr c-lst) (append nlst (list (car c-lst))))
          (rld (cdr c-lst) (append nlst (list (repeat-char (car c-lst) num))))
        )))
   ))

(defun update-pair (pair)
  (let* ((val (cdr pair))
         (nval (1+ val)))
    (cons (car pair) nval)
    ))

(defun enc-char (pair)
  "Example: pair == (\"X\" . 2) => 2X "
  (cond
   ((> (cdr pair) 1) (concat (number-to-string (cdr pair)) (car pair)))
   ((= (cdr pair) 0) "")
   ((< (cdr pair) 0) (throw 'Error "caount must not me negative"))
   (t (car pair))
   ))

(defun build-list-num (lst)
  (cond
   ((= 0 (length lst)) (cons lst "0"))
   ((= 0 (string-to-number (car lst))) (cons lst "0"))
   (t (let ((cnum  (car lst))
            (c-lst (cdr lst)))
        (while (and (not (empty-list? c-lst))
                    (> (string-to-number (car c-lst)) 0))
          (setq cnum (concat cnum (car c-lst)))
          (setq c-lst (cdr c-lst))
          )
        (cons c-lst cnum)))
   ))

(defun empty-list? (lst)
  (= 0 (length lst)))

(defun join (lst sep)
  (mapconcat 'identity lst sep))

(defun repeat-char (str num)
  (make-string num (string-to-char str))
  )

(provide 'run-length-encoding)
;;; run-length-encoding.el ends here
