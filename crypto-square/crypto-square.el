;;; crypto-square.el --- Crypto Square (exercism)

;;; Commentary:

;;; Code:
(defun encipher (str)
  (cond
   ((= 0 (length str)) "")
   (t (square-enc (normalize str)))))

(defun square-enc (str)
  (if (= 1 (length str)) str
    ;; (str-extract (_square-enc (find-cr (length str)) str))
    (let* ((cr (find-cr (length str)))
           (c (car cr))
           (r (cdr cr))
           (ix 0)
           (s-ix 0)
           (e-ix c)
           (n (length str))
           (r-lst '()))
      (while (< ix r)
        ;; if len of substring < c => pad with space character
        (setq r-lst (append r-lst (list (substr-n str s-ix e-ix c)))
              s-ix (+ s-ix c)
              e-ix (min (+ e-ix c) n)
              ix (1+ ix)))
        (str-extract (cons r-lst c))
    ))
  )

(defun str-extract (lst-c)
  (let* ((ix 0)
         (fn (lambda (str) (substring str ix (+ 1 ix))))
         (s-lst (car lst-c))
         (c (cdr lst-c))
         (enc-lst '())
         (n (length s-lst)))
    (while (< ix c)
      (setq enc-lst (append enc-lst
                            (list (mapconcat 'identity (mapcar fn s-lst) "")))
            ix (1+ ix)
      ))
    ;; (replace-regexp-in-string "\s+" ""  str-enc)
    (mapconcat 'identity enc-lst " "))
  )

(defun normalize (str)
  "Suppress space and punctuation, downcase..."
  (let* ((nosp-str (replace-regexp-in-string "[^[:alnum:]]+" "" str))
         (norm-str (downcase nosp-str)))
    norm-str))

(defun find-cr (n)
  "find factors (c, r) such that c >= r and c - r <= 1 /
c x r is >= l (length of string to encode)"
  (let* ((r (round (sqrt n)))
         (c (1+ r)))
    (if (>= (* r r) n) (setq c r)) ;; perfect square
    (cons c r)))

(defun substr-n (str from to n)
  (let* ((s-str (substring str from to))
         (len-str (length s-str))
         (diff (- n len-str)))
    (if (= 0 diff) s-str
      (str-pad s-str diff))))

(defun str-pad (str n)
  "pad given str with n \" \" (space) char"
  (let ((ix 0))
    (while (< ix n)
      (setq str (concat str " ")
            ix (1+ ix)))
    str
    )
  )

(provide 'crypto-square)
;;; crypto-square.el ends here
