;;; luhn.el --- Luhn exercise (exercism)

;;; Commentary:

;;; Code:
(defun luhn-p (seq)
  "Given a sequence (string of digits and spaces - determine whethter seq is a luhn number.
Cf. README for a definition of a Luhn number"
  (cond
   ((string-match "^\s*$" seq) nil)
   ((string-match "^[0-9]\s*$" seq) nil)
   ((string-match "[^0-9\s]" seq)(throw 'Error "forbidden character"))
   (t (luhn? (ya-list-reverse (ya-str-to-dlist seq t)))))
  )

(defun luhn? (dlst)
  (let* ((n-lst (double-every-2nd dlst))
         (sum (apply '+ n-lst)))
   (= 0 (% sum 10)))
  )

(defun double-every-2nd (dlst)
  "Double every second digit of dlst (digit list) starting from leftmost position.
Ex. (\"4\" \"5\" \"3\" \"9\" \"3\" \"1\") -> (\"4\" \"1\" \"3\" \"9\" \"3\" \"2\")

Notice:
- we double 5 (2nd from left) - which gives 10 (we subtracted 9, to get back a digit) in this case 1
- we then double 9 (18 - 9) -> 9
- we double 1 -> 2
"
  (let* ((ix 0)
         (double-fn (lambda (d)
                      (progn
                        (setq ix (1+ ix))
                        (cond
                         ((= d 9) d)  ;; invariant as 2 * d - 9 == 9
                         ((= 0 (% ix 2)) (% (* 2 d) 9))
                         (t d))))))
    (mapcar double-fn dlst))
  )

(defun ya-str-to-dlist (str &optional ds)
  "from string (ex. \"1234 7543\") to list '(1 2 3 4 7 5 4 3)
Optional argument stands for delete space (if set)"
  (let ((slst (if ds ;; (and (boundp 'ds) ds)
                  (delete ""
                          (split-string (replace-regexp-in-string "\s+" "" str) ""))
                (delete "" (split-string str "")))))
    (mapcar 'string-to-number slst))
  ) ;; with optional arg set => complexity O(3.n)

(defun ya-empty? (lst)
  (= 0 (length lst)))

(defun ya-list-reverse (lst)
  (if (ya-empty? lst) lst
    (_reverse lst '()))
  )

;; private helpers

(defun _reverse (lst nlst)
  (if (ya-empty? lst) nlst
    (_reverse (cdr lst) (cons (car lst) nlst)))
  )

(provide 'luhn)
;;; luhn.el ends here
