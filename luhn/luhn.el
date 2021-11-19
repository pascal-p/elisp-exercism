;;; luhn.el --- Luhn exercise (exercism)

;;; Commentary:

;;; Code:'
(require 'cl-lib)

(defun luhn-p (seq)
  "Given a sequence (string of digits and spaces - determine whethter seq is a luhn number.
Cf. README for a definition of a Luhn number"
  (cond
   ((string-match "^\s*$" seq) nil)
   ((string-match "^[0-9]\s*$" seq) nil)
   ((string-match "[^0-9\s]" seq)(throw 'Error "forbidden character"))
   (t (luhn? (ya:list-reverse (ya:str-to-dlist seq t)))))
  )

;; inlining
(defsubst luhn? (dlst)
  (= 0 (% (apply '+ (double-every-2nd dlst)) 10)))

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
                        (setq ix (1+ ix)) ;; side-effect
                        (cond
                         ((= d 9) d)  ;; invariant as 2 * d - 9 == 9
                         ((= 0 (% ix 2)) (% (* 2 d) 9))
                         (t d))))))
    (mapcar double-fn dlst))
  )

;; inlining
(defsubst ya:split-string (str ch)
  (delete "" (split-string str ch)))

(defun ya:str-to-dlist (str &optional ds)
  "from string (ex. \"1234 7543\") to list '(1 2 3 4 7 5 4 3)
Optional argument stands for delete space (if set)"
  (let ((slst (if ds ;; (and (boundp 'ds) ds)
                  (ya:split-string (replace-regexp-in-string "\s+" "" str) "")
                (ya:split-string str "")
                )))
    (mapcar 'string-to-number slst))
  ) ;; with optional arg set => complexity O(3.n)

(defun ya:list-reverse (lst)
  (if (null lst) lst
    (cl-labels
        ((:_reverse-fn (lst: rlst:)
                       (if (null lst:) rlst:
                         (:_reverse-fn (cdr lst:) (cons (car lst:) rlst:)))))
      (:_reverse-fn lst '()))
    )
  )

(provide 'luhn)
;;; luhn.el ends here
