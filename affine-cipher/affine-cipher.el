;;; affine-cipher.el --- Affine-Cipher

;;; Commentary:

;;; Code:
(defun gen-alist ()
  "Return pair
 - l->ix letter to index,
 - ix->l index to letter"
  (gen-alist-fn 0 '() '()))

(defun gen-alist-fn (ix al r-al)
  (cond ((= M ix) (list al r-al))
        (t (gen-alist-fn (1+ ix)
                         (append al (list (cons (char-to-string (+ ?a ix)) ix)))
                         (append r-al (list (cons ix (char-to-string (+ ?a ix)))))
                         )))
  )

(setq M 26   ;; length of western alphabet
      LENGRP 5
      _pair (gen-alist)
      L->IX (car _pair)
      IX->L (cadr _pair))

(defun encode (src alpha beta)
  "E(x) = (α × x + β) ≡ m"
  (progn
    (check-coprime alpha)
    (let* ((ltxt (filter->list src)) ;; only valid char are allowed: alnum class
           (closure-translate (lambda (x)
                                (translate x alpha beta '+)))
           (cipher (mapcar closure-translate ltxt))
           (cipher-gp (grouping cipher)))
      (mapconcat 'identity cipher-gp " ")
      ))
    )

(defun decode (src alpha beta)
  "D(y) = α-¹ × (y - β) ≡ m"
  (progn
    (check-coprime alpha)
    (let* ((triplet (xgcd alpha M))
           (alpha (cadr triplet))
           (ltxt (filter->list src)) ;; only valid char are allowed: alnum class
           (closure-translate (lambda (x)
                                (translate x alpha beta '-)))
           (plain (mapcar closure-translate ltxt)))
      (mapconcat 'identity plain "")
      ))
  )

;; (% -7 26) == -7
;; (mod -7 26) == 19

(defun translate (x alpha beta op)
  (cond ((string-match "^[0-9]$" x) x)
        ((equal op '+) (cdr (assoc (% (+ (* alpha
                                            (cdr (assoc (downcase x) L->IX))) beta)
                                      M) IX->L)))
        ((equal op '-) (cdr (assoc (mod (* alpha
                                           (- (cdr (assoc (downcase x) L->IX)) beta)
                                           ) M) IX->L)))
        (t (throw 'Error "operator not supported, expecting - or +")))
  )

(defun xgcd (x y)
  "Extended GCD(x, y) == α * x + β * y == g

here y will be 26 (always), g is 1 thus find (α, β) relative integer Z
such that α × x + β × 26 ≡ 1

ref. https://en.wikipedia.org/wiki/Modular_multiplicative_inverse
     https://en.wikibooks.org/wiki/Algorithm_Implementation/Mathematics/Extended_Euclidean_algorithm
  "
  (let ((x0 0) (x1 1)
        (y0 1) (y1 0))
    (while (not (= 0 x))
      (setq dr (divrem y x)
            y x
            q (car dr)
            x (cdr dr))
      (setq n-y0 y1
            n-y1 (- y0 (* q y1))
            y0 n-y0
            y1 n-y1)
      (setq n-x0 x1
            n-x1 (- x0 (* q x1))
            x0 n-x0
            x1 n-x1)
      )
    (list y x0 y0)
    )
  )

(defun gcd (x y)
  "Compute gcd(x, y)
Assume x > 0 && y > 0"
  (let* ((xp x)
         (yp y))
    (progn
      (if (< x y) (setq xp y
                        yp x))
      (if (= yp 0) xp
        ;; else
        (let ((r xp))
          (while (> r 1)
            (setq r (% xp yp)
                  xp yp
                  yp r))
          (if (= 0 r) xp r))
        ))))

(defun co-prime? (alpha)
  (= (gcd alpha M) 1))

(defun divrem (n d)
  (if (= 0 d) (throw 'Error "Division by 0")
    (let ((q (/ n d))
          (r (% n d)))
      (cons q r))
    ))

(defun check-coprime (x)
  (if (not (co-prime? x)) (throw 'Error "x and M are not co-prime")
    t)
  )

(defun filter->list (src)
  (delete ""
          (split-string (replace-regexp-in-string "[^[:alnum:]]" "" src) ""))
  )

(defun grouping (lst)
  (let* ((r-lst '())
         (curr-gp ""))
    (while (> (length lst) 0)
      (setq curr-gp (concat curr-gp (car lst))
            lst (cdr lst))
      (if (= LENGRP (length curr-gp))
          (setq r-lst (append r-lst (list curr-gp))
                curr-gp ""))
      )
    (progn
      (if (> (length curr-gp) 0)
          (setq r-lst (append r-lst (list curr-gp))))
      r-lst
      )))

;; pratical extension

(defun encode-batch (txt-lst alpha beta)
  "Apply encode on a list of source text with a specific alpha and beta values"
  (let ((encode-fn (lambda (txt) (encode txt alpha beta))))
    (mapcar encode-fn txt-lst)
    ))

;; macro

(defmacro encode-ab (alpha beta txt)
  "Create a version of encode gicen alpha and beta"
  `(encode txt ,alpha ,beta)
  )

;; to see what happens:
;; (macroexpand '(encode-ab 21 3 txt)) ;; == (encode txt 21 3)

;; how to use it
(defun encode-batch (txt-lst alpha beta)
  (let ((encode-fn (lambda (txt)
                     (encode-ab alpha beta txt)
                     )))
    (mapcar encode-fn txt-lst)
    )
  )

;; assuming ltxt contain a list of texts to encode...
;; (encode-batch ltxt 21 3)

(provide 'affine-cipher)
;;; affine-cipher.el ends here
