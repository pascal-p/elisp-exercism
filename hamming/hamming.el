;;; hamming.el --- Hamming (exercism)

;;; Commentary:

;;; Code:
(defun hamming-distance (str-a str-b)
  (cond
   ((not (equal (length str-a) (length str-b))) (throw 'Error "input strings must have dame length"))
   ((string= str-a str-b) 0)
   (t (calc-number-of-mismatch str-a str-b)))
 )

(defun calc-number-of-mismatch (str-a str-b)
  "transform the 2 strings of same length into two lists of chars
merge the two lists into a list of pair
count the number of mistmatch pairs
sum"
  (let* ((lst-a (str-2-list str-a))
         (lst-b (str-2-list str-b))
         (lst (zip lst-a lst-b)))
    (apply '+ (mapcar 'mismatch-counter lst))
    )
  )

(defun mismatch-counter (p)
  "p is a pair ex. (\"A\" . \"B\")
if mismatch on the pair (p) return 1 else 0"
  (if (equal (car p) (cdr p)) 0 1)
)

(defun str-2-list (str)
  "from string to list of chars"
  (mapcar 'char-to-string str)
  )

(defun zip (l1 l2)
  "merging two lists of equal length into one list of pairs
(zip '(1 2 3) '(\"a\" \"b\" \"c\")) -> ((3. \"a\") (2 .\"b\") (1 . \"a\")))
NOTE: resulting list  is in reverse order; it does not matter."
  (cond
   ((or (not (listp l1)) (not (listp l2))) (throw 'Error "l1 and l2 should be 2 list of same length"))
   ((not (equal (length l1) (length l2))) (throw 'Error "l1 and l2 should be 2 list of same length"))
   (t (_zip l1 l2 '())))
  )

(defun _zip (l1 l2 lr)
  (cond
   ((= (length l1) 0) lr)
   (t (_zip (cdr l1) (cdr l2) (cons (cons (car l1) (car l2)) lr))))
  )

(defun reduce (fn acc base lst)
  (if (= (length lst) 0) base
    (acc (fn (car l)) (reduce fn acc base (cdr lst))))
  )

;; example - does not work!
;; (reduce (lambda (x) (mismatch-counter x)) + 0 (zip '("A" "A" "B") '("A" "C" "B")))

(provide 'hamming)
;;; hamming.el ends here
