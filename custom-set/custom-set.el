;;; custom-set.el --- Custom-Set

;;; Commentary:

;;; Code:
(defun cs:custom-set (lst)
  (if (<= (length lst) 1) lst
    (cs:cons lst '())))

(defun cs:cons (lst s)
  (cond
   ((null? lst) s)
   ((cs:member? (car lst) s) (cs:cons (cdr lst) s))
   (t (cs:cons (cdr lst) (cons (car lst) s)))
   ))

(defun cs:member? (elt lst)
  "Check whether elt is in list or set (lst)"
  (cond
   ((null? lst) nil)
   ((equal (car lst) elt) t)
   (t (cs:member? elt (cdr lst))))
  )

(defun cs:subset? (s1 s2)
  "is set s1 a subset of s2?
yes iff all element of s1 are in s2"
  (cond
   ((cs:empty? s1) t)
   ((> (length s1) (length s2)) nil)
   (t (cs:all? s1 s2)))
  )

(defun cs:equal? (s1 s2)
  "test whether 2 sets (emphasis on sets) are equal or not defines as
- sets must be of same length
- sets must have same elements regardless of order"
  (and (cs:same-length? s1 s2)
       (cs:same-elements? s1 s2))
  )

(defun cs:same-length? (s1 s2)
  (= (length s1) (length s2)))

(defun cs:same-elements? (s1 s2)
  (cond
   ((= (length s1) 0) t)
   ((= (length s1) 1) (equal (car s1) (car s2))) ;; we know that |s1| == |s2|
    ;; the 2 sets are now of length > 1
   (t (and (cs:all? s1 s2)
           (cs:all? s2 s1)))))

(defun cs:all? (s1 s2)
  (and (cs:member? (car s1) s2)
       ;; set s1 will eventually be empty
       (if (cs:empty? (cdr s1)) (cs:member? (car s1) s2)
         (cs:all? (cdr s1) s2)))
  )

(defun cs:union (s1 s2)
  "union of 2 sets, returns a (new) set"
  (cond
   ((cs:empty? s1) s2)
   ((cs:empty? s2) s1)
   (t
    (cl-labels
        ((:_union-fn (s1: s2: lst:)
                     (cond
                      ((cs:empty? s1:) (cs:add-each-to s2: lst:))
                      ((cs:empty? s2:) (cs:add-each-to s1: lst:))
                      ((cs:member? (car s1:) s2:)
                       (:_union-fn (cdr s1:) (cs:remove (car s1:) s2:) (cons (car s1:) lst:)))
                      (t (:_union-fn (cdr s1:) s2: (cons (car s1:) lst:))))))
      (:_union-fn s1 s2 '()))))
  )

(defun cs:intersect (s1 s2)
  "intersection of 2 sets, returns a (new) set"
  (if (or (cs:empty? s1) (cs:empty? s2)) '()
    (cl-labels
        ((:_intersect-fn (s1: s2: lst:)
                        (cond
                         ((or (cs:empty? s1:) (cs:empty? s2:)) lst:)
                         ((cs:member? (car s1:) s2:)
                          (:_intersect-fn (cdr s1:) (cs:remove (car s1:) s2:) (cons (car s1:) lst:)))
                         (t (:_intersect-fn (cdr s1:) s2: lst:)))))
      (:_intersect-fn s1 s2 '()))
    )
  )

(defun cs:diff (s1 s2)
  "non symetric difference
- returns a new set whose all elements are in set s1 and not in set s2"
  (cond
   ((cs:empty? s1) '())
   ((cs:empty? s2) s1)
   (t
    (cl-labels
        ((:_diff-fn (s1: s2: lst:)
                   (cond
                    ((cs:empty? s1:) lst:)
                    ((cs:member? (car s1:) s2:) (:_diff-fn (cdr s1:) s2: lst:))
                    (t (:_diff-fn (cdr s1:) s2: (cons (car s1:) lst:))))))
      (:_diff-fn s1 s2 '()))))
  )

(defun cs:add (e s)
  "Add element e in set s - iff e is not already present in s"
  (cond
   ;; ((equal e nil) s)
   ((cs:member? e s) s)
   (t (cons e s)))
  )

(defun cs:remove (e s)
  "Remove element e from set s, if element e is not in set, s remains invariant
- actually s will get reversed... and we will get back a new equivalent set"
  (if (cs:empty? s) s
    (cl-labels
        ((:_rm-fn (e: s: ns:)
                  (cond
                   ((null? s:) ns:)
                   ((equal e: (car s:)) (append ns: (cdr s:)))
                   (t (:_rm-fn e: (cdr s:) (cons (car s:) ns:))))))
      (:_rm-fn e s '())))
  )

(defun cs:add-each-to (s ns)
  "Add each element of (set) s into (set) ns"
  (append s ns))

(defun is-empty? (s)
  (= 0 (length s)))

(defalias 'cs:empty? 'is-empty?)

(defun null? (lst)
  (= 0 (length lst)))

(provide 'custom-set)
;;; custom-set.el ends here
