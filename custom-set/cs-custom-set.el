;;; cs-custom-set.el --- Custom-Set

;;; Commentary:
;; an attempt at representing a set as a pair (:set (<set element>))

;;; Code:
(require 'cl-lib)

(setq SET :set)

(defun cs:custom-set (lst)
  (if (<= (length lst) 1) (cs:tag-set lst)
    (cs:cons lst '())))

(defun cs:cons (lst s)
  (cond
   ((null? lst) (cs:tag-set s))
   ((member? (car lst) s) (cs:cons (cdr lst) s))
   (t (cs:cons (cdr lst) (cons (car lst) s)))
   ))

(defun cs:tag-set (s)
  (cons SET (list s)))

(defun member? (elt l)
  "Check whether elt is in list l"
  (cond
   ((= (length l) 0) nil)
   ((equal (car l) elt) t)
   (t (member? elt (cdr l)))))

(defun cs:member? (elt s)
  "Check whether elt is in set (s)"
  (if (not (cs:set? s)) (throw 'Error "s is not a set")
    (cl-labels
        ((:_member-fn (e: s:)
                    (cond
                     ((null? s:) nil)
                     ((equal (car s:) e:) t)
                     (t (:_member-fn e: (cdr s:))))))
      (:_member-fn elt (cadr s))))
  )

(defun cs:subset? (s1 s2)
  "is set s1 a subset of s2?
yes iff all element of s1 are in s2"
  (cond
   ((cs:empty? s1) t)
   ((> (cs:length s1) (cs:length s2)) nil)
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
  (= (cs:length s1) (cs:length s2)))

(defun cs:same-elements? (s1 s2)
  ;; we know that |s1| == |s2|
  (cond
   ((= (cs:length s1) 0) t)
   ((= (cs:length s1) 1) (equal (cs:car s1) (cs:car s2)))
    ;; the 2 sets are now of length > 1
   (t (and (cs:all? s1 s2)
           (cs:all? s2 s1)))))

(defun cs:all? (s1 s2)
  (and (cs:member? (cs:car s1) s2)
       ;; set s1 will eventually be empty
       (if (cs:empty? (cs:cdr s1)) (cs:member? (cs:car s1) s2)
         (cs:all? (cs:cdr s1) s2))
       )
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
                    ((cs:member? (cs:car s1:) s2:)
                     (:_union-fn (cs:cdr s1:) (cs:remove (cs:car s1:) s2:) (cons (cs:car s1:) lst:)))
                    (t (:_union-fn (cs:cdr s1:) s2: (cons (cs:car s1:) lst:))))))
      (:_union-fn s1 s2 '()))
    )
   )
  )

(defun cs:intersect (s1 s2)
  "intersection of 2 sets, return a (new) set"
  (if (or (cs:empty? s1) (cs:empty? s2)) '()
    (cl-labels
        ((:_intersect-fn (s1: s2: lst:)
                        (cond
                         ((or (cs:empty? s1:) (cs:empty? s2:)) (cs:tag-set lst:))
                         ((cs:member? (cs:car s1:) s2:)
                          (:_intersect-fn (cs:cdr s1:) (cs:remove (cs:car s1:) s2:) (cons (cs:car s1:) lst:)))
                         (t (:_intersect-fn (cs:cdr s1:) s2: lst:)))))
      (:_intersect-fn s1 s2 '()))
    )
  )

(defun cs:diff (s1 s2)
  "non symetric difference
- return a new set whose elements are the element of set s1 which are not in set s2"
  (cond
   ((cs:empty? s1) '())
   ((cs:empty? s2) s1)
   (t
    (cl-labels
        ((:_diff-fn (s1: s2: lst:)
                      (cond
                       ((cs:empty? s1:) (cs:tag-set lst:))
                       ((cs:member? (cs:car s1:) s2:) (:_diff-fn (cs:cdr s1:) s2: lst:))
                       (t (:_diff-fn (cs:cdr s1:) s2: (cons (cs:car s1:) lst:))))))
      (:_diff-fn s1 s2 '())))
   )
  )

(defun cs:add (e s)
  "Add element e in set s - iff e is not already present in s"
  (cond
   ((cs:member? e s) s)
   (t (list SET (cons e (cadr s))))) ;; (cons e s)
  )

(defun cs:remove (e s)
  "Remove element e from set s, if element e is not in set, s remains invariant
- actually s will get reversed... and we will get back a new equivalent set"
  (if (cs:empty? s) s
    (cl-labels
        ((:_rm-fn (e: s: lst:)
                  (cond
                   ((cs:empty? s:) (cs:tag-set lst:))
                   ((equal e: (cs:car s:)) (cs:add-each-to (cs:cdr s:) lst:))
                   (t (:_rm-fn e: (cs:cdr s:) (cons (cs:car s:) lst:))))))
      (:_rm-fn e s '())))
  )

(defun cs:add-each-to (s lst)
  "Add each element of s into list lst"
    (cs:tag-set (append lst (cadr s))))

(defun is-empty? (s)
  (and (equal (car s) SET) (= 0 (cs:length s))))

(defalias 'cs:empty? 'is-empty?)

(defun null? (lst)
  (= 0 (length lst)))

(defun cs:null? (s)
  ;; (and (equal (car s) SET) (null? (cs:cdr s)))
  (and (equal (car s) SET) (null? (cadr s)))
  )

(defun cs:length (s)
  ;; assuming s is a set - not checking it
  ;; a check would be: (= (car s) SET)
  (length (cadr s)))

(defun cs:car (s)
  ;; assuming s is a set, example (:set (a b c ...))
  ;; (cs:car s) == (caadr s)) == a
  ;; returns an element
  (caadr s)
  )

(defun cs:cdr (s)
  ;; assuming s is a set, example (:set (a b c ...))
  ;; (cs:cdr s) == (cdr s)) (:set (b c))
  ;; returns a set
  (list SET (cdadr s))
  )

(defun cs:set? (s)
  (and (equal (car s) SET) (listp (cadr s)))
  )

(provide 'cs-custom-set)
;;; cs-custom-set.el ends here
