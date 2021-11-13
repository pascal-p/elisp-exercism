;;; cs-custom-set.el --- Custom-Set

;;; Commentary:
;; an attempt at representing a set as a pair (:set (<set element>))

;;; Code:
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

(defun member? (e l)
  ;; check whether e is in list l
  (cond
   ((= (length l) 0) nil)
   ((equal (car l) e) t)
   (t (member? e (cdr l)))))

(defun cs:member? (e s)
  "Check whether elt is in set (s)"
  (if (not (cs:set? s)) (throw 'Error "s is not a set")
      (cs:memberp e (cadr s))))

(defun cs:memberp (e s)
  ;; s is now a list, and thus process as is
  (cond
   ((null? s) nil)
   ((equal (car s) e) t)
   (t (cs:memberp e (cdr s))))
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
  "union of 2 sets"
  (cond
   ((cs:empty? s1) s2)
   ((cs:empty? s2) s1)
   (t (cs:build-union s1 s2 '())))
  )

(defun cs:intersect (s1 s2)
  "intersection of 2 sets"
  (if (or (cs:empty? s1) (cs:empty? s2)) '()
    (cs:build-intersect s1 s2 '()))
  )

(defun cs:diff (s1 s2)
  "non symetric difference - return all elements of set s1 which are not in set s2"
  (cond
   ((cs:empty? s1) '())
   ((cs:empty? s2) s1)
   (t (cs:build-diff s1 s2 '())))
  )

(defun cs:add (e s)
  "Add element e in set s - iff e is not already present in s"
  (cond
   ;; ((equal e nil) s)
   ((cs:member? e s) s)
   (t (list SET (cons e (cadr s))))) ;; (cons e s)
  )

(defun cs:remove (e s)
  "Remove element e from set s, if element e is not in set, s remains invariant
- actually s will get reversed... and we will get back a new equivalent set"
  (if (cs:empty? s) s
    (cs:remove-fn e s '()))
  )

(defun cs:remove-fn (e s ns)
  ;; ns here is a list which is tagged as a set once recurrene completes
  (cond
   ((cs:empty? s) (cs:tag-set ns))
   ((equal e (cs:car s)) (cs:add-each-to (cs:cdr s) ns)) ;; (append ns (cs:cdr s))
   (t (cs:remove-fn e (cs:cdr s) (cons (cs:car s) ns))))
  )

(defun cs:build-union (s1 s2 ns)
  "union of 2 sets, returns a (new) set"
  ;; ns here is a list which is tagged as a set once recurrence completes
  (cond
   ((cs:empty? s1) (cs:add-each-to s2 ns))
   ((cs:empty? s2) (cs:add-each-to s1 ns))
   ((cs:member? (cs:car s1) s2)
    (cs:build-union (cs:cdr s1) (cs:remove (cs:car s1) s2) (cons (cs:car s1) ns)))
   (t (cs:build-union (cs:cdr s1) s2 (cons (cs:car s1) ns))))
  )

(defun cs:build-intersect (s1 s2 ns)
  (cond
   ((or (cs:empty? s1) (cs:empty? s2)) (cs:tag-set ns))
   ((cs:member? (cs:car s1) s2)
    (cs:build-intersect (cs:cdr s1) (cs:remove (cs:car s1) s2) (cons (cs:car s1) ns)))
   (t (cs:build-intersect (cs:cdr s1) s2 ns))))

(defun cs:build-diff (s1 s2 ns)
  (cond
   ((cs:empty? s1) (cs:tag-set ns))
   ((cs:member? (cs:car s1) s2) (cs:build-diff (cs:cdr s1) s2 ns))
   (t (cs:build-diff (cs:cdr s1) s2 (cons (cs:car s1) ns)))))

(defun cs:add-each-to (s lst)
  "Add each element of s into list lst"
  (progn
    (setq lst (append lst (cadr s)))
    ;; (let ((add-fn (lambda (e)
    ;;                 (setq lst (cons e lst)))))
    ;;   (mapcar add-fn (cadr s)))
    (cs:tag-set lst)
    ))

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
