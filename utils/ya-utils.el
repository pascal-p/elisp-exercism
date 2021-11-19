;;; Code
(require 'cl-lib)

(defsubst ya:last (lst)
  "returns the last element in a list
Rem: built-in `last` returns the last cons (ie a list
Ex.
(ya:last '(10 20 30)) ;; 30
(last '(10 20 30)) ;; (30)
"
  (car (last lst)))

(defsubst ya:singleton? (ref)
  "The function single tests whether something is a list of one element.
Lisp programs need to make this test rather often.
"
  ;; is ref a list without morethan 1 element
  (and (consp ref) (not (cdr ref)))
  )

(defsubst ya:append (lst obj)
  "attach an element obj to the end of list lst"
  (append lst (list obj)))

(defsubst ya:conc! (lst obj)
  "attach an element obj to the end of list lst - destructive version
which modify lst"
  (nconc lst (list obj)))

(defsubst ya:mklist (obj)
  "make sure we deal with a list..."
  (if (listp obj) obj (list obj)))

(defun ya:longer (obj1 obj2)
  "Since longer is for comparing lengths, it should work for anything that one could give as an arg to length.
Yet the possibility of comparing lengths in parallel only applies to lists, so the internal function (:_cmp-fn)
is only called if both arguments are lists.

If one list is actually longer we do not waste our time on traversing this longest list...
"
  (cl-labels
      ((:_cmp-fn (x y)
                 (and (consp x)
                      (or (null y)
                          (:_cmp-fn (cdr x) (cdr y))))))
    (if (and (listp obj1)
             (listp obj2)) (:_cmp-fn obj1 obj2) ;; these are lists
      (> (length obj1) (length obj2)))  ;; any other sequence supporting length
    ))


(defun ya:filter (pred-fn lst)
  "The combination of push and nreverse in the definition of ya:filter is the
standard Lisp idiom for accumulating a list."
  (let ((n-lst nil))
    ;; loop - linear in length of lst
    (dolist (x lst)
      (let ((v (funcall pred-fn x)))
        (and v (push v n-lst))))
    ;; finally re-order n-lst - in linear time
    (nreverse n-lst)))


(defun ya:group (src n)
  "note: internal rec-fn is tail recursive"
  (if (zerop n) (error "zero length")
    (cl-labels
        ((:rec-fn (src gp-lst)
                  (let ((rest (nthcdr n src)))
                    (if (consp rest)
                        (:rec-fn rest (cons (seq-subseq src 0 n) gp-lst))
                      (nreverse (cons src gp-lst))))))
      (if src (:rec-fn src nil)
        nil))
    ))
