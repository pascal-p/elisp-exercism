;;; Code

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
