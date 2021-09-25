;;; anagram.el --- Anagram (exercism)

;;; Commentary:

;;; Code:
;; (require 'cl-lib)
;; cf. https://www.gnu.org/software/emacs/manual/html_node/cl/
;; disabling this lib.

(defun anagrams-for (word wlst)
  "given a word and wlist produce all the anagram of word present in wdlst.
Ex. (anagrams-for \"ant\" '(\"stand\" \"tan\" \"at\") -> \"tan\" "
  (cond
   ((= (length word) 0) "")
   ((ya-empty? wlst) "")
   (t (find-ana word wlst)))
  )

(defun find-ana (word wlst)
  (let* ((n (length word))
         (f-wlst (p-cand n wlst))               ;; filter possible candidates
         (p-wlst (build-pairs f-wlst)))         ;; create list of pairs: ex. (("ant" . "tan") ("abr" ."bar"))
    (find-ana-aux word (gen-key word) p-wlst))  ;; lookup for match using word's key
  )

(defun find-ana-aux (word wkey plst)
  "ex. find-ana-aux \"tan\" \"ant\" '(...)
wkey is case insensitive
word preserves original case
"
  (if (ya-empty? plst) '()
    (let* ((pred? (lambda (pair)
                    (and (string= (car pair) wkey)
                         (not (string= (cdr pair) word))))))
      (mapcar 'cdr (ya-filter pred? plst))
      )))

(defun p-cand (n wlst)
  (let ((pred? (lambda (w) (= (length w) n)))) ;; predicate
    (ya-filter pred? wlst)                     ;; filter wlst given pred?
    ))

(defun build-pairs (wlst)
  (ya-reverse (build-pairs-aux wlst '()))
  )

(defun build-pairs-aux (wlst rlst)
  (if (ya-empty? wlst) rlst
    (build-pairs-aux (cdr wlst)
                     (cons (cons (gen-key (car wlst)) (car wlst))
                           rlst)))
  )

(defun gen-key (word)
  "case insensitive
ex. given \"tan\" returns \"ant\",
given \"Bar\" returns \"abr\"
"
  (apply 'concat
         (sort (delete "" (split-string (downcase word) "")) 'string<))
  )

;; copy from word-count:
;; we need a filter hof (higher order function)
(defun ya-filter (pred? lst)
  (delete ":x_d_e_l"              ;; a special token to remove from resulting list
          (mapcar (lambda (x)
                    (if (funcall pred? x) x (symbol-name ':x_d_e_l))) lst))
  )

(defun ya-reverse (lst)
  "reverse a given list (list of len n => O(n)
(ya-reverse '(30 20 10 0)) -> '(0 10 20 30)
"
  (cond
   ((ya-empty? lst) lst)
   ((= 1 (length lst)) lst)
   (t (ya-rev-aux lst '())))
  )

(defun ya-rev-aux (lst rlst)
  (if (ya-empty? lst) rlst
    (ya-rev-aux (cdr lst) (cons (car lst) rlst))))

(defun ya-empty? (lst)
  (= 0 (length lst)))

(provide 'anagram)
;;; anagram.el ends here
