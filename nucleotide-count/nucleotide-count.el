;; -*- lexical-binding: t -*-

;;; nucleotide-count.el --- nucleotide-count Exercise (exercism)

;;; Commentary:
;; make use of package asoc.el

;;; Code:
(defun ya-base ()
  "returns a copy which can then be modified"
  (copy-alist '((?A . 0) (?C . 0) (?G . 0) (?T . 0))))

(defun nucleotide-count (str)
  (cond
   ((= 0 (length str)) (ya-base))
   ((invalid-nucleotide? str) (throw 'Error "Invalid nucleotide"))
   (t (n-count (str-2-list str) (ya-base))))
  )

(defun n-count (lst cnt-map)
  (if (= 0 (length lst)) cnt-map
    (n-count (cdr lst)
             (update-map (str-2-char (car lst)) cnt-map))))

(defun update-map (nuc cnt-map)
  "assuming valid nucleotide ?A, ?C, ?G or ?T"
  (let* ((cnt (cdr (assoc nuc cnt-map)))
         (n-cnt (1+ cnt))
         (n-pair (cons nuc n-cnt)))
    (cons n-pair (assoc-delete-all nuc cnt-map))
    )
  ;; w/o let:
  ;; (cons (cons nuc (1+ (cdr (assoc nuc cnt-map))))
  ;;       (assoc-delete-all nuc cnt-map))
  )

(defun invalid-nucleotide? (str)
  (cond
   ((= 0 (length str)) nil)
   ((string-match "^[AaCcGgTt]+$" str) nil)
   (t t))
  )

(defun str-2-list (str)
  (delete "" (split-string str "")))

(defun str-2-char (nuc)
  (string-to-char (upcase nuc)))

(provide 'nucleotide-count)
;;; nucleotide-count.el ends here
