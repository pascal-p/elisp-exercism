;; -*- lexical-binding: t -*-

;;; nucleotide-count.el --- nucleotide-count (exercism)

;;; Commentary:

;;; Code:
(defun ya-base ()
  "returns a copy which can then be modified"
  (copy-alist '((?A . 0) (?C . 0) (?G . 0) (?T . 0))))

(defun nucleotide-count (str)
  (cond
   ((= 0 (length str)) (ya-base))
   ((invalid-nucleotide? str) (throw 'Error "Invalid nucleotide"))
   (t (n:count (str:2:list str) (ya-base))))
  )

(defun n:count (lst cnt-map)
  (if (= 0 (length lst)) cnt-map
    (n:count (cdr lst)
             (update-map! cnt-map (str:2:char (car lst))))))

(defmacro get-assoc-val (cnt-map nucleo)
  `(cdr (assoc ,nucleo ,cnt-map))
  )

(defmacro update-assoc! (cnt-map nucleo val)
  `(cons (cons ,nucleo ,val) (assoc-delete-all ,nucleo ,cnt-map))
  )

(defun update-map! (cnt-map nucleo)
  "assuming valid nucleotide ?A, ?C, ?G or ?T"
  (let* ((cnt (get-assoc-val cnt-map nucleo)))
    (update-assoc! cnt-map nucleo (1+ cnt)))
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

(defsubst str:2:list (str)
  (delete "" (split-string str "")))

(defsubst str:2:char (nuc)
  (string-to-char (upcase nuc)))

(provide 'nucleotide-count)
;;; nucleotide-count.el ends here
