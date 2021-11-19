;;; Code

;; Lisp etude - 1
;; a version of nconc

(defun ya:nconc-l (l1 l2)
  (let ((tail l1))
    ;; find last element of l1
    (while (cdr tail)
      (setq tail (cdr tail)))
    ;; either tail in empty - we then just need to copy l2 over
    ;; or we "attach" l2 to last element of l1 (tail)
    ;; in both cases => side-effecting l1
    (if (null tail) (setq l1 l2)
      (setcdr tail l2)))
  l1)

(defun ya:nconc-safe (l1 l2)
  (let ((tail l1))
    ;; find last of l1 which is not l2
    (while (and (cdr tail) (not (eq tail l2)))
      (setq tail (cdr tail)))
    ;; if last is not l2 - proceed as with the "unsafe" versiom
    (unless (eq tail l2)
      (if (null tail) (setq l1 l2)
        (setcdr tail l2)))
    ;; otherwise no-op
    l1))
