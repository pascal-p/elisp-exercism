;;; rna-transcription.el -- RNA Transcription (exercism)

;;; Commentary:


;;; Code:
(setq base
      '(("G" . "C")
        ("C" . "G")
        ("T" . "A")
        ("A" . "U")))

(defun to-rna (dna-strand)
  "Given a dna-strand converts it to a RNA ones string
ref. http://ergoemacs.org/emacs/elisp_string_functions.html"
  (cond
   ((<= (length dna-strand) 0) "")
   ((= (length dna-strand) 1) (single-symb dna-strand))
   (t (multi-symb dna-strand)))
  )


(defun single-symb (dna-symb)
    (let ((res (cdr (assoc dna-symb base))))
      (or (and (boundp 'res) res)
          (throw 'Error "not such key")))
  )

(defun multi-symb (dna-strand)
  ;; (mapconcat 'identity '("a" "b" "c") "") => "abc"
  (mapconcat 'identity
             (mapcar 'single-symb
                     (mapcar 'char-to-string dna-strand)) "")
  )

(provide 'rna-transcription)
;;; rna-transcription.el ends here
