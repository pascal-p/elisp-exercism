;;; nucleotide-count-test.el --- Tests for nucleotide-count (exercism)

;;; Commentary:

;;; Code:

(load-file "nucleotide-count.el")

(defun sort-pairs (lst fn)
  (sort lst (lambda (a b) (funcall fn (car a) (car b)))))

(ert-deftest empty-dna-strand-has-no-nucleotides-test ()
  (should (equal (sort-pairs (nucleotide-count "") #'<)
                 '((?A . 0) (?C . 0) (?G . 0) (?T . 0)))))

(ert-deftest dna-strand-has-4-base-nucleo-test ()
  (should (equal (sort-pairs
                  (nucleotide-count "GCAT") #'<)
                 '((?A . 1) (?C . 1) (?G . 1) (?T . 1)))))

(ert-deftest repetitive-sequence-has-only-guanine-test ()
  (should (equal (sort-pairs
                  (nucleotide-count "GGGGGGGG") #'<)
                 '((?A . 0) (?C . 0) (?G . 8) (?T . 0)))))

(ert-deftest count-all-nucleotides-test ()
  (should (equal (sort-pairs
                  (nucleotide-count "AGCTTTTCATTCTGACTGCAACGGGCAATATGTCTCTGTGTGGATTAAAAAAAGAGTGTCTGATAGCAGC") #'<)
                 '((?A . 20) (?C . 12) (?G . 17) (?T . 21)))))

(ert-deftest invalid-nucleotide-test ()
  (should-error (nucleotide-count "AGGTCCXGA")))

;; other tests

(ert-deftest test-invalid-nucleotide-valid-1 ()
  (should (not (invalid-nucleotide? ""))))

(ert-deftest test-invalid-nucleotide-valid-2 ()
  (should (not (invalid-nucleotide? "AGGTCCGA"))))

(ert-deftest test-invalid-nucleotide-3 ()
  (should (invalid-nucleotide? "AGGTXCCGA")))

(ert-deftest test-invalid-nucleotide-4 ()
  (should (invalid-nucleotide? "AGGTCCGAX")))

(provide 'nucleotide-count)
;;; nucleotide-count-test.el ends here
