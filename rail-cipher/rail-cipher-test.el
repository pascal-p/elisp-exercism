;;; raill-cipher-test.el --- Tests for Rail Cipher

;;; Commentary:

;;; Code:

(load-file "rail-cipher.el")


;; encode
(ert-deftest test-encode-1-rail ()
  (should (equal "One rail, only one rail" (encode 1 "One rail, only one rail"))))

(ert-deftest test-encode-3-rails ()
  (should (equal "WECRLTEERDSOEEFEAOCAIVDEN" (encode 3 "WE ARE DISCOVERED FLEE AT ONCE"))))

(ert-deftest test-encode-5-rails ()
  (should (equal "tiehlsdteiieadvnhisetl" (encode 5 "the devil, is in the details..."))))

(ert-deftest test-encode-5-rails-empty ()
  (should (equal "" (encode 5 ""))))

(ert-deftest test-encode-2-rails ()
  (should (equal "XXXXXXXXXOOOOOOOOO" (encode 2 "XOXOXOXOXOXOXOXOXO"))))

;; exception encode
(ert-deftest test-encode-wirh-more-rails-than-letter-in-src ()
  (should-error (encode 24 "More rails than letters")))


;; TODO: decode
(ert-deftest test-decode-empty-msg ()
  (should (equal "" (decode 5 ""))))

(ert-deftest test-decode-using-1-rail ()
  (should (equal "ONE RAIL, ONLY ONE RAIL" (decode 1 "ONE RAIL, ONLY ONE RAIL"))))

(ert-deftest test-decode-using-2-rails ()
  (should (equal "XOXOXOXOXOXOXOXOXO" (decode 2 "XXXXXXXXXOOOOOOOOO"))))

(ert-deftest test-decode-using-3-rails ()
  (should (equal "THEDEVILISINTHEDETAILS" (decode 3 "TEITELHDVLSNHDTISEIIEA"))))

(ert-deftest test-decode-using-6-rails ()
  (should (equal "112358132134558914423337761098715972584418167651094617711286"
                 (decode 6 "133714114238148966225439541018335470986172518171757571896261"))))

;; identity
;; define ad-hoc all? hof
(defun all? (pred-fn lst)
  "Return true iff all elements of lst verified the predicate function pred-fn
and false otherwise
"
  (eval (cons 'and (mapcar pred-fn lst)))
  )

(ert-deftest test-decode-encode-identity ()
  (should
   (all? (lambda (pair) (equal (car pair)
                               (encode (cdr pair) (decode (cdr pair) (car pair)))))
         '(("XOXOXOXOXOXOXOXOXO" . 2)
           ("WEAREDISCOVEREDFLEEATONCE" . 3)
           ("THEDEVILISINTHEDETAILS" . 3)
           ("THEDEVILISINTHEDETAILS". 5)
           ("133714114238148966225439541018335470986172518171757571896261" . 6)))
   )
  )

(ert-deftest test-replace-mark ()
  (should (equal '(("F" "." "O" "." "A" ".")
                   ("." "O" "." "B" "." "R"))
                 (replace-mark '(("?" "." "?" "." "?" ".")
                                 ("." "?" "." "?" "." "?")) '("F" "O" "A" "O" "B" "R")))))

(ert-deftest test-transpose ()
  (should (equal '(("F" ".") ("." "O") ("O" ".") ("." "B") ("A" ".") ("." "R"))
                 (transpose '(("F" "." "O" "." "A" ".") ("." "O" "." "B" "." "R"))))))

(ert-deftest test-squeeze ()
  (should (equal '(("F") ("O") ("O") ("B") ("A") ("R"))
                 (mapcar 'squeeze-rail '(("F" ".") ("." "O") ("O" ".") ("." "B") ("A" ".") ("." "R")))))
  )

(ert-deftest test-word-to-list ()
  (should (equal '("X" "X" "X" "X" "X" "X" "X" "X" "X" "O" "O" "O" "O" "O" "O" "O" "O" "O")
                 (word-to-list "XXXXXXXXXOOOOOOOOO")))
  )

;; other test helpers

(ert-deftest test-init-rails ()
  (should (equal '(() () ()) (init-rails 3))))

(ert-deftest test-init-rails-2 ()
  (should (equal '(()) (init-rails 1))))

(ert-deftest test-init-rails-3 ()
  (should (equal '() (init-rails 0))))


(ert-deftest test-add-to-3rd ()
  (should (equal '(("B" ".") ("C" "D" ".") ("W" "A") ("C" "D" "."))
                 (add "A" 2'(("B") ("C" "D") ("W") ("C" "D"))))))

(ert-deftest test-add-to-1st ()
  (should (equal '(("B" "A") ("C" ".") ("."))
                 (add "A" 0 '(("B") ("C") ())))))

(ert-deftest test-add-no-add ()
  (should (equal '(("B" "A") ("C") ())
                 (add "X" 3 '(("B" "A") ("C") ())))))

(provide 'rail-cipher-test)
;; raill-cipher-test.el ends here
