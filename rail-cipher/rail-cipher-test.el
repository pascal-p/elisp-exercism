;;; raill-cipher-test.el --- Tests for Rail Cipher

;;; Commentary:

;;; Code:

(load-file "rail-cipher.el")

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
;;


(ert-deftest test-init-rails ()
  (should (equal '(() () ()) (init-rails 3))))

(ert-deftest test-init-rails-2 ()
  (should (equal '(()) (init-rails 1))))

(ert-deftest test-init-rails-3 ()
  (should (equal '() (init-rails 0))))


(ert-deftest test-add-to-3rd ()
  (should (equal '(("B") ("C" "D") ("W" "A") ("C" "D"))
                 (add "A" 2'(("B") ("C" "D") ("W") ("C" "D"))))))

(ert-deftest test-add-to-1st ()
  (should (equal '(("B" "A") ("C") ())
                 (add "A" 0 '(("B") ("C") ())))))

(ert-deftest test-add-no-add ()
  (should (equal '(("B" "A") ("C") ())
                 (add "X" 3 '(("B" "A") ("C") ())))))

(provide 'rail-cipher-test)
;; raill-cipher-test.el ends here
