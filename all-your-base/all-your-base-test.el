;;; all-your-base-test.el --- Tests for all-your-base

;;; Commentary:

;;; Code:

(load-file "all-your-base.el")

(ert-deftest test-single-bit-one-to-decimal ()
  (should (equal '(1) (all-your-base '(1) 2 10))))

(ert-deftest test-binary-to-single-decimal ()
  (should (equal '(5) (all-your-base '(1 0 1) 2 10))))

(ert-deftest test-single-decimal-to-binary ()
  (and
    (should (equal '(1 0 1) (all-your-base '(5) 10 2)))
    (should (equal '(1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0) (all-your-base '(6 5 5 3 6) 10 2)))
    (should (equal '(1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1) (all-your-base '(6 5 5 3 5) 10 2)))
    ))

(ert-deftest test-binary-to-multi-decimal ()
  (should (equal '(4 2) (all-your-base '(1 0 1 0 1 0) 2 10))))

(ert-deftest test-decimal-to-binary ()
  (should (equal '(1 0 1 0 1 0) (all-your-base '(4 2) 10 2))))

(ert-deftest test-trinary-to-hexadecimal ()
  (should (equal '(2 10) (all-your-base '(1 1 2 0) 3 16))))

(ert-deftest test-decimal-decimal ()
  (should (equal '(1 1 0 9) (all-your-base '(1 1 0 9) 10 10))))

(ert-deftest test-octal-octal ()
  (should (equal '(1 1 7 7 ) (all-your-base '(1 1 7 7) 8 8))))

(ert-deftest test-hexa-hexa ()
   (should (equal '(10 11 15 12) (all-your-base '(10 11 15 12) 16 16))))

(ert-deftest test-decimal-to-hexadecimal ()
  (and
   (should (equal '(1 0 0 0 0) (all-your-base '(6 5 5 3 6) 10 16)))
   (should (equal '(15 15 15 15) (all-your-base '(6 5 5 3 5) 10 16)))
   (should (equal '(15 15 15 15 15 15 15 15) (all-your-base '(4 2 9 4 9 6 7 2 9 5) 10 16)))
   (should (equal '(1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 10) (all-your-base '(1 8 4 4 6 7 4 4 0 7 3 7 0 9 5 5 1 6 2 6) 10 16)))
   ))

(ert-deftest test-hexadecimal-to-trinary ()
   (should (equal '(1 1 2 0) (all-your-base '(2 10) 16 3)))
  )

(ert-deftest test-15-bit-integer ()
   (should (equal '(6 10 45) (all-your-base '(3 46 60) 97 73)))
)

(ert-deftest test-empty-list ()
   (should (equal '(0) (all-your-base '() 2 10)))
)

(ert-deftest test-single-zero ()
   (should (equal '(0) (all-your-base '(0) 10 2)))
)

(ert-deftest test-multi-zeros ()
   (should (equal '(0) (all-your-base '(0 0 0) 10 2)))
)

(ert-deftest test-leading-zeros ()
   (should (equal '(4 2) (all-your-base '(0 6 0) 7 10)))
)

;; other tests

(ert-deftest test-to-base-10-from-base-2-1 ()
  ;; (1)/base 2 => 1 in base 10
  (should (= 1 (to-base-10 '(1) 2))))

(ert-deftest test-to-base-10-from-base-2-1 ()
  ;; (1 1 0 1)/base 2 => 13 in base 10
  (should (= 13 (to-base-10 '(1 1 0 1) 2))))

(ert-deftest test-to-base-10-from-base-3 ()
  ;; (1 2 3 1)/base 3 => 55 in base 10
  (should (= 55 (to-base-10 '(1 2 3 1) 3))))

(ert-deftest test-to-base-10-from-base-3 ()
  ;; (1 2 3 0)/base 3 => 54 in base 10
  (should (= 54 (to-base-10 '(1 2 3 0) 3))))

(ert-deftest test-to-base-10-from-base-10 ()
  (should (= 12304 (to-base-10 '(1 2 3 0 4) 10))))

(ert-deftest test-divrem-103-by-2 ()
  (should (equal '(51 . 1) (divrem 103 2))))

(ert-deftest test-divrem-65-by-7 ()
  (should (equal '(9 . 2) (divrem 65 7))))

(ert-deftest test-divrem-1-by-10 ()
  (should (equal '(0 . 1) (divrem 1 10))))

(ert-deftest test-to-target-base-1-b10 ()
  (should (equal '(1) (to-target-base 1 10))))

(ert-deftest test-to-target-base-103-b2 ()
  (should (equal '(1 1 0 0 1 1 1) (to-target-base 103 2))))

(ert-deftest test-to-target-base-1025-b16 ()
  (should (equal '(4 0 1) (to-target-base 1025 16))))

(ert-deftest test-to-target-base-8192-b16 ()
  (should (equal '(2 0 0 0) (to-target-base 8192 16))))

(ert-deftest test-check-valid-digits-1 ()
  ;; error because of -1
  (should-error (check-valid_digits? '(1 2 -1 0) 3)))

(ert-deftest test-check-valid-digits-2 ()
  ;; error because of 3 - base 3 = (0 1 2) for base digits
  (should-error (check-valid_digits? '(1 2 3 0) 3)))

(ert-deftest test-check-valid-digits-3 ()
  ;; fine
  (should (check-valid_digits? '(1 2 3 2 1 0) 4)))

(ert-deftest test-are-all-digits-0-single-0 ()
  (should (are-all-digits-0? '(0))))

(ert-deftest test-are-all-digits-0-single-non-0 ()
  (should (not (are-all-digits-0? '(1)))))

(ert-deftest test-are-all-digits-0-no-digits ()
  (should (not (are-all-digits-0? '()))))

(ert-deftest test-are-all-digits-0-with-list-of-one-non-0 ()
  (should (not (are-all-digits-0? '(0 0 0 0 0 0 0 1 0 0)))))

(ert-deftest test-are-all-digits-0-with-list-of-0s ()
  (should (are-all-digits-0? '(0 0 0 0 0 0 0 0 0 0))))

(provide 'all-your-base-test)
;;; all-your-base-test.el ends here
