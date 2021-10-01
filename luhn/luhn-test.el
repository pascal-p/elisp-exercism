;;; luhn-test.el --- Tests for luhn (exercism)

;;; Commentary:

;;; Code:

(require 'cl-lib)
(load-file "luhn.el")

(ert-deftest an-empty-string ()
  (should-not (luhn-p "")))

(ert-deftest space-only ()
  (should-not (luhn-p "   ")))

(ert-deftest single-digit ()
  (should-not (luhn-p "0")))

(ert-deftest another-single-digit ()
  (should-not (luhn-p "9")))

(ert-deftest single-digit-with-space ()
  (should-not (luhn-p "0  ")))

(ert-deftest multiple-zeros ()
  (should (luhn-p "0000")))

(ert-deftest multiple-zeros-with-space ()
  (should (luhn-p "0 0 0")))

(ert-deftest a-valid-3-digit-input-that-can-be-reversed ()
  (should (luhn-p "059")))

(ert-deftest an-invalid-3-digit-input ()
  (should-not (luhn-p "095")))

(ert-deftest a-valid-luhn-number ()
  (should (luhn-p "49927398716")))

(ert-deftest an-invalid-11-digit-number ()
  (should-not (luhn-p "49927398717")))

(ert-deftest invalid-16-digit-luhn-number ()
  (should-not (luhn-p  "1234567812345678")))

(ert-deftest 16-digits-valid-luhn-number ()
  (should (luhn-p "1234567812345670")))

(ert-deftest input-string-containing-a-letter-in-the-middle ()
  (should-error (luhn-p "1234567a45670"))
  )

(ert-deftest input-string-containing-a-punctuation-in-the-middle ()
  (should-error (luhn-p "12_45678!!@45670")))

;; testing some helper function

(ert-deftest test-double-every-2nd-t1 ()
  (should (equal (double-every-2nd '(4 5 3 9 3 1 9 5 0 3 4 3))
                 '(4 1 3 9 3 2 9 1 0 6 4 6))))

(ert-deftest test-double-every-2nd-t2 ()
  (should (equal (double-every-2nd '(4 9 5 9))
                 '(4 9 5 9))))

(ert-deftest test-double-every-2nd-t3 ()
  (should (equal (double-every-2nd '())
                 '())))
(ert-deftest test-double-every-2nd-t4 ()
  (should (equal (double-every-2nd '(1))
                 '(1))))

(ert-deftest test-double-every-2nd-t5 ()
  (should (equal (double-every-2nd '(1 7))
                 '(1 5))))

(ert-deftest test-double-every-2nd-t6 ()
  (should (equal (double-every-2nd '(0 0 0 0))
                 '(0 0 0 0))))

(ert-deftest test-ya-str-to-dlist-1 ()
  (should (equal (ya-str-to-dlist "1  23 4" t)
                 '(1 2 3 4)))
  )

(ert-deftest test-ya-str-to-dlist-nil1 ()
  (should (equal (ya-str-to-dlist "1  23 4")
                 '(1 0 0 2 3 0 4)))
  )

(ert-deftest test-ya-str-to-dlist-nil2 ()
  (should (equal (ya-str-to-dlist "1  23 4" nil)
                 '(1 0 0 2 3 0 4)))
  )

(provide 'luhn-test)
;;; luhn-test.el ends here

