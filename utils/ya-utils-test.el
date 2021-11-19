;;; Code
(load-file "ya-utils.el")

;; run with: `emacs -batch -l ert -l ya-utils-test.el -f ert-run-tests-batch-and-exit`

;; ya:last
(ert-deftest test-ya-last-empty ()
  (should (eq '() (ya:last '())))
  )

(ert-deftest test-ya-last-singleton ()
  (should (= 10 (ya:last '(10))))
  )

(ert-deftest test-ya-last-singleton-2 ()
  (should (eq 'a (ya:last '(a))))
  )

(ert-deftest test-ya-last-general-case ()
  (should (= 20 (ya:last '(10 30 20))))
  )

(ert-deftest test-last-on-non-list ()
  (should-error (ya:last 'a)))

;; ya:singleton?
(ert-deftest test-ya-singleton-empty ()
  (should (not (ya:singleton? '())))
  )

(ert-deftest test-ya-singleton-on-list-of-more-than-1-item ()
  (should (not (ya:singleton? '(20 10 30))))
  )

(ert-deftest test-ya-singleton-on-list-of-1-item ()
  (should (ya:singleton? '(30)))
  )

;; ya:append
(ert-deftest test-ya-append-1 ()
  (should (equal '(20) (ya:append '() 20)))
  )

(ert-deftest test-ya-append-2 ()
  (should (equal '(30 10 20) (ya:append '(30 10) 20)))
  )

(ert-deftest test-ya-append-3 ()
  (should (equal '(30 10 20 (a b)) (ya:append '(30 10 20) '(a b))))
  )

(ert-deftest test-ya-append-4 ()
  (should (equal '(()) (ya:append '() '())))
  )

;; ya:conc!
(ert-deftest test-ya-conc!-1 ()
  (should (equal '(20) (ya:conc! '() 20)))
  )

(ert-deftest test-ya-conc!-2 ()
  (should (equal '(30 10 20) (ya:conc! '(30 10) 20)))
  )

(ert-deftest test-ya-conc!-3 ()
  (should (equal '(30 10 20 (a b)) (ya:conc! '(30 10 20) '(a b))))
  )

(ert-deftest test-ya-conc!-4 ()
  (should (equal '(()) (ya:conc! '() '())))
  )

;; ya:mklist
(ert-deftest test-ya-mklist-1a ()
  (should (equal '(20) (ya:mklist '(20))))
  )

(ert-deftest test-ya-mklist-1b ()
  (should (equal '(20) (ya:mklist 20)))
  )

(ert-deftest test-ya-mklist-2a ()
  (should (equal '(30 10 20) (ya:mklist '(30 10 20))))
  )

(ert-deftest test-ya-mklist-2b ()
  (should (equal '(t) (ya:mklist t)))
  )

(ert-deftest test-ya-mklist-3a ()
  (should (equal '() (ya:mklist nil)))
  )

(ert-deftest test-ya-mklist-3b ()
  (should (equal '(t nil) (ya:mklist '(t nil))))
  )
