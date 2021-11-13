;;; custom-set-test.el --- Tests for Custom set

;;; Commentary:

;;; Code:
(load-file "custom-set.el")

(ert-deftest test-empty ()
  (should (is-empty? (cs-custom-set '())))
  )

(ert-deftest test-not-empty ()
  (should (not (is-empty? (cs-custom-set '(1)))))
  )

(ert-deftest test-cons-set ()
  (should (equal '(9 10 6 5 2 4 3 1) (cs-custom-set '(1 3 4 2 3 2 5 1 2 4 6 10 9 10 4 2 6))))
  )

(ert-deftest test-set-equal-0 ()
  (should (cs-equal? '() nil))
  )

(ert-deftest test-set-equal-1 ()
  (should (cs-equal? '(1) '(1)))
  )

(ert-deftest test-set-equal-3 ()
  (should (cs-equal? '(1 2 3) '(2 3 1)))
  )

(ert-deftest test-set-equal-8 ()
  (should (cs-equal? '(9 10 6 5 2 4 3 1) '(2 4 3 1 10 9 6 5)))
  )

(ert-deftest test-not-set-equal-3 ()
  (should (not (cs-equal? '(1 2 3) '(2 4 1))))
  )

(ert-deftest test-not-set-equal ()
  (should (not (cs-equal? '(1 2 3) '(1 2 4 5))))
  )

(ert-deftest test-subset-base-cases ()
  (and (should (cs-subset? '() '()))
       (should (cs-subset? '() '(1)))
       (should (cs-subset? '(2) '(1 2 3)))
       (should (cs-subset? '(2 1 3) '(1 2 3)))
       (should (not (cs-subset? '(2 3 1) '(3 2))))
       ))

(ert-deftest test-add-elt ()
  (and (should (cs-equal? '(1 2 3) (cs-add 3 '(2 1 3))))
       (should (cs-equal? '(1 2 3) (cs-add 3 '(2 1))))
       (should (cs-equal? '(1) (cs-add 1 '(1))))
       (should (cs-equal? '(1) (cs-add 1 '())))
       (should (cs-equal? '(1 2 nil) (cs-add nil '(2 1))))
       )
  )

(ert-deftest test-union ()
  (and (should (cs-equal? '(1 3 5 2 4 6 8) (cs-union '(1 5 3) '(2 4 3 5 6 8))))
       (should (cs-equal? '(1 3 5 2 4 6 8) (cs-union '(1 5 3 2 4) '(2 4 6 8))))
       (should (cs-equal? '(1 3 5 2 4 6 8) (cs-union '(1 5 3) '(2 4 6 8))))
       (should (cs-equal? '(1 3 5 2 4 6 8) (cs-union '(1 5 3 2 4) '(2 4 3 5 6 8))))
       (should (cs-equal? '(1 3 5 2) (cs-union '(1 5 3 2) '(2 5 1 3))))
       (should (cs-equal? '(1 3 5 2) (cs-union '(1 3 5 2) '(1 3 5 2))))
       (should (cs-equal? '(1 3 5 2) (cs-union '() '(2 5 1 3))))
       (should (cs-equal? '(1 3 5 2) (cs-union '(2 1 3 5) '())))
       (should (cs-equal? '(1 3 5 2) (cs-union '(2 3 5 1) '(2 3 1))))
       )
  )

(ert-deftest test-intersect ()
  (and (should (cs-equal? '(3 5) (cs-intersect '(1 5 3) '(2 4 3 5 6 8))))
       (should (cs-equal? '(2 4) (cs-intersect '(1 5 3 2 4) '(2 4 6 8))))
       (should (cs-equal? '() (cs-intersect '(1 5 3) '(2 4 6 8))))
       (should (cs-equal? '(3 5 2 4) (cs-intersect '(1 5 3 2 4) '(2 4 3 5 6 8))))
       (should (cs-equal? '(1 3 5 2) (cs-intersect '(1 5 3 2) '(2 5 1 3))))
       (should (cs-equal? '(1 3 5 2) (cs-intersect '(1 3 5 2) '(1 3 5 2))))
       (should (cs-equal? '() (cs-intersect '() '(2 5 1 3))))
       (should (cs-equal? '() (cs-intersect '(2 1 3 5) '())))
       (should (cs-equal? '(1 3 2) (cs-intersect '(2 3 5 1) '(2 3 1))))
       )
  )

(ert-deftest test-diff ()
  (and (should (cs-equal? '(1) (cs-diff '(1 5 3) '(2 4 3 5 6 8))))
       (should (cs-equal? '(1 5 3) (cs-diff '(1 5 3 2 4) '(2 4 6 8))))
       (should (cs-equal? '(1 5 3) (cs-diff '(1 5 3) '(2 4 6 8))))
       (should (cs-equal? '(1) (cs-diff '(1 5 3 2 4) '(2 4 3 5 6 8))))
       (should (cs-equal? '() (cs-diff '(1 5 3 2) '(2 5 1 3))))
       (should (cs-equal? '() (cs-diff '(1 3 5 2) '(1 3 5 2))))
       (should (cs-equal? '() (cs-diff '() '(2 5 1 3))))
       (should (cs-equal? '(2 1 3 5) (cs-diff '(2 1 3 5) '())))
       (should (cs-equal? '(5) (cs-diff '(2 3 5 1) '(2 3 1))))
   )
  )

;; other test

(ert-deftest test-null-1 ()
  (should (null? '()))
  )

(ert-deftest test-null-2 ()
  (should (null? nil))
  )

(ert-deftest test-not-null ()
  (should (not (null? '(1 3 2))))
  )

(ert-deftest test-cs-member-1 ()
  (should (cs-member? 1 '(2 3 4 1 5))))

(ert-deftest test-cs-member-2 ()
  (should (not (cs-member? 1 '()))))

(ert-deftest test-cs-member-3 ()
  (should (not (cs-member? 1 '(a b c)))))


(provide 'custom-set-test)
;; custom-set-test.el ends here
