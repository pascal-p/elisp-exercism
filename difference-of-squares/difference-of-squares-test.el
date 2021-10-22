;;; difference-of-squares-test.el --- Tests for difference-of-squares (exercism)

;;; Commentary:

;;; Code:

(load-file "difference-of-squares.el")

(ert-deftest difference-of-squares-to-1000 ()
  (should (= 250166416500 (difference 1000))))

(ert-deftest square-of-sum-to-5 ()
  (should (= 225 (square-of-sum 5))))

(ert-deftest sum-of-squares-to-5 ()
  (should (= 55 (sum-of-squares 5))))

(ert-deftest difference-of-squares-to-5 ()
  (should (= 170 (difference 5))))


(ert-deftest square-of-sum-to-10 ()
  (should (= 3025 (square-of-sum 10))))

(ert-deftest sum-of-squares-to-10 ()
  (should (= 385 (sum-of-squares 10))))

(ert-deftest difference-of-squares-to-10 ()
  (should (= 2640 (difference 10))))

(ert-deftest square-of-sum-to-100 ()
  (should (= 25502500 (square-of-sum 100))))

(ert-deftest sum-of-squares-to-100 ()
  (should (= 338350 (sum-of-squares 100))))

(ert-deftest difference-of-squares-to-100 ()
  (should (= 25164150 (difference 100))))

;;
;; (ert-deftest test-square-of-sum-5 ()
;;   (should (= 225 (square-of-sum 5))))

;; (ert-deftest test-square-of-sum-100 ()
;;   (should (= 25502500 (square-of-sum 100))))

(ert-deftest test-square-of-sum-1000 ()
  (should (= 250500250000 (square-of-sum 1000))))


(ert-deftest test-sum-of-squares-5 ()
  (should (= 55 (sum-of-squares 5))))

;; (ert-deftest test-sum-of-squares-10 ()
;;   (should (= 385 (sum-of-squares 10))))

;; (ert-deftest test-sum-of-squares-100 ()
;;   (should (= 338350 (sum-of-squares 100))))

(provide 'difference-of-squares-test)
;;; difference-of-squares-test.el ends here
