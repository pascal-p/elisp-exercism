;;; space-age-test.el --- Tests for Space Age

;;; Commentary:

;;; Code:
(load-file "space-age.el")

(setq EPS 0.01)

(ert-deftest test-age-on-earth ()
  (should (within-eps 31.69 (space-age-on-earth 1000000000))))

(ert-deftest test-age-on-mercury ()
  (should (within-eps 280.88 (space-age-on-mercury 2134835688))))

(ert-deftest test-age-on-venus ()
  (should (within-eps 9.78 (space-age-on-venus 189839836))))

(ert-deftest test-age-on-mars ()
  (should (within-eps 35.88 (space-age-on-mars 2129871239))))

(ert-deftest test-age-on-jupiter ()
  (should (within-eps 2.41 (space-age-on-jupiter 901876382))))

(ert-deftest test-age-on-saturn ()
  (should (within-eps 2.15 (space-age-on-saturn 2000000000))))

(ert-deftest test-age-on-uranus ()
  (should (within-eps 0.46 (space-age-on-uranus 1210123456))))

(ert-deftest test-age-on-neptune ()
  (should (within-eps 0.35 (space-age-on-neptune 1821023456))))

(defun within-eps (exp act)
  (<= (abs (- act exp)) EPS))

(provide 'space-age-test)
;; space-age-test.el ends here
