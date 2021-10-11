;;; perfect-numbers-test.el --- Tests for perfect-numbers (exercism)

;;; Commentary:

;;; Code:

(load-file "perfect-numbers.el")

;;; Perfect Numbers
(ert-deftest smallest-perfect-number ()
    (should (equal (classify 6) 'perfect)))

(ert-deftest medium-perfect-number ()
  (should (equal (classify 28) 'perfect)))

(ert-deftest large-perfect-number ()
  (should (equal (classify 33550336) 'perfect)))

;; first 47 even perfect numbers

;; too slow
;; (ert-deftest first-47-perfect-number ()
;;   (let ((lst '(2 3 5 7 13 17 19 31 61 89 107 127 521 607 1279 2203 2281 3217 4253 4423
;;                  ;;9689 9941 11213 19937 21701 23209 44497 86243 110503 132049 216091 756839 859433
;;                  ;;1257787 1398269 2976221 3021377 6972593 13466917 20996011 24036583 25964951 30402457
;;                  ;;32582657 37156667 42643801 43112609
;;                  )))
;;         (while (> (length lst) 0)
;;           (setq p (car lst))
;;           (setq x (pow 2 (1- p)))
;;           (setq d (* x (- (* x 2) 1)))
;;           (and t ;; (should (= 1 (% d 2)))
;;                (should (equal (classify d) 'perfect)))
;;           (setq lst (cdr lst))
;;           ))
;;   )

;; (defun pow (n p)
;;   ;; (pow-fn n p 1)
;;   (pow-fn n p)
;;   )

;; ;; (defun pow-fn (n p r)
;; ;;   (if (= p 0) r
;; ;;     (pow-fn n (1- p) (* r n)))
;; ;;   )

;; (defun pow-fn (n p)
;;   (let ((r 1)
;;         (x 1))
;;     (while (<= x p)
;;       (setq r (* r n))
;;       (setq x (1+ x)))
;;     r)
;;   )

;;; Abundant Numbers
(ert-deftest smallest-abundant-number ()
  (should (equal (classify 12) 'abundant)))

(ert-deftest medium-abundant-number ()
  (should (equal (classify 30) 'abundant)))

(ert-deftest large-abundant-number ()
  (should (equal (classify 33550335) 'abundant)))

;; Deficient Numbers
(ert-deftest smallest-deficient-number ()
  (should (equal (classify 2) 'deficient)))

(ert-deftest smallest-non-prime-deficient-number ()
  (should (equal (classify 4) 'deficient)))

(ert-deftest medium-deficient-number ()
  (should (equal (classify 32) 'deficient)))

(ert-deftest large-deficient-number ()
  (should (equal (classify 33550337) 'deficient)))

(ert-deftest edge-case-no-factors-other-than-self ()
  (should (equal (classify 1) 'deficient)))

;; Invalid Inputs
(ert-deftest zero-is-rejected ()
  (should
   (equal
    (should-error (classify 0))
    '(error . ("Classification is only possible for natural numbers")))))

(ert-deftest negative-integer-is-rejected ()
  (should
   (equal
    (should-error (classify -1))
    '(error . ("Classification is only possible for natural numbers")))))

(provide 'perfect-numbers)
;;; perfect-numbers-test.el ends here
