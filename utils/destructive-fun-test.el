;;; Code
(load-file "destructive-fun.el")

;; run with: `emacs -batch -l ert -l destructive-fun-test.el -f ert-run-tests-batch-and-exit`

(ert-deftest test-ya-nconc-1 ()
  "Empty list + another list == another list"
  (should (equal '(10 20) (ya:nconc-l '() '(10 20))))
  )

(ert-deftest test-ya-nconc-2 ()
  "list + empty list == list"
  (should (equal '(10 20 30) (ya:nconc-l '(10 20 30) '())))
  )

(ert-deftest test-ya-nconc-3 ()
  "list + empty list == list"
  (should (equal '(10 20 30 50 60) (ya:nconc-l '(10 20 30) '(50 60))))
  )

;; (ert-deftest test-ya-nconc-circular! ()
;;   (let ((x '(10 20 30))
;;         (y '(50 60)))
;;     (should (equal (quote (10 20 30 50 60 50 60 50 . #4))
;;                    (ya:nconc-l (ya:nconc-l x y) y))))
;;   )

(ert-deftest test-ya-nconc-safe ()
  (let ((x '(10 20 30))
        (y '(50 60)))
    (should (equal (quote (10 20 30 50 60))
                   (ya:nconc-safe (ya:nconc-safe x y) y))))
  )
