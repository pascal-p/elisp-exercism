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

;; ya:longer
(ert-deftest test-ya-longer-1 ()
  (should (ya:longer '(t nil nil t nil) '()))
  )

(ert-deftest test-ya-longer-2 ()
  (should (not (ya:longer '(1 2 3) '(3 2 1))))
  )

(ert-deftest test-ya-longer-3 ()
  (should (not (ya:longer '(1 2 3) '(3 2 1 a b c))))
  )

(ert-deftest test-ya-longer-4 ()
  (should (ya:longer '(1 2 3 b a c) '(a b c)))
  )

(ert-deftest test-ya-longer-5a ()
  (should (ya:longer "123bac" "abc")))

(ert-deftest test-ya-longer-5b ()
  (should (not (ya:longer "bac" "a1b2c3"))))

(ert-deftest test-ya-longer-5c ()
  (should (not (ya:longer "bar" "foo"))))

;; ya:filter
(ert-deftest test-ya-filter-1 ()
  (let* ((even-fn (lambda (x) (and (= 0 (% x 2)) x)))
         (res (ya:filter even-fn '(7 6 5 8 3 4 2 1 0 9)))
         )
    (should (equal '(6 8 4 2 0) res))
    ))

(ert-deftest test-ya-filter-2 ()
  (should (equal '(7 6 5 8 4 9)
                 (let ((gt3-fn (lambda (x) (and (> x 3) x))))
                   (ya:filter gt3-fn '(7 6 5 8 3 4 2 1 0 9))))
          ))

(ert-deftest test-ya-filter-3 ()
  (should (equal '(3 2 1 0)
                 (let ((gt3-fn (lambda (x) (and (not (> x 3)) x))))
                   (ya:filter gt3-fn '(7 6 5 8 3 4 2 1 0 9))))
          ))

;; ya:group
(ert-deftest test-ya-group-1 ()
  (should (equal '((a) (b) (c) (d) (e) (f) (g)) (ya:group '(a b c d e f g) 1)))
  )

(ert-deftest test-ya-group-2 ()
  (should (equal '((a b) (c d) (e f) (g))  (ya:group '(a b c d e f g) 2)))
  )

(ert-deftest test-ya-group-3 ()
  (should (equal '((a b c) (d e f) (g)) (ya:group '(a b c d e f g) 3)))
  )

(ert-deftest test-ya-group-error ()
  (should-error (ya:group '(a b c d e f g) 0))
  )


;; ya:flatten
(ert-deftest test-ya-flatten-1 ()
  (should (equal '(a b c d e f g) (ya:flatten '((a) (b) (c) (d) (e) (f) (g)))))
  )

(ert-deftest test-ya-flatten-2-id ()
  (should (equal '(a b c d e f g) (ya:flatten '(a b c d e f g))))
  )

(ert-deftest test-ya-flatten-3-id ()
  (should (equal '() (ya:flatten '())))
  )

(ert-deftest test-ya-flatten-4 ()
  (should (equal '() (ya:flatten '(() (()) () (() ()) ((() (())))) )))
  )

;; ya:prune
(ert-deftest test-ya-prune-1a ()
  (let* ((odd-fn (lambda (x) (= 1 (% x 2))))
         (res (ya:prune odd-fn '(1 2 (3 (4 5) 6) 7 8 (9)))))
    (should (equal '(() 8 (6 (4)) 2) res))
    ))

(ert-deftest test-ya-prune-1b ()
  (let* ((odd-fn (lambda (x) (= 1 (% x 2))))
         (res (ya:prune odd-fn '(1 2 (3 (4 5) 6) 7 8 (9 10)))))
    (should (equal '((10) 8 (6 (4)) 2) res))
    ))

(ert-deftest test-ya-prune-2a ()
  (let* ((even-fn (lambda (x) (= 0 (% x 2))))
         (res (ya:prune even-fn '(1 2 (3 (4 5) 6) 7 8 (10)))))
    (should (equal '(() 7 ((5) 3) 1) res))
    ))

(ert-deftest test-ya-prune-2b ()
  (let* ((even-fn (lambda (x) (= 0 (% x 2))))
         (res (ya:prune even-fn '(1 2 (3 (4 5) 6) 7 8 (9)))))
    (should (equal '((9) 7 ((5) 3) 1) res))
    ))

;; ya:find

(ert-deftest test-ya-find-1 ()
  (should (equal '(21 21)
                 (ya:find (lambda (x) (and (< x 100) x)) '(101 202 21 10 303 30 404 45 505))))
  )

(ert-deftest test-ya-find-2 ()
  (should (equal '()
                 (ya:find (lambda (x) (> x 1000)) '(101 202 21 10 303 30 404 45 505))))
  )

(ert-deftest test-ya-find-3 ()
  (should (equal '(6 12)
                 (ya:find (lambda (x) (and (< x 100) (/ x 2))) '(101 202 12 10 303 30 404 45 505))))
  )


;; ya:split

(ert-deftest test-ya-split-1 ()
  (should (equal '((1 2 3 4) (5 10 20 30))
                 (ya:split (lambda (x) (> x 4)) '(1 2 3 4 5 10 20 30))))
  )

(ert-deftest test-ya-split-2 ()
  (should (equal '(() (5 10 20 30))
                 (ya:split (lambda (x) (> x 4)) '(5 10 20 30))))
  )

(ert-deftest test-ya-split-3 ()
  (should (equal '(() ())
                 (ya:split (lambda (x) (> x 4)) '())))
  )

(ert-deftest test-ya-split-4 ()
  (should (equal '((101 202 303 404 505) ())
                 (ya:split (lambda (x) (< x 10)) '(101 202 303 404 505))))
  )
