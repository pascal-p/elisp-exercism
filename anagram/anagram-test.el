;;; anagram-test.el --- Tests for Anagram (exercism)

;;; Commentary:

;;; Code:

(load-file "anagram.el")

;; test helpers

(ert-deftest test-ya-empty-1 ()
  (should (ya-empty? '())))

(ert-deftest test-ya-empty-1 ()
  (should (not (ya-empty? '(1 2 3)))))

(ert-deftest gen-key-given-word-1 ()
  (should (equal "ant" (gen-key "tan"))))

(ert-deftest gen-empty-key-given-empty-word ()
  (should (equal "" (gen-key ""))))

(ert-deftest gen-empty-key-case-insensitive ()
  (should (equal "abr" (gen-key "Bar"))))

(ert-deftest p-cand-3-letter-words ()
  (should (equal '("ant" "bar" "foo")
                 (p-cand 3 '("babar" "ant" "bar" "titi" "foo")))))

(ert-deftest p-cand-no-4-letter-words ()
  (should (equal '()
                 (p-cand 4 '("babar" "ant" "bar" "brain" "neuron")))))

(ert-deftest find-ana-aux-given-args ()
  (should (equal '("tan")
                 (find-ana-aux "ant"
                               "ant"
                               '(("abr" . "bar") ("ant" . "tan") ("foo" . "foo"))))))

(ert-deftest find-ana-given-args ()
  (should (equal '("tan")
                 (find-ana "ant" '("bar" "tan" "foo" "off")))))

(ert-deftest test-ya-reverse-list ()
  (should (equal '(1 2 3) (ya-reverse '(3 2 1)))))

(ert-deftest test-ya-reverse-list ()
   (should (equal '() (ya-reverse '()))))

(ert-deftest test-ya-reverse-singletonlist ()
   (should (equal '(1) (ya-reverse '(1)))))


;; test main function
(ert-deftest no-matches ()
  (should (equal '() (anagrams-for
                      "diaper"
                      '("hello" "world" "zombies" "pants")))))

(ert-deftest detect-simple-anagram ()
  (should (equal '("tan") (anagrams-for
                           "ant"
                           '("tan" "stand" "at")))))

(ert-deftest does-not-confuse-different-duplicates ()
  (should (equal '() (anagrams-for
                      "galea"
                      '("eagle")))))

(ert-deftest eliminate-anagram-subsets ()
  (should (equal '() (anagrams-for
                      "good"
                      '("dog" "goody")))))

(ert-deftest detect-anagram ()
  (should (equal '("inlets") (anagrams-for
                              "listen"
                              '("enlists" "google" "inlets" "banana")))))

(ert-deftest multiple-anagrams ()
  (should (equal '("gallery" "regally" "largely")
                 (anagrams-for
                  "allergy"
                  '("gallery" "ballerina" "regally" "clergy" "largely" "leading")))))

(ert-deftest case-insensitive-anagrams ()
    (should (equal '("Carthorse")
                   (anagrams-for
                    "Orchestra"
                    '("cashregister" "Carthorse" "radishes")))))

(ert-deftest word-is-not-own-anagram ()
  (should (equal '()
                 (anagrams-for
                  "banana"
                  '("banana")))))


(provide 'anagram-test)
;;; anagram-test.el ends here
