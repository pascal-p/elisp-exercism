;;; crypto-square-test.el --- Tests for Crypto Square (exercism)

;;; Commentary:

;;; Code:

(load-file "crypto-square.el")

(ert-deftest empty-plaintext-results-in-an-empty-ciphertext ()
  (should (equal "" (encipher ""))))

(ert-deftest lowercase ()
  (should (equal "a" (encipher"A"))))

(ert-deftest remove-spaces ()
  (should (equal "b" (encipher "  b "))))

(ert-deftest remove-punctuation ()
  (should (equal "1" (encipher "@1,%!"))))

(ert-deftest 9-character-plaintext-results-in-3-chunks-of-3-characters ()
  (should (equal "tsf hiu isn" (encipher "This is fun!"))))

(ert-deftest 8-character-plaintext-results-in-3-chunks-the-last-one-with-a-trailing-space ()
  (should (equal "clu hlt io " (encipher "Chill out."))))

(ert-deftest 54-character-plaintext-results-in-7-chunks-the-last-two-with-trailing-spaces ()
  (should (equal "imtgdvs fearwer mayoogo anouuio ntnnlvt wttddes aohghn  sseoau "
                 (encipher "If man was meant to stay on the ground, god would have given us roots."))))

;; other  functions

(ert-deftest test-normalize-text-1 ()
  (should (equal "ifmanwasmeanttostayonthegroundgodwouldhavegivenusroots"
                 (normalize "If man was meant to stay on the ground, god would have given us roots."))))

(ert-deftest test-normalize-text-2 ()
  (should (equal "unix"
                 (normalize "Unix"))))

(ert-deftest test-normalize-text-3 ()
  (should (equal "" (normalize ""))))

(ert-deftest test-normalize-text-4 ()
  (should (equal "aaaaaa" (normalize " A   AA! AAA... "))))

(ert-deftest test-normalize-text-5 ()
  (should (equal "123soleil" (normalize "1-2-3 Soleil!"))))

;;
(ert-deftest test-find-cr-54 ()
  (should (equal '(8 . 7) (find-cr 54))))

(ert-deftest test-find-cr-64 ()
  (should (equal '(8 . 8) (find-cr 64))))

(ert-deftest test-find-cr-112 ()
  (should (equal '(11 . 11) (find-cr 112))))

(ert-deftest test-find-cr-8 ()
  (should (equal '(3 . 3) (find-cr 8))))
;;

(ert-deftest test-str-pad ()
  (should (equal "foo   " (str-pad "foo" 3))))

;;

(ert-deftest test-substr-n-final ()
  (should (equal "sroots  "
                 (substr-n "ifmanwasmeanttostayonthegroundgodwouldhavegivenusroots" 48 54 8))))

(ert-deftest test-substr-n-first ()
  (should (equal "ifmanwas"
                 (substr-n "ifmanwasmeanttostayonthegroundgodwouldhavegivenusroots" 0 8 8))))

(ert-deftest test-substr-n-second ()
  (should (equal "meanttos"
                 (substr-n "ifmanwasmeanttostayonthegroundgodwouldhavegivenusroots" 8 16 8))))


(provide 'crypto-square-test)
;;; crypto-square-test.el ends here
