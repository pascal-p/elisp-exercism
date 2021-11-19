;;; rotational-cipher-test.el --- Tests for Rotational Cipher

;;; Commentary:

;;; Code:

(require 'cl-lib)

(load-file "rotational-cipher.el")

(ert-deftest rotate-1 ()
  (should (equal "b" (rotate "a" 1))))

(ert-deftest rotate-13 ()
  (should (equal "z" (rotate "m" 13))))

(ert-deftest rotate-13-wrap-around ()
  (should (equal "a" (rotate "n" 13))))

(ert-deftest rotate-26-full-rotation-a ()
  (should (equal "a" (rotate "a" 26))))

(ert-deftest rotate-26-full-rotation-z ()
  (should (equal "z" (rotate "z" 26))))

(ert-deftest rotate-26-full-rotation-Z ()
  (should (equal "Z" (rotate "Z" 26))))

(ert-deftest rotate-5-OMG ()
  (should (equal "TRL" (rotate "OMG" 5))))

(ert-deftest rotate-5-o_m_g ()
  (should (equal "t r l" (rotate "o m g" 5))))

(ert-deftest rotate-27-eq-rotate-1 ()
  (should (equal (rotate "Let's talk, GrandPa!" 1)
                 (rotate "Let's talk, GrandPa!" 27))))

(ert-deftest rotate-13-pangram ()
  (should (equal "Gur dhvpx oebja sbk whzcf bire gur ynml qbt."
                (rotate "The quick brown fox jumps over the lazy dog." 13))))

(ert-deftest rotate-21 ()
  (should (equal "Gzo'n zvo, Bmviyhv!"
                 (rotate "Let's eat, Grandma!" 21))))

(ert-deftest rotate-2 ()
  (should (equal "K'o uq Ogvc, Gxgp Vjku Cetqpao"
                 (rotate "I'm so Meta, Even This Acronym" 2))))

(ert-deftest rotate-17 ()
  (should (equal "Z'd jf Dvkr, Vmve Kyzj Rtifepd"
                 (rotate "I'm so Meta, Even This Acronym" 17))))


;; other tests

(ert-deftest map-char-fn-rot13-n-a ()
  (should (equal "a" (map-char-fn "n" 13))))

(ert-deftest map-char-fn-rot13-o-b ()
  (should (equal "b" (map-char-fn "o" 13))))

(ert-deftest map-char-fn-rot13-m-z ()
  (should (equal "z" (map-char-fn "m" 13))))

(provide 'rotational-cipher-test)
;;; rotational-cipher-test.el ends here
