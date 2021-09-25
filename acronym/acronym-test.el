;;; acronym-test.el --- Tests for Acronym (exercism)

;;; Commentary:

;;; Code

(load-file "acronym.el")

(ert-deftest basic ()
  (should (equal "PNG" (acronym "Portable Network Graphics"))))

(ert-deftest basic-lgtm ()
  (should (equal "LGTM" (acronym "Looks good to me!"))))

(ert-deftest basic-lgtm ()
  (should (equal "LGTM" (acronym "Looks good to me!"))))

(ert-deftest basic-julia ()
  ;; totally made up
  (should (equal "JULIA" (acronym "Julia universal language intelligent autonomous"))))

(ert-deftest basic-wtf ()
  ;; made up
  (should (equal "WTF" (acronym "What the fun"))))

(ert-deftest basic-unix ()
  ;; made up
  (should (equal "UNIX" (acronym "Unix not intentionally xenomorphic"))))

(ert-deftest lowercase-words ()
  (should (equal "ROR" (acronym "Ruby on Rails"))))

(ert-deftest punctuation ()
  (should (equal "FIFO" (acronym "First In, First Out"))))

(ert-deftest punctuation2 ()
  (should (equal "LIFO" (acronym "Last In, First Out"))))

(ert-deftest all-caps-words ()
  (should (equal "PHP" (acronym "PHP: Hypertext Preprocessor"))))

(ert-deftest non-acronym-all-caps-word ()
  (should (equal "GIMP" (acronym "GNU Image Manipulation Program"))))

(ert-deftest hyphenated ()
  (should (equal "CMOS" (acronym "Complementary metal-oxide semiconductor"))))



(provide 'acronym-test)
;;; acronym-test.el ends here
