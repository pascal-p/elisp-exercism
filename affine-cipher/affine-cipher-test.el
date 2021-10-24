;;; affine-cipher-test.el --- Tests for Affine Cipher

;;; Commentary:

;;; Code:
(load-file "affine-cipher.el")

;; Encode
(ert-deftest test-encode-yes ()
  (should (equal "xbt" (encode "yes" 5 7))))

(ert-deftest test-encode-no ()
  (should (equal "fu" (encode "no" 15 18))))

(ert-deftest test-encode-space+ ()
  (should (equal "" (encode "   " 5 3))))

(ert-deftest test-encode-Test ()
  (should (equal "ybty" (encode "Test" 5 7))))

(ert-deftest test-encode-OMG ()
  (should (equal "lvz" (encode "OMG" 21 3))))

(ert-deftest test-encode-O_M_G ()
  (should (equal "hjp" (encode "O M G" 25 47))))

(ert-deftest test-encode-mindblowingly ()
  (should (equal "rzcwa gnxzc dgt" (encode "mindblowingly" 11 15))))

(ert-deftest test-encode-Testing+ ()
  (should (equal "jqgjc rw123 jqgjc rw" (encode "Testing,1 2 3, testing." 3 4))))

(ert-deftest test-encode-Truth_is_Fiction ()
  (should (equal "iynia fdqfb ifje" (encode  "Truth is Fiction." 5 17))))

(ert-deftest test-encode-TruthisFiction ()
  (should (equal "iynia fdqfb ifje" (encode  "TruthisFiction." 5 17))))

(ert-deftest test-encode-FreeWillisaDelusionFaceIt ()
  (should (equal "agttp vqqvn rmtqb nvlea rftvu" (encode  "Free Will is a delusion. Face it" 7 17))))

(ert-deftest test-encode-pangram ()
  (should (equal "swxtj npvyk lruol iejdc blaxk swxmh qzglf" (encode  "The quick brown fox jumps over the lazy dog." 17 33))))

(ert-deftest test-encode-rqndom ()
  (should (equal "anobs tacle isoft enast eppin gston e" (encode "tgxknetbyjznxaejgtnejoozgrnexgj" 23 31))))

(setq LongStr "
Chor. Two households, both alike in dignity,
In fair Verona, where we lay our scene,
From ancient grudge break to new mutiny,
Where civil blood makes civil hands unclean.
From forth the fatal loins of these two foes
A pair of star-cross'd lovers take their life;
Whose misadventur'd piteous overthrows
Doth with their death bury their parents' strife,
The fearful passage of their death-mark'd love,
And the continuance of their parents' rage,
Which, but their children's end, naught could remove,
Is now the two hours' traffic of our stage;
The which if you with patient ears attend,
What here shall miss, our toil shall strive to mend.
")

(ert-deftest test-encode-long-sentence ()
  (should (equal "bixwe dxixv njixy snkxe ityzh jzgsz rgzel zgatz wmjwx gtdij wjdjy tlxvw nbjgj awxpt gbzjg erwvs rjkwj thexg jdpve zgldi jwjbz mzyky xxspt hjnbz mzyit gsnvg byjtg awxpa xweie ijate tyyxz gnxae ijnje dxaxj ntotz wxane twbwx nnsyx mjwne thjei jzwyz ajdix njpzn tsmjg evwso zejxv nxmjw eiwxd nsxei dzeie ijzws jteik vwlei jzwot wjgen newza jeija jtwav yotnn trjxa eijzw sjtei ptwhs yxmjt gseij bxgez gvtgb jxaei jzwot wjgen wtrjd izbik veeij zwbiz yswjg njgsg tvrie bxvys wjpxm jzngx deije dxixv wnewt aazbx axvwn etrje ijdiz bizal xvdze iotez jgejt wntee jgsdi teijw jnity ypznn xvwex zynit yynew zmjex pjgs" (encode LongStr 17 19))))

;; Exception encode
;; (ert-deftest test-encode-with-exception ()
;;   (should-error (encode "" alpha beta)))


;; Decode
;; (ert-deftest test-decode-empty-msg ()
;;   (should (equal "" (decode "" alpha beta))))


;; Identity (decode o encode) == Id
;;
;; define ad-hoc all? hof
(defun all? (pred-fn lst)
  "Return true iff all elements of lst verified the predicate function pred-fn
and false otherwise
"
  (eval (cons 'and (mapcar pred-fn lst)))
  )

;; (ert-deftest test-decode-encode-identity ()
;;   (should
;;    (all? (lambda (pair) (equal (car pair)
;;                                (encode (cdr pair) (decode (cdr pair) (car pair)))))
;;          '(("XOXOXOXOXOXOXOXOXO" . 2)
;;            ("WEAREDISCOVEREDFLEEATONCE" . 3)
;;            ("THEDEVILISINTHEDETAILS" . 3)
;;            ("THEDEVILISINTHEDETAILS". 5)
;;            ("133714114238148966225439541018335470986172518171757571896261" . 6)))
;;    )
;;   )

;; Other tests
(ert-deftest test-gcd-36-48 ()
  (should (= 12 (gcd 36 48))))

(ert-deftest test-gcd-48-36 ()
  (should (equal 12 (gcd 48 36))))

(ert-deftest test-gcd-37-36 ()
  (should (equal 1 (gcd 37 36))))

(ert-deftest test-gcd-2096-3182 ()
  (should (equal 2 (gcd 2096 3182))))

(ert-deftest test-gcd-3116-2052 ()
  (should (equal 76 (gcd 3116 2052))))

;;

(ert-deftest test-grouping ()
  (should (equal'("FOOBA" "RBA") (grouping '("F" "O" "O" "B" "A" "R" "B" "A")))))

(ert-deftest test-grouping-foo ()
  (should (equal'("FOO") (grouping '("F" "O" "O")))))


(provide 'affine-cipher-test)
;; affinel-cipher-test.el ends here