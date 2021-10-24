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

(ert-deftest test-encode-random ()
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
(ert-deftest test-encode-with-exception-1 ()
  (should-error (encode "This is a test" 6 17))) ;; alpha and M no co-prime

(ert-deftest test-encode-with-exception-2 ()
  (should-error (encode "This is another test" 13 21))) ;; alpha and M no co-prime


;; Decode
(ert-deftest test-decode-ybty ()
  (should (equal "test" (decode "ybty" 5 7))))

(ert-deftest test-decode-obstacle ()
  (should (equal "anobstacleisoftenasteppingstone" (decode "qdwju nqcro muwhn odqun oppmd aunwd o" 19 16))))

(ert-deftest test-decode-testing+ ()
  (should (equal "testing123testing" (decode "odpoz ub123 odpoz ub" 25 7))))

(ert-deftest test-decode-pangram-17-33()
  (should (equal "thequickbrownfoxjumpsoverthelazydog" (decode "swxtj npvyk lruol iejdc blaxk swxmh qzglf" 17 33))))

(ert-deftest test-decode-testing-pangram-19-13 ()
  (should (equal "thequickbrownfoxjumpsoverthelazydog" (decode "kqlfd jzvgy tpaet icdhm rtwly kqlon ubstx" 19 13))))

(ert-deftest test-decode-jolly+ ()
  (should (equal "jollygreengiant" (decode "vszzm    cly   yd cg    qdp" 15 16))))

(ert-deftest test-decode-testing-obstacle-2 ()
  (should (equal "tgxknetbyjznxaejgtnejoozgrnexgj" (decode "AnObstacleIsOftenASteppingStone" 23 31))))

;; add more

;; Exception decode
(ert-deftest test-decode-with-exception-1 ()
  (should-error (decode "This is a test" 13 5))) ;; alpha and M no co-prime

(ert-deftest test-decode-with-exception-2 ()
  (should-error (decode "This is another test" 18 13))) ;; alpha and M no co-prime


;; Identity (decode o encode) == Id
;;
;; define ad-hoc all? hof
(defun all? (pred-fn lst)
  "Return true iff all elements of lst verified the predicate function pred-fn
and false otherwise
"
  (let* ((res (mapcar pred-fn lst))
         (n (length res)))
    (progn
      (cond
        ((= n 0) t)
        ((= n 1) (setq res (cons t res)))
        (t t))
      (eval (cons 'and res))
      )
    ))

(ert-deftest test-decode-encode-identity ()
  (let ((alpha 21)
        (beta 3))
    (should
     (all? (lambda (txt) (equal (downcase (mapconcat 'identity (filter->list txt) ""))
                                (decode (encode txt alpha beta) alpha beta)))
           '(
             "yes"
             "omg"
             "OMG"
             "mindblowingly"
             "I M A G I N E!"
             "Truth is fiction."
             "The quick brown fox jumps over the lazy dog."
             "Testing,1 2 3, testing."
             "zmlyhgzxovrhlugvmzhgvkkrmthglmv"
             "anobstacleisoftenasteppingstone"
             "An obstacle is often a stepping stone"
             ))
     )))

;; and the other way around!

(ert-deftest test-encode-decode-identity ()
  (let ((alpha 21)
        (beta 3))
    (should
     (all? (lambda (txt) (equal txt
                                (encode (decode txt alpha beta) alpha beta)))
           '(
             "njr"
             "lvz"
             "lvz"
             "vpqoy alxpq zan"
             "pvdzp qj"
             "mwhmu prept mplq"
             "mujbh ptfyw lxqel skhvg rlcjw mujad inolz"
             "mjrmp qz123 mjrmp qz"
             "ivanu zislc wuahz cviuz cffwv muzav c"
             "dqlyr mdtaj prlem jqdrm jggpq zrmlq j"
             "dqlyr mdtaj prlem jqdrm jggpq zrmlq j"
             ))
     )))

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
