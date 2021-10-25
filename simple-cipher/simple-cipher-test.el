;;; simple-cipher-test.el --- Tests for Simple Cipher

;;; Commentary:

;;; Code:
(load-file "simple-cipher.el")

;; Encode
(ert-deftest test-encode-panda-id ()
  (should (equal '("iamapandabear" . "aaaa")  (encode "I am a panda bear!" "aaaa"))))

(ert-deftest test-encode-panda-a ()
  (should (equal '("iboaqcnecbfcr" . "abc")  (encode "I am a panda bear" "abc"))))

(ert-deftest test-encode-panda-b ()
  (should (equal '("ldpdsdqgdehdu" . "ddddddd")  (encode "I am a panda bear" "ddddddd"))))

(ert-deftest test-encode-free-will ()
  (should (equal '("irfgzimnlsoqwhjpjbvvdnjnoutkrn" . "dabc")
                 (encode "Free Will is nothing but an Illusion" "dabc"))))

(ert-deftest test-encode-free-will-longkey ()
  (should (equal '("ljsvxgpsvqdflarwftusmtjzrqbmvh" .
                   "gsorbyehnyqrstjjzsazmgbogwjehuaqyhsogutoasnxvzwdxvnqzyvetgndkuukapncsusudmepuvgrmhtryekvgqlcmnbgrzth")
                 (encode "Free Will is nothing but an Illusion"
                         "gsorbyehnyqrstjjzsazmgbogwjehuaqyhsogutoasnxvzwdxvnqzyvetgndkuukapncsusudmepuvgrmhtryekvgqlcmnbgrzth"))))

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

;; (ert-deftest test-encode-long-sentence ()
;;   (should (equal "bixwe dxixv njixy snkxe ityzh jzgsz rgzel zgatz wmjwx gtdij wjdjy tlxvw nbjgj awxpt gbzjg erwvs rjkwj thexg jdpve zgldi jwjbz mzyky xxspt hjnbz mzyit gsnvg byjtg awxpa xweie ijate tyyxz gnxae ijnje dxaxj ntotz wxane twbwx nnsyx mjwne thjei jzwyz ajdix njpzn tsmjg evwso zejxv nxmjw eiwxd nsxei dzeie ijzws jteik vwlei jzwot wjgen newza jeija jtwav yotnn trjxa eijzw sjtei ptwhs yxmjt gseij bxgez gvtgb jxaei jzwot wjgen wtrjd izbik veeij zwbiz yswjg njgsg tvrie bxvys wjpxm jzngx deije dxixv wnewt aazbx axvwn etrje ijdiz bizal xvdze iotez jgejt wntee jgsdi teijw jnity ypznn xvwex zynit yynew zmjex pjgs" (encode LongStr 17 19))))

;; Exception encode
;; (ert-deftest test-encode-with-exception-1 ()
;;   (should-error (encode "This is a test" 6 17))) ;; alpha and M no co-prime

;; (ert-deftest test-encode-with-exception-2 ()
;;   (should-error (encode "This is another test" 13 21))) ;; alpha and M no co-prime


;; Decode
;; (ert-deftest test-decode-ybty ()
;;   (should (equal "test" (decode "ybty" 5 7))))

;; (ert-deftest test-decode-obstacle ()
;;   (should (equal "anobstacleisoftenasteppingstone" (decode "qdwju nqcro muwhn odqun oppmd aunwd o" 19 16))))


;; Exception decode
;; (ert-deftest test-decode-with-exception-1 ()
;;   (should-error (decode "This is a test" 13 5))) ;; alpha and M no co-prime

;; (ert-deftest test-decode-with-exception-2 ()
;;   (should-error (decode "This is another test" 18 13))) ;; alpha and M no co-prime


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

;; (ert-deftest test-decode-encode-identity ()
;;   (let ((alpha 21)
;;         (beta 3))
;;     (should
;;      (all? (lambda (txt) (equal (downcase (mapconcat 'identity (filter->list txt) ""))
;;                                 (decode (encode txt alpha beta) alpha beta)))
;;            '(
;;              "yes"
;;              "omg"
;;              "OMG"
;;              "mindblowingly"
;;              "I M A G I N E!"
;;              "Truth is fiction."
;;              "The quick brown fox jumps over the lazy dog."
;;              "Testing,1 2 3, testing."
;;              "zmlyhgzxovrhlugvmzhgvkkrmthglmv"
;;              "anobstacleisoftenasteppingstone"
;;              "An obstacle is often a stepping stone"
;;              ))
;;      )))

;; and the other way around!

;; (ert-deftest test-encode-decode-identity ()
;;   (let ((alpha 21)
;;         (beta 3))
;;     (should
;;      (all? (lambda (txt) (equal txt
;;                                 (encode (decode txt alpha beta) alpha beta)))
;;            '(
;;              "njr"
;;              "lvz"
;;              "lvz"
;;              "vpqoy alxpq zan"
;;              "pvdzp qj"
;;              "mwhmu prept mplq"
;;              "mujbh ptfyw lxqel skhvg rlcjw mujad inolz"
;;              "mjrmp qz123 mjrmp qz"
;;              "ivanu zislc wuahz cviuz cffwv muzav c"
;;              "dqlyr mdtaj prlem jqdrm jggpq zrmlq j"
;;              "dqlyr mdtaj prlem jqdrm jggpq zrmlq j"
;;              ))
;;      )))

;; Other tests

(provide 'simple-cipher-test)
;; simplel-cipher-test.el ends here
