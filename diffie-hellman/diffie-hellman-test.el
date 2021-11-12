;;; diffie-hellman-test.el --- Tests for Diffie-Hellman Cipher

;;; Commentary:

;;; Code:
(load-file "diffie-hellman.el")

(ert-deftest test-can-calculate-pubkey-using-privkey ()
  (let ((p 23)
        (g 5)
        (privkey 6)
        (exp 8))
    (should (equal exp (public-key p g privkey)))
    ))

(ert-deftest test-can-calculate-secret-using-other-party-pubkey ()
  (let ((p 23)
        (their-pub-key 19)
        (our-priv-key 6)
        (exp 2))
    (should (equal exp (secret p their-pub-key our-priv-key)))
    )
  )

(ert-deftest test-can-calculate-pubkey-using-privkey-2 ()
  (let ((p 2951) ;; was  29501
        (g 8713)
        (priv-key 2931) ;; 29331
        (exp 2757)) ;; 15518
    (should (= exp (public-key p g priv-key)))
    ))

(ert-deftest test-can-calculate-secret-using-other-party-pubkey-2()
(let ((p 3967)             ;; was 29501
      (their-pub-key 3017) ;; was 8017
      (our-priv-key 3849)  ;; was 8849
      (exp 1015)) ;; was 184
  (should (= exp (secret p their-pub-key our-priv-key)))
  ))

(ert-deftest test-key-exchange ()
  (let* ((pairs '((23 . 5) (43 . 47) (1483 . 1553)))
         (p (car (car pairs)))
         (g (cdr (car pairs)))
         (res t))
    (while (and res (> (length pairs) 1))
      (setq pair-sec-a-sec-b (key-exchange-fn p g)
            sec-a (car pair-sec-a-sec-b)
            sec-b (cdr pair-sec-a-sec-b)
            res (equal sec-a sec-b)
            pairs (cdr pairs)
            p (car (car pairs))
            g (cdr (car pairs)))
      )
    ;; last one
    (let* ((p (car (car pairs)))
           (g (cdr (car pairs)))
           (pair-sec-a-sec-b (key-exchange-fn p g))
           (sec-a (car pair-sec-a-sec-b))
           (sec-b (cdr pair-sec-a-sec-b)))
      (should (and res (equal sec-a sec-b)))
      )
    )
  )

(ert-deftest test-private-key-in-range-1-p ()
  (let* ((primes '(100000937 100000939 100000963 100000969 100001029 100001053 100001059 100001081 100001087 100001107
                             100001119 100001131 100001147 100001159 100001177 100001183 100001203 100001207 100001219 100001227))
         (p (car primes))
         (priv-key (private-key p)))
    (while (and (> (length primes) 1) (> priv-key 1) (< priv-key p))
      (setq primes (cdr primes)
            p (car primes)
            priv-key (private-key p))
      )
    ;; check that last priv key satisfies the constraiants
    (let* ((last-p (car primes))
           (last-priv-key (private-key last-p)))
      (should (and (> priv-key 1) (< priv-key p)))
      )
    )
  )

;; Exception


;; check-factor
(ert-deftest test-check-factor-3 ()
  (should (check-factor 3)))

(ert-deftest test-check-factor-101 ()
  (should (check-factor 101)))

(ert-deftest test-check-factor-exception-4096 ()
  (should-error (check-factor 4096))) ;; 4096 is not prime

(ert-deftest test-check-factor-exception-99 ()
  (should-error (check-factor 99))) ;; 99 is not prime

(ert-deftest test-check-factor-exception-m101 ()
  (should-error (check-factor -101))) ;; less than 2

;; is_prime?
(ert-deftest test-is-prime-101 ()
  (should (is-prime? 101)))

(ert-deftest test-is-prime ()
  (let* ((prime-list '(2 3 5 7 11 13 17 19 23 29 31 37 41 43 47 53 59 61 67 71 73 79 83 89 97
                         101 103 107 109 113 127 131 137 139 149 151 157 163 167173 179 181 191 193 197
                         199 211 223 227 229 233 239 241 251 257 263 269 271 277 281 283 293 307 311 313
                         317 331 337 347 349 353 359 367 373 379 383 389 397 401 409 419 421 431 433 439
                         443 449 457 461 463 467 479 487 491 499 503 509 521 523 541 547 557 563 569 571
                         577 587 593 599 601 607 613 617 619 631 641 643 647 653 659 661 673 677 683 691
                         701 709 719 727 733 739 743 751 757 761 769 773 787 797 809 811 821 823 827 829
                         839 853 857 859 863 877 881 883 887 907 911 919 929 937 941 947 953 967 971 977
                         983 991 997 1009 1013 1019 1021 1031 1033 1039 1049 1051 1061 1063 1069 1087 1091 1093
                         1097 1103 1109 1117 1123 1129 1151 1153 1163 1171 1181 1187 1193 1201 1213 1217 1223
                         1229 1231 1237 1249 1259 1277 1279 1283 1289 1291 1297 1301 1303 1307 1319 1321 1327
                         1361 1367 1373 1381 1399 1409 1423 1427 1429 1433 1439 1447 1451 1453 1459 1471 1481
                         1483 1487 1489 1493 1499 1511 1523 1531 1543 1549 1553 1559 1567 1571 1579 1583 1597
                         1601 1607 16091613 1619 1621 1627 1637 1657 1663 1667 1669 1693 1697 1699 1709 1721
                         1723 1733 1741 1747 1753 1759 1777 1783 1787 1789 1801 1811 1823 1831 1847 1861 1867
                         1871 1873 1877 1879 1889 1901 1907 1913 1931 1933 1949 1951 1973 1979 1987 1993 1997
                         1999 2003 2011 2017 2027 2029 2039 2053 2063 2069 2081 2083 2087 2089 2099 2111 2113
                         2129 2131 2137 2141 2143 2153 2161 2179 2203 2207 2213 2221 2237 2239 2243 2251 2267 2269
                         2273 2281 2287 2293 2297 2309 2311 2333 2339 2341 2347 2351 2357 2371 2377 2381 2383 2389
                         2393 2399 2411 2417 2423 2437 2441 2447 2459 2467 2473 2477 2503 2521 2531 2539 2543 2549
                         2551 2557 2579 2591 2593 2609 2617 2621 2633 2647 2657 2659 2663 2671 2677 2683 2687 2689
                         2693 2699 2707 2711 2713 2719 2729 2731 2741 2749 2753 2767 2777 2789 2791 2797 2801 2803
                         2819 2833 2837 2843 2851 2857 2861 2879 2887 2897 2903 2909 2917 2927 2939 2953 2957 2963
                         2969 2971 2999 3001 3011 3019 3023 3037 3041 3049 3061 3067 3079 3083 3089 3109 3119 3121
                         3137 3163 3167 3169 3181 3187 3191 3203 3209 3217 3221 3229 3251 3253 3257 3259 3271 3299
                         3301 3307 3313 3319 3323 3329 3331 3343 3347 3359 3361 3371 3373 3389 3391 3407 3413 3433
                         3449 3457 3461 3463 3467 3469 3491 3499 3511 3517 3527 3529 3533 3539 3541 3547 3557 3559
                         3571 3581 3583 3593 3607 3613 3617 3623 3631 3637 3643 3659 3671 3673 3677 3691 3697 3701
                         3709 3719 3727 3733 3739 3761 3767 3769 3779 3793 3797 3803 3821 3823 3833 3847 3851 3853
                         3863 3877 3881 3889 3907 3911 3917 3919 3923 3929 3931 3943 3947 3967 3989 4001 4003 4007
                         4013 4019 4021 4027 4049 4051 4057 4073 4079 4091 4093 4099 4111 4127 4129 4133 4139 4153
                         4157 4159 4177 4201 4211 4217 4219 4229 4231 4241 4243 4253 4259 4261 42714273 4283 4289
                         4297 4327 4337 4339 4349 4357 4363 4373 4391 4397 4409 4421 4423 4441 4447 4451 4457 4463
                         4481 4483 4493 4507 4513 4517 4519 4523 4547 4549 4561 4567 4583 4591 4597 4603 4621 4637
                         4639 4643 4649 4651 4657 4663 4673 4679 4691 4703 4721 4723 4729 4733 4751 4759 4783 4787
                         4789 4793 4799 4801 4813 4817 4831 4861 4871 4877 4889 4903 4909 4919 4931 4933 4937 4943
                         4951 4957 4967 4969 4973 4987 4993 4999))
        (prime t)
        (p (car prime-list)))
    (while (and prime (> 0 (length prime-list)))
      (if (not (is-prime? p)) (setq prime nil)
        (setq prime-list (cdr prime-list)
              p (car prime-list))
        ))
    (should prime))
  )

(ert-deftest test-is-not-prime-1000001 ()
  (should (not (is-prime? 1000001))))

(ert-deftest test-is-not-prime-1073741824 ()
  (should (not (is-prime? 1073741824))))

;; helpers

(defun key-exchange-fn (p g)
  (let* ((alice-privkey (private-key p))
         (bob-privkey (private-key p))
         (alice-pubkey (public-key p g alice-privkey))
         (bob-pubkey (public-key p g bob-privkey))
         (secret-a (secret p bob-pubkey alice-privkey))
         (secret-b (secret p alice-pubkey bob-privkey)))
    (cons secret-a secret-b)
    )
  )

(provide 'diffie-hellman-test)
;; diffie-hellman-test.el ends here
