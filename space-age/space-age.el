;;; space-age.el --- Space-Age

;;; Commentary:

;;; Code:
(setq EARTH_YEAR 365.25
      DAY_SEC 86400
      YEAR_DUR (list (cons "Earth" EARTH_YEAR)
                     (cons "Mercury" (* 0.2408467 EARTH_YEAR))
                     (cons "Venus" (* 0.61519726 EARTH_YEAR))
                     (cons "Mars" (* 1.8808158 EARTH_YEAR))
                     (cons "Jupiter" (* 11.862615 EARTH_YEAR))
                     (cons "Saturn" (* 29.447498 EARTH_YEAR))
                     (cons "Uranus"  (* 84.016846 EARTH_YEAR))
                     (cons "Neptune" (* 164.79132 EARTH_YEAR))))
(setq lst YEAR_DUR)

;; generate the functions on the fly
(dolist (pair lst)
  (defalias (intern (concat "space-age-on-" (downcase (car pair))))
    `(lambda (sec)
       (let ((d (/ sec DAY_SEC)))
         ;; 2 digits after decimal
         (/ (fround (* 100 (/ d ,(cdr (assoc (car pair) YEAR_DUR))))) 100)
         )
       )
    ))

;; ref. https://stackoverflow.com/questions/12714039/emacs-lisp-declare-functions-with-a-variable-varying-name

(provide 'space-age)
;;; space-age.el ends here
