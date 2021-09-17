;;; leap.el --- Leap exercise (exercism)

;;; Commentary:

;;; Code:
(defun leap-year-p (year)
  "Predicate which determines if a given year is a leap year: on very
   year that is evenly divisible by 4 except every year that is evenly
   divisible by 100 unless the year is also evenly divisible by 400"
  (or (and (= (% year 4) 0) (not (= (% year 100) 0))) (= (% year 400) 0))
  )

(defun leap-year-pp (year)
  (cond
   ((or (and (= (% year 4) 0) (not (= (% year 100) 0)))
        (= (% year 400) 0)) t)
   (t nil))
)

(provide 'leap-year-p)
;;; leap.el ends here
