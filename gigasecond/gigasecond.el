;;; gigasecond.el --- Gigasecond exercise (exercism)

;;; Commentary:
;; Calculate the date one gigasecond (10^9 seconds) from the
;; given date.
;;
;; NB: Pay attention to  Emacs' handling of time zones and dst
;; in the encode-time and decode-time functions.

;;; Code:

;; ref. exercism download --exercise=gigasecond --track=emacs-lisp
;;
;; Function: decode-time &optional time zone form
;;
;; This function converts a time value into calendrical information. If you don’t specify time, it decodes the current time, and similarly zone defaults to the current time zone rule.
;; See Time Zone Rules. The operating system limits the range of time and zone values.
;;
;; The form argument controls the form of the returned seconds element, as described below.
;; The return value is a list of nine elements, as follows:
;;  (seconds minutes hour day month year dow dst utcoff)
;; seconds  The number of seconds past the minute, with form described below.
;; minutes  The number of minutes past the hour, as an integer between 0 and 59.
;; hour     The hour of the day, as an integer between 0 and 23.
;; day      The day of the month, as an integer between 1 and 31.
;; month    The month of the year, as an integer between 1 and 12.
;; year     The year, an integer typically greater than 1900.
;; dow      The day of week, as an integer between 0 and 6, where 0 stands for Sunday.
;; dst      t if daylight saving time is effect, nil if it is not in effect, and -1 if this information is not available.
;; utcoff   An integer indicating the Universal Time offset in seconds, i.e., the number of seconds east of Greenwich.

(setq GIGA_SEC 1000000000) ;; expressed in s

(setq LOCAL-TX
      (current-time-zone)) ;; (46800 "NZDT")

;; UTC
(defun from (ss mm hh d m yyyy)
  "
ss   seconds
mm   minutes
hh   hours
d    day
m    month
yyyy year
"
  (progn
    (to-utc)
    (let* ((ts (encode-time ss mm hh d m yyyy 0 t 0))
           (ts-giga (+ (time-convert ts 'integer) GIGA_SEC))
           (res (decode-time ts-giga))
           (s- (car res))
           (m- (cadr res))
           (h- (caddr res))
           (dd- (car (cdddr res)))
           (mm- (car (cddr (cddr res))))
           (yy- (cadr (cddr (cddr res))))
           )
      (progn
        ;; (to-localtime)
        (list s- m- h- dd- mm- yy-)
        ))))

(defun to-utc ()
  (setenv "TZ" "UTC0"))

(defun to-localtime ()
  LOCAL-TX)

(provide 'gigasecond)
;;; gigasecond.el ends here
