;;; rotational-cipher.el --- Rotational-Cipher

;;; Commentary:


;;; Code:
(setq LEN_ALPHA 26)

(defmacro map-char-if (ch from to)
  `(and (or (string= ,ch ,from) (string> ,ch ,from))
        (or (string= ,ch ,to) (string< ,ch ,to))))

;; entry point
(defun rotate (plaintext rotkey)
  "Encode plaintext with rotational-cipher encoding, given rotkey"
  (cond
   ((= 0 (length plaintext)) plaintext)
   ((= 0 (% rotkey LEN_ALPHA)) plaintext)
   ((< rotkey 0) (throw 'Error "rotkey cannot be negative"))
   (t (encode-msg plaintext (% rotkey LEN_ALPHA))))
  )

(defun encode-msg (plaintext rotkey)
  ;; define a closure
  (let ((closure:encode-word
         (lambda (word) (encode-word word rotkey))))
    (string-join
     (mapcar closure:encode-word (split-string plaintext " "))
     " ")
    ))

(defun encode-word (word rotkey)
  (let ((encode-char
         (lambda (ch) (map-char-fn ch rotkey))))
    (string-join
     (mapcar encode-char (delete "" (split-string word "")))
     "")
    ))

(defun map-char-fn (ch rotkey)
  "map the given character (actually a string) to its new value
returns a string"
  (cond
   ((map-char-if ch "a" "z") (map-char ch rotkey ?a))
   ((map-char-if ch "A" "Z") (map-char ch rotkey ?A))
   ((string-match "[[:space:][:punct:][:digit:]]" ch) ch)
   (t (throw 'Error "unsupported character")))
  )

;; inlining
(defsubst map-char (ch rotkey sym)
  (char-to-string (+ sym (% (+ (- (string-to-char ch) sym) rotkey) LEN_ALPHA))))

;; inlining too
(defsubst string-join (lst sep)
  (mapconcat 'identity lst sep))

(provide 'rotational-cipher)
;;; rotational-cipher.el ends here
