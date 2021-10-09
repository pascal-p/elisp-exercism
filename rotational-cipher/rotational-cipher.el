;;; atbash-cipher.el --- Atbash-Cipher (exercism)

;;; Commentary:


;;; Code:
(setq LEN_ALPHA 26)

(defun rotate (plaintext rotkey)
  "Encode plaintext with rotational-cipher encoding, given rotkey"
  (cond
   ((= 0 (length plaintext)) plaintext)
   ((= 0 (% rotkey LEN_ALPHA)) plaintext)
   ((< rotkey 0) (throw 'Error "rotkey cannot be negative"))
   (t (encode-msg plaintext (% rotkey LEN_ALPHA))))
  )

(defun encode-msg (plaintext rotkey)
  (let ((encode-word-closure
         (lambda (word) (encode-word word rotkey))))
    (string-join
     (mapcar encode-word-closure (split-string plaintext " "))
     " ")
    ))

(defun encode-word (word rotkey)
  (let ((encode-char
         (lambda (ch) (map-char ch rotkey))))
    (string-join
     (mapcar encode-char (delete "" (split-string word "")))
     "")
    ))

(defun map-char (ch rotkey)
  "map the given character (actually a string) to its new value
returns a string"
  (cond
   ((and (or (string= ch "a") (string> ch "a"))
         (or (string= ch "z") (string< ch "z")))
    (char-to-string (+ ?a (% (+ (- (string-to-char ch) ?a) rotkey) LEN_ALPHA))))
   ((and (or (string= ch "A") (string> ch "A"))
         (or (string= ch "Z") (string< ch "Z")))
    (char-to-string (+ ?A (% (+ (- (string-to-char ch) ?A) rotkey) LEN_ALPHA))))
   ((string-match "[[:space:][:punct:][:digit:]]" ch) ch)
   (t (throw 'Error "unsupported character")))
  )

(defun string-join (lst sep)
  (mapconcat 'identity lst sep))

(provide 'rotational-cipher)
;;; rotational-cipher.el ends here
