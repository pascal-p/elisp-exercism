;;; hof.el --- higher order function

;;; Commentary:

;;; Code:
(require 'cl-lib)

(defun ya:odd-pred (x) (= 1 (% x 2)))

(defmacro ya:remove-if-not (pred-fn &rest args)
  "build the complementary function of remove-if"
  `(cl-remove-if (lambda (x) (not (funcall ,pred-fn x))) ,@args))

(defmacro ya:complement (fn)
  "defines the complement of a given function"
  `(lambda (&rest args) (not (apply ,fn args))))

(defalias 'ya:~ 'ya:complement)

;; now we can rewrite remove-if-not in term of complement
(defmacro ya:remove-if-not-b (pred-fn &rest args)
  `(cl-remove-if (ya:~ ,pred-fn) ,@args)
  )

;; build an operator to return destrcutive counter-part of a non destructive function
;; non destructive | destructive
;;  cl-remove-if   | cl-delete-if
;;  reverse        | nreverse
;;  append         | nconc

(defvar *!store-fns* (make-hash-table))

(defmacro ! (fn)
  "retrieve destructive alias from *!store-fns*"
  `(or (gethash ,fn *!store-fns*) ,fn))

(defmacro def! (fn fn!)
  `(setf (gethash ,fn *!store-fns*) ,fn!))

(def! 'cl-remove-if 'cl-delete-if)  ;; now we can use (! cl-remove-if) (as destructive equivalnet of cl-remove-if instead of ...)
(def! 'reverse 'nreverse)
(def! 'append 'nconc)

(defalias 'ya:remove-if! (! 'cl-remove-if))
(defalias 'ya:reverse! (! 'reverse))
(defalias 'ya:append! (! 'append))

;; simple memoize function
(defun memoize (fn)
  (let ((cache (make-hash-table :test 'equal)))
    (lambda (&rest args)
      (cl-multiple-value-bind (val hit) (gethash args cache)
        (print val " - " hit)
        (if hit val
          (setf (gethash args cache)
                (apply fn args))))))
  )

;; (defun fib (n)
;;   (if (or (= 0 n) (= 1 n)) 1
;;     (+ (fib (- n 1)) (fib (- n 2)))))

;; (require 'benchmark)
;;
;; (benchmark-elapse (funcall memoize-fib 8)) ;; 1st exec - build the cache
;; (benchmark-elapse (funcall memoize-fib 8)) ;; 2nd exec faster

;; (benchmark-run 2 (funcall memoize-fib 30)) ;; (3.088015825 42 1.6604243900000029)
;; 2 exec - total of 3 sec => 42 gc ops taking 1.66s

;; composing functions
(defalias 'λ 'lambda)

;; (defun ya:compose (&rest fns)
;;   "(compose (f1 f2 f3)(x) ≡  (f1 ∘ f2 ∘ f3)(x)"
;;   (if fns
;;       (let ((l:fn (car (last fns)))
;;             (l:fns (butlast fns)))
;;         (print l:fn)
;;         (print l:fns)
;;         (λ (&rest args)
;;            (cl-reduce 'funcall l:fns
;;                       :from-end t
;;                       :initial-value (apply l:fn args))))
;;     'identity))

;; (defmacro ya:compose1 (&rest fns)
;;   "(compose (f1 f2 f3)(x) ≡  (f1 ∘ f2 ∘ f3)(x)"
;;   (if fns
;;       (let ((l:fn (gensym))
;;             (l:fns (gensym)))
;;         `(let ((l:fn ,(car (last fns)))
;;                (l:fns (list ,@(butlast fns))))
;;            (λ (&rest args)
;;              (cl-reduce 'funcall l:fns
;;                         :from-end t
;;                         :initial-value (apply l:fn args)))))
;;     'identity))

(defmacro ya:compose (&rest fns)
  "(compose (f1 f2 f3)(x) ≡  (f1 ∘ f2 ∘ f3)(x)"
  (if fns
      (let ((l:fn (gensym))
            (l:fns (gensym)))
        `(let ((,l:fn ,(car (last fns)))
               (,l:fns (list ,@(butlast fns))))
           (λ (&rest args)
              (cl-reduce 'funcall ,l:fns
                         :from-end t
                         :initial-value (apply ,l:fn args)))))
    'identity))

(defalias '∘ 'ya:compose)

(provide 'hof)
;;; hof.el ends here
