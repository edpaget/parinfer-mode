;;; -*- lexical-binding: t -*-

(require 'json)

;;(start-process-shell-command "parinfer" "*parinfer-process*" "./bin/parinfer-mode")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro parinfer-mode-> (&rest body)                                       ;;
  (let ((result (pop body)))                                                 ;;
    (dolist (form body result)                                               ;;
      (setq result (append (list (car form) result)                          ;;
                           (cdr form))))))                                   ;;

(defmacro parinfer-mode->> (&rest body)
  (let ((result (pop body)))
    (dolist (form body result)
      (setq result (append form (list result))))))

(defun parinfer-mode-plist-update (plist key fn)
  (plist-put plist key (fn (plist-get plist key))))

(defconst parinfer-mode-backslash "\\")
(defconst parinfer-mode-comma ",")
(defconst parinfer-mode-double-quote "\"")
(defconst parinfer-mode-newline "\n")
(defconst parinfer-mode-semicolon ";")
(defconst parinfer-mode-tab "\t")

(defconst parinfer-mode-parens
  (parinfer-mode-> '()
                   (plist-put "{" "}")
                   (plist-put "}" "{")
                   (plist-put "[" "]")
                   (plist-put "]" "[")
                   (plist-put "(" ")")
                   (plist-put ")" "("))
  "Lookup table for paren matching")

(defun parinfer-mode-get-initial-result ()
  (parinfer-mode-> '()
                   (plist-put :lines [])
                   (plist-put :line-no -1)
                   (plist-put :ch "")
                   (plist-put :x -1)
                   (plist-put :stack [])
                   (plist-put :backup [])
                   (plist-put :insert (parinfer-mode-> '()
                                                       (plist-put :line-no nil)
                                                       (plist-put :x nil)))
                   (plist-put :paren-trail (parinfer-mode-> '()
                                                            (plist-put :start nil)
                                                            (plist-put :end nil)))
                   (plist-put :top-level-line-nums [])
                   (plist-put :cursor-x nil)
                   (plist-put :cursor-line nil)
                   (plist-put :cursor-dex nil)
                   (plist-put :read-only nil)
                   (plist-put :quote-danger nil)
                   (plist-put :track-ident nil)
                   (plist-put :curosor-in-comment nil)
                   (plist-put :quit nil)
                   (plist-put :process nil)
                   (plist-put :success nil)
                   (plist-put :max-indent nil)
                   (plist-put :indent-delta nil)))

;; Reader Operations

(defun parinfer-mode-is-open-paren (c)
  (or (= c "{") (= c "(") (= c "[")))

(defun parinfer-mode-is-close-paren (c)
  (or (= c "}") (= c ")") (= c "]")))

(defun parinfer-mode-is-whitespace (c)
  (or (= c " ") (= c parinfer-mode-tab) (= c parinfer-mode-newline)))

;; String Operations

(defun insert-string (orig idx insert)
  (concat (substring orig 0 idx) insert (substring orig idx)))

(defun replace-string (orig start end replace)
  (concat (substring orig 0 start) replace (substring orig end)))

(defun remove-string-range (orig start end)
  (concat (substring orig 0 start) (substring orig end)))

;; Stack States

(defun parinfer-mode-peek (stack i)
  (let ((idx (length stack)))
    (if (< idx 0)
        nil
      (elt stack idx))))

(defun parinfer-mode-get-prev-ch (stack i)
  (let ((e (peek stack i)))
    (if e
        (plist-get :ch e)
      nil)))

(defun parinfer-mode-is-escaping (stack)
  (= (parinfer-mode-get-prev-ch stack 1) parinfer-mode-backslash))

(defun parinfer-mode-prev-non-esc-ch (stack)
  (parinfer-mode-get-prev-ch stack (if (parinfer-mode-is-escaping stack) 2 1)))

(defun parinfer-mode-is-in-str (stack)
  (= (parinfer-mode-prev-non-esc-ch stack) parinfer-mode-double-quote))

(defun parinfer-mode-is-in-comment (stack)
  (= (parinfer-mode-prev-non-esc-ch stack) parinfer-mode-semicolon))

(defun parinfer-mode-is-in-code (stack)
  (not (and (parinfer-mode-is-in-str stack) (parinfer-mode-is-in-comment stack))))

(defun parinfer-mode-is-valid-closer (stack ch)
  (= (parinfer-mode-get-prev-ch stack 1) (plist-get ch parinfer-mode-parens)))

;; Stack Operations

(defun pop-stack (result)
  (plist-update result :result (lambda (stack) (subseq stack 1))))

(defun parinfer-mode-push-open (result)
  (let ((stack (plist-get :stack result))
        (top-level-fn (lambda (nums)
                        (append nums (plist-get result :line-no))))
        (result-fn (lambda (stack)
                     (append stack (parinfer-mode-> '()
                                                    (plist-put :x (plist-get result :x))
                                                    (plist-put :ch (plist-get result :ch))
                                                    (plist-put :indent-delta (plist-get result :ident-delta)))))))
    (cond
     (parinfer-mode-is-escaping stack) (pop-stack result)
     (parinfer-mode-is-in-code stack) (parinfer-mode-> (if (and (plist-get :read-only result)
                                                                (= 0 (length stack)))
                                                           (plist-update result :top-level-line-nums top-level-fn)
                                                         result)
                                                       (plist-update :stack result-fn))
     t result)))

(defun parinfer-mode-push-close (result)
  (let ((stack (plist-get result :stack))
        (ch (plist-get result :ch)))
    (cond
     (parinfer-mode-is-escaping stack) (pop-stack result)
     (and (parinfer-mode-is-in-code stack)
          (parinfer-mode-is-valid-closer stack ch)
          (not (plist-get result :read-only)))
     (progn
       (pop-stack result)
       (parinfer-mode-> result
                        (plist-put :max-indent
                                   (plist-get (first stack)
                                              :x))
                        (plist-put :backup
                                   (append (plist-get result
                                                      :backup)
                                           (first stack)))))
     (and (parinfer-mode-is-in-code stack)
          (parinfer-mode-is-valid-closer stack ch))
     (pop-stack result)
     (parinfer-mode-is-in-code stack) (plist-put result :ch "")
     t result)))

(defun parinfer-mode-push-tab (result)
  (if (not (parinfer-mode-is-in-str (plist-get result :stack)))
      (plist-put result :ch "  ")
    result))

(defun parinfer-mode-push-semicolon (result)
  (let ((stack (plist-get result :stack)))
    (cond
     (parinfer-mode-is-escaping stack) (pop-stack result)
     (parinfer-mode-is-in-code stack) (plist-put result :stack (parinfer-mode-> '()
                                                                                (plist-put :x (plist-get result :x))
                                                                                (plist-put :ch (plist-get result :ch))))
     t result)))

(defun parinfer-mode-push-newline (result)
  (let ((stack (plist-get result :stack)))
    (cond
     (parinfer-mode-is-escaping stack) (pop-stack result)
     (parinfer-mode-is-in-comment stack) (pop-stack result)
     t (plist-put result :ch ""))))

(defun parinfer-mode-push-escape (result)
  (let ((stack (plist-get result :stack)))
    (if (parinfer-mode-is-escaping stack)
        (pop-stack result)
      (plist-put result :stack (parinfer-mode-> '()
                                                (plist-put :x (plist-get result :x))
                                                (plist-put :ch (plist-get result :ch)))))
    ))

(defun parinfer-mode-push-quote (result)
  (let ((stack (plist-get result :stack)))
    (cond
     (parinfer-mode-is-escaping stack) (pop-stack result)
     (parinfer-mode-is-in-str stack) (pop-stack result)
     (parinfer-mode-is-in-comment stack) (plist-put result
                                                    :quote-danger
                                                    (not (plist-get result :quote-danger)))
     t (plist-put result :stack (parinfer-mode-> '()
                                                 (plist-put result :x (plist-get result :x))
                                                 (plist-put result :ch (plist-get result :ch)))))))

(defun parinfer-mode-push-default (result)
  (let ((stack (plist-get result :stack)))
    (if (parinfer-mode-is-escaping stack)
        (pop-stack result)
      result)))

(defun parinfer-mode-push-char (result)
  (let ((ch (plist-get result :ch)))
    (cond
     (parinfer-mode-is-open-paren ch) (parinfer-mode-push-open result)
     (parinfer-mode-is-close-paren ch) (parinfer-mode-push-close result)
     (= ch parinfer-mode-tab) (parinfer-mode-push-tab result)
     (= ch parinfer-mode-semicolon) (parinfer-mode-push-semicolon result)
     (= ch parinfer-mode-newline) (parinfer-mode-push-newline result)
     (= ch parinfer-mode-backslash) (parinfer-mode-push-escape result)
     (= ch parinfer-mode-double-quote) (parinfer-mode-push-quote result)
     t (parinfer-mode-push-default result))))

(defun parinfer-mode-push-line (result line)
  (reduce (lambda (res char)
            (parinfer-mode-> (plist-put res :x (+ 1 (plist-get res :x)))
                             (plist-put res :ch char)
                             (parinfer-mode-push-char)))
          line :initial-value result))

(defun parinfer-mode-read (text)
  (let ((lines (split-string text "\n"))
        (result (plist-put (parinfer-mode-get-initial-result)
                           :read-only t)))
    (reduce (lambda (result line)
              (parinfer-mode-push-line (plist-put result :line-no
                                                  (+ 1 (plist-get result :line-no)))))
            lines :initial-value result)))

;; Indent Mode Operations

(defun parinfer-mode-close-parens (result indent-x)
  (let ((indent-x (or indent-x 0))
        (stack (plist-get result :stack))
        (parens (reduce (lambda (ps stack-line)
                          (cons (plist-get parinfer-mode-parens (plist-get stack-line :ch))
                                ps))
                        stack))
        (new-string (insert-string (elt (plist-get result :lines) (plist-get result :line-no))
                                   (plist-get (plist-get result :insert) :x)
                                   parens)))
    (aset (plist-get result :lines) (plist-get result :line-no) new-string)
    result))

(defun parinfer-mode-update-paren-trail (result)
  (let ((ch (plist-get result :ch))
        (stack (plist-get result :stack))
        (close-paren (parinfer-mode-is-close-paren ch))
        (escaping (parinfer-mode-is-escaping stack))
        (in-code (parinfer-mode-is-in-code stack))
        (should-pass (or (= ch parinfer-mode-semicolon)
                         (= ch parinfer-mode-comma)
                         (parinfer-mode-is-whitespace ch)
                         close-paren))
        (should-reset (and in-code (or escaping (not should-pass))))
        (should-upate (and in-code (not escaping) close-paren (parinfer-mode-is-valid-closer stack ch)))
        (parinfer-mode-> (cond
                          should-reset (parinfer-mode-> (plist-put result :backup [])
                                                        (plist-put :parent-trail
                                                                   (parinfer-mode-> '()
                                                                                    (plist-put :start
                                                                                               nil)
                                                                                    (plist-put :end
                                                                                               nil))))
                          (and should-update (not (plist-get (plist-get result :parent-trail) :start)))
                          (parinfer-mode-> (plist-put result :paren-trail
                                                      (plist-put (plist-get result :paren-trail)
                                                                 :start (plist-get result :x))
                                                      :start (plist-get result :x))
                                           (plist-put :parent-trail
                                                      (plist-put (plist-get result :parent-trail)
                                                                 :end (+ (plist-get result :x) 1))))
                          should-update (plist-put result :parent-trail
                                                   (plist-put (plist-get result :parent-trail)
                                                              :end (+ (plist-get result :x) 1)))
                          t result)
                         (plist-put :cursor-in-comment (or (plist-get result :cursor-in-comment)
                                                           (and (= (plist-get result :cursor-line)
                                                                   (plist-get result :line-no))
                                                                (= (plist-get result :x)
                                                                   (plist-get result :cursor-x))
                                                                (parinfer-mode-is-in-comment stack))))))))

(defun parinfer-mode-block-paren-trail (result)
  (let ((start (plist-get (plist-get result :paren-trail) :start))
        (end (plist-get (plist-get result :paren-trail) :end))
        (is-cursor-blocking (and (= (plist-get result :line-no)
                                    (plist-get result :cursor-line))
                                 start
                                 (> (plist-get result :cursor-x) start)
                                 (not (plist-get result :cursor-in-comment)))))
    (if (and start is-cursor-blocking)
        (setq start (ma)))
  )

(defun parinfer-mode-indent-mode (text &rest options)
  )

(defun parinfer-mode-process-text-paren (text options)
  )

(defun parinfer-mode-paren-mode (text &rest options)
  (let ((result (parinfer-mode-process-text-paren text options))
        (out-text (if (gethash :success result nil)
                      (mapconcat 'identity (gethash :text result) parinfer-mode-newline)
                    text)))
    (parinfer-mode->> (make-hash-table :test 'equal)
                      (plist-put :text out-text)
                      (plist-put :success (gethash :success result)))))

(define-minor-mode parinfer-mode
  "Uses Parinfer to Format lispy code"
  :lighter " parinfer"
  (if parinfer-mode
      (progn
        (parinfer-mode-paren-mode)
        (add-hook 'post-self-insert-hook 'parinfer-mode-indent-mode nil t))
    (remove-hook 'post-self-insert-hook 'parinfer-mode-indent-mode t)))

(provide 'parinfer-mode)
