;;; -*- lexical-binding: t -*-

(require 'json)

;;(start-process-shell-command "parinfer" "*parinfer-process*" "./bin/parinfer-mode")

(defmacro parinfer-mode-> (&rest body)
  (let ((result (pop body)))
    (dolist (form body result)
      (setq result (append (list (car form) result)
                           (cdr form))))))

(defun parinfer-mode-post (url text cursor line)
  (let ((url-request-method "POST")
        (url-request-extra-headers '(("Content-Type" . "application/json")))
        (url-request-data (parinfer-mode-> '()
                                           (plist-put :text text)
                                           (plist-put :cursor cursor)
                                           (plist-put :line line)
                                           (json-encode))))
    (url-retrieve url (parinfer-mode-kill-and-replace-buffer))))

(defun parinfer-mode-kill-and-replace-buffer ()
  (let ((old-buffer (current-buffer))
        (old-point (point)))
    (lambda (status)
      (re-search-forward "^$" nil 'move)
      (forward-char)
      (copy-to-buffer old-buffer (point) (buffer-size))
      (kill-buffer (current-buffer))
      (switch-to-buffer old-buffer)
      (goto-char old-point))))

(defun parinfer-mode-indent-mode ()
  (parinfer-mode-post "http://localhost:8088/indent-mode"
                      (buffer-string)
                      (current-column)
                      (- (line-number-at-pos) 1)))

(defun parinfer-mode-paren-mode ()
  (parinfer-mode-post "http://localhost:8088/paren-mode"
                      (buffer-string)
                      (current-column)
                      (- (line-number-at-pos) 1)))

(define-minor-mode parinfer-mode
  "Uses Parinfer to Format lispy code"
  :ligher " parinfer"
  (if parinfer-mode
      (progn
        (parinfer-mode-paren-mode)
        (add-hook 'post-self-insert-hook 'parinfer-mode-indent-mode nil t))
    (remove-hook 'post-self-insert-hook 'parinfer-mode-indent-mode t)))

(provide 'parinfer-mode)
