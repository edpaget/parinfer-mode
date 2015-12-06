;;; -*- lexical-binding: t -*-

(defvar proc
  (start-process-shell-command "parinfer" "*parinfer-process*" "./bin/parinfer-mode"))

(defun parinfer-mode-post (url text)
  (let ((url-request-method "POST")
        (url-request-extra-headers '(("Content-Type" . "text/plain")))
        (url-request-data text))
    (url-retrieve url (parinfer-mode-kill-and-replace-buffer))))

(defun parinfer-mode-kill-and-replace-buffer ()
  (let ((old-buffer (current-buffer)))
    (lambda (status)
      ;;      (copy-to-buffer old-buffer 1 (buffer-size))
      (re-search-forward "^$" nil 'move)
      (forward-char)
      (copy-to-buffer old-buffer (point) (buffer-size))
      (kill-buffer (current-buffer)))))

(parinfer-mode-post "http://localhost:8088/indent-mode" (buffer-string))
