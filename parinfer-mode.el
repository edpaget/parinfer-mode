(require 'parinferlib)

(defun parinfer-mode-insert-result (result)
  (if (and (plist-get result :success)
           (not (and (= (char-before (point)) 10)
                     (= (char-after (point)) 41))))
      (let ((old-buffer (current-buffer))
            (old-point (point)))
        (with-temp-buffer
          (insert (plist-get result :text))
          (copy-to-buffer old-buffer (point-min) (point-max)))
        (goto-char old-point))))

(defun parinfer-mode-indent-mode ()
  (parinfer-mode-insert-result (parinferlib-indent-mode (buffer-string)
                                                        (current-column)
                                                        (- (line-number-at-pos) 1)
                                                        nil)))

(defun parinfer-mode-paren-mode ()
  (parinfer-mode-insert-result (parinferlib-paren-mode (buffer-string)
                                                       (current-column)
                                                       (- (line-number-at-pos) 1)
                                                       nil)))

(define-minor-mode parinfer-mode
  "Uses Parinfer to Format lispy code"
  :lighter " parinfer "
  (if parinfer-mode
      (progn
        (parinfer-mode-paren-mode)
        (add-hook 'post-self-insert-hook 'parinfer-mode-indent-mode nil t))
    (remove-hook 'post-self-insert-hook 'parinfer-mode-indent-mode t)))

(provide 'parinfer-mode)
