;;; ~/.doom.d/my-functions.el -*- lexical-binding: t; -*-

(defun wd-set-window-pos (&optional main-screen-width &optional main-screen-height)
   "Set emacs window at proper position"
   (unless main-screen-height (setq main-screen-height 900))
   (unless main-screen-width (setq main-screen-width 1440))
   (let ((display-height (x-display-pixel-height))
         (display-width (x-display-pixel-width))
         (margin-left 300)
         (margin-top 0))
     (if (or (> display-width main-screen-width)
             (> display-height main-screen-height))
         (progn
           (setq margin-top -900))
       )
     (message "left %s, top %s" margin-left margin-top)
     (modify-frame-parameters (selected-frame) '((left . (+ margin-left)) (top . (+ margin-top))))
     (set-frame-size (selected-frame) 120 50))
   )

(defun wd-fullscreen ()
  (interactive)
  (set-frame-parameter nil 'fullscreen 'fullscreen)
)

(defun wd-halfscreen ()
  (interactive)
  (setq half-display-height (/ (x-display-pixel-height) 2)
        half-display-width (/ (x-display-pixel-width) 2))

  (setq margin-left (/ half-display-width 4)
        margin-top (/ half-display-height 4))

  (set-frame-size (selected-frame) 200 50)
  (set-frame-position (selected-frame) margin-left margin-top)
  )

(defun my-ivy-yank-word ()
  (interactive)
  (let (amend)
    (with-selected-window (ivy-state-window ivy-last)
      (goto-char swiper--opoint)
      (setq amend (thing-at-point 'symbol)))
    (when amend (insert amend))))


(defun back-to-indentation-or-beginning (arg)
  "combine two function into one call."
  (interactive "^p")
  (if (bolp)
      (back-to-indentation)
    (move-beginning-of-line arg)))

(defun counsel-ff-as-root ()
  (interactive)
  (ivy-exit-with-action #'counsel-find-file-as-root))

(defun wd-show-vterm-copy-mode ()
  (if vterm-copy-mode
      (propertize
       "COPY"
       'font-lock-face
       '(:foreground "green"
         :weight "bold"
         ))
    ""))

;; copy from https://with-emacs.com/posts/ui-hacks/execute-commands-like-marty-mcfly/
(defvar mcfly-commands
  '(query-replace-regexp
    flush-lines keep-lines ivy-read
    swiper swiper-backward swiper-all
    swiper-isearch swiper-isearch-backward
    lsp-ivy-workspace-symbol lsp-ivy-global-workspace-symbol
    counsel-grep-or-swiper counsel-grep-or-swiper-backward
    counsel-grep counsel-ack counsel-ag
    ))

(defvar mcfly-back-commands
  '(self-insert-command
    ivy-yank-char
    ivy-yank-word
    ivy-yank-symbol))

(defun mcfly-back-to-present ()
  (remove-hook 'pre-command-hook 'mcfly-back-to-present t)
  (cond ((and (memq last-command mcfly-commands)
              (equal (this-command-keys-vector) (kbd "M-p")))
         ;; repeat one time to get straight to the first history item
         (setq unread-command-events
               (append unread-command-events
                       (listify-key-sequence (kbd "M-p")))))
        ((memq this-command mcfly-back-commands)
         (delete-region (point)
                        (point-max)))))

(defun mcfly-time-travel ()
  (when (memq this-command mcfly-commands)
    (let* ((kbd (kbd "M-n"))
           (cmd (key-binding kbd))
           (future (and cmd
                        (with-temp-buffer
                          (when (ignore-errors
                                  (call-interactively cmd) t)
                            (buffer-string))))))
      (when future
        (save-excursion
          (insert (propertize
                   (replace-regexp-in-string
                    "\\\\_<" ""
                    (replace-regexp-in-string
                     "\\\\_>" ""
                     future))
                   'face 'shadow)))
        (add-hook 'pre-command-hook 'mcfly-back-to-present nil t)))))

(defun wd/join-line ()
  (interactive)
  (delete-indentation 1)
  )

(provide 'my-fuctions)
