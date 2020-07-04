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


(provide 'my-fuctions)
