;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "wd"
      user-mail-address "wd@wdicc.com")

;; Doom exposes five (optional) variables for controlling fonts in Doom. Here
;; are the three important ones:
;;
;; + `doom-font'
;; + `doom-variable-pitch-font'
;; + `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;;
;; They all accept either a font-spec, font string ("Input Mono-12"), or xlfd
;; font string. You generally only need these two:
;; (setq doom-font (font-spec :family "monospace" :size 12 :weight 'semi-light)
;;       doom-variable-pitch-font (font-spec :family "sans" :size 13))

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
;; (setq doom-theme 'doom-one)

(load-theme 'sanityinc-tomorrow-eighties t)
(setq doom-theme 'sanityinc-tomorrow-eighties)

(use-package! cnfonts
  :config
  (custom-set-variables
  '(cfs--current-profile "profile1" t)
  '(cfs--profiles-steps (quote (("profile1" . 4))) t))
  (cnfonts-enable)
  )

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org/")

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)


;; Here are some additional functions/macros that could help you configure Doom:
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.

;;
;; set path
;;
(setq exec-path (append exec-path '("/usr/local/bin")))
(setq exec-path (append exec-path '("~/bin/") ))
(setq default-directory "~/")

;;
;; set window size
;;
(defun wd-set-window-pos2 (&optional main-screen-width &optional main-screen-height)
   "Set emacs window at proper position"
   (unless main-screen-height (setq main-screen-height 900))
   (unless main-screen-width (setq main-screen-width 1440))
   (let ((display-height (x-display-pixel-height))
         (display-width (x-display-pixel-width))
         (margin-left 300)
         (margin-top 0))
     (if (or (> display-width main-screen-width)
             (> display-height main-screen-height))
         (modify-frame-parameters (selected-frame) (list (cons 'left  (+ margin-left)) (cons 'top  (+ -1080))))
       (modify-frame-parameters (selected-frame) (list (cons 'left (+ margin-left)) (cons 'top (+ 0))))
       )
     (set-frame-size (selected-frame) 120 50))
   )


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

(when (window-system)
  (menu-bar-mode -1)
  (tool-bar-mode -1)
  (scroll-bar-mode -1)
  (wd-halfscreen)
)

;;
;; misc settings
;; 
(mouse-wheel-mode 1)
;; 不弹出图形界面的确认窗口
(setq use-dialog-box nil)

(global-auto-revert-mode 1)

;;
;; for mac only
;; 
(when (eq system-type 'darwin) ;; mac specific settings
  (setq mac-option-modifier 'meta)
  (setq mac-command-modifier 'meta)
  (global-set-key [kp-delete] 'delete-char) ;; sets fn-delete to be right-delete
)


;; 和x公用剪贴板
(setq x-select-enable-clipboard t)
;; (setq x-select-enable-primary t)

;;'y' for 'yes', 'n' for 'no'
(fset 'yes-or-no-p 'y-or-n-p)

;;禁用启动信息
(setq inhibit-startup-message t)
(setq initial-scratch-message "")
;; 显示列号
(setq column-number-mode t) 

;; 防止页面滚动时跳动， scroll-margin 3 可以在靠近屏幕边沿3行时就开始滚动，可以很好的看到上下文。
;; (setq scroll-margin 3
;;       scroll-conservatively 2)

;;关闭烦人的出错时的提示声
;;(setq visible-bell t)

;;把title设置为“文件名@LC's Emacs"
(setq frame-title-format
        '("GNU Emacs - [ "(buffer-file-name "%f \]"
                (dired-directory dired-directory "%b \]"))))

;; 语法高亮
(global-font-lock-mode t)

;;光标靠近鼠标的时候，让鼠标自动让开，别挡住视线
(mouse-avoidance-mode 'animate)

;; 翻页后再回来的时候，光标到原来的位置
(setq scroll-preserve-screen-position t)

;;下面的这个设置可以让光标指到某个括号的时候显示与它匹配的括号
(show-paren-mode t)
;; (setq show-paren-style 'parentheses)
(setq show-paren-style 'expression)

;;设置缺省模式是text，而不是基本模式
(setq default-major-mode 'text-mode)
;; (setq fill-column 80)
;; (setq-default fill-column 80)
;; (setq longlines-show-hard-newlines t)
;; (setq longlines-auto-wrap t)
;; (add-hook 'text-mode-hook 'longlines-mode)
;; (add-hook 'text-mode-hook 'turn-on-auto-fill)

;; 打开文件的时候定位到上次的位置
(save-place-mode 1)

;; 所有的备份文件转移到~/backups目录下
(setq auto-save-default nil)
(setq make-backup-files t)
(setq backup-by-copying t)
(setq version-control t)
(setq kept-old-versions 2)
(setq kept-new-versions 5)
(setq delete-old-versions t)
(setq backup-directory-alist '(("." . "~/.emacs.d/backups")))
;; Emacs 中，改变文件时，默认都会产生备份文件(以 ~ 结尾的文件)。可以完全去掉
;; (并不可取)，也可以制定备份的方式。这里采用的是，把所有的文件备份都放在一
;; 个固定的地方("~/var/tmp")。对于每个备份文件，保留最原始的两个版本和最新的
;; 五个版本。并且备份的时候，备份文件是复本，而不是原件。

;;不产生备份文件
;(setq make-backup-files nil)

;;设置kill-ring-max(我不知道怎么翻译这个词：)为200，以防不测：）
(setq kill-ring-max 200)

;; 设置mark， C-x <SPC>
(global-set-key (kbd "C-t") 'set-mark-command)

;; Make Emacs UTF-8 compatible for both display and editing:
(prefer-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)

;; 打开 quick-calc
;; (global-set-key (kbd "M-#") 'quick-calc)

;; 高量当前行 // 会造成滚动的时候抖动，不是很爽。。
;; (require 'hl-line)
;; (global-hl-line-mode 1)

;; 查找打开当前光标所在的文件
(global-set-key (kbd "C-x f") 'find-file-at-point)

(defun back-to-indentation-or-beginning (arg)
  "combine two function into one call."
  (interactive "^p")
  (if (bolp)
      (back-to-indentation)
    (move-beginning-of-line arg)))
(define-key global-map (kbd "C-a") 'back-to-indentation-or-beginning)

;; proxy
;;(setq url-proxy-services
;;       '(("no_proxy" . "^\\(127.0.0.1\\|localhost\\|10.*\\)")
;;         ("http" . "127.0.0.1:6152")
;;         ("https" . "127.0.0.1:6152")))

(use-package! uniquify
  :config
  (setq uniquify-buffer-name-style 'post-forward)
  (setq uniquify-separator ":")
)

(use-package! whitespace
  :config
  (setq whitespace-style '(trailing tabs))
  (global-whitespace-mode)
)

;;
;; ivy swiper
;;
(defun my-ivy-yank-word ()
  (interactive)
  (let (amend)
    (with-selected-window (ivy-state-window ivy-last)
      (goto-char swiper--opoint)
      (setq amend (thing-at-point 'symbol)))
    (when amend (insert amend))))

(use-package! counsel
  :bind (("C-c i" . counsel-projectile-ag)
         ("M-x" . counsel-M-x)
         ("C-c f" . counsel-projectile-find-file)
         ("M-X" . ivy-switch-buffer)
         ("C-c v" . counsel-imenu)
         )
  :init
  (setq recentf-max-saved-items 200)
  ;; (setq ivy-virtual-abbreviate 'full)
  :config
  (use-package! wgrep)
  (ivy-mode 1)
  (setq ivy-use-virtual-buffers t ; treat recentf, bookmarks as virtual buffers.
        ivy-height 10
        ivy-display-style 'fancy
        ivy-virtual-abbreviate 'full
        ivy-count-format "(%d/%d) "
        ivy-initial-inputs-alist nil ; remove initial ^ input.
        ivy-extra-directories nil ; remove . and .. directory.
        ivy-wrap nil
        )

  (set-variable 'ivy-on-del-error-function '(lambda()))

  ;; (ivy-add-actions
  ;;  'counsel-find-file
  ;;  '(("g" counsel-find-file-ag-action "grep")))
  )

(use-package! ivy-rich
  :after ivy
  ;; :custom
  ;; (ivy-virtual-abbreviate 'full
  ;;                         ivy-rich-switch-buffer-align-virtual-buffer t
  ;;                         ivy-rich-path-style 'abbrev)
  :config
  (ivy-rich-mode 1)
  (setcdr (assq t ivy-format-functions-alist) #'ivy-format-function-line)
  )

(use-package! swiper
  :after ivy
  :bind (("C-s" . swiper-isearch)
         :map swiper-map
         ("M-q" . swiper-query-replace)
         ("C-w" . my-ivy-yank-word)
         ("C-'" . swiper-avy)
         )
  )

;; "M-q" swiper-query-replace
;; "C-l" swiper-recenter-top-bottom
;; "C-'" swiper-avy
;; "C-7" swiper-mc
;; "C-c C-f" swiper-toggle-face-matching

;; avy
(use-package! avy
  :bind (("M-s" . avy-goto-char-timer)
         ("C-." . avy-pop-mark))
  )

;; magit
(use-package! magit
  :config
  (global-set-key (kbd "C-x g") 'magit-status)
  )

(use-package! ace-window
  :config
  (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))
  ;; (setq aw-ignore-current t)
  (global-set-key (kbd "C-M-h") #'ace-window)
  (custom-set-faces
   '(aw-leading-char-face
     ((t
       (:height 10.0 :foreground "gold")
       ))
     ))
  )


(use-package! rainbow-delimiters
    :config
    (add-hook 'emacs-lisp-mode-hook #'rainbow-delimiters-mode)
    )


  (use-package! ibuffer
    ;; :ensure ibuffer-vc
    :config
    (add-hook 'ibuffer-hook
      (lambda ()

        (face-remap-add-relative 'default 'font-lock-comment-face)
        (copy-face 'font-lock-keyword-face 'tempface )
        (setq ibuffer-filter-group-name-face 'tempface)
        (face-remap-add-relative ibuffer-filter-group-name-face font-lock-doc-face)
        (ibuffer-vc-set-filter-groups-by-vc-root)
        (unless (eq ibuffer-sorting-mode 'alphabetic)
          (ibuffer-do-sort-by-alphabetic))))

    (defconst gcs-ibuffer-fontification-alist
      '((ruby-mode . font-lock-string-face)
        (sh-mode . font-lock-string-face)
        (objc-mode . font-lock-constant-face)
        (c-mode . font-lock-constant-face)
        (java-mode . font-lock-constant-face)
        (emacs-lisp-mode . font-lock-variable-name-face)
        (org-mode . font-lock-negation-char-face)
        (dired-mode . font-lock-function-name-face)
        (term-mode . font-lock-doc-string-face)
        (python-mode . font-lock-variable-name-face)))

    (setq ibuffer-fontification-alist
          `(,@(mapcar (lambda (b)
                        `(9999 (eq major-mode ',(car b)) ,(cdr b)))
                      gcs-ibuffer-fontification-alist)
            (90 (string-match "magit" (symbol-name major-mode))
                font-lock-function-name-face)
            (90 (or (string-match "^*" (buffer-name))
                    (memq major-mode ibuffer-help-buffer-modes))
                font-lock-comment-face)))

    (setq ibuffer-formats
        '((mark modified read-only vc-status-mini " "
                (name 18 18 :left :elide)
                " "
                (size 9 -1 :right)
                " "
                (mode 16 16 :left :elide)
                " "
                (vc-status 16 16 :left)
                " "
                filename-and-process)))
    ;; (define-key ibuffer-mode-map (kbd "C-g") 'quit-window)
    ;; (define-key ibuffer-mode-map (kbd "j") 'ibuffer-forward-line)
    ;; (define-key ibuffer-mode-map (kbd "k") 'ibuffer-backward-line)
    ;; (define-key ibuffer-mode-map (kbd "C-n") 'ibuffer-forward-filter-group)
    ;; (define-key ibuffer-mode-map (kbd "C-p") 'ibuffer-backward-filter-group)
    :bind ("C-x C-b" . ibuffer))


(use-package! osx-dictionary
  :bind ("C-c d" . osx-dictionary-search-pointer)
  )

(use-package! bing-dict
  :bind ("C-c k" . bing-dict-brief)
  )

(use-package! easy-hugo
  :init
  (setq easy-hugo-basedir "~/blog/")
  (setq easy-hugo-url "https://wdicc.com")
  (setq easy-hugo-postdir "content/post")
  (setq easy-hugo-previewtime "300")
  (setq easy-hugo-default-ext ".org")
  :bind ("C-c C-e" . easy-hugo)
  )
