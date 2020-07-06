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

(setq doom-theme 'doom-gruvbox)
(setq doom-font (font-spec :family "Source code pro" :size 12)
      doom-variable-pitch-font (font-spec :family "Source code pro")
      doom-unicode-font (font-spec :family "PingFang SC")
      doom-big-font (font-spec :family "Source code pro" :size 19))

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

(load! "my-functions.el")

;;
;; set path
;;
(setq exec-path (append exec-path '("/usr/local/bin")))
(setq exec-path (append exec-path '("~/bin/") ))
(setq default-directory "~/")
(use-package! exec-path-from-shell
  :config
  (when (memq window-system '(mac ns))
    (exec-path-from-shell-initialize))
  )

;;
;; set window size
;;

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

;;'y' for 'yes', 'n' for 'no'
(fset 'yes-or-no-p 'y-or-n-p)

;;禁用启动信息
(setq inhibit-startup-message t)
(setq initial-scratch-message "")
;; 显示列号
(setq column-number-mode t) 

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

;; Make Emacs UTF-8 compatible for both display and editing:
(prefer-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(setq confirm-kill-emacs nil)

(setq recentf-max-saved-items 200)

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

(use-package! ace-window
  :config
  (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))
  ;; (setq aw-ignore-current t)
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

(use-package! easy-hugo
  :init
  (setq easy-hugo-basedir "~/blog/")
  (setq easy-hugo-url "https://wdicc.com")
  (setq easy-hugo-postdir "content/post")
  (setq easy-hugo-previewtime "300")
  (setq easy-hugo-default-ext ".org")
  )

;;
;; company
;;
;; (after! python-mode
;;   (set-company-backend! 'python-mode
;;     'company-lsp 'company-keywords 'company-yasnippet))

(after! lsp-python-ms
  (set-lsp-priority! 'mspyls 1))

(add-hook! 'magit-mode-hook (setq hl-line-mode -1))

(after! vterm
  (setq vterm-max-scrollback 100000
        vterm-buffer-name-string "Term: %s"
        )
  (set-popup-rule! "^vterm" :size 0.5 :side 'bottom :modeline t
    :select t :quit nil :ttl 0)
  (remove-hook! 'vterm-mode-hook
    'hide-mode-line-mode t
    )
  )

(defun wd-show-vterm-copy-mode ()
  (if vterm-copy-mode
      (propertize "COPY"
                  'font-lock-face
                  '(:foreground "green"
                    :weight "bold"
                    ))
    "")
  )

(after! doom-modeline
  (add-to-list 'global-mode-string '(:eval (wd-show-vterm-copy-mode)))
  )

;; (add-hook! 'doom-escape-hook
;;   (if vterm-copy-mode
;;       (vterm-copy-mode-done nil)
;;     ))

(after! org
  (setq org-hide-leading-stars nil
        org-startup-indented nil)
  (remove-hook 'org-mode-hook #'org-superstar-mode))


(after! counsel
  (setq ivy-use-selectable-prompt t)
  )

(setq-default fill-column 120)
(add-hook! '(text-mode-hook prog-mode-hook conf-mode-hook)
           #'display-fill-column-indicator-mode)


;; keybindings
(map! "C-c h" 'easy-hugo
      "C-c b" 'bing-dict-brief
      "C-c d" 'osx-dictionary-search-pointer
      "C-c C-b" 'ibuffer
      "M-s" 'avy-goto-char-timer
      "C-." 'avy-pop-mark
      "C-x g" 'magit-status
      "C-M-h" 'ace-window
      "M-X" 'counsel-projectile-find-file
      "C-t" 'set-mark-command
      "C-x f" 'find-file-at-point
      "C-a" 'back-to-indentation-or-beginning
      "C-c a i" 'counsel-projectile-ag
      "C-C a v" 'counsel-imenu
      "C-s" 'swiper-isearch
      [remap kill-ring-save] 'easy-kill

      :map swiper-map
      "M-q" 'swiper-query-replace
      "C-w" 'my-ivy-yank-word
      "C-'" 'swiper-avy
  )
