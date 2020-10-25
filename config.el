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

;; (setq doom-theme 'doom-gruvbox)
(use-package! lab-themes
  :config
  (load-theme 'lab-dark t))

;; (use-package! solo-jazz-theme
;;   :config
;;   (load-theme 'solo-jazz t)
;; )
;; (use-package! flucui-themes
;;   :config
;;   (load-theme 'flucui-dark t))

(setq doom-font (font-spec :family "FiraCode Nerd Font Mono" :size 12)
      doom-variable-pitch-font (font-spec :family "FiraCode Nerd Font Mono")
      doom-unicode-font (font-spec :family "PingFang SC")
      doom-big-font (font-spec :family "FiraCode Nerd Font Mono" :size 19))

(after! diff-mode
  (set-face-background 'diff-refine-changed nil)
  (set-face-background 'diff-refine-added nil)
  (set-face-background 'diff-refine-removed nil)
  )

;; (set-face-attribute 'show-paren-match nil :weight 'extra-bold)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org/")

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type nil)

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
  (use-package! beacon
    :config
    (beacon-mode 1)
  )

  (toggle-frame-fullscreen)
  ;;(wd-halfscreen)
)

;; for mac only
(when (eq system-type 'darwin) ;; mac specific settings
  (setq mac-option-modifier 'meta)
  (setq mac-command-modifier 'meta)
  (global-set-key [kp-delete] 'delete-char) ;; sets fn-delete to be right-delete
)

;;'y' for 'yes', 'n' for 'no'
(fset 'yes-or-no-p 'y-or-n-p)


;; 所有的备份文件转移到~/backups目录下
(setq auto-save-default nil)
(setq make-backup-files t)
(setq backup-by-copying t)
(setq version-control t)
(setq kept-old-versions 2)
(setq kept-new-versions 5)
(setq delete-old-versions t)
(setq backup-directory-alist '(("." . "~/.emacs.d/backups")))

(setq kill-ring-max 200)

;; Make Emacs UTF-8 compatible for both display and editing:
(prefer-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(setq confirm-kill-emacs nil)

(setq recentf-max-saved-items 100)
(global-visual-line-mode 1)

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
(after! terraform-mode
  (set-company-backend! 'terraform-mode
    'company-terraform 'company-files 'company-keywords)
  )

(set-company-backend! '(text-mode
                        markdown-mode
                        org-mode)
  '(:seperate company-ispell
              company-files
              company-yasnippet
              company-dabbrev))

(set-company-backend! '(emacs-lisp-mode)
    '(company-capf company-yasnippet company-files company-dabbrev)
    )

(set-company-backend! '(jsonnet-mode)
  '(:seperate company-files company-dabbrev)
    )

(after! lsp-python-ms
  (set-lsp-priority! 'mspyls 1))

(add-hook! 'magit-mode-hook (setq hl-line-mode -1))

;; (use-package! multi-vterm
;; 	:config
;; 	(setq vterm-keymap-exceptions nil)
;;   (define-key! 'insert vterm-mode-map (kbd "C-c")      #'vterm--self-insert)
;;   )

(use-package! vterm
  :config
  (setq vterm-max-scrollback 100000
        ;; vterm-buffer-name-string "term: %s"
        )
  (set-popup-rule! "^vterm" :size 0.5 :side 'bottom :modeline t
    :select t :quit nil :ttl 0)

  (remove-hook! 'vterm-mode-hook 'hide-mode-line-mode t))
  ;; (add-hook! 'vterm-mode-hook #'toggle-truncate-lines))

(after! doom-modeline
  (add-to-list 'global-mode-string '(:eval (wd-show-vterm-copy-mode)))
  )

(after! org
  (setq org-hide-leading-stars nil
        org-startup-indented nil)
  (remove-hook 'org-mode-hook #'org-superstar-mode))


(after! ivy
  (setq ivy-use-selectable-prompt t
        ivy-magic-slash-non-match-action 'ivy-magic-slash-non-match-cd-selected)
  )

(setq-default fill-column 120)

;; disable hl-line-mode
(remove-hook! '(text-mode-hook prog-mode-hook conf-mode-hook)
           #'hl-line-mode)


;; disable spell check for string/text in terraform
(add-hook! 'terraform-mode-hook
  (setq flyspell-prog-text-faces '(font-lock-comment-face font-lock-doc-face))
  )

(after! flyspell
  (setq flyspell-duplicate-distance 0)
  )

;; ivy fly
(add-hook 'minibuffer-setup-hook #'mcfly-time-travel)
(add-hook 'minibuffer-exit-hook
          (lambda ()
            (remove-hook 'pre-command-hook 'mcfly-back-to-present t)))

;; jsonnet
(add-hook! 'jsonnet-mode-hook
          (setq comment-start "//"))


(use-package! zoom-window
  :config
  (setq  zoom-window-mode-line-color "DarkGreen")
  )

(defhydra wd/hydra ()
  ;; window
  ("j" windmove-down "down" :column "window")
  ("k" windmove-up "up")

  ("h" windmove-left "left")
  ("l" windmove-right "right")

  ("z" zoom-window-zoom "zoom")

  ;; project
  ("a" counsel-ag "ag" :column "project" :exit t)
  ("C-a" (lambda () (interactive)
                  (setq current-prefix-arg '(4))
                  (call-interactively 'counsel-ag))
   "ag current dir" :column "project" :exit t)
  ("g" magit-status "git" :exit t)
  ("f" +ivy/projectile-find-file "find file" :exit t)
  ("v" counsel-imenu "imenu" :exit t)

  ;; edit
  ("J" (lambda() (interactive)(delete-indentation 1)) "join line" :column "edit")
  ("j" goto-line "Goto line" :exit t)

  ;; misc
  ("d" osx-dictionary-search-pointer "osx dict" :column "misc")
  ("b" bing-dict-brief "bing dict")

  ("h" easy-hugo "hugo" :exit t)

  ("q" nil "quit" :column nil)
  )

(use-package! key-chord
  :config
  (key-chord-mode 1)
  (key-chord-define-global "df" 'kill-whole-line)
  (key-chord-define-global "jj" 'ace-window)
  (key-chord-define-global "jk" 'avy-goto-line)
  (key-chord-define-global "kk" 'wd/hydra/body)
 )

;; keybindings
(global-set-key [remap mark-sexp] 'easy-mark)
(map! "M-s" 'avy-goto-char-timer
      "C-." 'avy-pop-mark

      "M-X" 'counsel-projectile-find-file
      "C-t" 'set-mark-command
      "C-x f" 'ffap
      "C-x 4 f" 'ffap-other-window
      "C-a" 'back-to-indentation-or-beginning

      "C-s" 'swiper-isearch
      "C-r" 'counsel-grep-or-swiper
      "C-e" 'end-of-line
      [remap kill-ring-save] 'easy-kill
      "C-M-m" '+vterm/toggle

      :map swiper-map
      "M-q" 'swiper-query-replace
      "C-w" 'my-ivy-yank-word
      "C-'" 'swiper-avy

      :map counsel-find-file-map
      "M-r" 'counsel-ff-as-root

      :map vterm-mode-map
      "` [" 'vterm-copy-mode
      ;; "C-c" 'vterm-send-C-c

      :map flyspell-mode-map
      "C-M-j" 'flyspell-correct-at-point
      )

;; (add-hook! vterm-mode-hook
;;   (lambda ()
;;     (which-key-mode -1)
;;     (projectile-mode -1)
;;     (flyspell-mode-off)
;;     (flycheck-mode -1)
;;     ))


;; (map! :after vterm
;;       :map vterm-mode-map
;;       "C-c" #'vterm-send-C-c)
