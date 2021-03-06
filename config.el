;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

(setq user-full-name "wd"
      user-mail-address "wd@wdicc.com")

(setq doom-font (font-spec :family "FiraCode Nerd Font Mono" :size 12)
      doom-variable-pitch-font (font-spec :family "FiraCode Nerd Font Mono")
      doom-unicode-font (font-spec :family "PingFang SC")
      doom-big-font (font-spec :family "FiraCode Nerd Font Mono" :size 19))

;; (setq doom-theme 'doom-one)
(add-hook 'ns-system-appearance-change-functions
          #'(lambda (appearance)
              (mapc #'disable-theme custom-enabled-themes)
              (pcase appearance
                ('light (load-theme 'modus-operandi t))
                ('dark (load-theme 'modus-vivendi t)))))

(setq org-directory "~/org")
(setq display-line-numbers-type nil)

;; native-comp
(setq comp-async-jobs-number 7
    comp-deferred-compilation t
    ;; comp-deferred-compilation-black-list '()
    ;; or it will be too annoying
    comp-async-report-warnings-errors nil)

;; UI settings
(toggle-frame-maximized)

(setq initial-scratch-message
      ";; wd's Emacs

")

(setq-default fill-column 120)
(setq-default major-mode 'text-mode)

(when (eq system-type 'darwin) ;; mac specific settings
  (setq mac-option-modifier 'meta)
  (setq mac-command-modifier 'meta)
  (global-set-key [kp-delete] 'delete-char) ;; sets fn-delete to be right-delete
)

(setq confirm-kill-emacs nil)

(use-package! uniquify
  :config
  (setq uniquify-buffer-name-style 'forward)
  (setq uniquify-separator "/")
  (setq uniquify-after-kill-buffer-p t)    ; rename after killing uniquified
  (setq uniquify-ignore-buffers-re "^\\*") ; don't muck with special buffers
  )


(use-package! centaur-tabs
  :config
  (setq centaur-tabs-set-close-button nil)
  ;; (centaur-tabs-group-by-projectile-project)
  (centaur-tabs-mode t)
  )

(use-package! nyan-mode
  :config
  (nyan-mode)
  )

(use-package! org
  :custom
  (org-export-backends '(ascii html md))
  :bind (("C-c a" . org-agenda)
         :map org-mode-map
         ("C-x n s" . org-toggle-narrow-to-subtree))
  :hook
  ((org-archive . (lambda() (org-save-all-org-buffers)))
   (org-mode . (lambda() (whitespace-mode -1)))
   )
  :config
  (setq org-archive-location "archive.org::* From %s")
  (setq org-directory "~/org/")
  (setq org-default-notes-file "~/org/notes.org")
  (setq org-agenda-files
        '("~/org"))
  (setq org-todo-keywords
        '((sequence "TODO(t)" "|" "DONE(d)" "CANCEL(c)")
          (type "READ(r)" "BUG(b)" "RESEARCH(e)" "|" "DONE(d)")
          ))

  (setq org-capture-templates
        `(("r" "Readings(todo)" entry
           (file+headline "todo.org" "Readings")
           ,(concat "* %^{Link} :reading:%^g\n"
                    ":CAPTURED: %U\n:END:\n\n"
                    "%i%?"))

          ("t" "Tasks(todo)" entry
           (file+headline "todo.org" "Tasks")
           ,(concat "* TODO %^{Title} %^g\n"
                    "SCHEDULED: %^t\n"
                    ":PROPERTIES:\n:CAPTURED: %U\n:END:\n\n"
                    "%i%?"))

          ("n" "Thoughts(notes)" entry
           (file+headline "notes.org" "Thoughts")
           ,(concat "* %^{Title} %^g\n"
                    ":PROPERTIES:\n:CAPTURED: %U\n:END:\n\n"
                    "%i%?"))

          ("j" "Daily report"
           item (file+olp+datetree "~/org/daily.org")
           "%?"
           :jump-to-captured 1
           :tree-type week
           ))
  )
  )

;; (use-package! org-super-agenda
;;   :after org
;;   :config
;;   (org-super-agenda-mode +1)
;;   (setq org-agenda-block-separator nil)
;;   ;; (set-face-attribute 'org-agenda-structure nil :height 1.5)
;;   (setq org-agenda-custom-commands
;;         '(("h" "Agenda and tasks view"
;;            ((agenda "" ((org-agenda-span 'day)
;;                         (org-agenda-overriding-header "")
;;                         (org-deadline-warning-days 0)
;;                         ;; (org-deadline-past-days 0)
;;                         (org-scheduled-warning-days 0)
;;                         ;; (org-scheduled-past-days 0)
;;                         (org-super-agenda-groups
;;                          '((:name ">>> Today <<<\n"
;;                                   :time-grid t
;;                                   :date today
;;                                   :todo "TODAY"
;;                                   :scheduled today)
;;                            (:name ">>> Overdue <<<\n"
;;                                    :time-grid t
;;                                    :scheduled past
;;                                    :deadline past
;;                                    )
;;                            ))))

;;             (agenda "" ((org-agenda-span 7)
;;                         (org-agenda-start-day "+1d")
;;                         (org-agenda-start-on-weekday nil)
;;                         (org-agenda-overriding-header "\n>>> Next 7 days <<<\n")
;;                         (org-super-agenda-groups
;;                          '((:name ""
;;                                   :time-grid t
;;                                   :date t)
;;                            ))))

;;             (alltodo "" ((org-agenda-overriding-header "\n>>> Todos <<<\n")
;;                         (org-super-agenda-groups
;;                          '((:name ""
;;                                   :and (:scheduled nil :deadline nil))
;;                            (:discard (:anything t))
;;                            ))))
;;             (tags "reading" ((org-agenda-overriding-header "\n>>> Readings <<<\n")
;;                          (org-super-agenda-groups
;;                           '((:name ""
;;                                    :and (:tag "reading" :not (:todo "DONE"))
;;                                    :order 1)
;;                             (:discard (:anything t))
;;                             ))))))))
;;   )


(use-package! deft
  :config
  (setq deft-extensions '("md" "org"))
  (setq deft-directory "~/org")
  (setq deft-recursive t)
  (setq deft-use-filename-as-title nil)
  (setq deft-use-filter-string-for-filename t)
  (setq deft-file-naming-rules
        '(;; (noslash . "-")
          (nospace . "-")
          (case-fn . downcase)))

  (setq deft-default-extension "org")
  (setq deft-text-mode 'org-mode)
  (setq deft-auto-save-interval 0)
  )

(use-package! easy-hugo
  :init
  (setq easy-hugo-basedir "~/blog/")
  (setq easy-hugo-url "https://wdicc.com")
  (setq easy-hugo-postdir "content/post")
  (setq easy-hugo-previewtime "300")
  (setq easy-hugo-default-ext ".org")
  )

;; company
(after! terraform-mode
  (set-company-backend! 'terraform-mode
    '(:seperate company-terraform company-files company-capf company-keywords company-dabbrev))
 )

(set-company-backend! '(text-mode
                        org-mode)
  '(:seperate company-ispell
    company-files
    company-yasnippet
    company-dabbrev))

(after! yaml-mode
  (set-company-backend! 'yaml-mode
    '(:seperate company-files
      company-dabbrev))
  )

(after! markdown-mode
  (set-company-backend! 'markdown-mode
    '(:seperate company-files
      company-ispell
      company-dabbrev))
  )

(after! emacs-lisp-mode
  (set-company-backend! 'emacs-lisp-mode
    '(:seperate company-files
      company-keywords
      company-yasnippet
      company-capf
      company-dabbrev))
  )

(after! lua-mode
  (set-company-backend! 'lua-mode
    '(:seperate company-files
      company-keywords
      company-yasnippet
      company-capf
      company-dabbrev))
  )

(after! ivy
  (setq ivy-use-selectable-prompt t
        ivy-magic-slash-non-match-action 'ivy-magic-slash-non-match-cd-selected)
  )

(use-package! flyspell
  :hook
  ;; disable spell check for string/text in terraform
  (terraform-mode . (lambda ()
                     (setq flyspell-prog-text-faces '(font-lock-comment-face font-lock-doc-face))))
  )

; magit
(use-package! magit
  :config
  (defun wd/get-repo-name()
    (replace-regexp-in-string
              "\\`.+github\\.com:\\(.+\\)\\.git\\'" "\\1"
              (magit-get "remote"
                         (magit-get-push-remote)
                                                 "url"))
    )

  (defun wd/visit-pull-request-url ()
    "Visit the current branch's PR on Github."
    (interactive)
    (browse-url
     (format "https://github.com/%s/pull/new/%s"
             (wd/get-repo-name)
             (magit-get-current-branch))))
  )

(use-package! evil
  :after ivy
  :config
  (setq evil-vsplit-window-right t
        evil-split-window-below t)

  (defadvice! prompt-for-buffer (&rest _)
    :after '(evil-window-split evil-window-vsplit)
    (+ivy/switch-buffer))
  )

;;
;; keybindings
;;
(defun wd/switch-with-treemacs()
  (interactive)
  (require 'treemacs)
  (if (not (eq (treemacs-current-visibility) `visible))
      (+treemacs/toggle)
    (if (eq (treemacs-get-local-window) (get-buffer-window))
        (other-window -1)
      (select-window (treemacs-get-local-window))
      )
    )
  )

(defun back-to-indentation-or-beginning (arg)
  "combine two function into one call."
  (interactive "^p")
  (setq arg (or arg 1))

  ;; Move lines first
  (when (/= arg 1)
    (let ((line-move-visual nil))
      (forward-line (1- arg))))

  (let ((orig-point (point)))
    (back-to-indentation)
    (when (= orig-point (point))
      (move-beginning-of-line 1))))

(global-set-key "\C-a" 'back-to-indentation-or-beginning)
(map! :niv "C-e" #'end-of-line)
(map! :niv "C-h" #'centaur-tabs-backward)
(map! :niv "C-l" #'centaur-tabs-forward)
(map! :niv "M-o" #'centaur-tabs-counsel-switch-group)
(map! :after vterm
      :map vterm-mode-map
      :ni "C-c" #'vterm-send-C-c
      :ni "C-c" #'vterm-send-C-c)
(map! :after magit
      :map magit-mode-map
      :n "gv" #'wd/visit-pull-request-url)
(map! :niv "C-s" #'swiper)
(map! :niv "M-w" #'kill-current-buffer)
(map! :i "M-i" #'company-complete)
(global-set-key "\C-\M-j" 'wd/switch-with-treemacs)

;; for myself
(fset 'export-org-subtree-to-html
      (kmacro-lambda-form [?\C-c ?\C-e ?\C-b ?\C-s ?h ?o] 0 "%d"))
(map! :n "<SPC>ad" #'osx-dictionary-search-pointer)
(map! :map org-mode-map
      :n "<SPC>ae" #'export-org-subtree-to-html)
(map! :n "<SPC>ah" #'easy-hugo)
