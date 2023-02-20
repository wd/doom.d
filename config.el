;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

(setq user-full-name "wd"
      user-mail-address "wd@wdicc.com")

(setq doom-font (font-spec :family "FiraCode Nerd Font Mono" :size 12)
      doom-variable-pitch-font (font-spec :family "FiraCode Nerd Font Mono")
      doom-unicode-font (font-spec :family "PingFang SC")
      doom-big-font (font-spec :family "FiraCode Nerd Font Mono" :size 19))

;; (setq doom-theme 'doom-one)
;; (setq doom-theme 'modus-operandi)


(if (daemonp)
    (setq server-name "term")
  (setq server-name "server")
  )

(if (not (display-graphic-p))
    (setq doom-theme 'modus-vivendi)
  (add-hook 'ns-system-appearance-change-functions
            #'(lambda (appearance)
                (mapc #'disable-theme custom-enabled-themes)
                (pcase appearance
                  ('light (load-theme 'modus-operandi t))
                  ('dark (load-theme 'modus-vivendi t)))))
  )

(add-to-list 'default-frame-alist '(undecorated-round . t))

;;(set-face-attribute 'modus-themes-completion-selected nil :background "#2986cc")

(setq org-directory "~/org")
(setq display-line-numbers-type nil)

; disable Warning: docstring has wrong usage of unescaped single quotes
(setq text-quoting-style 'grave)
(setq byte-compile-warnings
      '(not
        docstrings
        ))

(setq use-dialog-box nil)

;; native-comp
(setq comp-async-jobs-number 7
    comp-deferred-compilation t
    ;; comp-deferred-compilation-black-list '()
    ;; or it will be too annoying
    comp-async-report-warnings-errors nil)

;; UI settings
;;(toggle-frame-maximized)

(setq initial-scratch-message
      ";; wd's Emacs

")

(remove-hook 'tty-setup-hook #'xterm-mouse-mode)

(setq-default fill-column 120)
(setq-default major-mode 'text-mode)
(setq ispell-dictionary "en")

(when (eq system-type 'darwin) ;; mac specific settings
  (setq mac-option-modifier 'meta)
  (setq mac-command-modifier 'meta)
  (global-set-key [kp-delete] 'delete-char) ;; sets fn-delete to be right-delete
)

(setq confirm-kill-emacs nil)

(setq +format-on-save-enabled-modes '(go-mode))

(use-package! uniquify
  :config
  (setq uniquify-buffer-name-style 'forward)
  (setq uniquify-separator "/")
  (setq uniquify-after-kill-buffer-p t)    ; rename after killing uniquified
  (setq uniquify-ignore-buffers-re "^\\*") ; don't muck with special buffers
  )

;; (use-package! persp-mode
;;   :config
;;   )

(after! persp-mode
  (setq persp-emacsclient-init-frame-behaviour-override "main")
  (setq +workspaces-on-switch-project-behavior t)
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
        '("~/org"
          "~/org/roam"))
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

(use-package! org-super-agenda
  :after org
  :config
  (org-super-agenda-mode +1)
  (setq org-agenda-block-separator nil)
  ;; (set-face-attribute 'org-agenda-structure nil :height 1.5)
  (setq org-agenda-custom-commands
        '(("h" "Agenda and tasks view"
           ((agenda "" ((org-agenda-span 'day)
                        (org-agenda-overriding-header "")
                        (org-deadline-warning-days 0)
                        ;; (org-deadline-past-days 0)
                        (org-scheduled-warning-days 0)
                        ;; (org-scheduled-past-days 0)
                        (org-super-agenda-groups
                         '((:name ">>> Today <<<\n"
                                  :time-grid t
                                  :date today
                                  :todo "TODAY"
                                  :scheduled today)
                           (:name ">>> Overdue <<<\n"
                                   :time-grid t
                                   :scheduled past
                                   :deadline past
                                   )
                           ))))

            (agenda "" ((org-agenda-span 7)
                        (org-agenda-start-day "+1d")
                        (org-agenda-start-on-weekday nil)
                        (org-agenda-overriding-header "\n>>> Next 7 days <<<\n")
                        (org-super-agenda-groups
                         '((:name ""
                                  :time-grid t
                                  :date t)
                           ))))

            (alltodo "" ((org-agenda-overriding-header "\n>>> Todos <<<\n")
                        (org-super-agenda-groups
                         '((:name ""
                                  :and (:scheduled nil :deadline nil))
                           (:discard (:anything t))
                           ))))
            (tags "reading" ((org-agenda-overriding-header "\n>>> Readings <<<\n")
                         (org-super-agenda-groups
                          '((:name ""
                                   :and (:tag "reading" :not (:todo "DONE"))
                                   :order 1)
                            (:discard (:anything t))
                            ))))))))
  )

;; (use-package! org-download
;;   :custom
;;   (org-download-method 'directory)
;;   :init
;;   (setq-default org-download-image-dir "./pics"
;;                 org-download-heading-lvl nil)
;;   )

;; (use-package! deft
;;   :config
;;   (setq deft-extensions '("md" "org"))
;;   (setq deft-directory "~/org")
;;   (setq deft-recursive t)
;;   (setq deft-use-filename-as-title nil)
;;   (setq deft-use-filter-string-for-filename t)
;;   (setq deft-file-naming-rules
;;         '(;; (noslash . "-")
;;           (nospace . "-")
;;           (case-fn . downcase)))

;;   (setq deft-default-extension "org")
;;   (setq deft-text-mode 'org-mode)
;;   (setq deft-auto-save-interval 0)
;;   )

(use-package! easy-hugo
  :init
  (setq easy-hugo-basedir "~/blog/")
  (setq easy-hugo-url "https://wdicc.com")
  (setq easy-hugo-postdir "content/posts")
  (setq easy-hugo-previewtime "300")
  (setq easy-hugo-default-ext ".org")
  )

;; ;; ;; company
;; (after! terraform-mode
;;   (set-company-backend! 'terraform-mode
;;     '(:seperate company-terraform company-files company-capf company-keywords company-dabbrev))
;;  )

;; (set-company-backend! '(text-mode
;;                         org-mode)
;;   '(:seperate company-ispell
;;     company-files
;;     company-yasnippet
;;     company-dabbrev))

;; (after! yaml-mode
;;   (set-company-backend! 'yaml-mode
;;     '(:seperate company-files
;;       company-dabbrev))
;;   )

;; (after! markdown-mode
;;   (set-company-backend! 'markdown-mode
;;     '(:seperate company-files
;;       company-ispell
;;       company-dabbrev))
;;   )

;; (after! emacs-lisp-mode
;;   (set-company-backend! 'emacs-lisp-mode
;;     '(:seperate company-files
;;       company-keywords
;;       company-yasnippet
;;       company-capf
;;       company-dabbrev))
;;   )

;; (after! lua-mode
;;   (set-company-backend! 'lua-mode
;;     '(:seperate company-files
;;       company-keywords
;;       company-yasnippet
;;       company-capf
;;       company-dabbrev))
;;   )

;; (after! ivy
;;   (setq ivy-use-selectable-prompt t
;;         ivy-magic-slash-non-match-action 'ivy-magic-slash-non-match-cd-selected)
;;   )

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
  ;;:after ivy
  :config
  (setq evil-vsplit-window-right t
        evil-split-window-below t)

(use-package! evil-snipe
  :custom
  (evil-snipe-spillover-scope 'buffer))

  ;; (defadvice! prompt-for-buffer (&rest _)
  ;;   :after '(evil-window-split evil-window-vsplit)
  ;;   (+ivy/switch-buffer))
  )

(use-package! doom-modeline
  :config
  (setq doom-modeline-persp-name t)
  (setq doom-modeline-display-default-persp-name t)
  )

;; (use-package! lsp-mode
;;   :config
;;   (setq lsp-go-library-directories `("/Users/wd/go/pkg/mod" "/usr/local/Cellar/go"))
;;   )
;

;; (defun local/lsp-bridge-get-pyright-config (project-path filepath)
;;   (let* ((json-object-type 'plist)
;;          (custom-dir (expand-file-name ".cache/lsp-bridge/pyright" user-emacs-directory))
;;          (custom-config (expand-file-name "pyright.json" custom-dir))
;;          (default-config (json-read-file (expand-file-name "../straight/repos/lsp-bridge/langserver/pyright.json" user-emacs-directory)))
;;          (settings (plist-get default-config :settings))
;;          )

;;     (message (concat "----------" project-path "/venv"))
;;     (plist-put settings :python (list :pythonPath (f-expand "bin/python" (concat project-path "/venv"))))
;;     (make-directory (file-name-directory custom-config) t)

;;     (with-temp-file custom-config
;;       (insert (json-encode default-config)))

;;     custom-config))

;; (use-package! pyvenv
;;   :config
;;   (add-hook 'pyvenv-post-activate-hooks
;;           (lambda ()
;;             (lsp-bridge-restart-process)))
;;   )

;; (use-package! lsp-bridge
;;   :config
;;   (add-hook 'python-mode-hook (lambda ()
;;                                 (setq-local lsp-bridge-get-single-lang-server-by-project 'local/lsp-bridge-get-pyright-config)))
;;   )


;; (defun wd/set-python-path ()
;;     (let* ((my-python-path (concat (projectile-project-root) "venv/bin")))
;;          (if (file-directory-p my-python-path)
;;              (progn ()
;;                (setenv "PATH" (concat my-python-path ":" (getenv "PATH")))
;;                ;;(add-to-list 'exec-path my-python-path)
;;                )))
;;     )

;; (use-package! python
;;   :hook (python-mode . wd/set-python-path)
;; )
;;


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

(use-package! treemacs
  :custom
  (treemacs-read-string-input 'from-minibuffer)
  )

;; (use-package! persp-mode
;;   :config
;;   (defun wd/update-neotree-root (name &optional auto-create-p)
;;     (if (cl-loop for win in (window-list)
;;                  when (string=
;;                        (buffer-name (window-buffer win))
;;                        neo-buffer-name)
;;                  collect win)

;;         ;; runnig with a timer ensures that any other post-processing is finished after a perspective
;;         ;; was run since commands like `spacemacs/helm-persp-switch-project' first create a perspective
;;         ;; and only afterwards select the file to display
;;         (run-with-timer
;;          0.1 nil
;;          (lambda ()
;;            (message (format "wd root after timer: %s" (projectile-project-root)))
;;            (let ((cur-win (get-buffer-window)))
;;              (save-excursion
;;                (neo-open-dir (projectile-project-root))
;;                (select-window cur-win)
;;              ))
;;            ))))
;;   (advice-add '+workspace-switch :after #'wd/update-neotree-root)
;;   )

(defun wd/switch-with-neotree()
  (interactive)
  (if (not (neo-global--window-exists-p))
      (+neotree/open)
    (if (eq (neo-global--get-window) (get-buffer-window))
        (other-window -1)
      (select-window (neo-global--get-window))
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


;; (defun formatted-copy ()
;;   "Export region to HTML, and copy it to the clipboard."
;;   (interactive)
;;   (save-window-excursion
;;     (let* ((buf (org-export-to-buffer 'html "*Formatted Copy*" nil t t t))
;;            (html (with-current-buffer buf (buffer-string))))
;;       (with-current-buffer buf
;;         (shell-command-on-region
;;          (point-min)
;;          (point-max)
;;          "htmlcopy"))
;;       (kill-buffer buf))))

;; (map! :map org-mode-map
;;       :nv "<SPC>ae" #'formatted-copy)

(global-set-key "\C-a" 'back-to-indentation-or-beginning)
(map! :niv "C-e" #'end-of-line)
;; (map! :niv "C-h" #'previous-buffer)
;; (map! :niv "C-l" #'next-buffer)
(map! :niv "C-h" #'+tabs:previous-or-goto)
(map! :niv "C-l" #'+tabs:next-or-goto)
(map! :niv "M-o" #'+workspace/switch-to)
(map! :niv "C-M-p" #'other-window)
(map! :niv "C-M-n" (lambda (other-window -1)))
(map! :after vterm
      :map vterm-mode-map
      :ni "C-c" #'vterm-send-C-c)
(map! :after magit
      :map magit-mode-map
      :n "gv" #'wd/visit-pull-request-url)
(map! :niv "C-s" #'consult-line)
(map! :niv "M-w" #'kill-current-buffer)
(global-set-key "\C-\M-m" 'wd/switch-with-treemacs)

;; (fset 'export-org-subtree-to-html
;;       (kmacro-lambda-form [?\C-c ?\C-e ?\C-b ?\C-s ?h ?o] 0 "%d"))
;; (map! :map org-mode-map
;;       :n "<SPC>ae" #'export-org-subtree-to-html)

(map! :n "<SPC>dd" #'osx-dictionary-search-pointer)
(map! :n "<SPC>dh" #'easy-hugo)
(map! :n "<SPC>sa" #'consult-flycheck)
