;; Initialize package sources
(require 'package)

(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")
                         ("elpa" . "https://elpa.gnu.org/packages/")))

(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

  ;; Initialize use-package on non-Linux platforms
(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

(setq inhibit-startup-message t)

;; Set up the visible bell
(setq visible-bell t)

(column-number-mode)
(global-display-line-numbers-mode t)

;; Disable line numbers for some modes
(dolist (mode '(org-mode-hook
                term-mode-hook
                shell-mode-hook
                treemacs-mode-hook
                eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

;; Remembering recently edited files
(recentf-mode 1)

;; Save what you enter into minibuffer prompts
;; Use M-n and M-p for next and previous
(setq history-length 25)
(savehist-mode 1)

;; Don't pop up UI dialogs when prompting
(setq use-dialog-box nil)

;; set doom-ayu-dark theme
(load-theme 'wombat t)

(use-package which-key
  :init (which-key-mode)
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 1))

(use-package ivy
    :diminish
    :bind (("C-s" . swiper)
           :map ivy-minibuffer-map
           ("TAB" . ivy-alt-done)
           ("C-l" . ivy-alt-done)
           ("C-j" . ivy-next-line)
           ("C-k" . ivy-previous-line)
           :map ivy-switch-buffer-map
           ("C-k" . ivy-previous-line)
           ("C-l" . ivy-done)
           ("C-d" . ivy-switch-buffer-kill)
           :map ivy-reverse-i-search-map
           ("C-k" . ivy-previous-line)
           ("C-d" . ivy-reverse-i-search-kill))
    :config
    (ivy-mode 1)
    (setq ivy-use-selectable-prompt t))

(use-package ivy-rich
  :init
  (ivy-rich-mode 1))

(use-package counsel
    :bind (("C-M-j" . 'counsel-switch-buffer)
           :map minibuffer-local-map
           ("C-r" . 'counsel-minibuffer-history))
    :custom
    (counsel-linux-app-format-function #'counsel-linux-app-format-function-name-only)
    :config
    (counsel-mode 1))

(use-package all-the-icons)

(use-package doom-modeline
  :init
  (doom-modeline-mode 1)
  :custom ((doom-modeline-height 15)
           (doom-modeline-icon nil)
           ))

(use-package helpful
  :custom
  (counsel-describe-function-function #'helpful-callable)
  (counsel-describe-variable-function #'helpful-variable)
  :bind
  ([remap describe-function] . counsel-describe-function)
  ([remap describe-command] . helpful-command)
  ([remap describe-variable] . counsel-describe-variable)
  ([remap describe-key] . helpful-key))

(require 'org)
  (setq org-catch-invisible-edits 1)
  (setq org-blank-before-new-entry '((heading) (plain-list-item)))
  (setq org-startup-indented nil)
  (setq org-log-done 'time)
  (setq org-startup-indented nil)

  ;; define todo-keywords
  (setq org-todo-keywords
        '((sequence "NEXT(n)" "TODO(t!)" "WAITING(w)" "ONGOING(o)" "|" "DONE(d!)" "CANC(c!)")))

  ;; define org contexts
  (setq org-tag-alist '((:startgroup . nil)
		      ("@home" . ?h)
		      ("@work" . ?w)
		      ("@comp" . ?c)
		      ("@errands" . ?e)
		      ("@phone" . ?p)
		      ("@anywhere" . ?a)
		      (:grouptags . nil)
		      (:endgroup . nil)))

;; global  #+PROPERTY: Effort_ALL 0 5 10 15 30 45 60 90 120 999
  (setq org-global-properties
      '(("Effort_ALL" . "0 5 10 15 30 45 60 90 120 999")))

;; set org-deadline-warning-days to 0
(setq org-deadline-warning-days 0)

(setq org-directory "~/Documents/org/roam")

(setq org-agenda-files (list "20231026232223-org_inbox.org" "20231026231716-org_agenda.org"
                             "20231026232325-org_projects.org" "20231026232155-org_habits.org"))

(setq org-capture-templates
      `(("i" "Inbox" entry (file "20231026232223-org_inbox.org")
         ,(concat "* TODO %?\n"
                  ":PROPERTIES:\n"
                  ":TRIGGER: next-sibling todo!(NEXT)\n"
                  ":END:\n"
                  "/Entered on/ %U"))
                  ("j" "Journal Entry"
       entry (file+datetree "20231026232259-org_journal.org")
       "* %?")
        ("h" "@home task" entry (file+headline "20231026232325-org_projects.org" "single tasks @home")  "* TODO %?\n :PROPERTIES:\n :TAGS: @home\n :END:\n /Entered on/ %U")
        ("w" "@work task" entry (file+headline "20231026232325-org_projects.org" "single tasks @work")  "* TODO %?\n :PROPERTIES:\n :TAGS: @work\n :END:\n /Entered on/ %U")
        ("c" "@computer task" entry (file+headline "20231026232325-org_projects.org" "single tasks @computer")  "* TODO %?\n :PROPERTIES:\n :TAGS: @computer\n :END:\n /Entered on/ %U")
        ("e" "@errands task" entry (file+headline "20231026232325-org_projects.org" "single tasks @errands")  "* TODO %?\n :PROPERTIES:\n :TAGS: @errands\n :END:\n /Entered on/ %U")
        ("p" "@phone task" entry (file+headline "20231026232325-org_projects.org" "single tasks @phone")  "* TODO %?\n :PROPERTIES:\n :TAGS: @phone\n :END:\n /Entered on/ %U")
        ))

(defun org-capture-inbox ()
  (interactive)
  (call-interactively 'org-store-link)
  (org-capture nil "i"))

;; use full window for org-capture
(add-hook 'org-capture-mode-hook 'delete-other-windows)

(define-key global-map (kbd "C-c a") 'org-agenda)
(define-key global-map (kbd "C-c c") 'org-capture)
(define-key global-map (kbd "C-c i") 'org-capture-inbox)

(setq org-refile-use-outline-path 'file)
 (setq org-outline-path-complete-in-steps nil)
(setq org-refile-allow-creating-parent-nodes t)
(setq org-refile-targets (quote (("20231026232325-org_projects.org" :maxlevel . 1)
                                 ("20231026231716-org_agenda.org" :maxlevel . 1)
                                 ("20231026232404-org_sdm.org" :maxlevel . 1))))

;; jump to top of the agenda after parsing
(add-hook 'org-agenda-finalize-hook #'org-agenda-goto-today)
  (setq org-agenda-custom-commands
	    '(("g" "Get Things Done (GTD)"
	       ((agenda "Agenda"
			((org-agenda-span 3)
			 (org-agenda-prefix-format "%t %s [%e] ")
			 (org-agenda-overriding-header "AGENDA")))
		(tags "+inbox"
		    ((org-agenda-prefix-format "%t %s")
		     (org-agenda-overriding-header "INBOX")))
                (tags "/ONGOING"
		      ((org-agenda-prefix-format "%t %s [%e] ")
		       (org-agenda-overriding-header "ONGOING")))
		(tags "@home/NEXT"
		      ((org-agenda-prefix-format "%t %s [%e] ")
		       (org-agenda-overriding-header "NEXT @home")))
		(tags "@work-project/NEXT"
		      ((org-agenda-prefix-format "%t %s [%e] ")
		       (org-agenda-overriding-header "NEXT @work")))
		(tags "@comp-project/NEXT"
		      ((org-agenda-prefix-format "%t %s [%e] ")
		       (org-agenda-overriding-header "NEXT @comp")))
		(tags "@errands-project/NEXT"
		    ((org-agenda-prefix-format "%t %s [%e] ")
		     (org-agenda-overriding-header "NEXT @errands")))
		(tags "@phone-project/NEXT"
		    ((org-agenda-prefix-format "%t %s [%e] ")
		     (org-agenda-overriding-header "NEXT @phone")))
  		(tags "@anywhere-project/NEXT"
		    ((org-agenda-prefix-format "%t %s [%e] ")
		     (org-agenda-overriding-header "NEXT @anywhere")))
		(tags "project/NEXT"
		      ((org-agenda-prefix-format "%t %s [%e] ")
		       (org-agenda-overriding-header "NEXT project tasks"))
		(tags "/WAITING"
		      ((org-agenda-prefix-format "%t %s [%e] ")
		       (org-agenda-overriding-header "Waiting tasks")))
		(tags "CLOSED>=\"<today>\"<-<tomorrow>"
		      ((org-agenda-prefix-format "%t %s")
		       (org-agenda-overriding-header "Completed today"))))))))

(defun org-delegate ()
  "Delegate a task by setting status to WAITING and recording assignment details."
  (interactive)
  (let ((task (nth 4 (org-heading-components)))
        (assignee (read-string "Assignee: ")))
    (if (not (string-empty-p task))
        (progn
          (org-entry-put nil "DELEGATED_TO" assignee)
          (org-entry-put nil "DELEGATED_ON" (format-time-string "[%Y-%m-%d %H:%M]"))
          (org-todo "WAITING")
          (message "Task '%s' delegated to %s on %s" task assignee (format-time-string "%Y-%m-%d %H:%M")))
      (message "Task name cannot be empty"))))

(global-set-key (kbd "C-c d") 'org-delegate)

(org-babel-do-load-languages
    'org-babel-load-languages
    '((python . t)
      (latex . t)
      (shell . t)
      (awk . t)
      (sed . t)
      ))

(add-hook 'org-mode-hook
        (lambda ()
          (if (string-match-p ".*\\.org\\'" (or (buffer-file-name) ""))
              (auto-revert-mode 1))))

(add-to-list 'org-modules 'org-habit t)

(setq org-log-into-drawer t)

(setq org-habit-show-habits-only-for-today nil)

(defun org-clock-todo-change ()
  "Clock in/out when task state changes to/from ONGOING"
  (if (string= org-state "ONGOING")
      (org-clock-in)
    (org-clock-out nil t)))

(add-hook 'org-after-todo-state-change-hook 'org-clock-todo-change)

(use-package org-edna
  :ensure t
  :config
  (org-edna-mode))

(defun org-babel-tangle-file ()
  (let ((org-confirm-babel-evaluate nil))
    (org-babel-tangle)))

(add-hook 'org-mode-hook (lambda () (add-hook 'after-save-hook #'org-babel-tangle-file)))

(use-package org-roam
  :ensure t
  :custom
  (org-roam-directory "~/Documents/org/roam")
  (org-roam-completion-everywhere t)
  (org-roam-db-autosync-mode)
  :bind (("C-c n l" . org-roam-buffer-toggle)
         ("C-c n o" . org-id-get-create)
         ("C-c n t" . org-roam-tag-add)
         ("C-c n T" . org-roam-node-find-and-tag)
         ("C-c n a" . org-roam-alias-add)
         ("C-c n f" . org-roam-node-find)
         ("C-c n i" . org-roam-node-insert))
  :config
  (org-roam-setup))

(defun generate-org-roam-index ()
  "Generate an index.org file for Org Roam based on files in the configured directory."
  (interactive)
  (let* ((roam-dir "~/Documents/org/roam/")
         (file-pattern "^[0-9]+-[a-z_]+\\.org$")
         (files (directory-files roam-dir t file-pattern))
         (index-file (concat roam-dir "20231003135905-index.org"))
         (clean-names '()))
    ;; Clean file names
    (dolist (filename files)
      (when (string-match-p "/[0-9]+-[a-z_]+\\.org$" filename)
        (let* ((basename (file-name-nondirectory filename))
               (clean-name (replace-regexp-in-string "[0-9]+-\\(.+\\)\\.org$" "\\1" basename)))
          (push clean-name clean-names))))
    (setq clean-names (sort clean-names #'string<))
    ;; Generate index.org file
    (with-temp-file index-file
      (insert ":PROPERTIES:\n")
      (insert ":ID:       63b47519-3495-4a15-b748-fb15d49f9209\n")
      (insert ":END:\n")
      (insert "#+TITLE: index\n")
      (dolist (name clean-names)
        (let ((file (car (seq-filter (lambda (f) (string-match-p (concat "[0-9]+-" name "\\.org") f)) files))))
          (when file
            (insert (format "** [[file:%s][%s]]\n" file name)))))))
  (message "Index file generated successfully."))

(setq org-element-cache-persistent 'nil)

(use-package org-roam-ui
    :after org-roam
;;         normally we'd recommend hooking orui after org-roam, but since org-roam does not have
;;         a hookable mode anymore, you're advised to pick something yourself
;;         if you don't care about startup time, use
;;  :hook (after-init . org-roam-ui-mode)
    :config
    (setq org-roam-ui-sync-theme t
          org-roam-ui-follow t
          org-roam-ui-update-on-save t
          org-roam-ui-open-on-start t))

(defun org-roam-node-find-and-tag ()
  "Run org-roam-node-find and then tag the current note."
  (interactive)
  (org-roam-node-find)
  (org-roam-tag-add (completing-read-multiple "Tag: " (org-roam-tag-completions))))

(defun dired-copy-images-links ()
  "Works only in dired-mode, put in kill-ring,
ready to be yanked in some other org-mode file,
the links of marked image files using file-name-base as #+CAPTION.
If no file marked then do it on all images files of directory.
No file is moved nor copied anywhere.
This is intended to be used with org-redisplay-inline-images."
  (interactive)
  (if (derived-mode-p 'dired-mode)                           ; if we are in dired-mode
      (let* ((marked-files (dired-get-marked-files))         ; get marked file list
             (number-marked-files                            ; store number of marked files
              (string-to-number                              ; as a number
               (dired-number-of-marked-files))))             ; for later reference
        (when (= number-marked-files 0)                      ; if none marked then
          (dired-toggle-marks)                               ; mark all files
          (setq marked-files (dired-get-marked-files)))      ; get marked file list
        (message "Files marked for copy")                    ; info message
        (dired-number-of-marked-files)                       ; marked files info
        (kill-new "\n")                                      ; start with a newline
        (dolist (marked-file marked-files)                   ; walk the marked files list
          (when (org-file-image-p marked-file)               ; only on image files
            (kill-append                                     ; append image to kill-ring
             (concat "#+CAPTION: "                           ; as caption,
                     (file-name-base marked-file)            ; use file-name-base
                     "\n[[file:" marked-file "]]\n\n") nil))) ; link to marked-file
        (when (= number-marked-files 0)                      ; if none were marked then
          (dired-toggle-marks)))                             ; unmark all
    (message "Error: Does not work outside dired-mode")      ; can't work not in dired-mode
    (ding)))                                                 ; error sound

(use-package calfw
  :ensure t
  :config
  ;; Additional configuration can go here
)

;; This is needed as of Org 9.2
(require 'org-tempo)

(add-to-list 'org-structure-template-alist '("sh" . "src shell"))
(add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))
(add-to-list 'org-structure-template-alist '("py" . "src python"))
(add-to-list 'org-structure-template-alist '("awk" . "src awk :results output code :in-file ~/Documents/library/awk/"))
(add-to-list 'org-structure-template-alist '("sed" . "src sed :results output code :in-file ~/Documents/library/sed/"))
(add-to-list 'org-structure-template-alist '("hdl" . "src hdl :tangle /home/fede/Documents/lectures/nand2tetris/software/projects/01/xxx.hdl"))

(defun efs/lsp-mode-setup ()
  (setq lsp-headerline-breadcrumb-segments '(path-up-to-project file symbols))
  (lsp-headerline-breadcrumb-mode))

(use-package lsp-mode
  :commands (lsp lsp-deferred)
  :hook (lsp-mode . efs/lsp-mode-setup)
  :init
  (setq lsp-keymap-prefix "C-c l")  ;; Or 'C-l', 's-l'
  :config
  (lsp-enable-which-key-integration t))

(use-package lsp-ui
  :hook (lsp-mode . lsp-ui-mode)
  :custom
  (lsp-ui-doc-position 'bottom))

(use-package lsp-treemacs
  :after lsp)

(use-package lsp-ivy)

;; (use-package dap-mode
;;   ;; Uncomment the config below if you want all UI panes to be hidden by default!
;;   ;; :custom
;;   ;; (lsp-enable-dap-auto-configure nil)
;;   ;; :config
;;   ;; (dap-ui-mode 1)

;;   :config
;;   ;; Set up Node debugging
;;   ;;(require 'dap-node)
;;   ;;(dap-node-setup) ;; Automatically installs Node debug adapter if needed

;;   ;; Set up Pyton debugging
;;   (require 'dap-python)
;;   ;; if you installed debugpy, you need to set this
;;   ;; https://github.com/emacs-lsp/dap-mode/issues/306
;;   (setq dap-python-debugger 'debugpy)

;;   ;; Bind `C-c l d` to `dap-hydra` for easy access
;;   (general-define-key
;;     :keymaps 'lsp-mode-map
;;     :prefix lsp-keymap-prefix
;;     "d" '(dap-hydra t :wk "debugger")))

(use-package python-mode
  :ensure t
  :hook (python-mode . lsp-deferred)
  :custom
  ;; NOTE: Set these if Python 3 is called "python3" on your system!
  (python-shell-interpreter "python3")
  (dap-python-executable "python3")
  (dap-python-debugger 'debugpy)
  :config
  (require 'dap-python))

(use-package pyvenv
  :config
  (pyvenv-mode 1))

(use-package company
  :after lsp-mode
  :hook (lsp-mode . company-mode)
  :bind (:map company-active-map
         ("<tab>" . company-complete-selection))
        (:map lsp-mode-map
         ("<tab>" . company-indent-or-complete-common))
  :custom
  (company-minimum-prefix-length 1)
  (company-idle-delay 0.0))

(use-package company-box
  :hook (company-mode . company-box-mode))

(use-package projectile
  :diminish projectile-mode
  :config (projectile-mode)
  :custom ((projectile-completion-system 'ivy))
  :bind-keymap
  ("C-c p" . projectile-command-map)
  :init
  ;; NOTE: Set this to the folder where you keep your Git repos!
  (when (file-directory-p "~/Projects/Code")
    (setq projectile-project-search-path '("~/Projects/Code")))
  (setq projectile-switch-project-action #'projectile-dired))

(use-package counsel-projectile
  :config (counsel-projectile-mode))

(use-package magit
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))

;; NOTE: Make sure to configure a GitHub token before using this package!
;; - https://magit.vc/manual/forge/Token-Creation.html#Token-Creation
;; - https://magit.vc/manual/ghub/Getting-Started.html#Getting-Started
(use-package forge)

(use-package evil-nerd-commenter
  :bind ("M-/" . evilnc-comment-or-uncomment-lines))

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(add-hook 'org-agenda-finalize-hook #'beginning-of-buffer)
(add-hook 'org-agenda-finalize-hook #'org-agenda-goto-today)

(setq test_var 150)

(when (string= system-name "archie")
  (load-file (expand-file-name "~/git_repos/dotfiles/.config/.emacs_archie")))

(when (string= system-name "PF4PZYFJ")
    (load-file (expand-file-name "~/git_repos/dotfiles/.config/.emacs_ims")))

(when (string= system-name "debian")
  (load-file (expand-file-name "~/git_repos/dotfiles/.config/.emacs_ims")))
