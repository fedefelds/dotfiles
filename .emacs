;; Initialize package sources
(require 'package)

(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")
                         ("elpa" . "https://elpa.gnu.org/packages/")))

(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

(require 'use-package)
(setq use-package-always-ensure t)

(setq inhibit-startup-message t)

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

(recentf-mode 1)

;; Save what you enter into minibuffer prompts
;; Use M-n and M-p for next and previous
(setq history-length 25)
(savehist-mode 1)

(setq use-dialog-box nil)

(set-face-attribute 'default nil :height 130)

(defun set-default-font-height ()
  "Prompts the user for a font height and sets it for the default face."
  (interactive)
  (let ((new-height (read-number "Enter font height (in points): ")))
    (set-face-attribute 'default nil :height (* new-height 10))))

;; Bind this function to a key combination, e.g., C-c h
(global-set-key (kbd "C-c h") 'set-default-font-height)

(load-theme 'manoj-dark t)

(use-package helpful
:config
(global-set-key (kbd "C-h f") #'helpful-callable)
(global-set-key (kbd "C-h v") #'helpful-variable)
(global-set-key (kbd "C-h k") #'helpful-key)
(global-set-key (kbd "C-h x") #'helpful-command))

(global-set-key (kbd "C-x C-b") 'ibuffer)

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
    :init
    (ivy-mode 1)
    ;; (setq ivy-initial-inputs-alist nil)
    (setq ivy-use-selectable-prompt t))

(use-package counsel
    :bind (("C-M-j" . 'counsel-switch-buffer)
           :map minibuffer-local-map
           ("C-r" . 'counsel-minibuffer-history))
    :custom
    (counsel-linux-app-format-function #'counsel-linux-app-format-function-name-only)
    :config
    (counsel-mode 1))

;; (require 'org)

(setq org-catch-invisible-edits 'show-and-error)

(setq org-blank-before-new-entry '((heading) (plain-list-item)))

;; (setq org-startup-indented t)

(setq org-todo-keywords
'((sequence "NEXT(n)" "TODO(t!)" "WAIT(w@/!)" "ONGOING(o!)" "|" "DONE(d!)" "CNCL(c@/!)")))

(setq org-tag-alist '((:startgroup . nil)
("@home" . ?h)
("@work" . ?w)
("@comp" . ?c)
("@errands" . ?e)
("@phone" . ?p)
("@anywhere" . ?a)
(:grouptags . nil)
(:endgroup . nil)))

(setq org-global-properties
'(("Effort_ALL" . "0 1 2 3 4 5 10 15 20 25 30 35 45 50 55 60 65 70 75 80 85 90 95 100 120 999")))

(setq org-startup-folded 'fold)

(global-auto-revert-mode 1)
(setq auto-revert-use-notify 1) ; same thing with set to 1 
(setq auto-revert-verbose t)

(setq org-todo-repeat-to-state "TODO")

(use-package org-roam
  :ensure t
  :custom
  (org-roam-directory (file-truename "~/Documents/org/roam"))
  ;; (org-roam-db-location (expand-file-name "org-roam.db" org-roam-directory)) 
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
         (index-file (concat roam-dir "20240613154726-index.org"))
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
      (insert ":ID:       6fd2d211-c8be-495f-b498-42cbfa191dc5\n")
      (insert ":END:\n")
      (insert "#+TITLE: Index\n")
      (dolist (name clean-names)
        (let ((file (car (seq-filter (lambda (f) (string-match-p (concat "[0-9]+-" name "\\.org") f)) files))))
          (when file
            (insert (format "** [[file:%s][%s]]\n" file name)))))))
  (message "Index file generated successfully."))

(use-package org-roam-ui
     :after org-roam
 ;;         normally we'd recommend hooking orui after org-roam, but since org-roam does not have
 ;;         a hookable mode anymore, you're advised to pick something yourself
 ;;         if you don't care about startup time, use
:hook (after-init . org-roam-ui-mode)
     :config
     (setq org-roam-ui-sync-theme t
	   org-roam-ui-follow t
	   org-roam-ui-update-on-save t
	   org-roam-ui-open-on-start nil))

(use-package org-gtd
:after org
:demand t
:custom
(org-gtd-directory "~/Documents/org/gtd")
(org-agenda-files '("~/Documents/org/gtd"))
(org-edna-use-inheritance t)
(org-gtd-organize-hooks '(org-gtd-set-area-of-focus org-set-tags-command org-set-effort org-priority))
(org-gtd-areas-of-focus '("Home" "Upskill" "Health" "Family" "Career"))
(org-gtd-update-ack "3.0.0")
(org-habit-graph-column 50)
:config
(org-gtd-mode)
:bind
(("C-c d c" . org-gtd-capture)
("C-c d e" . org-gtd-engage)
("C-c d p" . org-gtd-process-inbox)
:map org-gtd-clarify-map
("C-c c" . org-gtd-organize)))

(use-package org-wild-notifier
  :ensure t
  :init (org-wild-notifier-mode 1)
  :custom
  (alert-default-style 'libnotify)
  (org-wild-notifier-alert-time '(1 10 30))
  ;; (org-wild-notifier-keywork-whitelist '("TODO" "NEXT"))
  ;; (org-wild-notifier-keywork-whitelist nil)
  (org-wild-notifier-notification-title "Org Reminder")
  :config
  (org-wild-notifier-mode 1))

(setq org-habit-show-habits-only-for-today nil)

(setq holiday-islamic-holidays nil)
(setq holiday-oriental-holidays nil)
(setq holiday-bahai-holidays nil)

(add-hook 'calendar-mode-hook #'calendar-mark-holidays)

(eval-when-compile
  (require 'calendar)
  (require 'holidays))

(defvar holiday-austrian-holidays nil
  "Austrian holidays")

(setq holiday-austrian-holidays
      `((holiday-fixed 12 31 "Silvester")
	(holiday-fixed 1 1 "Neujahr")
	(holiday-fixed 1 6 "Heilige drei Könige")
	(holiday-fixed 2 14 "Valentinstag")
	(holiday-fixed 5 1 "Staatsfeiertag")
	(holiday-fixed 6 21 "Fête de la musique")
	(holiday-fixed 8 15 "Mariä Himmelfahrt")
	(holiday-fixed 10 26 "Nationalfeiertag")
	(holiday-fixed 11 1 "Allerheiligen")
	(holiday-fixed 12 8 "Mariä Empfängnis")
	(holiday-fixed 12 24 "Weihnachten")
	(holiday-fixed 12 25 "Christtag")
	(holiday-fixed 12 26 "Stefanitag")
        ;; variable
        (holiday-easter-etc -48 "Rosenmontag")
        (holiday-easter-etc -47 "Faschingsdienstag")
        (holiday-easter-etc -46 "Aschermittwoch")
	(holiday-easter-etc -7 "Palmsonntag")
	(holiday-easter-etc -3 "Gründonnerstag")
	(holiday-easter-etc -2 "Karfreitag")
	(holiday-easter-etc -1 "Karsamstag")
	(holiday-easter-etc 0 "Ostersonntag")
        (holiday-easter-etc 1 "Ostermontag")
        (holiday-easter-etc 39 "Christi Himmelfahrt")
        (holiday-easter-etc 49 "Pfingstsonntag")
        (holiday-easter-etc 50 "Pfingstmontag")
        (holiday-easter-etc 60 "Fronleichnam")))
(provide 'austrian-holidays)
(require 'austrian-holidays)
(setq calendar-holidays (append calendar-holidays holiday-austrian-holidays))

;; Feriados en Argentina
  ;; Enero 1         Año Nuevo
  ;; Marzo/Abril #   Semana Santa - Viernes Santo
  ;; Abril 2##       Día del Veterano y de los Caídos en la Guerra de Malvinas
  ;; Mayo 1°         Día del Trabajo
  ;; Mayo 25         Aniversario del Primer Gobierno Patrio
  ;; Junio 20###     Día de la Bandera Nacional
  ;; Julio 9         Día de la Independencia Nacional
  ;; Agosto 17###    Aniversario de la muerte del general José de San Martín
  ;; Octubre 12##    Día de la Raza
  ;; Diciembre 8     Día de la Inmaculada Concepción
  ;; Diciembre 25    Navidad

  ;; # Feriado de fecha variable.
	 
  ;; ## Feriado que si se produce un día martes o miércoles, se traslada al
  ;;    lunes anterior y, si coincide con un día jueves o viernes, se
  ;;    cumple el lunes siguiente.
	 
  ;; ### Feriado que será cumplido el día que corresponda al tercer lunes del
  ;;     mes respectivo.
  (defun movable-holiday(date)
    (let* ((week-day (calendar-day-of-week date))
	   (day (calendar-absolute-from-gregorian date)))
      ;; (format "date %s, week day %d, day %d\n" date week-day day)
      (cond ((= week-day 2)
	     (calendar-gregorian-from-absolute (- day 1)))
	    ((= week-day 3)
	     (calendar-gregorian-from-absolute (- day 2)))
	    ((= week-day 4)
	     (calendar-gregorian-from-absolute (+ day 4)))
	    ((= week-day 5)
	     (calendar-gregorian-from-absolute (+ day 3)))
	    (t date))))

  (setq arg-holidays 
	'((holiday-fixed 1 1   "Anio Nuevo") 
	  (funcall 'holiday-sexp 
	   '(movable-holiday (list 4 2 year)) "Dia de Malvinas")
	  (holiday-fixed 5 1   "Dia del Trabajo") 
	  (holiday-fixed 5 25  "Revolucion de Mayo") 
	  (holiday-float 6 1 3 "Dia de la Bandera")
	  (holiday-fixed 7 9   "Dia de la Independencia") 
	  (holiday-float 8 1 3 "Dia de José de San Martín")
	  (funcall 'holiday-sexp 
	   '(movable-holiday (list 10 12 year)) "Dia de la Raza") 
	  (holiday-fixed 12 8  "Dia de la Virgen?") 
	  (holiday-fixed 12 25 "Navidad")))
(setq calendar-holidays (append calendar-holidays arg-holidays))

;; This is needed as of Org 9.2
(require 'org-tempo)

(add-to-list 'org-structure-template-alist '("sh" . "src shell"))
(add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))
(add-to-list 'org-structure-template-alist '("py" . "src python"))
(add-to-list 'org-structure-template-alist '("awk" . "src awk :results output code :in-file ~/Documents/library/awk/"))
(add-to-list 'org-structure-template-alist '("sed" . "src sed :results output code :in-file ~/Documents/library/sed/"))
(add-to-list 'org-structure-template-alist '("hdl" . "src shell :tangle ~/git_repos/nand2tetris/projects/03/a/xxx.hdl"))
(add-to-list 'org-structure-template-alist '("fl" . "src shell :tangle ~/git_repos/nand2tetris/projects/01/xxx.fl"))

(use-package magit
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))

;; NOTE: Make sure to configure a GitHub token before using this package!
;; - https://magit.vc/manual/forge/Token-Creation.html#Token-Creation
;; - https://magit.vc/manual/ghub/Getting-Started.html#Getting-Started
(use-package forge)

(setq magit-diff-refine-hunk 'all)

(use-package evil-nerd-commenter
  :bind ("M-/" . evilnc-comment-or-uncomment-lines))

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package tex
    :ensure auctex)
(add-hook 'LaTeX-mode-hook 'turn-on-reftex)
(setq reftex-plug-into-auctex t)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(all-the-icons async auctex calfw calfw-org company-box counsel-projectile doom-modeline evil-nerd-commenter forge helpful ivy-rich lsp-ivy lsp-treemacs lsp-ui org-bullets org-download org-edna org-roam-ui python-mode pyvenv rainbow-delimiters ssh-agency which-key xclip))
 '(safe-local-variable-values
   '((TeX-command-extra-options . "-shell-escape")
     (org-download-image-dir . "~/Documents/local_pictures"))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(put 'narrow-to-region 'disabled nil)

(use-package xclip
  :config
  (xclip-mode 1))
