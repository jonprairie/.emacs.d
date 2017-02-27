;;;;;;;;
;;;; This is a .emacs file. There are many like it, but this is mine.
;;;;;;;;

    ;;;;;;
    ;;; emacs preferences
    ;;;;;;

;; enter debugger on lisp evaluation error 
(setq debug-on-error t)

;; disable menu-bar, tool-bar and scroll-bar
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

;; turn off automatic line-breaks
(auto-fill-mode -1)

;; load pleasant, dark-blue theme
(load-theme 'deeper-blue t)

;; show function definition in mode line on "hover"
(which-function-mode 1)

;; show matching parentheses
(show-paren-mode 1)

;; mark up files with defined syntaxes (like source code)
(global-font-lock-mode 't)

;; turn off back-up file generation
(setq make-backup-files nil)

;; turn off annoying alarm
(setq ring-bell-function 'ignore)

;; start emacs maximized
(setq initial-frame-alist '((fullscreen . maximized)))

;; (global-linum-mode t)
(display-time-mode t) ;?
;; (line-number-mode t)

(global-set-key (kbd "M-x") 'helm-M-x)

    ;;;;;;
    ;;; install and update packages
    ;;;;;;

;; load default package-manager
(require 'package)

;; add standard repositories 
(add-to-list
 'package-archives
 '("org" . "http://orgmode.org/elpa/") t)

(add-to-list
 'package-archives
 '("melpa" . "http://melpa.org/packages/") t)

(add-to-list
 'package-archives
 '("melpa-stable" . "http://stable.melpa.org/packages/") t)

;; not sure what the intention here is...
(setq package-enable-at-startup nil)
(package-initialize)

;; make sure installed-package listing exists
(or (file-exists-p package-user-dir)
    (package-refresh-contents))

;; load custom functions (needed here for ensure-package-installed)
(load-file "~/.emacs.d/cust-funcs/cust-funcs.el")

;; install my "standard" packages if they aren't already
					;          (ensure-package-installed
					;              'iedit
					;              'magit
					;              'evil
					;              'evil-escape
					;              'evil-surround
					;              'evil-leader
					;              'key-chord
					;              'powerline
					;              'projectile
					;              'helm
					;              'helm-projectile
					;              'company
					;              'relative-line-numbers
					;              'fill-column-indicator
					;              'caps-lock
					;              'evil-visual-mark-mode
					;	      'evil-indent-plus
					;              'use-package)
					;'evil-tabs
					;'flycheck
					;'yasnippet
					;'jabber

    ;;;;;;
    ;;; configure cobol and rexx modes
    ;;;;;;

;; load cobol-mode
(add-to-list 'load-path "~/.emacs.d/cobol-mode")
(autoload
  'cobol-mode
  "cobol-mode"
  "Major mode for highlighting COBOL files."
  t
  nil)

;; load rexx-mode
(add-to-list 'load-path "~/.emacs.d/rexx-mode")
(autoload
  'rexx-mode
  "rexx-mode"
  "Major mode for highlighting REXX files."
  t
  nil)

;; auto-load cobol/rexx mode when editing source files
(setq auto-mode-alist
      (append
       '(("\\.cob\\'" . cobol-mode)
	 ("\\.cbl\\'" . cobol-mode)
	 ("\\.cpy\\'" . cobol-mode)
	 ("\\.rexx\\'" . rexx-mode))
       auto-mode-alist))

;; truncate lines in cobol and rexx modes
(add-hook 'cobol-mode-hook (lambda () (toggle-truncate-lines)))
(add-hook 'rexx-mode-hook (lambda () (toggle-truncate-lines)))

    ;;;;;;
    ;;; configure evil mode
    ;;;;;;

(use-package evil
  :ensure t
  :config
  (evil-mode 1)

  (use-package evil-leader
    :ensure t
    :config
    (global-evil-leader-mode)
    (setq evil-leader/in-all-states 1)
    (evil-leader/set-leader "SPC")
    (evil-leader/set-key-for-mode 'eshell-mode "c" 'helm-eshell-history))

  (use-package evil-indent-plus
    :ensure t
    :config
    (evil-indent-plus-default-bindings))

  (use-package evil-surround
    :ensure t
    :config
    (global-evil-surround-mode 1))

  (setq evil-emacs-state-cursor '("red" box))
  (setq evil-normal-state-cursor '("green" box))
  (setq evil-visual-state-cursor '("orange" box))
  (setq evil-insert-state-cursor '("red" bar))
  (setq evil-replace-state-cursor '("red" bar))
  (setq evil-operator-state-cursor '("red" hollow))

  (setq evil-cross-lines t)

  (evil-define-motion evil-next-line-and-scroll-to-top (count)
    "Move the cursor count lines down, then set top of buffer to current line"
    :type line
    (evil-next-line count)
    (evil-scroll-line-to-top nil))
  
  (evil-define-motion evil-prev-line-and-scroll-to-top (count)
    "Move the cursor count lines up, then set top of buffer to current line"
    :type line
    (evil-previous-line count)
    (evil-scroll-line-to-top nil))

  (evil-def-multi-keys-list
   "ienvmourp"
   (((kbd "<f5>") 'evil-prev-buffer)
    ((kbd "<f6>") 'evil-next-buffer)
    ((kbd "C-S-H") 'shrink-window-horizontally)
    ((kbd "C-S-L") 'enlarge-window-horizontally)
    ((kbd "C-S-J") 'enlarge-window)
    ((kbd "C-S-K") 'shrink-window)
    ((kbd "<f7>") 'evil-scroll-up)
    ((kbd "S-<f7>") 'evil-goto-first-line)
    ((kbd "<f8>") 'evil-scroll-down)
    ((kbd "S-<f8>") 'evil-goto-line)))

  (evil-def-multi-keys-list
   "nvo"
   (((kbd "L") 'evil-end-of-line)
    ((kbd "H") 'evil-digit-argument-or-evil-beginning-of-line)
    ((kbd "C-m") 'evil-visual-mark-mode)
    ((kbd "m") 'evil-goto-mark)
    ((kbd "M") 'evil-set-marker)))

  (evil-def-multi-keys-list
   "env"
   (((kbd "]") 'evil-next-line-and-scroll-to-top)
    ((kbd "[") 'evil-prev-line-and-scroll-to-top)))

  (key-chord-def-list
   "ievmourp"
   (("jk" 'evil-normal-state)
    ("Jk" 'evil-normal-state)
    ("jK" 'evil-normal-state)
    ("JK" 'evil-normal-state)))

  (evil-leader/set-key
    "mx" 'helm-M-x
    "g" 'magit-status                              
    "j" 'evil-window-down                          
    "k" 'evil-window-up                            
    "h" 'evil-window-left                          
    "l" 'evil-window-right                         
    "st" 'eval-buffer  
    "ev" 'eval-last-sexp
    "p" 'helm-projectile-find-file-in-known-projects 
    "o" 'helm-buffers-list                           
    "v" 'helm-show-kill-ring                         
    "c" 'helm-comint-input-ring                      
    "i" #'(lambda ()
	    (interactive)
	    (save-mark-and-excursion
	      (mark-whole-buffer)
	      (indent-region (point) (mark))))
    "T" 'eshell
    "w" 'save-buffer
    "qq" #'(lambda ()
	    (interactive)
	    (cd "c:/users/e018462/appdata/Roaming"))
    "qw" #'(lambda ()
	    (interactive)
	    (cd "c:/Users/e018462/Documents/src")))

  ;; look into setting "jk" to these values
  (define-key evil-normal-state-map [escape] 'keyboard-quit)
  (define-key evil-visual-state-map [escape] 'keyboard-quit)
  (define-key minibuffer-local-map [escape] 'minibuffer-keyboard-quit)
  (define-key minibuffer-local-ns-map [escape] 'minibuffer-keyboard-quit)
  (define-key minibuffer-local-completion-map [escape] 'minibuffer-keyboard-quit)
  (define-key minibuffer-local-must-match-map [escape] 'minibuffer-keyboard-quit)
  (define-key minibuffer-local-isearch-map [escape] 'minibuffer-keyboard-quit)
  (global-set-key [escape] 'evil-exit-emacs-state)

  (define-key evil-insert-state-map (kbd "C-;")
    'caps-lock-mode) 

  (add-hook
   'evil-insert-state-exit-hook
   (lambda ()
     (if caps-lock-mode
	 (caps-lock-mode -1)))))

    ;;;;;;
    ;;; configure helm
    ;;;;;;

(use-package helm
  :ensure t
					;:defer 10
  :config
  (setq helm-M-x-fuzzy-match                  t
	helm-bookmark-show-location           t
	helm-buffers-fuzzy-matching           t
	helm-completion-in-region-fuzzy-match t
	helm-file-cache-fuzzy-match           t
	helm-imenu-fuzzy-match                t
	helm-mode-fuzzy-match                 t
	helm-locate-fuzzy-match               t 
	helm-quick-update                     t
	helm-recentf-fuzzy-match              t
	helm-semantic-fuzzy-match             t
	helm-split-window-in-side-p           t
	helm-echo-input-in-header-line        t)

  (helm-mode 1))

    ;;;;;;
    ;;; configure projectile
    ;;;;;;

(use-package projectile
  :ensure t
  :defer 10
  :config
  (projectile-mode t)

  (setq projectile-enable-caching t)

  (setq projectile-globally-ignored-directories
	(append '(
                  ".git"
                  ".svn"
                  "out"
                  "repl"
                  "target"
                  "venv"
		  "node_modules"
		  "platforms"
                  )
		projectile-globally-ignored-directories))
  
  (setq projectile-globally-ignored-files
	(append '(
                  ".DS_Store"
                  "*.gz"
                  "*.pyc"
                  "*.jar"
                  "*.tar.gz"
                  "*.tgz"
                  "*.zip"
		  "*.*~"
		  "*.swp"
		  "*.min.*"
                  )
		projectile-globally-ignored-files))
  (projectile-global-mode)

  (setq projectile-use-git-grep 1)
  (setq projectile-indexing-method 'alien))


(use-package helm-projectile :ensure t)
					;  :config
					;  (require 'helm-projectile))

    ;;;;;;
    ;;; configure org mode
    ;;;;;;

(use-package org
  :ensure t
  :defer  5
  :config
  (setq
   org-agenda-files
   '("c:/users/e018462/documents/src/todo.org"))
  
  (add-hook 'org-mode-hook
	    (lambda()
	      (relative-line-numbers-mode t)
	      (org-indent-mode t)))
  
  (add-to-list
   'load-path
   "~/.emacs.d/plugins/evil-org-mode")

  (use-package evil-org :ensure t)
  
					;(require 'evil-org)
  
  (setq org-todo-keywords
	'((sequence
	   "TODO"
	   "IN-PROGRESS"
	   "WAITING"
	   "|"
	   "DONE"
	   "CANCELLED")))
  
  (setq org-tag-alist
	'((:startgroup . nil)
	  ("@work" .?w)
	  ("@home" . ?h)
	  ("@errand" . ?e)
	  (:endgroup . nil)
	  ("@phone" . ?p)
	  ("@laptop" . ?l)))
  
  (setq org-agenda-text-search-extra-files '(agenda-archives))
  (setq org-enforce-todo-dependencies t)
  (setq org-log-done (quote time))
  (setq org-log-redeadline (quote time))
  (setq org-log-reschedule (quote time))
  (setq org-agenda-window-setup 'current-window)
  (setq org-agenda-restore-windows-after-quit t))

    ;;;;;;
    ;;; configure miscellaneous packages
    ;;;;;;

(use-package powerline
  :ensure t
  :config
					;(require 'powerline)
  (powerline-default-theme))

(use-package fill-column-indicator
  :ensure t
  :defer 15)

(use-package company
  :ensure t
  :defer 10
  :config 
  (global-company-mode))
					;(company-mode t) 
					;(add-hook 'after-init-hook 'global-company-mode))

(use-package relative-line-numbers
  :ensure t
  :config
  (relative-line-numbers-mode t)
  (setq relative-line-numbers-motion-function
	'forward-visible-line)
  (add-hook 'prog-mode-hook 'relative-line-numbers-mode t)
  (add-hook 'prog-mode-hook 'line-number-mode t)
  (add-hook 'rexx-mode-hook 'relative-line-numbers-mode t))
					;  (defun relative-line-numbers-default-format (offset)
					;    "The default formatting function.
					;Return the absolute value of OFFSET, converted to string."
					;    (number-to-string (abs offset)))
					;     (if (= offset 0)
					;	 (line-number-at-pos)
					;       (abs offset))))

(use-package key-chord
  :ensure t
  :config
  (key-chord-mode 1) 
  (setq key-chord-two-keys-delay 0.1))

(use-package iedit :defer 30)

(use-package magit
  :ensure t
  :defer 60)

(use-package caps-lock
  :ensure t
  :defer 10)

					;(use-package evil-tabs :defer 100)
(use-package flycheck :defer 100)
					;(use-package yasnippet :defer 90)
					;(use-package jabber :defer 100)


;;do more research on flycheck
;; flycheck
;;(package 'flycheck)
					; (add-hook 'after-init-hook #'global-flycheck-mode)

					; (after 'flycheck
					;   (setq flycheck-check-syntax-automatically '(save mode-enabled))
					;   (setq flycheck-checkers (delq 'emacs-lisp-checkdoc flycheck-checkers))
					;   (setq flycheck-checkers (delq 'html-tidy flycheck-checkers))
					;   (setq flycheck-standard-error-navigation nil))

					; (global-flycheck-mode t)

					; ;; flycheck errors on a tooltip (doesnt work on console)
					; (when (display-graphic-p (selected-frame))
					;   (eval-after-load 'flycheck
					;     '(custom-set-variables
					;       '(flycheck-display-errors-function #'flycheck-pos-tip-error-messages))))


					; (require 'jabber)
					; (setq jabber-invalid-certificate-servers '("cup1.aoins.com"))
(put 'narrow-to-region 'disabled nil)
