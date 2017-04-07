;;;;;;;;
;;;; This is a .emacs file. There are many like it, but this is mine.
;;;;;;;;


;;; emacs preferences


(setq debug-on-error t)

(set-face-attribute 'default nil :family "Consolas")
(set-fontset-font t 'unicode (font-spec :name "Segoe UI Symbol") nil 'append)

(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

;; turn off automatic line-breaks
(auto-fill-mode -1)

(load-theme 'deeper-blue t)

;; show function definition in mode line on cursor "hover"
(which-function-mode 1)

(show-paren-mode 1)

;; mark up files with defined syntaxes (like source code)
(global-font-lock-mode 't)

(setq make-backup-files nil)

(setq ring-bell-function 'ignore)

(setq initial-frame-alist '((fullscreen . maximized)))

(display-time-mode t)

;; replace emacs M-x with helm-M-x
(global-set-key (kbd "M-x") 'helm-M-x)

(require 'zone)
(zone-when-idle 240)


;;; mode line customization


(column-number-mode t)
(line-number-mode t)


;;; install and update packages


(require 'package)
(setq package-enable-at-startup nil)

(add-to-list
 'package-archives
 '("elpy" . "https://jorgenschaefer.github.io/packages/"))

(add-to-list
 'package-archives
 '("org" . "http://orgmode.org/elpa/") t)

(add-to-list
 'package-archives
 '("melpa" . "http://melpa.org/packages/") t)

(add-to-list
 'package-archives
 '("marmalade" . "http://marmalade-repo.org/packages/") t)

(add-to-list
 'package-archives
 '("melpa-stable" . "http://stable.melpa.org/packages/") t)

(add-to-list
 'package-archives
 '("gnu" . "http://elpa.gnu.org/packages/") t)

(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))
(require 'diminish)
;;(require 'bind-key)

;; load custom functions 
(load-file "~/.emacs.d/cust-funcs/cust-funcs.el")

;; install my "standard" packages if they aren't already
;;          (ensure-package-installed
;;              'iedit
;;              'magit
;;              'evil
;;              'evil-escape
;;              'evil-surround
;;              'evil-leader
;;              'key-chord
;;              'powerline
;;              'projectile
;;              'helm
;;              'helm-projectile
;;              'company
;;              'relative-line-numbers
;;              'fill-column-indicator
;;              'caps-lock
;;              'evil-visual-mark-mode
;;	      'evil-indent-plus
;;              'use-package)
;;'evil-tabs
;;'flycheck
;;'yasnippet
;;'jabber


;;; configure cobol and rexx modes


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


;;; configure evil mode


(use-package evil
  :ensure t
  :diminish undo-tree-mode
  :config
  (evil-mode 1)

  (use-package evil-leader
    :ensure t
    :diminish evil-leader-mode
    :config
    (global-evil-leader-mode)
    (setq evil-leader/in-all-states 1)
    (evil-leader/set-leader "SPC")
    (evil-leader/set-key-for-mode 'eshell-mode "c" 'helm-eshell-history))


  ;; ii: A block of text with the same or higher indentation.
  ;; ai: The same as ii, plus whitespace.
  ;; iI: A block of text with the same or higher indentation, including the first line above with less indentation.
  ;; aI: The same as iI, plus whitespace.
  ;; iJ: A block of text with the same or higher indentation, including the first line above and below with less indentation.
  ;; aJ: The same as iJ, plus whitespace.
  (use-package evil-indent-plus
    :ensure t
    :config
    (evil-indent-plus-default-bindings))

  (use-package evil-surround
    :ensure t
    :config
    (global-evil-surround-mode 1))

  (use-package evil-commentary
    :ensure t
    :config
    (evil-commentary-mode))

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
    ((kbd "[") 'evil-prev-line-and-scroll-to-top)
    ((kbd "s") nil)
    ((kbd "sj") 'evil-scroll-down)
    ((kbd "sJ") 'evil-goto-line)
    ((kbd "sk") 'evil-scroll-up)
    ((kbd "sK") 'evil-goto-first-line)
    ((kbd "sh") 'evil-scroll-left)
    ((kbd "sl") 'evil-scroll-right)))

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

  (evil-def-multi-keys-list
   "n"
   (((kbd "U") 'undo-tree-redo)))

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
	 (caps-lock-mode -1))))

  (add-hook
   'evil-insert-state-entry-hook
   (lambda ()
     (if (equal major-mode 'cobol-mode)
	 (caps-lock-mode t))))

  (defun evil-vblock-perf-advice (old-func)
    "turn off relative-line-number mode and/or line-number mode when repeating insert commands (like in visual block mode, for instance). then turn them back on, if they were on originally.

a bit hacky, but this substantially improves performance."
    (let ((old-relative-lines relative-line-numbers-mode)
	  (old-lines line-number-mode))
      (if old-relative-lines
	  (relative-line-numbers-mode -1))
      (if old-lines
	  (line-number-mode -1))
      (funcall old-func)
      (if old-relative-lines
	  (relative-line-numbers-mode 1))
      (if old-lines
	  (line-number-mode 1))))

  (advice-add 'evil-cleanup-insert-state :around 'evil-vblock-perf-advice))

;; (use-package evil-snipe
;;   :diminish evil-snipe-local-mode
;;   ;;:ensure t
;;   :after evil
;;   :init
;;   (evil-snipe-mode 1)
;;   (setq evil-snipe-scope 'buffer
;; 	evil-snipe-repeat-scope 'buffer)
;;   (add-hook 'magit-mode-hook 'turn-off-evil-snipe-override-mode))


;;; configure helm


(use-package helm
  :ensure t
  :diminish helm-mode
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


;;; configure projectile


(use-package projectile
  :ensure t
  :defer t
  :init
  (projectile-mode t)
  (use-package helm-projectile
    :ensure t
    :defer t
    :after helm
    :config
    (helm-projectile-on))

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
  (setq projectile-indexing-method 'alien)
  (setq-default projectile-mode-line
		'(:eval (format " [%s]" (projectile-project-name)))))


;;; configure org mode


(use-package org
  :ensure t
  :defer  t
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

  ;; syntax highlighting for code blocks
  (setq org-src-fontify-natively t)

  (use-package evil-org :ensure t)
  
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


;;; configure miscellaneous packages


;;(use-package powerline
;;  :ensure t
;;  :config
;;  (setq
;;   powerline-height (truncate (* 1.0 (frame-char-height)))
;;   powerline-default-separator 'utf-8))
;;   ;;(powerline-default-theme))


;;(use-package airline-themes
;;  :ensure t
;;  :config
;;  (setq powerline-utf-8-separator-left      #xe0b0
;;	powerline-utf-8-separator-right       #xe0b2
;;	airline-utf-glyph-separator-left      #xe0b0
;;	airline-utf-glyph-separator-right     #xe0b2
;;	airline-utf-glyph-subseparator-left   #xe0b1
;;	airline-utf-glyph-subseparator-right  #xe0b3
;;	airline-utf-glyph-branch              #xe0a0
;;	airline-utf-glyph-readonly            #xe0a2
;;	airline-utf-glyph-linenumber          #xe0a1)
;;  (load-theme 'airline-light t))


(use-package fill-column-indicator
  :ensure t
  :defer 15
  :config
  (setq-default fill-column 72))


(defun custom-company-complete-number (n)
  "if the company tooltip is visible, complete the candidate designated by N. Otherwise, call the function that is globally bound to N twice. 

this is a pretty hacky solution, I should probably clean it up a bit."
  (if (company-tooltip-visible-p)
      (company-complete-number n)
    (funcall (global-key-binding (number-to-string (if (= n 10) 0 n))) 2)))

(use-package company
  :ensure t
  :defer t
  :diminish company-mode
  :init (add-hook 'after-init-hook 'global-company-mode)
  :config 
  (setq company-idle-delay              .2
  	company-minimum-prefix-length   2
  	company-show-numbers            t
  	company-tooltip-limit           20
  	company-dabbrev-downcase        nil)
  (key-chord-def-list
   "i"
   (("11" '(lambda () (interactive) (custom-company-complete-number 1)))
    ("22" '(lambda () (interactive) (custom-company-complete-number 2)))
    ("33" '(lambda () (interactive) (custom-company-complete-number 3)))
    ("44" '(lambda () (interactive) (custom-company-complete-number 4)))
    ("55" '(lambda () (interactive) (custom-company-complete-number 5)))
    ("66" '(lambda () (interactive) (custom-company-complete-number 6)))
    ("77" '(lambda () (interactive) (custom-company-complete-number 7)))
    ("88" '(lambda () (interactive) (custom-company-complete-number 8)))
    ("99" '(lambda () (interactive) (custom-company-complete-number 9)))
    ("00" '(lambda () (interactive) (custom-company-complete-number 10))))))


(use-package relative-line-numbers
  :ensure t
  :config
  (relative-line-numbers-mode t)
  (setq relative-line-numbers-motion-function
	'forward-visible-line)
  (add-hook 'prog-mode-hook 'relative-line-numbers-mode t)
  (add-hook 'prog-mode-hook 'line-number-mode t)
  (add-hook 'rexx-mode-hook 'relative-line-numbers-mode t)
  (add-hook 'dired-mode-hook 'relative-line-numbers-mode t))
;;  (defun relative-line-numbers-default-format (offset)
;;    "The default formatting function.
;;Return the absolute value of OFFSET, converted to string."
;;    (number-to-string (abs offset)))
;;     (if (= offset 0)
;;	 (line-number-at-pos)
;;       (abs offset))))


(use-package key-chord
  :ensure t
  :config
  (key-chord-mode 1) 
  (setq key-chord-two-keys-delay 0.075))


(use-package iedit
  :defer t)


(use-package magit
  :ensure t
  :defer t
  :config
  (setq magit-commit-show-diff nil
	magit-revert-buffers 1))


(use-package caps-lock
  :ensure t
  :defer t)


(use-package esup
  ;; :ensure t
  :defer t)


(use-package elpy
  :ensure t
  :defer 300
  :config
  (elpy-enable))

(use-package ov
  :ensure t)

;; Diminish extraneous info in the modeline
(diminish 'abbrev-mode)
(defun sk/diminish-auto-revert ()
  "Diminishes the 'auto-revert-mode' in the mode line."
  (interactive)
  (diminish 'auto-revert-mode ""))
(add-hook 'auto-revert-mode-hook 'sk/diminish-auto-revert)

					;(defmacro rename-major-mode (package-name mode new-name)
					;  "Renames a major mode."
					;  `(eval-after-load ,package-name
					;     '(defadvice ,mode (after rename-modeline activate)
					;	(setq mode-name ,new-name))))


					;(rename-major-mode "python" python-mode "π")
					;(rename-major-mode "shell" shell-mode "🐚")
					;(rename-major-mode "org" org-mode "📓") 
					;(rename-major-mode "org-agenda" org-agenda-mode "📅")
					;(rename-major-mode "lisp" lisp-mode "λ")
					;(rename-major-mode "cobol" cobol-mode "ಠ_ಠ")


(add-hook 'python-mode-hook (lambda () (setq mode-name "π")))
(add-hook 'lisp-mode-hook (lambda () (setq mode-name "λ")))
(add-hook 'emacs-lisp-mode-hook (lambda () (setq mode-name "ϵλ")))
(add-hook 'cobol-mode-hook (lambda () (setq mode-name "ಠ_ಠ")))
(add-hook 'eshell-mode-hook (lambda () (setq mode-name "🐚")))
(add-hook 'shell-mode-hook (lambda () (setq mode-name "🐚")))
(add-hook 'org-mode-hook (lambda () (setq mode-name "📓")))
(add-hook 'org-agenda-mode-hook (lambda () (setq mode-name "📅")))


;;(use-package evil-tabs :defer 100)
;;(use-package flycheck :defer 100)
;;(use-package yasnippet :defer 90)
;;(use-package jabber :defer 100)


;;do more research on flycheck
;; flycheck
;;(package 'flycheck)
;; (add-hook 'after-init-hook #'global-flycheck-mode)

;; (after 'flycheck
;;   (setq flycheck-check-syntax-automatically '(save mode-enabled))
;;   (setq flycheck-checkers (delq 'emacs-lisp-checkdoc flycheck-checkers))
;;   (setq flycheck-checkers (delq 'html-tidy flycheck-checkers))
;;   (setq flycheck-standard-error-navigation nil))

;; (global-flycheck-mode t)

;; ;; flycheck errors on a tooltip (doesnt work on console)
;; (when (display-graphic-p (selected-frame))
;;   (eval-after-load 'flycheck
;;     '(custom-set-variables
;;       '(flycheck-display-errors-function #'flycheck-pos-tip-error-messages))))


;; (require 'jabber)
;; (setq jabber-invalid-certificate-servers '("cup1.aoins.com"))
(put 'narrow-to-region 'disabled nil)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("962dacd99e5a99801ca7257f25be7be0cebc333ad07be97efd6ff59755e6148f" default)))
 '(package-selected-packages
   (quote
    (ov evil-commentary airline-themes elpy esup zone-matrix yasnippet use-package relative-line-numbers powerline magit key-chord jabber iedit helm-projectile flycheck fill-column-indicator evil-visual-mark-mode evil-tabs evil-surround evil-org evil-indent-plus evil-escape company caps-lock))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
