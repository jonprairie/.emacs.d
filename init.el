;;;;;;;;
;;;; This is a .emacs file. There are many like it, but this is mine.
;;;;;;;;


;;; emacs preferences


(setq debug-on-error nil)

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

(fset 'yes-or-no-p 'y-or-n-p)


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

(use-package auto-compile
  :ensure t
  :config
  (setq auto-compile-display-buffer nil)
  (setq auto-compile-mode-line-counter t)
  (setq load-prefer-newer t)
  (auto-compile-on-load-mode)
  (auto-compile-on-save-mode))

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

;;; load utilities


(use-package dash
  :ensure t
  :config
  (use-package dash-functional
    :ensure t)
  (eval-after-load 'dash '(dash-enable-font-lock)))


(use-package s
  :ensure t)


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


(use-package key-chord
  :ensure t
  :config
  (key-chord-mode 1) 
  (setq key-chord-two-keys-delay 0.075))


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
    ;;((kbd "r") nil)
    ((kbd "s") nil)
    ((kbd "sj") 'evil-scroll-down)
    ((kbd "sJ") 'evil-goto-line)
    ((kbd "sk") 'evil-scroll-up)
    ((kbd "sK") 'evil-goto-first-line)
    ((kbd "sh") 'evil-scroll-left)
    ((kbd "sl") 'evil-scroll-right)
    ((kbd "sn") 'code-branch-forward)
    ((kbd "sp") 'code-branch-backward)))

  (key-chord-def-list
   "ievmourp"
   (("jk" 'evil-normal-state)
    ("Jk" 'evil-normal-state)
    ("jK" 'evil-normal-state)
    ("JK" 'evil-normal-state)))

  (evil-leader/set-key
    "mx" 'helm-M-x
    "g" 'magit-status                              
    "dr" 'dired
    "j" 'evil-window-down                          
    "k" 'evil-window-up                            
    "h" 'evil-window-left                          
    "l" 'evil-window-right                         
    "st" 'eval-buffer  
    "ev" 'eval-last-sexp
    "p" 'helm-projectile-find-file-in-known-projects 
    "o" 'helm-buffers-list                           
    "v" 'helm-show-kill-ring                         
    ;;"c" 'helm-comint-input-ring                      
    "c" 'org-capture
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

  (evil-leader/set-key-for-mode 'cobol-mode
    "f" 'cobol-smart-goto-def
    "F" 'cobol-find-paragraph-callee-at-point
    "dp" 'cobol-jump-to-procedure-division
    "dd" 'cobol-jump-to-data-division
    " " nil
    "[" 'cobol--jump-to-prev-paragraph-or-exit
    "]" 'cobol--jump-to-next-paragraph-or-exit
    "{" 'cobol--jump-to-prev-paragraph
    "}" 'cobol--jump-to-next-paragraph)

  (evil-leader/set-key-for-mode 'org-mode
    "q" nil
    "q" 'org-set-tags-command)

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
	helm-echo-input-in-header-line        t
	helm-candidate-number-limit          25
	helm-ff-candidate-number-limit       25
	helm-ff-skip-boring-files             t)
  (helm-mode 1)
  (add-to-list 'helm-boring-file-regexp-list "node_modules")
  (add-to-list 'helm-boring-file-regexp-list "__pycache__")
  (add-to-list 'helm-boring-file-regexp-list "\\.min\\.js$"))


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
		  "*.min.js"
                  )
		projectile-globally-ignored-files))
  (projectile-global-mode)

  (setq projectile-use-git-grep 1)
  (setq projectile-indexing-method 'alien)
  (setq-default projectile-mode-line
		'(:eval (format " [%s]" (projectile-project-name)))))


(use-package org
  :ensure t
  :defer  t
  :diminish org-indent-mode
  :config
  (let ((main-org-file "~/.emacs.d/todo/todo.org"))
    (setq org-agenda-files (list main-org-file))
    (setq org-default-notes-file main-org-file)
    (setq org-capture-templates
	  '(("i" "Idea" entry (file+headline main-org-file "Half-Baked Ideas")
	     "* TODO %?")
	    ("j" "Jira" entry (file+headline main-org-file "Jiras")
	     "* TODO %^{prompt}%? :%\\1:\n  %c")
	    ("l" "Listing" entry (file+headline main-org-file "Listings")
	     "* TODO opt%^{prompt}%? :opp%\\1:\n  %c")
	    ("t" "Todo" entry (file+headline main-org-file "Tasks")
	     "* TODO %?"))))
  
  (add-hook 'org-mode-hook
	    (lambda()
	      (relative-line-numbers-mode t)
	      (org-indent-mode t)))

  (add-hook 'org-agenda-mode-hook
	    (lambda ()
	      (relative-line-numbers-mode t)))
  
  ;; syntax highlighting for code blocks
  (setq org-src-fontify-natively t)

  (setq org-agenda-custom-commands
	'(("s" "Week-agenda with TOP-3 tags"
	   ((tags-todo "\@top3")
	    (todo "WAITING")
	    (todo "IN-PROGRESS")
	    (agenda "" ((org-agenda-ndays 2)))
	    (todo "TODO" 
		  ((org-agenda-skip-function
		    '(org-agenda-skip-if
		      nil
		      '(scheduled deadline regexp ":@top3:"))))))
	   ((org-agenda-skip-function
	     '(org-agenda-skip-entry-if 'todo 'done))))))

  (setq org-use-fast-todo-selection t)

  (setq org-todo-keywords
	'((sequence
	   "TODO(t)"
	   "IN-PROGRESS(p)"
	   "WAITING(w)"
	   "|"
	   "DONE(d)"
	   "CANCELLED(c)")))

  (setq org-tag-alist
	'((:startgroup . nil)
	  ("@top3" . ?t)
	  ("@work" . ?w)
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
  (setq org-agenda-restore-windows-after-quit t)

  ;;(add-to-list
  ;; 'load-path
  ;; "~/.emacs.d/plugins/evil-org-mode")

  (use-package evil-org
    :load-path "plugins/evil-org-mode/"
    :defer t
    :diminish evil-org-mode
    :after org)

  (evil-define-key 'emacs org-agenda-keymap
    (kbd "ss") 'org-save-all-org-buffers
    (kbd "q") 'org-agenda-set-tags
    "j" 'org-agenda-next-item
    "k" 'org-agenda-previous-item
    "c" 'org-capture
    " " nil
    " j" 'evil-window-down                                                    
    " k" 'evil-window-up                                                      
    " h" 'evil-window-left                                                    
    " l" 'evil-window-right))

;;(evil-define-key 'emacs org-agenda-keymap (kbd "q") 'org-agenda-set-tags)

;;(define-key org-agenda-mode-map " " nil)

;;(define-key org-agenda-mode-map "j" 'org-agenda-next-item)
;;(define-key org-agenda-mode-map "k" 'org-agenda-previous-item)

;;(evil-define-key 'emacs org-agenda-mode-map
;;  " j" 'evil-window-down                          
;;  " k" 'evil-window-up                            
;;  " h" 'evil-window-left                          
;;  " l" 'evil-window-right))


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


;;(use-package fill-column-indicator
;;  :ensure t
;;  :defer 15
;;  :config
;;  (setq-default fill-column 72))


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


(use-package iedit
  :defer t)


(use-package magit
  :ensure t
  :defer t
  :config
  (setq magit-commit-show-diff nil
	magit-revert-buffers 1
	magit-refresh-status-buffer nil
	magit-diff-highlight-indentation nil
	magit-diff-highlight-trailing nil
	magit-diff-paint-whitespace nil
	magit-diff-highlight-hunk-body nil
	magit-diff-refine-hunk nil
	vc-handled-backends nil))


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


(use-package emmet-mode
  :ensure t
  :config
  (add-hook 'sgml-mode-hook 'emmet-mode)
  (add-hook 'css-mode-hook 'emmet-mode))


(use-package js2-mode
  :mode "\\.js\\'"
  :init
  (use-package js2-refactor
    :ensure t
    :config
    (add-hook 'js2-mode-hook #'js2-refactor-mode)
    (evil-define-minor-mode-key 'normal 'js2-refactor-mode 
      " ;ee" #'js2r-expand-node-at-point
      " ;cc" #'js2r-contract-node-at-point
      " ;wi" #'js2r-wrap-buffer-in-iife
      " ;ig" #'js2r-inject-global-in-iife
      " ;ev" #'js2r-extract-var
      " ;iv" #'js2r-inline-var
      " ;rv" #'js2r-rename-var
      " ;vt" #'js2r-var-to-this
      " ;ag" #'js2r-add-to-globals-annotation
      " ;sv" #'js2r-split-var-declaration
      " ;ss" #'js2r-split-string
      " ;ef" #'js2r-extract-function
      " ;em" #'js2r-extract-method
      " ;ip" #'js2r-introduce-parameter
      " ;lp" #'js2r-localize-parameter
      " ;tf" #'js2r-toggle-function-expression-and-declaration
      " ;ta" #'js2r-toggle-arrow-function-and-expression
      " ;ao" #'js2r-arguments-to-object
      " ;uw" #'js2r-unwrap
      " ;wl" #'js2r-wrap-in-for-loop
      " ;3i" #'js2r-ternary-to-if
      " ;lt" #'js2r-log-this
      " ;dt" #'js2r-debug-this
      " ;sl" #'js2r-forward-slurp
      " ;ba" #'js2r-forward-barf
      " ;k" #'js2r-kill)
    (evil-define-minor-mode-key 'visual 'js2-refactor-mode 
      " ;ee" #'js2r-expand-node-at-point
      " ;cc" #'js2r-contract-node-at-point
      " ;wi" #'js2r-wrap-buffer-in-iife
      " ;ig" #'js2r-inject-global-in-iife
      " ;ev" #'js2r-extract-var
      " ;iv" #'js2r-inline-var
      " ;rv" #'js2r-rename-var
      " ;vt" #'js2r-var-to-this
      " ;ag" #'js2r-add-to-globals-annotation
      " ;sv" #'js2r-split-var-declaration
      " ;ss" #'js2r-split-string
      " ;ef" #'js2r-extract-function
      " ;em" #'js2r-extract-method
      " ;ip" #'js2r-introduce-parameter
      " ;lp" #'js2r-localize-parameter
      " ;tf" #'js2r-toggle-function-expression-and-declaration
      " ;ta" #'js2r-toggle-arrow-function-and-expression
      " ;ao" #'js2r-arguments-to-object
      " ;uw" #'js2r-unwrap
      " ;wl" #'js2r-wrap-in-for-loop
      " ;3i" #'js2r-ternary-to-if
      " ;lt" #'js2r-log-this
      " ;dt" #'js2r-debug-this
      " ;sl" #'js2r-forward-slurp
      " ;ba" #'js2r-forward-barf
      " ;k" #'js2r-kill)
    (define-key js2-refactor-mode-map (kbd "<C-S-down>") #'js2r-move-line-down)
    (define-key js2-refactor-mode-map (kbd "<C-S-up>") #'js2r-move-line-up))
  ;;(js2r-add-keybindings-with-prefix "C-k"))
  (use-package skewer-mode
    :ensure t
    :config
    (add-hook 'js2-mode-hook 'skewer-mode)
    (add-hook 'css-mode-hook 'skewer-css-mode)
    (add-hook 'html-mode-hook 'skewer-html-mode))
  :ensure t
  :config
  (setq js2-include-browser-externs t)
  (setq js2-include-node-externs t))

(use-package woit
  :load-path "plugins/woit/")


;; Diminish extraneous info in the modeline
(diminish 'abbrev-mode)
(defun sk/diminish-auto-revert ()
  "Diminishes the 'auto-revert-mode' in the mode line."
  (interactive)
  (diminish 'auto-revert-mode ""))
(add-hook 'auto-revert-mode-hook 'sk/diminish-auto-revert)

;;(defmacro rename-major-mode (package-name mode new-name)
;;  "Renames a major mode."
;;  `(eval-after-load ,package-name
;;     '(defadvice ,mode (after rename-modeline activate)
;;	(setq mode-name ,new-name))))


;;(rename-major-mode "python" python-mode "π")
;;(rename-major-mode "shell" shell-mode "🐚")
;;(rename-major-mode "org" org-mode "📓") 
;;(rename-major-mode "org-agenda" org-agenda-mode "📅")
;;(rename-major-mode "lisp" lisp-mode "λ")
;;(rename-major-mode "cobol" cobol-mode "ಠ_ಠ")


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
(setq custom-file "~/.emacs.d/cust-vars.el")
