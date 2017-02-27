    ;;;
    ;;; custom function definitions
    ;;;

       ;; ensure that my "standard" packages are installed
          (defun ensure-package-installed (&rest packages)
            "Assure every package is installed, ask for installation if it's not.
          
          Return a list of installed packages or nil for every skipped package."
            (mapcar
             (lambda (package)
               (if (package-installed-p package)
                   nil
                 (if (y-or-n-p
          	    (format "Package %s is missing. Install it? "
          		    package))
                     (package-install package)
                   package)))
             packages))

;;should probably extract this
(setq evil-mode-mappings
      (list
       "i" 'evil-insert-state-map
       "e" 'evil-emacs-state-map
       "n" 'evil-normal-state-map
       "v" 'evil-visual-state-map
       "m" 'evil-motion-state-map
       "o" 'evil-operator-state-map
       "u" 'evil-outer-text-objects-map
       "r" 'evil-inner-text-objects-map
       "p" 'evil-replace-state-map))

(defmacro define-multi-keys (def-func state-list binding fn)
  `(progn
     ,@(mapcar
	(lambda (state-key-char)
	    `(,def-func
		,(lax-plist-get
		    evil-mode-mappings
		    (char-to-string state-key-char))
		,binding
		,fn))
	(string-to-list state-list))))

(defmacro define-multi-keys-list (def-func state-list bindings)
  `(progn
     ,@(mapcar
	(lambda (binding)
	  `(define-multi-keys
	    ,def-func
	    ,state-list
	    ,(cl-first binding)
	    ,(cl-second binding)))
	bindings)))

(defmacro evil-def-multi-keys (state-list binding fn)
  "Binds a key combination 'binding' to a function 'fn' in multiple evil-states at once. 

'state-list' should be a string consisting of one or more of the following chars:
       'i' - evil-insert-state-map
       'e' - evil-emacs-state-map
       'n' - evil-normal-state-map
       'v' - evil-visual-state-map
       'm' - evil-motion-state-map
       'o' - evil-operator-state-map
       'u' - evil-outer-text-objects-map
       'r' - evil-inner-text-objects-map
       'p' - evil-replace-state-map

for example:

(evil-def-multi-keys \"ienv\" (kbd \"<f5>\") \'evil-prev-buffer)

will bind <f5> to evil-prev-buffer in insert, emacs, normal and visual modes"
  `(define-multi-keys-list define-key ,state-list ,binding ,fn))

(defmacro evil-def-multi-keys-list (state-list bindings)
  `(define-multi-keys-list define-key ,state-list ,bindings))

(defmacro key-chord-def-list (state-list bindings)
  `(define-multi-keys-list key-chord-define ,state-list ,bindings))

;; esc quits
(defun minibuffer-keyboard-quit ()
  "Abort recursive edit.
In Delete Selection mode, if the mark is active, just deactivate it;
then it takes a second \\[keyboard-quit] to abort the minibuffer."
  (interactive)
  (if (and delete-selection-mode transient-mark-mode mark-active)
      (setq deactivate-mark  t)
    (when (get-buffer "*Completions*") (delete-windows-on "*Completions*"))
    (abort-recursive-edit)))

(provide 'ensure-package-installed)
(provide 'evil-def-multi-keys)
