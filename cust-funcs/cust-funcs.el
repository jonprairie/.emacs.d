(defun and-func (x y) (and x y))
(defun or-func (x y) (or x y))


(defun zip (&rest lists)
  "zip takes an arbitrary number of lists and returns a single list consisting of a list of the first element of each list, a list of the second element in each list, etc. for example: (zip '(1 2 3) '(a b c)) evaluates to ((1 a) (2 b) (3 c)). 

If one list is shorter than the others, the returned list will only be as long as the shortest list"
  (if (cl-reduce #'or-func (mapcar #'null lists))
      nil
    (cons (mapcar #'car lists) (apply #'zip (mapcar #'cdr lists)))))


(defun range (bound-1 &optional bound-2)
  "return a list representing either:

a. the range from 0 to bound-1 (not including bound-1), if bound-2 is nil. Or
b. the range from bound-1 to bound-2 (not including bound-2), if bound-2 is non-nil"
  (let ((start (if (null bound-2) 0 (min bound-1 bound-2)))
	(end (if (null bound-2) bound-1 (max bound-1 bound-2))))
    (cl-loop for n from start to (1- end) collecting n)))

  
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


;; should probably extract this
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

(defun cobol-find-paragraph-def (paragraph-name)
  "search for definition of paragraph and, if found, scroll line to top of page"
  (let* ((search-regexp (concat "[ 0-9]\\{6\\} *" paragraph-name))
         (regex-found (re-search-forward search-regexp nil t)))
    (if regex-found
	(evil-scroll-line-to-top nil))))

(defun cobol-find-paragraph-def-at-point ()
  "search for the definition of paragraph at point"
  (interactive)
  (evil-set-marker ?')
  (let* ((paragraph-name (thing-at-point 'word 'no-properties)))
	 (message "%s" paragraph-name)
	 (cobol-find-paragraph-def paragraph-name)))
