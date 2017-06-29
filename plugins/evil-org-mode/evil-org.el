;;; evil-org.el --- evil keybindings for org-mode

;; Copyright (C) 2012-2015 by Edward Tjörnhammar
;; Author: Edward Tjörnhammar
;; URL: https://github.com/edwtjo/evil-org-mode.git
;; Git-Repository; git://github.com/edwtjo/evil-org-mode.git
;; Created: 2012-06-14
;; Version: 0.1.2
;; Package-Requires: ((evil "0") (org "0") (evil-leader "0"))
;; Keywords: evil vim-emulation org-mode key-bindings presets

;; This file is not part of GNU Emacs

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; Known Bugs:
;; See, https://github.com/edwtjo/evil-org-mode/issues
;;
;;; Code:
(require 'evil)
(require 'org)

(define-minor-mode evil-org-mode
  "Buffer local minor mode for evil-org"
  :init-value nil
  :lighter " EvilOrg"
  :keymap (make-sparse-keymap) ; defines evil-org-mode-map
  :group 'evil-org)

(add-hook 'org-mode-hook 'evil-org-mode) ;; only load with org-mode

(defun clever-insert-item ()
  "Clever insertion of org item."
  (if (not (org-in-item-p))
      (insert "\n")
    (org-insert-item))
  )

(defun evil-org-eol-call (fun)
  "Go to end of line and call provided function.
FUN function callback"
  (end-of-line)
  (funcall fun)
  (evil-append nil)
  )

;; recompute clocks in visual selection
(evil-define-operator evil-org-recompute-clocks (beg end type register yank-handler)
  :keep-visual t
  :move-point nil
  (interactive "<r>")
  (progn
    (message "start!" )
    (save-excursion
      (while (< (point) end)
	(org-evaluate-time-range)
	(next-line)
	(message "at position %S" (point))
        ))))

;; open org-mode links in visual selection
(defun evil-org-generic-open-links (beg end type register yank-handler incog)
  (progn
    (save-excursion 
      (goto-char beg)
      (catch 'break
        (while t
          (org-next-link)
          ;;; break from outer loop when there are no more
          ;;; org links
          (when (or 
                 (not (< (point) end)) 
                 (not (null org-link-search-failed)))
            (throw 'break 0))

          (if (not (null incog))
              (let* ((new-arg
                      ;;; if incog is true, decide which incognito settings to
                      ;;; use dependening on the browser
                      (cond ((not (null (string-match "^.*\\(iceweasel\\|firefox\\).*$" browse-url-generic-program)))  "--private-window")
                            ((not (null (string-match "^.*\\(chrome\\|chromium\\).*$"  browse-url-generic-program)))   "--incognito"     )
                            (t "")
                            ))
                     (old-b (list browse-url-generic-args " " ))
                     (browse-url-generic-args (add-to-ordered-list 'old-b new-arg 0)))
                (progn
                  (org-open-at-point)))
            (let ((browse-url-generic-args '("")))
              (org-open-at-point)))
          )))))


;;; open links in visual selection
(evil-define-operator evil-org-open-links (beg end type register yank-handler)
  :keep-visual t
  :move-point nil
  (interactive "<r>")
  (evil-org-generic-open-links beg end type register yank-handler nil)
  )

;;; open links in visual selection in incognito mode
(evil-define-operator evil-org-open-links-incognito (beg end type register yank-handler)
  :keep-visual t
  :move-point nil
  (interactive "<r>")
  (evil-org-generic-open-links beg end type register yank-handler t)
  )

;; normal state shortcuts
(evil-define-key 'normal evil-org-mode-map
  "gh" 'outline-up-heading
  "gp" 'outline-previous-heading
  "gj" (if (fboundp 'org-forward-same-level) ;to be backward compatible with older org version
	   'org-forward-same-level
	 'org-forward-heading-same-level)
  "gk" (if (fboundp 'org-backward-same-level)
	   'org-backward-same-level
	 'org-backward-heading-same-level)
  "gl" 'outline-next-visible-heading
  "t" 'org-todo
  "J" 'org-metadown
  "K" 'org-metaup
  "o" '(lambda () (interactive) (evil-org-eol-call 'clever-insert-item))
  "O" '(lambda () (interactive) (evil-org-eol-call 'org-insert-heading))
  "L" 'org-end-of-line
  "H" 'org-beginning-of-line
  "<" 'org-metaleft
  ">" 'org-metaright
  "-" 'org-shiftleft
  "=" 'org-shiftright
  "_" 'org-shiftdown
  "+" 'org-shiftup
  "T" 'org-toggle-checkbox
  (kbd "<tab>") 'org-cycle)

;; leader maps
(evil-leader/set-key-for-mode 'org-mode
  "t"  'org-show-todo-tree
  "a"  'org-agenda
  "r"  'org-archive-subtree
  ;; "l"  'evil-org-open-links
  "d"  'org-deadline
  "s"  'org-schedule
  ;;"o"  'evil-org-recompute-clocks
  ;; should generalize these
  "ie" '(lambda ()
	  (interactive)
	  (unless (looking-at-p "^[ \t]*$")
	    (evil-open-below 1)
	    (evil-insert-state -1))
	  (insert "#+BEGIN_SRC emacs-lisp\n")
	  (insert "#+END_SRC")
	  (previous-line)
	  (evil-open-below 1)
	  (evil-normal-state)
	  (org-edit-special))
  "ij" '(lambda ()
	  (interactive)
	  (unless (looking-at-p "^[ \t]*$")
	    (evil-open-below 1)
	    (evil-insert-state -1))
	  (insert "#+BEGIN_SRC javascript\n")
	  (insert "#+END_SRC")
	  (previous-line)
	  (evil-open-below 1)
	  (evil-normal-state)
	  (org-edit-special))
  "il" '(lambda ()
	  (interactive)
	  (unless (looking-at-p "^[ \t]*$")
	    (evil-open-below 1)
	    (evil-insert-state -1))
	  (insert "#+BEGIN_LaTex latex\n")
	  (insert "#+END_LaTex latex")
	  (previous-line)
	  (evil-open-below 1)
	  (evil-normal-state)
	  (org-edit-special))
  "'" 'org-edit-special)

;;(debug)
(evil-define-minor-mode-key 'normal 'org-src-mode
  " '" 'org-edit-src-exit
  " \"" 'org-edit-src-abort)

(evil-define-minor-mode-key 'normal 'org-capture-mode
  " \'" 'org-capture-finalize
  " \"" 'org-capture-refile
  " r" 'org-capture-kill)

;; normal & insert state shortcuts.
(mapc (lambda (state)
        (evil-define-key state evil-org-mode-map
          (kbd "M-l") 'org-metaright
          (kbd "M-h") 'org-metaleft
          (kbd "M-k") 'org-metaup
          (kbd "M-j") 'org-metadown
          (kbd "M-L") 'org-shiftmetaright
          (kbd "M-H") 'org-shiftmetaleft
          (kbd "M-K") 'org-shiftmetaup
          (kbd "M-J") 'org-shiftmetadown
	  (kbd "C-S-RET") '(lambda ()
			     (interactive)
			     (evil-org-eol-call 'org-insert-todo-heading))
	  (kbd "C-RET") '(lambda ()
			   (interactive)
			   (evil-org-eol-call 'org-insert-heading-respect-content))
          (kbd "M-o") '(lambda () (interactive)
                         (evil-org-eol-call
                          '(lambda()
                             (org-insert-heading)
                             (org-metaright))))
          (kbd "M-t") '(lambda () (interactive)
                         (evil-org-eol-call
                          '(lambda()
                             (org-insert-todo-heading nil)
                             (org-metaright))))
          ))
      '(normal insert))

(provide 'evil-org)
;;; evil-org.el ends here
