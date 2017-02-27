;;;;Major mode for rexx files


;;;allow hooks
(defvar rexx-mode-hook nil)

;;(defvar rexx-mode-map
;;  (let ((map (make-sparse-keymap)))
;;    (define-key map "\C-j" 'newline-and-indent)
;;    map)
;;  "Keymap for rexx major mode")

;;;autoload on .rexx files
(add-to-list 'auto-mode-alist '("\\.rexx\\'" . rexx-mode))

;;;keywords
(defvar rexx-keywords
  (regexp-opt 
   '("address"
     "arg"
     "call"
     "do"
     "drop"
     "else"
     "end"
     "exit"
     "expose"
     "forward"
     "forever"
     "guard"
     "if"
     "interpret"
     "iterate"
     "leave"
     "nop"
     "numeric"
     "otherwise"
     "parse"
     "procedure"
     "pull"
     "push"
     "queue"
     "raise"
     "reply"
     "return"
     "say"
     "select"
     "signal"
     "source"
     "then"
     "trace"
     "until"
     "upper"
     "use"
     "var"
     "value"
     "when"
     "while") t))

;;;built-in functions
(defvar rexx-builtins
  (regexp-opt
   '("abbrev"
     "abs"
     "address"
     "arg"
     "beep"
     "bitand"
     "bitor"
     "bitxor"
     "b2x"
     "center"
     "centre"
     "changestr"
     "charin"
     "charout"
     "chars"
     "compare"
     "condition"
     "copies"
     "countstr"
     "c2d"
     "c2x"
     "datatype"
     "date"
     "delstr"
     "delword"
     "digits"
     "directory"
     "d2c"
     "d2x"
     "errortext"
     "filespec"
     "form"
     "format"
     "fuzz"
     "index"
     "insert"
     "lastpos"
     "left"
     "length"
     "linein"
     "lineout"
     "lines"
     "max"
     "min"
     "overlay"
     "pos"
     "queued"
     "random"
     "reverse"
     "right"
     "sign"
     "sourceline"
     "space"
     "stream"
     "strip"
     "substr"
     "subword"
     "time"
     "trace"
     "translate"
     "trunc"
     "value"
     "var"
     "verify"
     "word"
     "wordindex"
     "wordlength"
     "wordpos"
     "words"
     "xrange"
     "x2b"
     "x2c"
     "x2d") t))

(defvar rexx-parens
  "\\([()]\\)")
		
(defvar rexx-operators
  (concat
   "\\("
   "//\\|"
   "||\\|"
   "\\\\\\|"
   "[-\\+\\*?=/%&><^!|.~]"
   "\\)"))

(defvar rexx-var-defs "\\(^\\s-*[[:alpha:][:digit:]_\\.]*\\(:?\\s-*=\\)\\)")

(defvar rexx-func-defs "\\(^\\w*:\\)")

(defvar rexx-const "\\<\\([0-9]*\\)\\>")

(defvar rexx-parens-face (make-face 'rexx-parens-face))
(set-face-attribute 'rexx-parens-face nil :height 160)

(defconst rexx-font-lock-keywords
  (list
   `(,(concat "\\<" rexx-keywords "\\>") . font-lock-keyword-face)
   `(,(concat "\\<" rexx-builtins "\\>") . font-lock-builtin-face)
   `(,rexx-parens (1 rexx-parens-face))
   `(,rexx-operators (1 font-lock-builtin-face))
   `(,rexx-var-defs (1 font-lock-variable-name-face))
   `(,rexx-const (1 font-lock-constant-face))
   `(,rexx-func-defs (1 font-lock-function-name-face)))
  "Basic highlighting for rexx-mode")

(defvar rexx-mode-syntax-table
  (let ((st (make-syntax-table)))
    (modify-syntax-entry ?_ "w" st)
    (modify-syntax-entry ?/ ". 14" st)
    (modify-syntax-entry ?* ". 23" st)
    st)
  "Syntax table for rexx-mode")

(defun rexx-mode ()
  "Major mode for editing rexx files"
  (interactive)
  (kill-all-local-variables)
  (set-syntax-table rexx-mode-syntax-table)
  (set (make-local-variable 'font-lock-defaults)
       '(rexx-font-lock-keywords))
  (setq major-mode 'rexx-mode)
  (setq mode-name "rexx")
  (setq indent-tabs-mode nil)
  (run-hooks 'rexx-mode-hook))

(provide 'rexx-mode)
