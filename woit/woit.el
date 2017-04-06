(setq incr-string "asdfjkl;ghqweruioptyzxcnm,.b1234567890")
(setq tab-size 4)

(setq incr-plist
      (apply
       #'append
       (cl-loop
	for c in (string-to-list incr-string)
	for i from 0 to (length (string-to-list incr-string))
	collect (list c i))))

(defun make-overlay-string (width)
  (let ((result "")
	(count 0)
	(incr-list (string-to-list incr-string)))
    (dotimes (n width result)
      (cond
       ((> count (1- (length incr-string)))
	nil)
       ((and (not (= n 0))
	     (= (mod n 4) 0))
	(progn
	  (setq result (concat result (char-to-string (nth count incr-list))))
	  (setq count (1+ count))))
       (t 
	(setq result (concat result " ")))))))

(defun width-point-to-window ()
  (interactive)
  (save-excursion
    (let ((p (point))
	  (bol (progn (beginning-of-line) (point))))
      (message "%s" (- (window-max-chars-per-line) (- p bol)))
      (- (window-max-chars-per-line) (- p bol) 8))))
                                                
(defun ask-for-indent ()
  (interactive)
  (setq foo (make-overlay (point) (point)))
  (overlay-put foo 'before-string
	       (make-overlay-string (width-point-to-window)))
  (overlay-put foo 'window t)
  (let ((input-char (read-char)))
    (delete-overlay foo)
    (insert (make-string
	     (* (1+ (cl-getf incr-plist input-char)) tab-size)
	     (string-to-char " ")))))

(evil-def-multi-keys-list "en" (((kbd ">") 'ask-for-indent)))

(key-chord-def-list
 "en"
 (("OO" (lambda ()
	  (interactive)
	  (evil-open-above 1)
	  (normal-mode)
	  (if (string-equal mode-name "ಠ_ಠ")
	      (evil-previous-line))
	  (ask-for-indent)
	  (evil-insert-state t)))
  ("oo" (lambda ()
	  (interactive)
	  (evil-open-below 1)
	  (normal-mode)
	  (if (string-equal mode-name "ಠ_ಠ")
	      (evil-previous-line))
	  (ask-for-indent)
	  (evil-insert-state t)))))
