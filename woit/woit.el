(setq foo (make-overlay  1 10))

(setq incr-string "asdfjkl;ghqweruioptyzxcnm,.b1234567890")

(defun make-overlay-string (width)
  (let ((result "")
	(count 0)
	(incr-list (string-to-list incr-string)))
    (dotimes (n width result)
      (if (and (not (= n 0))
	       (= (mod n 4) 0))
	  (progn
	    (setq result (concat result (char-to-string (nth count incr-list))))
	    (setq count (1+ count)))
	(setq result (concat result " "))))))

(overlay-put foo 'before-string
	     (make-overlay-string (- (window-total-width) 5)))
