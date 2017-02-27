(defmacro with-indirect-file (file-build-body after)
  `(let ((temp-file (make-temp-file "temp")))
     (with-temp-file temp-file
       ,file-build-body)
     ,after
     (delete-file temp-file)))


;(defun with-ftp (user host pass cmds)
;  (let ((temp-pass (or pass (read-passwd (concat "please enter password for " user "@" host ": ")))))
;    (with-indirect-file
;     (progn
;       (insert (concat user "\n"))
;       (insert (concat temp-pass "\n"))
;       (insert "ascii\n")
;       (if (listp cmds)
;	   (mapc
;	    #'(lambda (x) (insert (concat x "\n")))
;	    cmds)
;	 (insert (concat cmds "\n")))
;       (insert "quit"))
;     (shell-command (concat "ftp -s:" temp-file " " host)))))


(defmacro with-ftp (user-host-pass &rest body)
  (let ((temp-pass (cl-gensym)))
    (cl-destructuring-bind (user host pass) user-host-pass
      `(let ((,temp-pass (or ,pass
			     (read-passwd (concat "please enter password for " ,user "@" ,host ": ")))))
	 (with-indirect-file
	  (progn
	    (insert (concat ,user "\n"))
	    (insert (concat ,temp-pass "\n"))
	    (insert "ascii\n")
	    ,@(mapcar
	       #'(lambda (x) `(insert (concat ,x "\n")))
	       body)
	    (insert "quit"))
	  (shell-command (concat "ftp -s:" temp-file " " ,host)))))))


(defmacro ftp-get (file &optional lfile)
  (concat "get " file " " (or lfile) "\n"))


(defmacro ftp-put (lfile file)
  (concat "put " lfile " " file))


;(with-ftp ("e018462" "test1.aoins.com" nil)
;	    (ftp-get
;	     "'TDS.UBOPSTAT.JP50G361'"
;	     "c:/users/e018462/appdata/roaming/.emacs.d/simple_ftp/jp50g361"))
