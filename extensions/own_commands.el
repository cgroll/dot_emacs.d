(defun cg/change-latex-src-headers ()
   "change SRC_latex to _LATEX"
   (interactive)
   (goto-char (point-min))
   (let ((proceed-flag 'not-nil) curr-point)
      (while proceed-flag
         (setq curr-point (search-forward "BEGIN_SRC latex" nil t))
         (if (eq curr-point nil)
            (setq proceed-flag nil)
            ;; else
            (delete-char -10)
            (insert "_LATEX")
            (search-forward "END_SRC")
            (delete-char -4)
            (insert "_LATEX")
            )
         )
      )
   )