;; Via http://blogisticreflections.wordpress.com/2009/10/01/r-object-tooltips-in-ess/
;;
;; ess-R-object-tooltip.el
;; 
;; I have defined a function, ess-R-object-tooltip, that when
;; invoked, will return a tooltip with some information about
;; the object at point.  The information returned is
;; determined by which R function is called.  This is controlled
;; by an alist, called ess-R-object-tooltip-alist.  The default is
;; given below.  The keys are the classes of R object that will
;; use the associated function.  For example, when the function
;; is called while point is on a factor object, a table of that
;; factor will be shown in the tooltip.  The objects must of course
;; exist in the associated inferior R process for this to work.
;; The special key "other" in the alist defines which function
;; to call when the class is not mached in the alist.  By default,
;; the str function is called, which is actually a fairly useful
;; default for data.frame and function objects.
;; 
;; The last line of this file shows my default keybinding.
;; I simply save this file in a directory in my load-path
;; and then place (require 'ess-R-object-tooltip) in my .emacs

;; the alist
(setq ess-R-object-tooltip-alist
   '((numeric    . "summary")
       (factor     . "table")
       (integer    . "summary")
       (lm         . "summary")
       (other      . "str")))

(defun ess-R-object-tooltip ()
   "Get info for object at point, and display it in a tooltip."
   (interactive)
   (let ((proc (get-ess-process))
           (objname (current-word))
           (curbuf (current-buffer))
           (tmpbuf (get-buffer-create " *ess-R-object-tooltip*"))
           bs)
      (when objname
         (ess-command (concat "class(" objname ")\n") tmpbuf nil nil nil proc)
         (with-current-buffer tmpbuf
            (goto-char (point-min))
            (unless (re-search-forward "\(object .* not found\)\|unexpected" nil t)
               (re-search-forward "\"\\(.*\\)\"" nil t)
               (let* ((objcls (match-string 1))
                        (myfun (or (cdr (assoc-string objcls ess-R-object-tooltip-alist))
                                  (cdr (assoc 'other ess-R-object-tooltip-alist)))))
                  (ess-command (concat myfun "(" objname ")\n") tmpbuf nil nil nil proc))
               (setq bs (buffer-string)))))
      (when bs
         (ess-tooltip-show-at-point bs 0 30))))

;; my default key map
(define-key ess-mode-map "\C-c\C-g" 'ess-R-object-tooltip)

(defun cg/ess-tooltip-heads ()
   "Show heads info of last kill-ring object in tooltip"
   (interactive)
   (let ((proc (get-ess-process))
           (objname (car kill-ring))
           (curbuf (current-buffer))
           (tmpbuf (get-buffer-create " *ess-R-object-tooltip*"))
           bs)
      (with-current-buffer tmpbuf
         (ess-command (concat "head(" objname ")\n") tmpbuf
                         nil nil nil proc)
         (setq bs (buffer-string)))
      (when bs
         (ess-tooltip-show-at-point bs 0 30))))

(defun cg/ess-tooltip-str ()
   "Show str info of last kill-ring object in tooltip"
   (interactive)
   (let ((proc (get-ess-process))
           (objname (car kill-ring))
           (curbuf (current-buffer))
           (tmpbuf (get-buffer-create " *ess-R-object-tooltip*"))
           bs)
      (with-current-buffer tmpbuf
         (ess-command (concat "str(" objname ")\n") tmpbuf
                         nil nil nil proc)
         (setq bs (buffer-string)))
      (when bs
         (ess-tooltip-show-at-point bs 0 30))))

(define-key ess-mode-map (kbd "C-1") 'cg/ess-tooltip-heads)
(global-set-key (kbd "C-1") 'cg/ess-tooltip-heads)
(define-key ess-mode-map (kbd "C-2") 'cg/ess-tooltip-str)
(global-set-key (kbd "C-2") 'cg/ess-tooltip-str)

(provide 'ess-R-object-tooltip)