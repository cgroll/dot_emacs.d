;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;;;       measuring start-up time
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defvar *emacs-load-start* (current-time))

;; show bookmark list at startup
(setq inhibit-splash-screen t)
(require 'bookmark)
(bookmark-bmenu-list)
(split-window-horizontally)
(setq split-height-threshold nil)
(switch-to-buffer "*Messages*")
(switch-to-buffer-other-window "*Bookmark List*")
(recentf-mode 1)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;;;       include package archives
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; org-mode archive
(require 'package)
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/")
   t)

(when (>= emacs-major-version 24)
  (require 'package)
  (package-initialize)
  (add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)
   )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;;;       set some load-paths
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(add-to-list 'load-path "~/.emacs.d/extensions/")
(add-to-list 'load-path "~/")
(add-to-list 'load-path "~/.emacs.d/elpa/auctex-11.87.5")
(add-to-list 'load-path "~/.emacs.d/extensions/auto-complete")
(add-to-list 'load-path "~/.emacs.d/extensions/yasnippet")
(add-to-list 'load-path
   "~/.emacs.d/elpa/org-plus-contrib-20140616/org")
(add-to-list 'load-path
   "~/.emacs.d/elpa/org-plus-contrib-20140616/")
(add-to-list 'load-path
   "~/.emacs.d/extensions/org-mode/lisp/")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;;;       require org-mode and use-package
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'org)
(require 'use-package)                  ; enables autoloading of functions

(org-babel-load-file "~/.emacs.d/init-all.org")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;;;       IDO-mode
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; ido
;; (require 'ido)                          
(setq ido-enable-flex-matching t)
(setq ido-everywhere t)
(ido-mode t)
(setq ido-use-filename-at-point 'guess)
(setq ido-file-extensions-order
   '(".org" ".m" ".jl" ".r" ".el" ".txt"))
(setq ido-ignore-extensions t)
(unbind-key "C-t" global-map)
(bind-key* "C-t r" 'ido-toggle-regexp)

;; occur / multi-occur
(bind-key* "C-t o" 'occur)
(bind-key* "C-t O" 'multi-occur)

(use-package browse-kill-ring
   )

;; for bookmarks of recent files
(bind-key* "C-t b" 'recentf-open-files)
(bind-key "C-x r B" 'bookmark-jump-other-window)
(bind-key* "C-M-ü" 'bookmark-jump-other-window)




;; remove old .el files
(condition-case nil
   (progn
      (delete-file "~/.emacs.d/init-ac.el")
      (delete-file "~/.emacs.d/init-org.el")
      (delete-file "~/.emacs.d/init-R.el")
      (delete-file "~/.emacs.d/init-latex.el")
      (delete-file "~/.emacs.d/init-bibtex-config.el")
      (delete-file "~/.emacs.d/init-config.el")
      )
   (delete-file filename)
   (error nil))

;; load tags-table, so that ac-source-etags will not fail
;; (visit-tags-table "~/Dropbox/db_research/TAGS")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;;;       magit
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; depending on computer, actual path could be different
(add-to-list 'load-path "/usr/local/share/emacs/site-lisp/")
(add-to-list 'load-path "/usr/share/emacs/site-lisp/magit-master")

(bind-key "C-t m" 'magit-status)
(use-package magit
   :defer t
   :load-path "/usr/share/emacs/site-lisp/magit-master"
   :commands magit-status
   :init
   (bind-key "C-t m" 'magit-status)
   )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;;;       autopair: parentheses matching
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; autopair
;; [[https://raw.github.com/capitaomorte/autopair/master/autopair.el][autopair.el]]

(require 'autopair)
(autopair-global-mode 1)

;; highlight matching parentheses
(show-paren-mode 1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;;;       miscellaneous
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; new block
(defun new-block ()
   "Create new block header at point"
   (interactive)
   (setq counter 1)
   (while (< counter 60)
      (insert comment-start)
      (incf counter))
   (newline)
   (setq counter 1)
   (while (< counter 4)
      (insert comment-start)
      (incf counter))
   (newline)
   (setq counter 1)
   (while (< counter 6)
      (insert comment-start)
      (incf counter))
   (setq counter 1)
   (while (< counter 8)
      (insert " ")
      (incf counter))
   (push-mark)
   (deactivate-mark)
   (newline)
   (setq counter 1)
   (while (< counter 4)
      (insert comment-start)
      (incf counter))
   (newline)
   (setq counter 1)
   (while (< counter 60)
      (insert comment-start)
      (incf counter))
   (newline)
   (exchange-point-and-mark))


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(LaTeX-math-abbrev-prefix "M-g")
 '(calendar-latitude 48.139)
 '(calendar-longitude 11.58)
	'(ebib-citation-commands
		 (quote
			 ((any
				  (("cite" "\\cite%<[%A]%>{%K}")))
				 (org-mode
					 (("ebib" "[[ebib:%K][%D]]")
						 ("text" "@%K%< [%A]%>")
						 ("paren" "[%(%<%A %>@%K%<, %A%>%; )]")
						 ("year" "[-@%K%< %A%>]")
						 ("ci" "[@%K]")))
				 (markdown-mode
					 (("text" "@%K%< [%A]%>")
						 ("paren" "[%(%<%A %>@%K%<, %A%>%; )]")
						 ("year" "[-@%K%< %A%>]")
						 ("ci" "[@%K]"))))))
 '(ebib-preload-bib-files (quote ("~/research/project/dissertation_main/refs.bib")))
 '(global-auto-complete-mode t)
 '(matlab-shell-command "~/remote_matlab" t)
 '(mode-require-final-newline nil)
	'(org-agenda-files
		 (quote
			 ("~/customs/gtd/todo.org" "~/customs/gtd/refile.org" "~/customs/notes/priv_comp_notes.org" "~/customs/notes/priv_install_notes.org" "~/how_to/comp_records.org" "~/customs/chronicle/oracle.org")))
 '(org-agenda-show-all-dates t)
 '(org-agenda-skip-deadline-if-done t)
 '(org-agenda-skip-scheduled-if-done t)
 '(org-agenda-start-on-weekday nil)
 '(org-clock-into-drawer t)
 '(org-clock-mode-line-total (quote current))
 '(org-deadline-warning-days 14)
	'(org-drill-optimal-factor-matrix
		 (quote
			 ((3
				  (2.46 . 2.46)
				  (1.56 . 2.2)
				  (2.04 . 2.244)
				  (2.08 . 2.166)
				  (2.6 . 2.6)
				  (1.96 . 2.316)
				  (2.2199999999999998 . 2.379)
				  (1.9000000000000001 . 2.111)
				  (1.8199999999999998 . 2.26)
				  (2.5 . 2.5)
				  (2.36 . 2.439))
				 (2
					 (2.08 . 2.166)
					 (1.8199999999999998 . 2.26)
					 (2.2199999999999998 . 2.379)
					 (2.22 . 2.22)
					 (2.46 . 2.497)
					 (1.7000000000000002 . 2.255)
					 (2.04 . 2.319)
					 (1.96 . 2.238)
					 (2.1799999999999997 . 2.325)
					 (2.36 . 2.439)
					 (2.5 . 2.5)
					 (2.6 . 2.588)
					 (1.56 . 2.2))
				 (1
					 (2.08 . 3.902)
					 (1.8199999999999998 . 3.615)
					 (1.56 . 3.52)
					 (2.2199999999999998 . 3.806)
					 (2.04 . 3.59)
					 (2.36 . 3.902)
					 (2.1799999999999997 . 3.804)
					 (2.5 . 4.0)
					 (2.6 . 4.14)
					 (1.96 . 3.706)
					 (1.7000000000000002 . 3.608)))))
 '(org-fast-tag-selection-single-key nil)
	'(org-format-latex-options
		 (quote
			 (:foreground default :background default :scale 1.7 :html-foreground "Black" :html-background "Transparent" :html-scale 2.0 :matchers
				 ("begin" "$1" "$" "$$" "\\(" "\\["))))
 '(org-reverse-note-order nil)
 '(require-final-newline nil)
	'(safe-local-variable-values
		 (quote
			 ((org-src-preserve-indentation . t)
				 (TeX-master . evt_main\.tex)
				 (org-export-babel-evaluate . t)
				 (org-export-publishing-directory . "./src_results/")
				 (org-export-babel-evaluate . no-export)))))
;; (custom-set-faces
;;   ;; custom-set-faces was added by Custom.
;;   ;; If you edit it by hand, you could mess it up, so be careful.
;;   ;; Your init file should contain only one such instance.
;;   ;; If there is more than one, they won't work right.
;;  '(org-level-1 ((t (:inherit outline-1 :foreground "yellow" :height 1.5 :width expanded))))
;;  '(org-level-2 ((t (:inherit outline-2 :foreground "orange" :height 1.4 :width normal))))
;;  '(org-level-3 ((t (:inherit outline-3 :foreground "green yellow" :height 1.3 :width normal))))
;;  '(org-level-4 ((t (:inherit outline-4 :foreground "deep sky blue" :height 1.2))))
;;  '(org-level-5 ((t (:inherit outline-5 :foreground "deep pink")))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-level-1 ((t (:inherit outline-1 :foreground "yellow" :height 1.1 :width expanded))))
 '(org-level-2 ((t (:inherit outline-2 :foreground "orange" :height 1.1 :width normal))))
 '(org-level-3 ((t (:inherit outline-3 :foreground "green yellow" :height 1.1 :width normal))))
 '(org-level-4 ((t (:inherit outline-4 :foreground "deep sky blue" :height 1.1))))
 '(org-level-5 ((t (:inherit outline-5 :foreground "deep pink")))))
(put 'upcase-region 'disabled nil)



;;(message "My .emacs loaded in %ds" (destructuring-bind (hi lo ms) (current-time)
;;                           (- (+ hi lo) (+ (first *emacs-load-start*) (second *emacs-load-start*)))))

(put 'narrow-to-region 'disabled nil)
