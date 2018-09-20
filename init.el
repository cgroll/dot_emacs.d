;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;;;       measuring start-up time
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; set path for ESS
;;;;;;;;;;;;;;;;;;;

(add-to-list 'load-path "/home/chris/programs/ESS/lisp/")
(add-to-list 'load-path "/home/grollc/programs/ESS/lisp/")
(require 'ess-site)


(defvar *emacs-load-start* (current-time))

;; show bookmark list at startup
(setq inhibit-splash-screen t)
(require 'bookmark)
(bookmark-bmenu-list)
(split-window-horizontally)
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
;;;;;       htmlize
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (require 'htmlize)
;; the problem is that htmlize uses the same syntax highlighting
;; colors that I use within emacs itself. However, in emacs I have a
;; black background, while I usually have a with one for exported
;; files. This way, syntax highlighting might be leading to hardly
;; visible code snippets.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;;;       require org-mode and use-package
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'org)
(require 'use-package)                  ; enables autoloading of functions

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;;;       auctex
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (require 'auctex-autoloads)
;; (load "auctex.el" nil t t)  ; previously, this was crucial!
;; (load "preview-latex.el" nil t t) ; previously, this was crucial!

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;;;       bbdb
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; allow bbdb to be loaded through call to bbdb function
;; externally, bbdb should be called whenever gnus is activated
(use-package bbdb
   :defer t
   :command (bbdb )
   :init
   (autoload 'bbdb "bbdb" nil t)
   
   :load-path "~/.emacs.d/extensions/bbdb/lisp/"
   :config
   (bbdb-initialize 'gnus 'message)
   )

(setq bbdb-always-add-addresses t)
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;;;       auto-complete-mode and yasnippet
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(add-to-list 'load-path "~/.emacs.d/extensions/yasnippet")
(add-to-list 'load-path "~/.emacs.d/extensions/auto-complete")

(use-package yasnippet
   :config
   (progn
      (yas-global-mode 1)
      (setq yas-snippet-dirs "~/.emacs.d/extensions/yasnippet/snippets")
      )
   )
(global-set-key (kbd "C-i") 'yas-expand)

(define-key yas-minor-mode-map (kbd "C-i") 'yas-expand)
                                        ;(define-key yas-minor-mode-map (kbd "C-u") 'yas-next-field)
                                        ;(define-key yas-minor-mode-map (kbd "C-U") 'yas-prev-field)
(define-key yas-minor-mode-map (kbd "C-S-i") 'yas-prev-field)
                                        ; REMARK: it behaves completely different than what the settings say:
                                        ; C-u is yas-prev-field, while TAB is yas-next-field

(use-package auto-complete-config
   :config
   (progn
                                        ;(ac-set-trigger-key "C-o")
      (bind-key "C-o" 'ac-expand ac-mode-map)
      (bind-key "C-o" 'ac-complete) 
                                        ;(unbind-key "M-/")
                                        ;(bind-key "M-/" 'ac-stop)
      )
   
   )


(org-babel-load-file "~/.emacs.d/init-all.org")
(bind-key "C-o" 'ac-complete)
(bind-key* "C-O" 'auto-complete)
(bind-key* "C-S-o" 'auto-complete)
(bind-key "C-o" 'ac-expand ac-mode-map)


                                        ;(require 'auto-complete)


                                        ;(require 'yasnippet)
                                        ;(global-set-key (kbd "C-o") 'yas-prev-field)
                                        ;(define-key yas-minor-mode-map (kbd "C-o") 'yas-expand)


;; Add ac-source-dictionary to ac-sources of all buffer
(defun cg/ac-add-ess-functionality ()
   "allow manually adding etags-source, so that it is not loaded from
startup"
   (interactive)
   (setq ac-sources (append ac-sources '(ac-source-etags ac-source-R))))




;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;;;
;; ;;;;;       yasnippet
;; ;;;
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (add-to-list 'load-path
;;               "~/.emacs.d/extensions/yasnippet")
;; (require 'yasnippet)
;; (setq yas-snippet-dirs "~/.emacs.d/extensions/yasnippet/snippets")
;; (add-hook 'LaTeX-mode-hook '(lambda ()
;;                             (yas-minor-mode 1)))
;; (add-hook 'org-mode-hook '(lambda () (yas-minor-mode 1)))

;; (yas/reload-all)
;; (define-key yas-minor-mode-map (kbd "C-i") 'auto-complete) ; restart ac-mode
;; (define-key yas-minor-mode-map (kbd "C-i") 'ac-complete) ; restart ac-mode
;; (define-key yas-minor-mode-map (kbd "C-i") 'auto-complete) ; restart ac-mode
;; (define-key yas-minor-mode-map (kbd "C-i") 'ac-complete) ; restart ac-mode
;; (define-key yas-minor-mode-map (kbd "C-o") 'yas-expand)
;; (define-key yas-minor-mode-map (kbd "C-S-o") 'yas-prev-field)





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
;;;;;       python
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(package-initialize)
(elpy-enable)
(setq python-shell-interpreter "python"
	python-shell-interpreter-args "-i")
(unbind-key "C-c C-d" elpy-mode-map)
(unbind-key "C-c C-n" elpy-mode-map)
(unbind-key "C-c C-j" elpy-mode-map)
(define-key elpy-mode-map (kbd "M-C-x") 'elpy-shell-send-group)
(define-key elpy-mode-map (kbd "C-c C-c") 'elpy-shell-send-group-and-step)
(define-key elpy-mode-map (kbd "C-c C-n") 'elpy-shell-send-group-and-step-and-go)
(define-key elpy-mode-map (kbd "C-c C-j") 'elpy-shell-send-statement)
(define-key elpy-mode-map (kbd "C-c M-j") 'elpy-shell-send-statement-and-step)
(define-key elpy-mode-map (kbd "C-c C-b") 'elpy-shell-send-buffer)
(define-key elpy-mode-map (kbd "C-c M-b") 'elpy-shell-send-buffer-and-go)
(define-key elpy-mode-map (kbd "C-c C-d C-e") 'elpy-shell-set-local-shell)


(defun cg/show-python-variable ()
   "Show value of python variable in python process"
   (interactive)
	(save-window-excursion
		(cg/copy-nowhitespace-or-double-sexp)
		(elpy-shell-switch-to-shell)
		(comint-send-input)
		(yank)
		(comint-send-input)
		)
	)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;;;       julia
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (add-to-list 'load-path "/usr/share/emacs/site-lisp/ess")

;; allow julia to be loaded through call to julia-mode or
;; ess-inferior process
;; follow-ups: etags?
(use-package julia-mode
   :defer t
   :commands julia-mode
   :mode ("\\.jl$" . julia-mode)
   :init
   (progn
      (autoload 'julia-mode "julia-mode" nil t)
      (setq inferior-julia-program-name "/usr/bin/julia")
      )
   :config
   (progn
      (add-to-list 'julia-mode-hook 'cg/modify-current-syntax-table)
      (setq inferior-julia-program-name "/usr/bin/julia")
      (add-to-list 'julia-mode-hook 'cg/command-line-keybindings)
      ;; (add-to-list 'inferior-ess-mode-hook 'cg/command-line-keybindings)      
      )
   )

(use-package ess-julia.el
   :defer t
   :commands julia
   :init                                ; run before actual loading
   (progn
      (autoload 'julia "ess-julia.el" nil t)
      (setq inferior-julia-program-name "/usr/bin/julia")
      )
   :config
   (progn
      (require 'ess-site)
      (setq inferior-julia-program-name "/usr/bin/julia")
      (setq ess-tracebug-prefix "\M-c")   ; define debug-mode starting key
      (setq ess-use-tracebug t)           ; tracebug is called for R
                                        ; AND JULIA!!
      (setq ess-tracebug-inject-source-p t)
      (add-to-list 'julia-mode-hook 'cg/command-line-keybindings)
      ;; (add-to-list 'inferior-ess-mode-hook 'cg/command-line-keybindings)            
      )
   )
;; in order to add ess-process afterward, apply julia-mode again on
;; open buffers - probably ess-julia.el has to be loaded again also:
;; M-x load-file ess-julia.el


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;;;       R-mode
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun cg/R-assign ()
   "Insert assignment sign of R language"
   (interactive)
   (insert " <- ")
   )

(use-package ess-site
   :defer t
   :commands R
   :mode ("\\.[Rr]$" . R-mode)
   :config
   (progn
      (unbind-key "_" ess-mode-map)
      (unbind-key "_" inferior-ess-mode-map)      
      (bind-key "C-M--" 'cg/R-assign ess-mode-map)
      (bind-key "C-M--" 'cg/R-assign inferior-ess-mode-map)
      (setq ess-tracebug-prefix "\M-c")   ; define debug-mode starting key
      (setq ess-use-tracebug t)           ; tracebug is called for R
                                        ; AND JULIA!!
      (setq ess-tracebug-inject-source-p t)
      (require 'ess-r-args)
      (require 'ess-R-object-tooltip)
      (define-key ess-mode-map (kbd "C-c 1") 'r-show-head)
      (define-key ess-mode-map (kbd "C-c 2") 'r-show-str)
      
      )
   )


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
;;;;;       bibtex
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (org-babel-load-file "~/.emacs.d/init-bibtex-config.org")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;;;       gnus, additional to .gnus file
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'gnus)
                                        ;(org-babel-load-file "~/.emacs.d/init-gnus.org")
;; (add-to-list 'load-path "~/.emacs.d/extensions/bbdb/lisp/")
;; (require 'bbdb)
;; (bbdb-initialize 'gnus 'message)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;;;       auctex
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; auctex
;; (org-babel-load-file "~/.emacs.d/init-latex.org")


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
;;;;;       w3m
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; ;; load w3m
;; (add-to-list 'load-path "/usr/share/emacs/site-lisp/w3m/")
;; (add-to-list 'load-path "/usr/share/emacs/site-lisp/w3m")
;; (require 'w3m)
;; (require 'mime-w3m)

;; ;; load settings
(org-babel-load-file "~/.emacs.d/init-w3m.org")


;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;;;
;; ;;;;;       google-translate
;; ;;;
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (require 'google-translate)
(use-package google-translate
   :defer t
   :commands (google-translate-query-translate-reverse
                google-translate-query-translate
                google-translate-at-point
                google-translate-at-point-reverse)
   :init
   (progn
      (bind-key "C-t l" 'google-translate-query-translate-reverse)
      (bind-key "C-t L" 'google-translate-query-translate)
      (bind-key "C-t K" 'google-translate-at-point)
      (bind-key "C-t k" 'google-translate-at-point-reverse)
      )
   :config
   (setq google-translate-default-source-language "en")
   (setq google-translate-default-target-language "de")
   ;; (org-babel-load-file "~/.emacs.d/init-google-translate.org") 
   )

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;;;
;; ;;;;;       thesaurus.el
;; ;;;
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package thesaurus
   :config
   (progn
      (org-babel-load-file "~/Dropbox/customs/personal_data/thesaurus_api_setup.org")
      (bind-key "C-t u" 'thesaurus-choose-synonym-and-replace)
      )
   )

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;;;
;; ;;;;;       org-drill
;; ;;;
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package org-drill
   :defer t
   :load-path "~/.emacs.d/extensions/org/contrib/lisp/"
   :command org-drill
   :config
   (progn
      (setq org-drill-add-random-noise-to-intervals-p t) ; add random noise
      (setq org-drill-adjust-intervals-for-early-and-late-repetitions-p t)
      (setq org-drill-maximum-duration 20)
      (setq org-drill-learn-fraction 0.35)
      )
   )

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
