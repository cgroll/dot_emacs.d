
;(setq inferior-julia-program-name "/path/to/julia/julia-release-basic")

;; (defface org-block-begin-line
;;   '((t (:underline "#A7A6AA" :foreground "#ff0000" :background "#6b6b6b")))
;;   "Face used for the line delimiting the begin of source blocks.")

;; (defface org-block-background
;;   '((t (:background "#262626")))
;;   "Face used for the source block background.")

;; (defface org-block-end-line
;;   '((t (:overline "#A7A6AA" :foreground "#ff0000" :background "#6b6b6b")))
;;   "Face used for the line delimiting the end of source blocks.")

(defface org-block-begin-line
  '((t (:underline "#A7A6AA" :foreground "#ff0000" :background "#262626")))
  "Face used for the line delimiting the begin of source blocks.")

(defface org-block-background
  '((t (:background "#262626")))
  "Face used for the source block background.")

(defface org-block-end-line
  '((t (:overline "#A7A6AA" :foreground "#ff0000" :background "#262626")))
  "Face used for the line delimiting the end of source blocks.")


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
(visit-tags-table "~/Dropbox/research_current_ntb_head/TAGS")
(setq inferior-julia-program-name
   "~/julia/usr/bin/julia-release-basic")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;;;       julia
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(add-to-list 'load-path "~/julia/contrib/") ;Tell emacs to look for the file there.
(require 'julia-mode)                             ;Tell it to load it.
(setq auto-mode-alist                             ;Tell it to go to
                                        ;julia-mode when a Julia
                                        ;file(.jl) is spotted. 
      (append '(("\\.jl$" . julia-mode)) auto-mode-alist))
(setq inferior-julia-program-name "~/julia/usr/bin/julia-release-basic")



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;;;       org-mode
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; org
(add-to-list 'load-path "/usr/share/emacs/site-lisp/org")
(require 'org-install)

;; individual org-mode settings
(org-babel-load-file "~/.emacs.d/init-org.org")

(org-babel-load-file "~/.emacs.d/init-bibtex-config.org")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;;;       auto-complete-mode
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(add-to-list 'load-path "~/.emacs.d/extensions/auto-complete")
(add-to-list 'load-path "~/.emacs.d/extensions/yasnippet")
(require 'auto-complete)
(ac-set-trigger-key "C-o")
(require 'yasnippet)
(yas-global-mode 1)
(setq yas-snippet-dirs "~/.emacs.d/extensions/yasnippet/snippets")
(org-babel-load-file "~/.emacs.d/init-ac.org")
(global-set-key (kbd "C-o") 'yas-prev-field)
(define-key yas-minor-mode-map (kbd "C-o") 'yas-expand)

;; ;; auto-complete
;; (add-to-list 'load-path "~/.emacs.d/extensions/")
;; (org-babel-load-file "~/.emacs.d/init-ac.org")

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;;;       bibtex
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(org-babel-load-file "~/.emacs.d/init-bibtex-config.org")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;;;       gnus, additional to .gnus file
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;(org-babel-load-file "~/.emacs.d/init-gnus.org")
  (add-to-list 'load-path "~/.emacs.d/extensions/bbdb/lisp/")
  (require 'bbdb)
  (bbdb-initialize 'gnus 'message)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;;;       auctex
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; auctex
(org-babel-load-file "~/.emacs.d/init-latex.org")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;;;       emacs extensions
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; minor extensions
(add-to-list 'load-path "~/.emacs.d/extensions/")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;;;       parentheses
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

;; load w3m
(add-to-list 'load-path "/usr/share/emacs/site-lisp/w3m/")
(add-to-list 'load-path "/usr/share/emacs/site-lisp/w3m")
(require 'w3m)
(require 'mime-w3m)

;; load settings
(org-babel-load-file "~/.emacs.d/init-w3m.org")


;; magit
(add-to-list 'load-path "/usr/share/emacs/site-lisp/magit-master")
(require 'magit)

;; ido
(require 'ido)
(ido-mode t)

;; ess: better is probably autoload in order to speed up starting procedure
(add-to-list 'load-path "/usr/share/emacs/site-lisp/ess-12.09/lisp/")
(require 'ess-site)
(org-babel-load-file "~/.emacs.d/init-R.org")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;;;       configurations
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(org-babel-load-file "~/.emacs.d/init-config.org")   

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;;;       google-translate
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'google-translate)

;; load settings
(org-babel-load-file "~/.emacs.d/init-google-translate.org")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;;;       thesaurus.el
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'thesaurus)
(org-babel-load-file "~/Dropbox/personal_data/thesaurus_api_setup.org")
(define-key global-map (kbd "C-t u") 'thesaurus-choose-synonym-and-replace)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;;;       matlab
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; load matlab only on some computers

(if (string= system-name "chris-wk")
   (org-babel-load-file "~/.emacs.d/init-matlab.org")
   (custom-set-variables
      '(matlab-shell-command "/home/chris/MATLAB/R2012a/bin/matlab")
      )
   )

(if (string= system-name "chris-lpt")
   (org-babel-load-file "~/.emacs.d/init-matlab.org")
   (custom-set-variables
      '(matlab-shell-command "~/remote_matlab")
      )
   )

(if (string= system-name "chris-ntb")
   (org-babel-load-file "~/.emacs.d/init-matlab.org")
   (custom-set-variables
      '(matlab-shell-command "~/remote_matlab")
      )
   )



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;;;       org-drill
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(add-to-list 'load-path "~/.emacs.d/extensions/org/contrib/lisp/")

(require 'org-drill)
(setq org-drill-add-random-noise-to-intervals-p t) ; add random noise
(setq org-drill-adjust-intervals-for-early-and-late-repetitions-p t)
(setq org-drill-maximum-duration 20)
(setq org-drill-learn-fraction 0.35)


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
 '(ac-dictionary-files (quote ("~/.emacs.d/extensions/auto-complete/dict/own_word_list")))
 '(ac-trigger-key "C-o")
 '(calendar-latitude 48.139)
 '(calendar-longitude 11.58)
 '(global-auto-complete-mode t)
 '(matlab-shell-command "~/remote_matlab")
 '(org-agenda-files (quote ("~/org/refile.org" "~/org/todo.org")))
 '(org-agenda-show-all-dates t)
 '(org-agenda-skip-deadline-if-done t)
 '(org-agenda-skip-scheduled-if-done t)
 '(org-agenda-start-on-weekday nil)
 '(org-clock-into-drawer t)
 '(org-deadline-warning-days 14)
 '(org-drawers (quote ("" "CLOCK" "LOGBOOK" "RESULTS")))
 '(org-drill-optimal-factor-matrix (quote ((3 (2.46 . 2.46) (1.56 . 2.2) (2.04 . 2.244) (2.08 . 2.166) (2.6 . 2.6) (1.96 . 2.316) (2.2199999999999998 . 2.379) (1.9000000000000001 . 2.111) (1.8199999999999998 . 2.26) (2.5 . 2.5) (2.36 . 2.439)) (2 (2.08 . 2.166) (1.8199999999999998 . 2.26) (2.2199999999999998 . 2.379) (2.22 . 2.22) (2.46 . 2.497) (1.7000000000000002 . 2.255) (2.04 . 2.319) (1.96 . 2.238) (2.1799999999999997 . 2.325) (2.36 . 2.439) (2.5 . 2.5) (2.6 . 2.588) (1.56 . 2.2)) (1 (2.08 . 3.902) (1.8199999999999998 . 3.615) (1.56 . 3.52) (2.2199999999999998 . 3.806) (2.04 . 3.59) (2.36 . 3.902) (2.1799999999999997 . 3.804) (2.5 . 4.0) (2.6 . 4.14) (1.96 . 3.706) (1.7000000000000002 . 3.608)))))
 '(org-fast-tag-selection-single-key nil)
 '(org-format-latex-options (quote (:foreground default :background default :scale 1.7 :html-foreground "Black" :html-background "Transparent" :html-scale 2.0 :matchers ("begin" "$1" "$" "$$" "\\(" "\\["))))
 '(org-reverse-note-order nil)
 '(safe-local-variable-values (quote ((org-export-babel-evaluate . t) (org-export-publishing-directory . "./src_results/") (org-export-babel-evaluate . no-export))))
 '(yas-fallback-behavior (quote call-other-command))
 '(yas-prompt-functions (quote (yas-ido-prompt yas-x-prompt yas-dropdown-prompt yas-completing-prompt yas-ido-prompt yas-no-prompt)))
 '(yas-triggers-in-field t)
 '(yas-wrap-around-region t))
(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(org-level-1 ((t (:inherit outline-1 :foreground "yellow" :height 1.8 :width expanded))))
 '(org-level-2 ((t (:inherit outline-2 :foreground "orange" :height 1.6 :width normal))))
 '(org-level-3 ((t (:inherit outline-3 :foreground "green yellow" :height 1.4 :width normal))))
 '(org-level-4 ((t (:inherit outline-4 :foreground "deep sky blue" :height 1.2))))
 '(org-level-5 ((t (:inherit outline-5 :foreground "deep pink")))))
(put 'upcase-region 'disabled nil)
