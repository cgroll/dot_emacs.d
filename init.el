
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;;;       auctex
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; auctex
(org-babel-load-file "~/.emacs.d/init-latex.org")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;;;       auto-complete-mode
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; auto-complete
(org-babel-load-file "~/.emacs.d/init-ac.org")

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

(org-babel-load-file "~/.emacs.d/init-matlab.org")


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
;;;;;       google-translate
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'google-translate)
(org-babel-load-file "~/.emacs.d/init-google-translate.org")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;;;       thesaurus.el
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'thesaurus)
(org-babel-load-file
   "~/Dropbox/personal_data/thesaurus_api_setup.org")
(define-key global-map (kbd "C-t u") 'thesaurus-choose-synonym-and-replace)

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
 '(org-agenda-files (quote ("~/org/refile.org" "~/org/todo.org")))
 '(org-agenda-show-all-dates t)
 '(org-agenda-skip-deadline-if-done t)
 '(org-agenda-skip-scheduled-if-done t)
 '(org-agenda-start-on-weekday nil)
 '(org-clock-into-drawer t)
 '(org-deadline-warning-days 14)
 '(org-drill-optimal-factor-matrix (quote ((3 (2.5 . 2.5) (2.36 . 2.36)) (2 (2.08 . 2.166) (1.8199999999999998 . 2.159) (2.2199999999999998 . 2.328) (2.22 . 2.22) (2.46 . 2.497) (1.7000000000000002 . 2.15) (2.04 . 2.243) (1.96 . 2.238) (2.1799999999999997 . 2.325) (2.36 . 2.439) (2.5 . 2.5) (2.6 . 2.588) (1.56 . 2.074)) (1 (1.56 . 3.482) (2.2199999999999998 . 3.806) (2.04 . 3.59) (2.36 . 3.902) (2.1799999999999997 . 3.804) (2.5 . 4.0) (2.6 . 4.14) (1.96 . 3.706) (1.7000000000000002 . 3.608)))))
 '(org-fast-tag-selection-single-key nil)
 '(org-format-latex-options (quote (:foreground default :background default :scale 1.7 :html-foreground "Black" :html-background "Transparent" :html-scale 2.0 :matchers ("begin" "$1" "$" "$$" "\\(" "\\["))))
 '(org-reverse-note-order nil)
 '(safe-local-variable-values (quote ((org-export-babel-evaluate . t) (org-export-publishing-directory . "./src_results/") (org-export-babel-evaluate . no-export)))))
(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 )
