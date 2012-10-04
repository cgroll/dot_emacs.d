
(setq default-major-mode 'org-mode)

(add-hook 'text-mode-hook 'turn-on-auto-fill)
(add-hook 'org-mode-hook 'turn-on-auto-fill)
(add-hook 'fundamental-mode-hook 'turn-on-auto-fill)
(add-hook 'ess-mode-hook 'turn-on-auto-fill)
(add-hook 'emacs-lisp-mode-hook 'turn-on-auto-fill)
(add-hook 'message-mode-hook 'turn-on-auto-fill)
(setq fill-column 69)

(delete-selection-mode 1)

(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))

(column-number-mode 1)

(add-to-list 'load-path "~/.emacs.d/extensions/color-theme-6.6.0")
(require 'color-theme)
(color-theme-initialize)
(color-theme-jsc-dark)

(global-hl-line-mode 1)
(set-cursor-color "red")

(setq scroll-step 1)

(setq-default tab-width 3)
(setq-default tab-stop-list '(3 6 9 12 15 18 21 24))
(setq-default lisp-indent-offset 3)

(global-set-key (kbd "C-j") 'indent-for-tab-command)
(define-key ac-mode-map (kbd "C-j") 'indent-for-tab-command)
(define-key ess-mode-map (kbd "C-j") 'ess-indent-command)

(global-set-key (kbd "RET") 'newline-and-indent)
(global-set-key (kbd "C-M-<return>") 'newline)
(global-set-key (kbd "C-M-S-<return>") 'indent-new-comment-line)

(setq-default indent-tabs-mode nil)
(setq indent-tabs-mode nil)
(tab-to-tab-stop)
  (defun cg/insert-tab ()
    "insert TAB at point"
    (interactive)
    (insert-tab)
    )
  (global-set-key (kbd "M-j") 'cg/insert-tab)

(org-indent-mode nil)

(global-set-key (kbd "M-n") 'forward-word)
(global-set-key (kbd "M-p") 'backward-word)
(global-set-key (kbd "M-<backspace>") 'backward-kill-word)
(global-set-key (kbd "M-d") 'kill-word)

(global-set-key (kbd "C-M-n") 'forward-sexp)
(global-set-key (kbd "C-M-p") 'backward-sexp)
(global-set-key (kbd "C-M-<backspace>") 'backward-kill-sexp)
(global-set-key (kbd "C-M-d") 'kill-sexp)

(setq sentence-end-double-space nil)

(defun cg/kill-start-of-line ()
  "kill from point to start of line"
  (interactive)
  (kill-line 0)
  )
(global-set-key (kbd "M-k") 'cg/kill-start-of-line)

(define-key org-mode-map (kbd "C-ü") 'org-edit-special)
(define-key org-src-mode-map (kbd "C-ü") 'org-edit-src-exit)

(global-set-key (kbd "M-SPC") 'other-window)
(defun cg/inverse-other-window ()
  "window cycling in inverse direction"
  (other-window -1)
  )
(global-set-key (kbd "M-S-SPC") 'cg/inverse-other-window)

(defun cg/increase-current-window ()
  "Increase current window by two lines"
  (interactive)
  (enlarge-window 2)
  )
(global-set-key (kbd "C-+") 'cg/increase-current-window)

(defun cg/decrease-current-window ()
  "Decrease current window by two lines"
  (interactive)
  (other-window 1)
  (enlarge-window 2)
  (other-window -1)
  )
(global-set-key (kbd "M-+") 'cg/decrease-current-window)

(defun toggle-major-window ()
  "Set focus on second window, and enlargen it
to cover about 3/4 of overall area"
  (interactive)
  (if (not (one-window-p))              ; if more than one window
      (progn
        (other-window 1)                ; switch to other window
        (balance-windows)               ; split overall area equally
        (enlarge-window 8))))           ; enlargen current window by 8 lines
    (global-set-key (kbd "C-M-+") 'toggle-major-window)

(defun set-focus-lower-window ()
  "Move focus of lower window so that last line of buffer
exactly matches last line of frame"
    (interactive)
    (if (not (one-window-p))            ; if more than one window
    (progn
      (other-window 1)                  ; move point to second window
      (end-of-buffer)                   ; go to end of buffer
      (recenter -1)                     ; move point to last line of frame
      (other-window 1))))               ; move point back again
(global-set-key (kbd "C-x C-l") 'set-focus-lower-window)

(global-set-key (kbd "C-x C-b") 'ido-switch-buffer-other-window)
(global-set-key (kbd "C-x C-d") 'dired-other-window)
(global-set-key (kbd "C-x C-f") 'ido-find-file-other-window)
(global-set-key (kbd "C-x 4 b") 'list-buffers)

(global-set-key (kbd "C-x f") 'ido-find-file)

(defun comment-or-uncomment-line ()
  (interactive)
  (comment-or-uncomment-region (line-beginning-position)
                               (line-end-position)))
(global-set-key (kbd "C-#") 'comment-or-uncomment-line)
(define-key org-mode-map (kbd "C-#") 'comment-or-uncomment-line)
