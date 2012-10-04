
(add-to-list 'load-path "~/.emacs.d")
(add-to-list 'load-path "~/.emacs.d/extensions/auto-complete")
(require 'auto-complete-config)
(ac-config-default)
(add-to-list 'ac-dictionary-directories "~/.emacs.d/extensions/auto-complete/dict")

(setq-default ac-sources '(ac-source-filename ac-source-features ac-source-functions ac-source-yasnippet ac-source-variables ac-source-symbols ac-source-abbrev ac-source-dictionary ac-source-words-in-same-mode-buffers))

(defun cg/ac_toggle_language_german ()
  "Switch autocompletion language to german"
  (interactive)
  (setq ac-user-dictionary-files '("~/.emacs.d/auto-complete/dict/german_words"))
  (ac-clear-dictionary-cache)
)

(defun cg/ac_toggle_language_english ()
  "Switch autocompletion language to english"
  (interactive)
  (setq ac-user-dictionary-files '("~/.emacs.d/auto-complete/dict/own_word_list"))
  (ac-clear-dictionary-cache)
)

(auto-complete-mode t)
(setq ac-ignore-case nil)               ; do not ignore cases

(ac-set-trigger-key "C-i")
(global-set-key (kbd "C-i") 'ac-expand) ; use C-j as trigger key / problem with new line?
(define-key ac-completing-map "\M-/" 'ac-stop) ; undo completion
(define-key ac-mode-map (kbd "C-i") 'auto-complete) ; restart ac-mode
(define-key ac-mode-map (kbd "C-i") 'ac-complete) ; restart ac-mode

(setq ac-auto-start 3)                  ; minimum number of typed characters to start
(setq ac-delay 0)                       ; delay time to start auto-completion
(setq ac-auto-show-menu 1.4)            ; delay time to show menu

(add-to-list 'ac-modes 'text-mode)
(add-to-list 'ac-modes 'org-mode)
(add-to-list 'ac-modes 'fundamental-mode)
(add-to-list 'ac-modes 'matlab-mode)
(add-to-list 'ac-modes 'ess-mode)
(add-to-list 'ac-modes 'emacs-lisp-mode)
(add-to-list 'ac-modes 'message-mode)
(global-auto-complete-mode t)
