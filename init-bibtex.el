;; multiple functions
;; cg/get-bibtex-to-processing-buffer
;; cg/clean-up-bib-info-entry
;; cg/create-org-mode-entry

(defun cg/get-bibtex-to-processing-buffer ()
   "the function asks for literature information input and brings it
   to a file that will be starting point for further proceeding"
   (interactive)
   (let ((mylist (list "emacs-kill-ring" "clipboard" "file"))
           (process-file "~/literature/management/bib_process_file.bib")
           bib-info-type bib-info-entry
           )
      
      (setq bib-info-type (ido-completing-read
                             "Where is the bibtex entry? " mylist))
      (cond
         ((string= bib-info-type "file")  ; in case I want to choose a
                                        ; file
            (save-excursion
               (ido-find-file-other-window) ; copy bibtex text 
               (setq bib-info-entry (buffer-string))
               (kill-buffer)
               )
            (find-file process-file)
            (delete-region (point-min) (point-max))
            (goto-char (point-min))
            (insert bib-info-entry)
            )
         
         ((string= bib-info-type "emacs-kill-ring") ; in case bibtex is
                                        ; given via emacs-kill-ring 
            (setq bib-info-entry (substring-no-properties (car
                                                             kill-ring)))
            (find-file-other-window process-file)
            (delete-region (point-min) (point-max))
            (goto-char (point-min))
            (insert bib-info-entry)
            )
         
         ((string= bib-info-type "clipboard") ; get entry from
                                        ; clipboard
            (find-file-other-window process-file)
            (delete-region (point-min) (point-max))
            (goto-char (point-min))
            (clipboard-yank)
            )
         )
      )
   )

(global-set-key (kbd "C-4") 'cg/get-bibtex-to-processing-buffer)

(beginning-of-buffer)
(search-forward "@")
(backward-char)

(defun cg/clean-up-bibtex-entry ()
   "the function shall clean-up an imported bibtext entry"
   (interactive)
   (bibtex-clean-entry t)
   (bibtex-fill-entry)
   )

(global-set-key (kbd "C-5") 'cg/clean-up-bibtex-entry)

(forward-sexp 2)
(insert "\n\n")
(insert "* org-entry-creation")

(setq org-capture-templates
   (append org-capture-templates
      (quote (
                ("B" "BIBTEX entry"
                   entry
                   (file+headline "~/literature/management/bib_process_file.bib" "org-entry-creation")
                   "* %:title   %^G
    :PROPERTIES:
    :author: %:author
    :year: %:year
    :journal: %:journal
    :Bibtex key: %:key
    :link to bibtex entry: [file:~/literature/management/refs_test.bib%:key]
    :link to pdf:
   %(cg/prompt-for-file-and-return-bibtex-key-file-name-with-extension
   %:key) 
    :link to notes:
   %(cg/prompt-for-file-and-return-bibtex-key-file-name %:key)
    :entered: %U
    :END:
    Comment: %?
    " :empty-lines 2)
                )
         )
      )
   )


(defun cg/prompt-for-file-and-return-bibtex-key-file-name-with-extension
   (key)
   "function shall prompt for file, create clean new name for
   file, move file to literature database, and return org-link to
   file"
   (interactive)
   (let (local-file new-local-file-name return-string)
      (setq local-file (read-file-name
                          "Where is associated document file?"
                          "~/Downloads/" nil 'confirm))
      (if (file-exists-p local-file)
         (progn
            (setq new-local-file-name
               (concat "~/literature/" key "."
                  (file-name-extension local-file)))
            (rename-file local-file new-local-file-name)
            (setq return-string (concat "[file:" new-local-file-name
   "]"))
            )
         (setq return-string (read-string "Where can file be found?"))
         return-string
      )
      )
   )

(defun cg/prompt-for-file-and-return-bibtex-key-file-name
   (key)
   "function shall prompt for file, create clean new name for notes 
   file, and return link name"
   (interactive)
   (concat "[file:~/literature/" key ".org]")
   )

(defun cg/caller-function ()
   "call function above"
   (interactive)
   (cg/prompt-for-file-and-return-bibtex-key-file-name-with-extension
      "engl_05_03_test_str")
   )

(setq link_path (cg/prompt-for-file-and-return-bibtex-key-file-name-with-extension
      "engl_05_03_test_str"))

(global-set-key (kbd "C-6") 'cg/caller-function)
   
   
   
   )

(setq current-bib-info-entry-alist-of-fields (bibtex-parse-entry))
;; ;; prepend two newlines
;;       (setq bib-info-entry (concat "\n\n" bib-info-entry))

;;       ;; now append bib-info-entry to refs_test.bib file
;;       (write-region bib-info-entry nil 'process-file t)

;;       ;; get location 
;;       (setq location (read-file-name
;;                         "Where is associated document file?"
;;                         "~/Downloads/" nil 'confirm))
;;       (if (file-exists-p location)
;;          ;; mv file to bibliography with new title

;;          ;; create location-string with link properties

;;          ;; location string has to be plugged into template


;;          ;; change name of file according to title
;;          (progn
;;             (org-capture)

;;             )


;;          ;; get title string



;;          )

;;       ;; (with-temp-buffer
;;       ;;    (insert bib-info-entry)
;;       ;;    (write-region (point-min)
;;       ;;       (point-max)
;;       ;;       "~/literature/management/refs_test.bib") )
;;       )
;;    )


;; (defun cg/capture-bibtex ()
;;    "function shall create org-entry for literature document, thereby
;;        optionally renaming pdf file and creating bibtext entry"
;;    (interactive)
;;    (let ((mylist (list "emacs-kill-ring" "clipboard" "file"))
;;            bib-info-type bib-info-entry
;;            (tmpbuf (get-buffer-create "*captured-bib-info-entry"))
;;            )
;;       (setq bib-info-type (ido-completing-read "Where is the bibtex
;;    entry? " mylist))
;;       (cond

;;          ((string= bib-info-type "file")  ; in case I want to choose a
;;                                         ; file
;;             (ido-find-file-other-window) ; copy bibtex text 
;;             (setq bib-info-entry (buffer-string))
;;             )

;;          ((string= bib-info-type "emacs-kill-ring") ; in case bibtex is
;;                                         ; given via emacs-kill-ring 
;;             (setq bib-info-entry (substring-no-properties (car kill-ring)))
;;             )

;;          ((string= bib-info-type "clipboard") ; get entry from clipboard
;;             (with-current-buffer tmpbuf
;;                (delete-region (point-min) (point-max))
;;                (goto-char (point-min))
;;                (clipboard-yank)
;;                (setq bib-info-entry (buffer-string))
;;                )
;;             )
;;          )
;;       ;; prepend two newlines
;;       (setq bib-info-entry (concat "\n\n" bib-info-entry))

;;       ;; now append bib-info-entry to refs_test.bib file
;;       (write-region bib-info-entry nil "~/literature/management/refs_test.bib" t)

;;       ;; get location 
;;       (setq location (read-file-name
;;                         "Where is associated document file?"
;;                         "~/Downloads/" nil 'confirm))
;;       (if (file-exists-p location)
;;          ;; mv file to bibliography with new title

;;          ;; create location-string with link properties

;;          ;; location string has to be plugged into template


;;          ;; change name of file according to title
;;          (progn
;;             (org-capture)

;;             )


;;          ;; get title string



;;          )

;;       ;; (with-temp-buffer
;;       ;;    (insert bib-info-entry)
;;       ;;    (write-region (point-min)
;;       ;;       (point-max)
;;       ;;       "~/literature/management/refs_test.bib") )
;;       )
;;    )






;; (global-set-key (kbd "C-4") 'cg/parse-bibtex-and-make-local-variable)