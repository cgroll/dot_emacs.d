
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-cc" 'org-capture)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)

(add-to-list 'auto-mode-alist '("\\.\\(org\\|org_archive\\|txt\\)$" . org-mode))

(setq org-agenda-files (quote ("~/org/")))
(setq org-default-notes-file "~/org/refile.org")

(setq org-use-fast-todo-selection t)

(setq org-todo-keywords
   (quote ((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d!/!)")
             (sequence "WAITING(w@/!)" "|" "CANCELLED(c@/!)" "SOMEDAY(s!/!)"))))

(setq org-todo-keyword-faces
   (quote (("TODO" :foreground "red" :weight bold)
             ("NEXT" :foreground "blue" :weight bold)
             ("DONE" :foreground "forest green" :weight bold)
             ("WAITING" :foreground "orange" :weight bold)
             ("SOMEDAY" :foreground "magenta" :weight bold)
             ("CANCELLED" :foreground "forest green" :weight bold))))

(setq org-treat-S-cursor-todo-selection-as-state-change nil)

(setq org-capture-templates
   (quote (
             ("e" "Templates for events")
             
             ;; Birthday entry into anniversaries calendar with prompt
             ;; for date; date is active -> appears in agenda
             ("ea" "annually repeating event"
             entry (file "~/org/annuals.org")
                "* %? \nDate: %^{Do not forget to add +1y!}t\n"
                :clock-in t :clock-resume t)
             
             ;; Future event: prompt for date without time
             ("ed" "daily event without time"
             entry (file "~/org/refile.org")
                "* %? :calendar: \n%^{Which date?}t \nAdded:%U\n"
                :clock-in t :clock-resume t)
             
             ;; Future event: prompt for date WITH time
             ("et" "event with time specification"
             entry (file "~/org/refile.org")
                "* %? :calendar: \n%^{Which date and time?}T \nAdded: %U\n"
                :clock-in t :clock-resume t)

             ;; Future event lasting for multiple days
             ("ee" "enduring event"
             entry (file "~/org/refile.org")
                "* %? :calendar: \n%^{Starting time?}T--%^{Ending time?}T \nAdded: %U\n"
                :clock-in t :clock-resume t)
             
             ;; Entry in log: at current time finished activity with completion
             ("el" "logbook: finished activities"
             entry (file+datetree "~/org/log.org")
                "* %U - %^{Activity?|lunch|break|buy|program|read|work} "
                :clock-in t :clock-resume t)
             
             ;; Stopwatch activity without prompt
             ("es" "stopwatch"
             entry (file+datetree "~/org/log.org")
                "* Stopwatch %? \nStarted: %U\n"
                :clock-in t :clock-resume t)
             
             ;; Entry in creditcard with prompt for sum and cursor for item specification
             ("ec" "credit-card info"
             entry (file+datetree "~/org/creditcard.org")
                "* %? - %^{Amount?} \nAdded: %U\n"
                :clock-in t :clock-resume t)
             
             )
      )
   )

(setq org-capture-templates
     (append org-capture-templates
        (quote (
                  ;; notes without code or yanking
                  ("N" "Plain notes without code or yanking")
  
                  ;; git-note
                  ("Ng" "entry to git_notes"
                     entry (file+headline "~/comp_science/git_notes.org" "Captured
                  notes")
                     "* %? ")
  
                  ;; emacs-note
                  ("Ne" "entry to emacs_notes"
                     entry (file+headline "~/comp_science/emacs_notes"
                  "Captured notes")
                     "* %? ")
  
                  ;; bash-note
                  ("Nb" "entry to bash_notes"
                     entry (file+headline "~/comp_science/bash_notes"
                  "Captured notes")
                     "* %? ")
                  
                  ;; gtd-note
                  ("No" "entry to gtd_notes"
                     entry (file+headline "~/comp_science/gtd_notes"
                  "Captured notes")
                     "* %? ")
                  
                  ;; ubuntu-note
                  ("Nu" "entry to ubuntu_notes"
                     entry (file+headline "~/comp_science/ubuntu_notes"
                  "Captured notes")
                     "* %? ")
                  
                  ;; matlab-note
                  ("Nm" "entry to matlab_notes"
                     entry (file+headline "~/comp_science/matlab_notes"
                  "Captured notes")
                     "* %? ")
                  )
           )
        )
)

(setq org-capture-templates
     (append org-capture-templates
        (quote (
                  ;; notes with code yanking
                  ("c" "Notes with code yanking")
  
                  ;; git-note
                  ("cg" "entry to git_notes"
                     entry (file+headline "~/comp_science/git_notes.org" "Captured
                  notes")
                     "* %? \n#+begin_src
                  %^{Language?|emacs-lisp|sh|matlab|r|julia} \n%^C\n#+end_src \n%a\n%U\n")
  
                  ;; emacs-note
                  ("ce" "entry to emacs_notes"
                     entry (file+headline "~/comp_science/emacs_notes.org" "Captured
                  notes")
                     "* %? \n#+begin_src
                  %^{Language?|emacs-lisp|sh|matlab|r|julia} \n%^C\n#+end_src \n%a\n%U\n")
                  
                  ;; bash-note
                  ("cb" "entry to bash_notes"
                     entry (file+headline "~/comp_science/bash_notes.org" "Captured
                  notes")
                     "* %? \n#+begin_src
                  %^{Language?|emacs-lisp|sh|matlab|r|julia} \n%^C\n#+end_src \n%a\n%U\n")
  
                  ;; ubuntu-note
                  ("cu" "entry to ubuntu_notes"
                     entry (file+headline "~/comp_science/ubuntu_notes.org" "Captured
                  notes")
                     "* %? \n#+begin_src
                  %^{Language?|emacs-lisp|sh|matlab|r|julia} \n%^C\n#+end_src \n%a\n%U\n")
  
                  ;; matlab-note
                  ("cu" "entry to matlab_notes"
                     entry (file+headline "~/comp_science/matlab_notes.org" "Captured
                  notes")
                     "* %? \n#+begin_src
                  %^{Language?|emacs-lisp|sh|matlab|r|julia} \n%^C\n#+end_src \n%a\n%U\n")
  
                  )
           )
        )
)

(setq org-capture-templates
   (append org-capture-templates
      (quote (
                ("t" "Templates for tasks")
                
                ; TODO entry, inactive timestamp, heading needs to be inserted, manual scheduling
                ("tt" "task, manual scheduling"
                   entry (file "~/org/refile.org")
                   "* TODO %? \nAdded: %U\n"
                   :clock-in t :clock-resume t) 
                
                ;; TODO entry, inactive timestamp, prompt for tag, heading needs to be inserted 
                ("tT" "task, tag prompt"
                   entry (file "~/org/refile.org")
                   "* TODO %? %^G \nAdded: %U\n"
                   :clock-in t :clock-resume t)

                ;; TODO entry, inactive timestamp, prompt for yanking
                ("tk" "task with yanking" entry (file "~/org/refile.org") 
                   "* TODO %? %^G \n%^C\nAdded: %U\n"
                   :clock-in t :clock-resume t)
                
                ;; TODO entry, inactive timestamp, prompt for tag and clipboard entry
                ("tK" "task with tag and yanking"
                   entry (file "~/org/refile.org")
                   "* TODO %? %^G \n%^C\nAdded: %U\n"
                   :clock-in t :clock-resume t)
                
                ;; New research project: create project heading in todo.org under research projects
                ;; includes: link to file, timestamp, prompt for project tag as property %^{TAGS}p
                                        ; project related tasks with link to origin
                ("tP" "project with tag, automatic source"
                   entry (file+headline "~/org/todo.org" "Research")
                   "* NEXT %? %^{TAGS}p \n%a\nAdded: %U\n %?"
                   :clock-in t :clock-resume t)

                
                ("tp" "project task"
                   entry (file+headline "~/org/todo.org" "Research")
                   "* NEXT %? %^{TAGS}p \nAdded: %U\n %?"
                   :clock-in t :clock-resume t)         
                ;; write function to shift todo tasks from project file to agenda ! 
                
                ("tb" "buy item" entry (file "~/org/refile.org")
                   "* TODO %? %^{Arcade or shop?}g   \n%U\n"
                   :clock-in t :clock-resume t) 
                
                ("r" "email response"
                   entry (file "~/org/refile.org")
                   "* TODO Respond to %:from on %:subject :EMAIL:\n%t\n%a\n"
                   :clock-in t :clock-resume t :immediate-finish t
                   )
                
                ("h" "habit"
                   entry (file "~/org/refile.org")
                   "* NEXT %?\n%U\nSCHEDULED: %t .+1d/3d\n
:PROPERTIES:\n:STYLE: habit\n:REPEAT_TO_STATE: NEXT\n:END:\n")

                )
         )
      )
   )

(setq org-refile-use-outline-path 'file)
(setq org-outline-path-complete-in-steps t)
(setq org-refile-allow-creating-parent-nodes (quote confirm))
;; (setq org-completion-use-ido t)
;; (setq ido-everywhere t)
;; (setq ido-max-directory-size 100000)
;; (ido-mode (quote both))

;; (setq org-refile-targets
;;    (quote
;;       (("~/comp_science/bash_notes.org" :maxlevel . 4)
;;          ;;(nil :maxlevel . 3)          
;;          ;;(org-agenda-files :maxlevel . 4)

;;          ("~/comp_science/e_auto-complete_notes.org" :maxlevel . 4)         
;;          ("~/comp_science/emacs_notes.org" :maxlevel . 4)
;;                   
;;          
;;          
;;          )
;;       )
;;    )


;; refile targets
(setq org-refile-targets
   (quote
      (("~/org/todo.org" :maxlevel . 1)
         ("~/org/log.org" :maxlevel . 1)
         ("~/org/creditcard.org" :maxlevel . 1)
         ("~/comp_science/e_auto-complete_notes.org" :maxlevel . 3)
         ("~/comp_science/emacs_notes.org" :maxlevel . 3)
         ("~/comp_science/git_notes.org" :maxlevel . 3)
         ("~/comp_science/gtd_notes.org" :maxlevel . 3)
         ("~/comp_science/ubuntu_notes.org" :maxlevel . 3)
         ("~/org/annuals.org" :maxlevel . 1))))
;; (defun cg/org-refile ()
;;    (interactive)
;;    (add-file-local-variable 'org-refile-targets t)
;;    (setq org-refile-targets
;;       (quote
;;          (("~/org/todo.org" :maxlevel . 1)
;;             ("~/org/log.org" :maxlevel . 1)
;;             ("~/org/creditcard.org" :maxlevel . 1)
;;             ("~/org/annuals.org" :maxlevel . 1))))
;;    )

(setq org-mobile-directory "~/Dropbox/MobileOrg")

(custom-set-variables
   '(org-agenda-ndays 14)
   '(org-deadline-warning-days 14)
   '(org-agenda-show-all-dates t)
   '(org-agenda-skip-deadline-if-done t)
   '(org-agenda-skip-scheduled-if-done t)
   '(org-agenda-start-on-weekday nil) ; start agenda at current day
   '(org-reverse-note-order nil) ; append new nodes
   '(org-fast-tag-selection-single-key nil) ; you have to press RET to exit tag menu 
   )
(org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)
       (sh . t)
       (R . t)))
