
(add-to-list 'gnus-secondary-select-methods '(nnimap "gmx"
                                  (nnimap-address "imap.gmx.com")
                                  (nnimap-server-port 993)
                                  (nnimap-stream ssl)))

(add-to-list 'gnus-secondary-select-methods '(nnimap "lrz"
                                  (nnimap-address "mailin.lrz.de")
                                  (nnimap-server-port 993)
                                  (nnimap-stream ssl)))

(add-to-list 'gnus-secondary-select-methods '(nnimap "gmail"
                                  (nnimap-address "imap.gmail.com")
                                  (nnimap-server-port 993)
                                  (nnimap-stream ssl)))

(setq gnus-check-new-newgroups nil)

(setq gnus-save-newsrc-file nil)

(setq gnus-large-newsgroup 5000)

(setq gnus-auto-select-first 'unseen)

(setq imap-ping-interval (* 10 60))
(setq imap-ping-timer nil)

(defun imap-ping-handler ()
  ;; ping all active IMAP servers in `nnimap-server-buffer-alist'
  (when (boundp 'nnimap-server-buffer-alist)
    (let ((servers nil))
      (mapc
       (lambda (server-buffer)
         (let ((server (car server-buffer))
               (buffer (cadr server-buffer)))
           (when (and (get-buffer buffer) (not (member server servers)))
             (ignore-errors
               (with-local-quit
                 (with-temp-message
                     (format "Pinging %s..." server)
                   (imap-send-command-wait "NOOP" buffer)
                   (message "Pinging %s...done" server))))
             (setq servers (cons server servers)))))
       nnimap-server-buffer-alist)))

  (let* ((current (current-time))
         (timer imap-ping-timer)
         ;; compute the time when this timer will run again
         (next-time (timer-relative-time
                     (list (aref timer 1) (aref timer 2) (aref timer 3))
                     (* 5 (aref timer 4)) 0)))
    ;; if the activation time is far in the past, skip executions
    ;; until we reach a time in the future.  This avoids a long
    ;; pause if Emacs has been suspended for hours.
    (or (> (nth 0 next-time) (nth 0 current))
        (and (= (nth 0 next-time) (nth 0 current))
             (> (nth 1 next-time) (nth 1 current)))
        (and (= (nth 0 next-time) (nth 0 current))
             (= (nth 1 next-time) (nth 1 current))
             (> (nth 2 next-time) (nth 2 current)))
        (progn
          (timer-set-time timer (timer-next-integral-multiple-of-time
                                 current imap-ping-interval)
                          imap-ping-handler)
          (timer-activate timer)))))

(setq imap-ping-timer
      (run-at-time t imap-ping-interval 'imap-ping-handler))
