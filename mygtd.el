;;; mygtd -- a getting things done implementation

;; Copyright (C) 2023   Tristan de Cacqueray

;; Package-Requires: ((org-ql))

;;; Commentary:
;;;    Use "C-h v" to display variable documentations (or "C-c C-d" for symbol at point with helpful)
;;; Code:

(require 'org-ql)

;; Unfortunately org-mode does not support timestamp with timezone, so this ensure correct format on export.
(setq org-icalendar-timezone "America/New_York")

;; Files
(setq tc/org-reviews-files '("~/org/projects.org.gpg"))
(setq tc/inbox "~/org/inbox.org.gpg")
(setq tc/journal "~/org/journal.org.gpg")

;; Look for agenda item in these files
(setq-default org-agenda-files '("~/org/projects.org.gpg" "~/org/home.org.gpg" "~/org/inbox.org.gpg"))

(use-package org
  :config
  (setq-default
   ;; Tell org where are the files
   org-directory "~/org/"

   ;; Display image inline
   org-startup-with-inline-images t

   ;; Insead of "..." show "⤵" when there's hidden folded content
   ;; Some characters to choose from: …, ⤵, ▼, ↴, ⬎, ⤷, and ⋱
   org-ellipsis "⤵"

   ;; Show headings up to level 2 by default when opening an org files
   org-startup-folded 'content

   ;; Simple TODO sequence, DONE and CANCELLED are terminal
   org-todo-keywords (quote ((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d)")
                             (sequence "WAITING(w@/!)" "HOLD(h@/!)" "|" "CANCELLED(c@/!)")))
   org-todo-keyword-faces (quote (("TODO" :foreground "dark orange")
                                  ("NEXT" :foreground "blue" :weight bold)
                                  ("DONE" :foreground "forest green" :weight bold)
                                  ("WAITING" :foreground "orange" :weight bold)
                                  ("HOLD" :foreground "magenta" :weight bold)
                                  ("CANCELLED" :foreground "forest green" :weight bold)))

   ;; disable org-todo pop-up on C-c C-t
   org-use-fast-todo-selection 'expert

   ;; But don't bother with notes when using shift arrows
   org-treat-S-cursor-todo-selection-as-state-change nil

   ;; Only show one star, though this is overridden by org-bullets
   ;; org-hide-leading-stars t

   ;; Mail link description format, %c if from or to when sent by me
   org-email-link-description-format "Email %c (%d): %s"
   )

  ;; Ensure shift arrows don't execute org commands, e.g. change todo state.
  (setq-default org-support-shift-select nil)
  ;; should make windmove key work in org-mode
  (setq-default org-replace-disputed-keys t)

  ;; Enable windmove-default-keybindings in org-mode
  (add-hook 'org-shiftup-final-hook 'windmove-up)
  (add-hook 'org-shiftleft-final-hook 'windmove-left)
  (add-hook 'org-shiftdown-final-hook 'windmove-down)
  (add-hook 'org-shiftright-final-hook 'windmove-right)


  ;; Custom links
  ;; http://endlessparentheses.com/embedding-youtube-videos-with-org-mode-links.html
  (defvar yt-iframe-format
    ;; You may want to change your width and height.
    (concat "<iframe width=\"560\""
            " height=\"315\""
            " src=\"https://www.youtube.com/embed/%s\""
            " frameborder=\"0\""
            " allow=\"accelerometer; autoplay; encrypted-media; gyroscope; picture-in-picture\""
            " allowfullscreen></iframe>")


    )
  (org-add-link-type
   "yt"
   (lambda (handle)
     (browse-url
      (concat "https://www.youtube.com/embed/"
              handle)))
   (lambda (path desc backend)
     (cl-case backend
       (md (format yt-iframe-format   path (or desc "")))
       (html (format yt-iframe-format path (or desc "")))
       (latex (format "\href{%s}{%s}" path (or desc "video"))))))
  )


(use-package org-capture
  :bind ("C-c c" . org-capture)
  :demand
  :config
  (setq
   ;; Use any org-agendas file as refile target, only first level
   org-refile-targets '((org-agenda-files :maxlevel . 1))
   ;; Use full outline paths for refile targets
   org-refile-use-outline-path t
   ;; Targets complete directly
   org-outline-path-complete-in-steps nil
   )
  (setq org-refile-use-cache nil)
  (setq org-capture-templates
        '(
          ("t" "todo" entry (file tc/inbox)
           "* TODO %?\n/Entered on/ %U\n")
          ("d" "done" entry (file tc/inbox)
           "* DONE %?\nCLOSED: %U\n")
          ("j" "Journal" entry (file+olp+datetree tc/journal)
           "* %?\n")
          ))

  (define-key global-map (kbd "<f5>") (lambda () (interactive) (org-capture nil "t")))

  (defun tc/org-capture-done ()
    (interactive)
    (org-capture nil "d"))

  (define-key global-map (kbd "<f8>") 'tc/org-capture-done)

  (defun tc/org-capture-journal ()
    "Capture a journal item"
    (interactive)
    (org-capture nil "j"))
  (define-key global-map (kbd "C-c j") 'tc/org-capture-journal)
  )

;; example agenda config
(defun tc/org-agenda-show-agenda-and-todo (&optional arg)
  (interactive "P")
  (org-agenda arg "a"))

(use-package org-agenda
  :bind ("<f12>"   . tc/org-agenda-show-agenda-and-todo)
  :config
  (define-key global-map (kbd "C-c a") 'org-agenda)
  (setq
   ;; Start agenda at today
   org-agenda-start-on-weekday nil
   ;; Do not dim blocked tasks
   org-agenda-dim-blocked-tasks nil
   ;; Compact the block agenda view
   org-agenda-compact-blocks t
   ))

;; gtd settings from [[file:/srv/github.com/rougier/emacs-GTD/GTD.org]]
(setq-default org-agenda-hide-tags-regexp ".")
(setq-default org-agenda-prefix-format
              '((agenda . " %i %-12:c%?-12t% s")
                (todo   . " ")
                (tags   . " %i %-12:c")
                (search . " %i %-12:c")))

(setq org-habit-following-days 7
      org-habit-preceding-days 35
      org-habit-show-habits t)

;; Toggle to show all habits ;; todo: make that into a view
(setq org-habit-show-all-today nil)

(setq-default org-agenda-custom-commands
              '(("g" "Get Things Done (GTD)"
                 ((agenda ""
                          ((org-agenda-skip-function '(org-agenda-skip-entry-if 'deadline))
                           (org-deadline-warning-days 0)))
                  (todo "NEXT"
                        ((org-agenda-skip-function '(org-agenda-skip-entry-if 'deadline))
                         (org-agenda-prefix-format "  %i %-12:c [%e] ")
                         (org-agenda-overriding-header "\nTasks\n")))
                  (tags-todo "inbox"
                             ((org-agenda-prefix-format "  %?-12t% s")
                              (org-agenda-overriding-header "\nInbox\n")))
                  (tags "CLOSED>=\"<today>\""
                        ((org-agenda-overriding-header "\nCompleted today\n")))))))

;; f4 shows the gtd view
(define-key global-map (kbd "<f4>") (lambda () (interactive) (org-agenda nil "g")))

(defun tc/get-buffer-content ()
  "return buffer or region content."
  (if (region-active-p)
      (buffer-substring-no-properties (region-beginning) (region-end))
    (buffer-substring-no-properties 1 (buffer-size))
    )
  )

(defun s-unlines (xs)
  "The inverse of 's-lines'."
  (s-join "\n" xs))

;;; remove elem when f return true
(defun tc/drop (f xs)
  "The inverse of 'seq-filter'."
  (seq-filter (lambda (s) (not (funcall f s))) xs))

(progn
  "First attempt at creating daily report by processing the agenda view buffer"
  (defun tc/get-done (line)
    "Match closed line and return a formated string"
    (if-let ((show (lambda (cat ev) (s-join " " (list "**" (s-pad-left 13 " " cat) "-" (tc/remove-links ev)))))
             (match (s-match-strings-all "^ *\\([^:]+\\): .* Closed: .* DONE \\(.*\\)$" line))
             )
        (apply show (cdr (car match)))
      )
    )
  (defun tc/is-org-date (line)  (s-match "[A-Z][a-z]+ *[0-9]+ [A-Z][a-z]+ [0-9]+.*" line))

  (defun tc/process-agenda (line) (if (tc/is-org-date line) line (tc/get-done line)))

  (defun tc/daily-report ()
    "Produce a report for team daily"
    (let* (
           (lines (s-lines (tc/get-buffer-content)))
           (events (tc/drop 's-blank? (mapcar 'tc/process-agenda lines)))
           )
      (message (s-unlines events))
      )
    )
  )

(defun tc/todo-list ()
  "Improve org-todo-list."
  (interactive)
  (org-ql-search tc/org-reviews-files '(or (todo "WAITING") (todo "TODO"))
    :super-groups '((:auto-category))))

(defun tc/remove-links (s)
  (s-trim (s-replace-regexp "\\[\\[.*\\]\\]" "" s)))

(defun tc/get-cat (properties)
  (or (org-entry-get (plist-get properties :org-marker) "ARCHIVE_CATEGORY")
      (org-entry-get (plist-get properties :org-marker) "CATEGORY")))

(defun tc/daily-format-item (item)
  "Format 'org-ql-select' output. ITEM is a prop list."
  (let* ((properties (cadr item))
         (title (plist-get properties :raw-value))
         (status (plist-get properties :todo-keyword))
         (status-str (if (string= status "DONE") "" (concat status ": ")))
         (category (tc/get-cat properties))
         )
    (format "- %s - %s%s" (s-pad-left 9 " " category) status-str (tc/remove-links title))))

(defun tc/daily-format (items)
  "Format all ITEMS."
  (let ((today (format-time-string "*** %Y-%m-%d %A"))
        (report (mapcar 'tc/daily-format-item items)))
    (format "%s\n%s:\n%s" today "tdecacqu" (s-unlines report))))

(defun tc/mk-next-meeting-query ()
  "The next meeting query."
  '(and (not (done)) (not (habit)) (scheduled :from ,(ts-now))))

(defun tc/get-next-meeting ()
  (org-ql-search org-agenda-files (tc/mk-next-meeting-query)
    :sort '(date)))

(defun tc/sched-format (item)
  (let* ((properties (cadr item))
         (title (plist-get properties :raw-value))
         (scheduled (plist-get properties :scheduled))
         (ts (format-time-string "%FT%T%z" (org-timestamp-to-time scheduled)))
         )
    (format "%s %s" ts (tc/remove-links title))))

(defun tc/render-org-next-events ()
  (let* ((entries (org-ql-select org-agenda-files (tc/mk-next-meeting-query)
                    :action 'element-with-markers
                    :sort 'tc/compare-entry
                    ))
         (report (s-unlines (reverse (mapcar 'tc/sched-format entries)))))
    (f-write-text report 'utf-8 "~/.local/share/gnome-org-next-schedule/events")
    )
  )

(defun tc/mk-daily-query ()
  "The daily query."
  ;; TODO: make that one day during the week
  '(and (or (todo "DONE") (todo "NEXT")) (ts :from -3 :to today)))

(defun tc/mk-review-query ()
  "The review query."
  '(and (or (todo "DONE") (todo "NEXT")) (ts :from -21 :to today)))

(defun tc/show-daily-report ()
  "Show daily report."
  (interactive)
  (org-ql-search tc/org-reviews-files (tc/mk-daily-query)
    :super-groups '((:auto-ts t))))

(defun tc/compare-entry (b a)
  "Order entry A and B so that they appears from newest to oldest.
This is like org-ql--date< but considering closed date too."
  (cl-macrolet ((ts (item)
                  `(or (org-element-property :closed ,item)
                       (org-element-property :deadline ,item)
                       (org-element-property :scheduled ,item))))
    (org-ql--org-timestamp-element< (ts a) (ts b))))

(defun tc/compare-cat-entry (a b)
  (cl-macrolet ((cat (item)
                  `(or
                    (org-element-property :category ,item))))
    (string< (cat a) (cat b))))


;; TODO: append the report to the journal (e.g. running org-capture "j")
(defun tc/mk-daily-report ()
  "Produce a report for team daily."
  (interactive)
  (let* ((entries (org-ql-select tc/org-reviews-files
                    (tc/mk-daily-query)
                    :action 'element-with-markers
                    :sort 'tc/compare-entry
                    ))
         (report (tc/daily-format entries))
         (*buffer* (get-buffer-create "*tc/daily*")))
    (with-current-buffer *buffer*
      (erase-buffer)
      (insert report)
      (set-text-properties (point-min) (point-max) nil)
      ;; move the cursor at begining of entries
      (goto-char (point-min))
      (forward-line 2)
      (cl-dolist (window (get-buffer-window-list nil nil t))
        (set-window-point window (point)))
      )
    (switch-to-buffer-other-window *buffer*)
    )
  )

(defun tc/show-review-report ()
  "Show report report."
  (interactive)
  (org-ql-search tc/org-reviews-files (tc/mk-review-query)
    :super-groups '((:auto-category))))

(defun tc/monthly-format-item (acc item)
  "Format an entry for the review.
ACC is tuple of current content and category string.
ITEM is an org entry."
  (let* ((properties (cadr item))
         (prev-cat (cadr acc))
         (content (car acc))
         (category (tc/get-cat properties))
         (cat-sep (if (string= category prev-cat) ""
                    (format "%s\n# %s" (if prev-cat "\n" "") category)))
         (title (plist-get properties :raw-value)))
    (list (format "%s%s\n- %s" content cat-sep (tc/remove-links title)) category)))

(defun tc/monthly-format (items)
  "Format all ITEMS."
  (let ((report (car (seq-reduce 'tc/monthly-format-item items '("" nil)))))
    ;; todo: compute the date of 3 weeks ago
    (format "**** Sprint review (from )\n%s" report)))

(defun tc/mk-review-report ()
  "Produce a report for team review."
  (interactive)
  (let* ((entries (org-ql-select tc/org-reviews-files
                    (tc/mk-review-query)
                    :action 'element-with-markers
                    :sort 'tc/compare-cat-entry
                    ))
         (report (tc/monthly-format entries))
         (*buffer* (get-buffer-create "*tc/review*")))
    (with-current-buffer *buffer*
      (erase-buffer)
      (insert report)
      (set-text-properties (point-min) (point-max) nil)
      ;; move the cursor at begining of entries
      (goto-char (point-min))
      (forward-line 2)
      (cl-dolist (window (get-buffer-window-list nil nil t))
        (set-window-point window (point)))
      )
    (switch-to-buffer-other-window *buffer*)
    )
  )

;; From anywhere, f6 shows the daily report
(define-key global-map (kbd "<f6>") 'tc/show-daily-report)

;; In the daily report calendar view, f6 format the report
(define-key org-agenda-mode-map (kbd "<f6>") 'tc/mk-daily-report)

(provide 'mygtd)
